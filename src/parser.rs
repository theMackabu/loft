use crate::ast::{Expr, Stmt, Type};
use crate::lexer::{Lexer, Location, Token, TokenInfo};

const PRECEDENCE_LOWEST: i32 = 0;
const PRECEDENCE_EQUALS: i32 = 1; // ==, !=
const PRECEDENCE_COMPARE: i32 = 2; // >, >=, <, <=
const PRECEDENCE_OR: i32 = 3; // ||
const PRECEDENCE_AND: i32 = 4; // &&
const PRECEDENCE_SUM: i32 = 5; // +, -
const PRECEDENCE_PRODUCT: i32 = 6; // *, /
const PRECEDENCE_PREFIX: i32 = 7; // -X, !X
const PRECEDENCE_CALL: i32 = 8; // function(X)

#[derive(Debug)]
pub enum ParseError {
    UnexpectedToken { found: TokenInfo, expected: Option<String> },
    ExpectedIdentifier { location: Location },
    ExpectedExpression { location: Location },
    Custom { message: String, location: Location },
}

impl std::fmt::Display for ParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ParseError::UnexpectedToken { found, expected } => {
                if let Some(expected) = expected {
                    write!(
                        f,
                        "Error at line {}, column {}: Expected {}, but found {:?}",
                        found.location.line, found.location.column, expected, found.token
                    )
                } else {
                    write!(f, "Error at line {}, column {}: Unexpected token {:?}", found.location.line, found.location.column, found.token)
                }
            }
            ParseError::ExpectedIdentifier { location } => {
                write!(f, "Error at line {}, column {}: Expected identifier", location.line, location.column)
            }
            ParseError::ExpectedExpression { location } => {
                write!(f, "Error at line {}, column {}: Expected expression", location.line, location.column)
            }
            ParseError::Custom { message, location } => {
                write!(f, "Error at line {}, column {}: {}", location.line, location.column, message)
            }
        }
    }
}

pub struct Parser {
    lexer: Lexer,
    current: TokenInfo,
}

impl Parser {
    pub fn new(mut lexer: Lexer) -> Self {
        let current = lexer.next_token();
        Self { lexer, current }
    }

    fn advance(&mut self) { self.current = self.lexer.next_token(); }

    fn expect(&mut self, token: Token) -> Result<(), ParseError> {
        if self.current.token == token {
            self.advance();
            Ok(())
        } else {
            Err(ParseError::UnexpectedToken {
                found: self.current.clone(),
                expected: Some(format!("{:?}", token)),
            })
        }
    }

    pub fn parse_program(&mut self) -> Result<Vec<Stmt>, ParseError> {
        let mut statements = Vec::new();

        while self.current.token != Token::EOF {
            statements.push(self.parse_statement()?);
        }

        Ok(statements)
    }

    fn parse_statement(&mut self) -> Result<Stmt, ParseError> {
        match self.current.token {
            Token::Let => self.parse_let_statement(),
            Token::Const => self.parse_const_statement(),
            Token::Fn => self.parse_function_statement(),
            Token::Return => self.parse_return_statement(),
            Token::Type => self.parse_type_alias(),

            Token::If => {
                let expr = self.parse_if_expression()?;
                Ok(Stmt::ExpressionValue(expr))
            }

            _ => {
                let expr = self.parse_expression(0)?;

                match self.current.token {
                    Token::RightBrace => Ok(Stmt::ExpressionValue(expr)),

                    Token::Semicolon => {
                        self.advance();
                        Ok(Stmt::ExpressionStmt(expr))
                    }

                    _ => Err(ParseError::UnexpectedToken {
                        found: self.current.clone(),
                        expected: Some("';' or '}'".to_string()),
                    }),
                }
            }
        }
    }

    fn parse_function_statement(&mut self) -> Result<Stmt, ParseError> {
        self.advance(); // consume 'fn'

        let name = match &self.current.token {
            Token::Identifier(name) => {
                let name = name.clone();
                self.advance();
                name
            }
            _ => {
                return Err(ParseError::ExpectedIdentifier {
                    location: self.current.location.to_owned(),
                })
            }
        };

        self.expect(Token::LeftParen)?;

        // parse parameters
        let mut params = Vec::new();
        while self.current.token != Token::RightParen {
            if !params.is_empty() {
                self.expect(Token::Comma)?;
            }

            let param_name = match &self.current.token {
                Token::Identifier(name) => {
                    let name = name.clone();
                    self.advance();
                    name
                }
                _ => {
                    return Err(ParseError::ExpectedIdentifier {
                        location: self.current.location.to_owned(),
                    })
                }
            };

            self.expect(Token::Colon)?;

            let param_type = match &self.current.token {
                Token::Identifier(typ) => {
                    let typ = typ.clone();
                    self.advance();
                    typ
                }
                _ => {
                    return Err(ParseError::ExpectedIdentifier {
                        location: self.current.location.to_owned(),
                    })
                }
            };

            params.push((param_name, param_type));
        }

        self.expect(Token::RightParen)?;

        // parse return type
        let return_type = if self.current.token == Token::Arrow {
            self.advance(); // consume ->
            Some(self.parse_type()?)
        } else {
            None
        };

        // parse function body
        self.expect(Token::LeftBrace)?;

        let mut body = Vec::new();
        while self.current.token != Token::RightBrace {
            body.push(self.parse_statement()?);
        }

        self.expect(Token::RightBrace)?;

        Ok(Stmt::Function { name, params, return_type, body })
    }

    fn parse_type_alias(&mut self) -> Result<Stmt, ParseError> {
        self.advance(); // consume 'type'

        let name = self.expect_identifier()?;

        let type_params = if self.current.token == Token::LeftAngle {
            self.advance();
            let params = self.parse_type_params()?;
            self.expect(Token::RightAngle)?;
            params
        } else {
            Vec::new()
        };

        self.expect(Token::Assign)?;

        let ty = self.parse_type()?;
        self.expect(Token::Semicolon)?;

        Ok(Stmt::TypeAlias { name, type_params, ty })
    }

    fn parse_type(&mut self) -> Result<Type, ParseError> {
        let name = self.expect_identifier()?;

        if self.current.token == Token::LeftAngle {
            self.advance();
            let mut type_params = Vec::new();

            while self.current.token != Token::RightAngle {
                if !type_params.is_empty() {
                    self.expect(Token::Comma)?;
                }
                type_params.push(self.parse_type()?);
            }

            self.expect(Token::RightAngle)?;

            Ok(Type::Generic { name, type_params })
        } else {
            Ok(Type::Simple(name))
        }
    }

    fn parse_type_params(&mut self) -> Result<Vec<String>, ParseError> {
        let mut params = Vec::new();

        while self.current.token != Token::RightAngle {
            if !params.is_empty() {
                self.expect(Token::Comma)?;
            }
            params.push(self.expect_identifier()?);
        }

        Ok(params)
    }

    fn expect_identifier(&mut self) -> Result<String, ParseError> {
        match &self.current.token {
            Token::Identifier(name) => {
                let name = name.clone();
                self.advance();
                Ok(name)
            }
            Token::Type => {
                self.advance();
                Ok("type".to_string())
            }
            _ => Err(ParseError::ExpectedIdentifier {
                location: self.current.location.to_owned(),
            }),
        }
    }

    fn parse_return_statement(&mut self) -> Result<Stmt, ParseError> {
        self.advance(); // consume 'return'

        let value = if self.current.token == Token::Semicolon { None } else { Some(self.parse_expression(0)?) };

        self.expect(Token::Semicolon)?;

        Ok(Stmt::Return(value))
    }

    fn parse_let_statement(&mut self) -> Result<Stmt, ParseError> {
        self.advance(); // consume 'let'

        let mutable = if self.current.token == Token::Mut {
            self.advance(); // consume 'mut'
            true
        } else {
            false
        };

        let name = match &self.current.token {
            Token::Identifier(name) => {
                let name = name.clone();
                self.advance();
                name
            }
            _ => {
                return Err(ParseError::ExpectedIdentifier {
                    location: self.current.location.to_owned(),
                })
            }
        };

        let type_annotation = if self.current.token == Token::Colon {
            self.advance();
            Some(self.parse_type()?)
        } else {
            None
        };

        self.expect(Token::Assign)?;
        let initializer = Box::new(self.parse_expression(0)?);
        self.expect(Token::Semicolon)?;

        Ok(Stmt::Let {
            name,
            mutable,
            type_annotation,
            initializer,
        })
    }

    fn parse_const_statement(&mut self) -> Result<Stmt, ParseError> {
        self.advance(); // consume 'const'

        let name = match &self.current.token {
            Token::Identifier(name) => {
                let name = name.clone();
                self.advance();
                name
            }
            _ => {
                return Err(ParseError::ExpectedIdentifier {
                    location: self.current.location.to_owned(),
                })
            }
        };

        let type_annotation = if self.current.token == Token::Colon {
            self.advance();
            Some(self.parse_type()?)
        } else {
            None
        };

        self.expect(Token::Assign)?;
        let initializer = Box::new(self.parse_expression(0)?);
        self.expect(Token::Semicolon)?;

        Ok(Stmt::Const { name, type_annotation, initializer })
    }

    fn parse_expression(&mut self, precedence: i32) -> Result<Expr, ParseError> {
        let mut left = self.parse_prefix()?;

        while precedence < self.get_precedence(&self.current.token) {
            left = self.parse_infix(left)?;
        }

        Ok(left)
    }

    fn parse_prefix(&mut self) -> Result<Expr, ParseError> {
        match &self.current.token {
            Token::If => self.parse_if_expression(),

            Token::Minus | Token::Not => {
                let operator = self.current.token.clone();
                self.advance();
                let operand = Box::new(self.parse_expression(PRECEDENCE_PREFIX)?);
                Ok(Expr::Unary { operator, operand })
            }

            Token::LeftBrace => {
                self.advance();
                self.parse_block_expression()
            }

            Token::Integer(n) => {
                let n = *n;
                self.advance();
                Ok(Expr::Integer(n))
            }

            Token::String(s) => {
                let s = s.clone();
                self.advance();
                Ok(Expr::String(s))
            }

            Token::True => {
                self.advance();
                Ok(Expr::Boolean(true))
            }

            Token::False => {
                self.advance();
                Ok(Expr::Boolean(false))
            }

            Token::LeftParen => {
                self.advance();
                let expr = self.parse_expression(0)?;
                self.expect(Token::RightParen)?;
                Ok(expr)
            }

            Token::Identifier(name) => {
                let name = name.clone();
                self.advance();

                match self.current.token {
                    Token::Assign => {
                        self.advance(); // consume '='
                        let value = Box::new(self.parse_expression(0)?);
                        Ok(Expr::Assignment { target: name, value })
                    }
                    Token::LeftParen => {
                        self.advance(); // consume (
                        let mut arguments = Vec::new();

                        //  arguments
                        while self.current.token != Token::RightParen {
                            if !arguments.is_empty() {
                                self.expect(Token::Comma)?;
                            }

                            arguments.push(self.parse_expression(0)?);
                        }

                        self.expect(Token::RightParen)?;

                        Ok(Expr::Call {
                            function: Box::new(Expr::Identifier(name)),
                            arguments,
                        })
                    }
                    _ => Ok(Expr::Identifier(name)),
                }
            }

            _ => {
                return Err(ParseError::ExpectedIdentifier {
                    location: self.current.location.to_owned(),
                })
            }
        }
    }

    fn parse_infix(&mut self, left: Expr) -> Result<Expr, ParseError> {
        match &self.current.token {
            Token::Plus
            | Token::Minus
            | Token::Star
            | Token::Slash
            | Token::Equals
            | Token::NotEquals
            | Token::LeftAngle
            | Token::RightAngle
            | Token::LessEquals
            | Token::GreaterEquals
            | Token::And
            | Token::Or => {
                let operator = self.current.token.clone();
                let precedence = self.get_precedence(&operator);
                self.advance();
                let right = self.parse_expression(precedence)?;
                Ok(Expr::Binary {
                    left: Box::new(left),
                    operator,
                    right: Box::new(right),
                })
            }

            _ => Ok(left),
        }
    }

    fn parse_if_expression(&mut self) -> Result<Expr, ParseError> {
        self.advance();

        let condition = Box::new(self.parse_expression(0)?);

        self.expect(Token::LeftBrace)?;
        let then_branch = Box::new(self.parse_block_expression()?);

        let else_branch = if self.current.token == Token::Else {
            self.advance();

            if self.current.token == Token::If {
                Some(Box::new(self.parse_if_expression()?))
            } else {
                self.expect(Token::LeftBrace)?;
                Some(Box::new(self.parse_block_expression()?))
            }
        } else {
            None
        };

        Ok(Expr::If { condition, then_branch, else_branch })
    }

    // { block }
    fn parse_block_expression(&mut self) -> Result<Expr, ParseError> {
        let mut statements = Vec::new();
        let mut returns = false;

        while self.current.token != Token::RightBrace {
            let stmt = self.parse_statement()?;

            if matches!(stmt, Stmt::Return(_)) {
                returns = true;
            }

            statements.push(stmt);

            if returns {
                // warn about unreachable code
                break;
            }
        }

        let value = if !returns && !statements.is_empty() {
            if let Some(Stmt::ExpressionValue(expr)) = statements.pop() {
                Some(Box::new(expr))
            } else {
                None
            }
        } else {
            None
        };

        self.expect(Token::RightBrace)?;

        Ok(Expr::Block { statements, value, returns })
    }

    fn get_precedence(&self, token: &Token) -> i32 {
        match token {
            Token::Equals | Token::NotEquals => PRECEDENCE_EQUALS,
            Token::LeftAngle | Token::RightAngle | Token::LessEquals | Token::GreaterEquals => PRECEDENCE_COMPARE,
            Token::Or => PRECEDENCE_OR,
            Token::And => PRECEDENCE_AND,
            Token::Plus | Token::Minus => PRECEDENCE_SUM,
            Token::Star | Token::Slash => PRECEDENCE_PRODUCT,
            _ => PRECEDENCE_LOWEST,
        }
    }
}
