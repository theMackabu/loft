use crate::ast::{Expr, Stmt};
use crate::lexer::{Lexer, Location, Token, TokenInfo};

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
            Token::Fn => self.parse_function_statement(),
            Token::Return => self.parse_return_statement(),

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
            self.advance();
            match &self.current.token {
                Token::Identifier(typ) => {
                    let typ = typ.clone();
                    self.advance();
                    Some(typ)
                }
                _ => {
                    return Err(ParseError::ExpectedIdentifier {
                        location: self.current.location.to_owned(),
                    })
                }
            }
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

    fn parse_return_statement(&mut self) -> Result<Stmt, ParseError> {
        self.advance(); // consume 'return'

        let value = if self.current.token == Token::Semicolon { None } else { Some(self.parse_expression(0)?) };

        self.expect(Token::Semicolon)?;

        Ok(Stmt::Return(value))
    }

    fn parse_let_statement(&mut self) -> Result<Stmt, ParseError> {
        self.advance(); // consume 'let'

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
            match &self.current.token {
                Token::Identifier(typ) => {
                    let typ = typ.clone();
                    self.advance();
                    Some(typ)
                }
                _ => {
                    return Err(ParseError::ExpectedIdentifier {
                        location: self.current.location.to_owned(),
                    })
                }
            }
        } else {
            None
        };

        self.expect(Token::Assign)?;
        let initializer = Box::new(self.parse_expression(0)?);
        self.expect(Token::Semicolon)?;

        Ok(Stmt::Let { name, type_annotation, initializer })
    }

    fn parse_expression(&mut self, precedence: i32) -> Result<Expr, ParseError> {
        let mut left = match &self.current.token {
            Token::If => self.parse_if_expression(),

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

            Token::Identifier(name) => {
                let name = name.clone();
                self.advance();
                Ok(Expr::Identifier(name))
            }

            Token::LeftParen => {
                self.advance();
                let expr = self.parse_expression(0)?;
                self.expect(Token::RightParen)?;
                Ok(expr)
            }

            _ => {
                return Err(ParseError::ExpectedIdentifier {
                    location: self.current.location.to_owned(),
                })
            }
        }?;

        while precedence < self.get_precedence(&self.current.token) {
            left = match &self.current.token {
                Token::Plus | Token::Minus | Token::Star | Token::Slash => {
                    let operator = self.current.token.clone();
                    self.advance();
                    let right = self.parse_expression(self.get_precedence(&operator))?;
                    Ok(Expr::Binary {
                        left: Box::new(left),
                        operator,
                        right: Box::new(right),
                    })
                }
                _ => Ok(left),
            }?;
        }

        Ok(left)
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

        while self.current.token != Token::RightBrace {
            statements.push(self.parse_statement()?);
        }

        let value = if let Some(Stmt::ExpressionValue(expr)) = statements.pop() { Some(Box::new(expr)) } else { None };

        self.expect(Token::RightBrace)?;
        Ok(Expr::Block { statements, value })
    }

    // use for later
    fn peek_token(&self) -> Token { self.lexer.to_owned().next_token().token }

    fn get_precedence(&self, token: &Token) -> i32 {
        match token {
            Token::Plus | Token::Minus => 1,
            Token::Star | Token::Slash => 2,
            _ => 0,
        }
    }
}
