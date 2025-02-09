use crate::ast::{Expr, Stmt};
use crate::lexer::{Lexer, Token};

#[derive(Debug)]
pub enum ParseError {
    UnexpectedToken(Token),
    ExpectedIdentifier,
    ExpectedExpression,
    Custom(String),
}

pub struct Parser {
    lexer: Lexer,
    current_token: Token,
}

impl Parser {
    pub fn new(mut lexer: Lexer) -> Self {
        let current_token = lexer.next_token();
        Self { lexer, current_token }
    }

    fn advance(&mut self) { self.current_token = self.lexer.next_token(); }

    fn expect(&mut self, token: Token) -> Result<(), ParseError> {
        if self.current_token == token {
            self.advance();
            Ok(())
        } else {
            Err(ParseError::UnexpectedToken(self.current_token.clone()))
        }
    }

    pub fn parse_program(&mut self) -> Result<Vec<Stmt>, ParseError> {
        let mut statements = Vec::new();

        while self.current_token != Token::EOF {
            statements.push(self.parse_statement()?);
        }

        Ok(statements)
    }

    fn parse_statement(&mut self) -> Result<Stmt, ParseError> {
        match self.current_token {
            Token::Let => self.parse_let_statement(),
            Token::Fn => self.parse_function_statement(),
            Token::Return => self.parse_return_statement(),

            _ => {
                let expr = self.parse_expression(0)?;
                self.expect(Token::Semicolon)?;
                Ok(Stmt::Expression(expr))
            }
        }
    }

    fn parse_function_statement(&mut self) -> Result<Stmt, ParseError> {
        self.advance(); // consume 'fn'

        let name = match &self.current_token {
            Token::Identifier(name) => {
                let name = name.clone();
                self.advance();
                name
            }
            _ => return Err(ParseError::ExpectedIdentifier),
        };

        self.expect(Token::LeftParen)?;

        // parse parameters
        let mut params = Vec::new();
        while self.current_token != Token::RightParen {
            if !params.is_empty() {
                self.expect(Token::Comma)?;
            }

            let param_name = match &self.current_token {
                Token::Identifier(name) => {
                    let name = name.clone();
                    self.advance();
                    name
                }
                _ => return Err(ParseError::ExpectedIdentifier),
            };

            self.expect(Token::Colon)?;

            let param_type = match &self.current_token {
                Token::Identifier(typ) => {
                    let typ = typ.clone();
                    self.advance();
                    typ
                }
                _ => return Err(ParseError::ExpectedIdentifier),
            };

            params.push((param_name, param_type));
        }

        self.expect(Token::RightParen)?;

        // parse return type
        let return_type = if self.current_token == Token::Arrow {
            self.advance();
            match &self.current_token {
                Token::Identifier(typ) => {
                    let typ = typ.clone();
                    self.advance();
                    Some(typ)
                }
                _ => return Err(ParseError::ExpectedIdentifier),
            }
        } else {
            None
        };

        // parse function body
        self.expect(Token::LeftBrace)?;

        let mut body = Vec::new();
        while self.current_token != Token::RightBrace {
            body.push(self.parse_statement()?);
        }

        self.expect(Token::RightBrace)?;

        Ok(Stmt::Function { name, params, return_type, body })
    }

    fn parse_return_statement(&mut self) -> Result<Stmt, ParseError> {
        self.advance(); // consume 'return'

        let value = if self.current_token == Token::Semicolon { None } else { Some(self.parse_expression(0)?) };

        self.expect(Token::Semicolon)?;

        Ok(Stmt::Return(value))
    }

    fn parse_let_statement(&mut self) -> Result<Stmt, ParseError> {
        self.advance(); // consume 'let'

        let name = match &self.current_token {
            Token::Identifier(name) => {
                let name = name.clone();
                self.advance();
                name
            }
            _ => return Err(ParseError::ExpectedIdentifier),
        };

        let type_annotation = if self.current_token == Token::Colon {
            self.advance();
            match &self.current_token {
                Token::Identifier(typ) => {
                    let typ = typ.clone();
                    self.advance();
                    Some(typ)
                }
                _ => return Err(ParseError::ExpectedIdentifier),
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
        let mut left = match &self.current_token {
            Token::If => self.parse_if_expression(),

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
            _ => Err(ParseError::ExpectedExpression),
        }?;

        while precedence < self.get_precedence(&self.current_token) {
            left = match &self.current_token {
                Token::Plus | Token::Minus | Token::Star | Token::Slash => {
                    let operator = self.current_token.clone();
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

        let else_branch = if self.current_token == Token::Else {
            self.advance();

            if self.current_token == Token::If {
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

    fn parse_block_expression(&mut self) -> Result<Expr, ParseError> {
        let mut statements = Vec::new();
        let mut value = None;

        while self.current_token != Token::RightBrace {
            if self.peek_token() == Token::RightBrace && self.current_token != Token::Semicolon {
                value = Some(Box::new(self.parse_expression(0)?));
                break;
            }

            statements.push(self.parse_statement()?);
        }

        self.expect(Token::RightBrace)?;

        Ok(Expr::Block { statements, value })
    }

    fn peek_token(&self) -> Token { self.lexer.to_owned().next_token() }

    fn get_precedence(&self, token: &Token) -> i32 {
        match token {
            Token::Plus | Token::Minus => 1,
            Token::Star | Token::Slash => 2,
            _ => 0,
        }
    }
}
