use crate::ast::*;
use crate::lexer::*;

use std::collections::HashSet;

enum DeclarationKind {
    Const,
    Static,
}

const MAX_RECURSION_DEPTH: i32 = 500;
const PRECEDENCE_LOWEST: i32 = 0;
const PRECEDENCE_EQUALS: i32 = 1;
const PRECEDENCE_COMPARE: i32 = 2;
const PRECEDENCE_OR: i32 = 3;
const PRECEDENCE_AND: i32 = 4;
const PRECEDENCE_SUM: i32 = 5;
const PRECEDENCE_PRODUCT: i32 = 6;
const PRECEDENCE_PREFIX: i32 = 7;
const PRECEDENCE_CALL: i32 = 8;
const PRECEDENCE_QUESTION: i32 = 8;
const PRECEDENCE_INDEX: i32 = 9;
const PRECEDENCE_MEMBER: i32 = 9;

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
    peek: TokenInfo,
    current: TokenInfo,
    recursion_depth: i32,
    struct_types: HashSet<String>,
}

impl Parser {
    pub fn new(mut lexer: Lexer) -> Self {
        let current = lexer.next_token();
        let peek = lexer.next_token();

        Self {
            lexer,
            current,
            peek,
            recursion_depth: 0,
            struct_types: HashSet::new(),
        }
    }

    fn register_struct(&mut self, name: String) { self.struct_types.insert(name); }

    fn is_struct_type(&self, name: &str) -> bool { self.struct_types.contains(name) }

    fn advance(&mut self) { self.current = std::mem::replace(&mut self.peek, self.lexer.next_token()); }

    fn exit_recursion(&mut self) { self.recursion_depth -= 1; }

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

    fn enter_recursion(&mut self) -> Result<(), ParseError> {
        self.recursion_depth += 1;
        if self.recursion_depth > MAX_RECURSION_DEPTH {
            return Err(ParseError::Custom {
                message: "Maximum recursion depth exceeded".to_string(),
                location: self.current.location.clone(),
            });
        }
        Ok(())
    }

    pub fn parse_program(&mut self) -> Result<Vec<Stmt>, ParseError> {
        let mut statements = Vec::new();

        while self.current.token != Token::EOF {
            statements.push(self.parse_statement()?);
        }

        Ok(statements)
    }

    fn parse_statement(&mut self) -> Result<Stmt, ParseError> {
        self.enter_recursion()?;

        let visibility = if matches!(self.current.token, Token::Pub) {
            self.advance(); // consume 'pub'
            true
        } else {
            false
        };

        let result = match self.current.token {
            Token::Let => self.parse_let_statement(),
            Token::Return => self.parse_return_statement(),

            Token::Use => self.parse_use_statement(visibility),
            Token::Type => self.parse_type_alias(visibility),
            Token::Enum => self.parse_enum_declaration(visibility),
            Token::Module => self.parse_module_statement(visibility),
            Token::Struct => self.parse_struct_declaration(visibility),
            Token::Fn => self.parse_function_statement(visibility),

            Token::Const => self.parse_const_static_statement(DeclarationKind::Const, visibility),
            Token::Static => self.parse_const_static_statement(DeclarationKind::Static, visibility),

            Token::LeftBrace => {
                self.advance();
                let block = self.parse_block_expression()?;
                Ok(Stmt::ExpressionStmt(block))
            }

            Token::If => {
                let expr = self.parse_if_expression()?;
                Ok(Stmt::ExpressionValue(expr))
            }

            Token::Async => {
                if self.peek.token == Token::Fn {
                    self.parse_function_statement(visibility)
                } else {
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
        };

        self.exit_recursion();
        return result;
    }

    fn parse_struct_declaration(&mut self, visibility: bool) -> Result<Stmt, ParseError> {
        self.advance(); // consume 'struct'

        let name = self.expect_identifier()?;

        self.register_struct(name.clone());

        let type_params = if self.current.token == Token::LeftAngle {
            self.advance();
            let params = self.parse_type_params()?;
            self.expect(Token::RightAngle)?;
            params
        } else {
            Vec::new()
        };

        self.expect(Token::LeftBrace)?;

        let mut fields = Vec::new();

        while self.current.token != Token::RightBrace {
            if !fields.is_empty() {
                self.expect(Token::Comma)?;

                if self.current.token == Token::RightBrace {
                    break;
                }
            }

            let field_visibility = if self.current.token == Token::Pub {
                self.advance();
                true
            } else {
                false
            };

            let field_name = self.expect_identifier()?;
            self.expect(Token::Colon)?;
            let field_type = self.parse_type()?;

            fields.push((field_name, field_visibility, field_type));
        }

        self.expect(Token::RightBrace)?;

        Ok(Stmt::Struct {
            name,
            visibility,
            type_params,
            fields,
        })
    }

    fn parse_path(&mut self) -> Result<Path, ParseError> {
        let mut segments = Vec::new();

        segments.push(self.expect_identifier()?);

        while self.current.token == Token::DoubleColon {
            self.advance(); // consume '::'
            segments.push(self.expect_identifier()?);
        }

        Ok(Path { segments })
    }

    fn parse_use_statement(&mut self, visibility: bool) -> Result<Stmt, ParseError> {
        self.advance(); // consume 'use'

        let path = self.parse_use_path()?;

        let alias = if self.current.token == Token::As {
            self.advance(); // consume 'as'
            Some(self.expect_identifier()?)
        } else {
            None
        };

        self.expect(Token::Semicolon)?;

        Ok(Stmt::Use { path, alias, visibility })
    }

    fn parse_use_path(&mut self) -> Result<UsePath, ParseError> {
        let mut segments = Vec::new();

        segments.push(self.expect_identifier()?);

        while self.current.token == Token::DoubleColon {
            self.advance(); // consume '::'
            segments.push(self.expect_identifier()?);
        }

        if segments.len() == 1 {
            Ok(UsePath::Simple(segments.remove(0)))
        } else {
            Ok(UsePath::Nested(segments))
        }
    }

    fn parse_module_statement(&mut self, visibility: bool) -> Result<Stmt, ParseError> {
        self.advance(); // consume 'module'

        let name = self.expect_identifier()?;

        self.expect(Token::LeftBrace)?;

        let mut body = Vec::new();
        while self.current.token != Token::RightBrace {
            body.push(self.parse_statement()?);
        }

        self.expect(Token::RightBrace)?;

        Ok(Stmt::Module { name, visibility, body })
    }

    fn parse_function_statement(&mut self, visibility: bool) -> Result<Stmt, ParseError> {
        let is_async = if self.current.token == Token::Async {
            self.advance(); // consume 'async'
            true
        } else {
            false
        };

        self.expect(Token::Fn)?; // consume 'fn'

        let name = self.expect_identifier()?;

        // Parse generic type parameters
        let type_params = if self.current.token == Token::LeftAngle {
            self.advance(); // consume '<'
            let mut params = Vec::new();

            while self.current.token != Token::RightAngle {
                if !params.is_empty() {
                    self.expect(Token::Comma)?;

                    if self.current.token == Token::RightAngle {
                        break;
                    }
                }
                params.push(self.expect_identifier()?);
            }

            self.expect(Token::RightAngle)?;
            params
        } else {
            Vec::new()
        };

        self.expect(Token::LeftParen)?;

        let mut params = Vec::new();
        while self.current.token != Token::RightParen {
            if !params.is_empty() {
                self.expect(Token::Comma)?;

                if self.current.token == Token::RightParen {
                    break;
                }
            }

            let is_mutable = if self.current.token == Token::Mut {
                self.advance(); // consume 'mut'
                true
            } else {
                false
            };

            let param_name = self.expect_identifier()?;
            self.expect(Token::Colon)?;
            let param_type = self.parse_type()?;

            params.push((param_name, is_mutable, param_type));
        }

        self.expect(Token::RightParen)?;

        let return_type = if self.current.token == Token::Arrow {
            self.advance();
            Some(self.parse_type()?)
        } else {
            None
        };

        self.expect(Token::LeftBrace)?;

        let mut body = Vec::new();
        while self.current.token != Token::RightBrace {
            body.push(self.parse_statement()?);
        }

        self.expect(Token::RightBrace)?;

        Ok(Stmt::Function {
            name,
            visibility,
            is_async,
            type_params,
            params,
            return_type,
            body,
        })
    }

    fn parse_enum_declaration(&mut self, visibility: bool) -> Result<Stmt, ParseError> {
        self.advance(); // consume 'enum'

        let name = self.expect_identifier()?;

        let type_params = if self.current.token == Token::LeftAngle {
            self.advance();
            let params = self.parse_type_params()?;
            self.expect(Token::RightAngle)?;
            params
        } else {
            Vec::new()
        };

        self.expect(Token::LeftBrace)?;

        let mut variants = Vec::new();

        while self.current.token != Token::RightBrace {
            if !variants.is_empty() {
                self.expect(Token::Comma)?;

                if self.current.token == Token::RightBrace {
                    break;
                }
            }

            variants.push(self.parse_enum_variant()?);
        }

        self.expect(Token::RightBrace)?;

        Ok(Stmt::Enum {
            name,
            visibility,
            type_params,
            variants,
        })
    }

    fn parse_enum_variant(&mut self) -> Result<EnumVariant, ParseError> {
        let variant_name = self.expect_identifier()?;

        match self.current.token {
            Token::Comma | Token::RightBrace => Ok(EnumVariant::Simple(variant_name)),

            Token::LeftParen => {
                self.advance();
                let mut types = Vec::new();

                while self.current.token != Token::RightParen {
                    if !types.is_empty() {
                        self.expect(Token::Comma)?;
                    }
                    types.push(self.parse_type()?);
                }

                self.expect(Token::RightParen)?;
                Ok(EnumVariant::Tuple(variant_name, types))
            }

            Token::LeftBrace => {
                self.advance();
                let mut fields = Vec::new();

                while self.current.token != Token::RightBrace {
                    if !fields.is_empty() {
                        self.expect(Token::Comma)?;

                        if self.current.token == Token::RightBrace {
                            break;
                        }
                    }

                    let field_name = self.expect_identifier()?;
                    self.expect(Token::Colon)?;
                    let field_type = self.parse_type()?;

                    fields.push((field_name, field_type));
                }

                self.expect(Token::RightBrace)?;
                Ok(EnumVariant::Struct(variant_name, fields))
            }

            _ => Err(ParseError::UnexpectedToken {
                found: self.current.clone(),
                expected: Some("',', '(', or '{'".to_string()),
            }),
        }
    }

    fn parse_type_alias(&mut self, visibility: bool) -> Result<Stmt, ParseError> {
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

        Ok(Stmt::TypeAlias { name, visibility, type_params, ty })
    }

    fn parse_type(&mut self) -> Result<Type, ParseError> {
        match &self.current.token {
            Token::LeftParen => {
                self.advance();
                if self.current.token == Token::RightParen {
                    self.advance();
                    return Ok(Type::Unit);
                }
                return Err(ParseError::Custom {
                    message: "Unexpected token after '('".to_string(),
                    location: self.current.location.clone(),
                });
            }

            Token::Fn => {
                self.advance(); // consume 'fn'
                self.expect(Token::LeftParen)?;

                let mut params = Vec::new();
                while self.current.token != Token::RightParen {
                    if !params.is_empty() {
                        self.expect(Token::Comma)?;

                        if self.current.token == Token::RightParen {
                            break;
                        }
                    }
                    params.push(self.parse_type()?);
                }

                self.expect(Token::RightParen)?;
                self.expect(Token::Arrow)?;
                let return_type = Box::new(self.parse_type()?);

                Ok(Type::Function { params, return_type })
            }

            Token::Identifier(_) => {
                let path = self.parse_path()?;

                if self.current.token == Token::LeftAngle {
                    self.advance();
                    let mut type_params = Vec::new();
                    while self.current.token != Token::RightAngle {
                        if !type_params.is_empty() {
                            self.expect(Token::Comma)?;
                        }
                        match &self.current.token {
                            Token::Identifier(name) => {
                                let name = name.clone();
                                self.advance();
                                if self.current.token == Token::DoubleColon {
                                    type_params.push(self.parse_type()?);
                                } else {
                                    type_params.push(Type::TypeParam(name));
                                }
                            }
                            _ => type_params.push(self.parse_type()?),
                        }
                    }
                    self.expect(Token::RightAngle)?;
                    Ok(Type::Generic { path, type_params })
                } else {
                    Ok(Type::Path(path))
                }
            }

            Token::LeftBracket => {
                self.advance(); // consume [
                let element_type = Box::new(self.parse_type()?);

                if self.current.token == Token::Semicolon {
                    self.advance(); // consume ;
                    let size = match &self.current.token {
                        Token::Integer(n, _) => {
                            let size = *n as usize;
                            self.advance();
                            size
                        }
                        _ => {
                            return Err(ParseError::ExpectedExpression {
                                location: self.current.location.clone(),
                            })
                        }
                    };
                    self.expect(Token::RightBracket)?;
                    Ok(Type::Array { element_type, size })
                } else {
                    self.expect(Token::RightBracket)?;
                    Ok(Type::Slice { element_type })
                }
            }

            _ => Err(ParseError::ExpectedIdentifier {
                location: self.current.location.clone(),
            }),
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

        let initializer = if self.current.token == Token::Assign {
            self.advance(); // consume =
            Some(Box::new(self.parse_expression(0)?))
        } else {
            None
        };

        self.expect(Token::Semicolon)?;

        Ok(Stmt::Let {
            name,
            mutable,
            type_annotation,
            initializer,
        })
    }

    fn parse_const_static_statement(&mut self, kind: DeclarationKind, visibility: bool) -> Result<Stmt, ParseError> {
        self.advance(); // consume 'const' or 'static'

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

        if self.current.token != Token::Colon {
            return Err(ParseError::UnexpectedToken {
                found: self.current.clone(),
                expected: Some("':'".to_string()),
            });
        }
        self.advance(); // consume ':'
        let type_annotation = Some(self.parse_type()?);

        self.expect(Token::Assign)?;
        let initializer = Box::new(self.parse_expression(0)?);
        self.expect(Token::Semicolon)?;

        match kind {
            DeclarationKind::Const => Ok(Stmt::Const {
                name,
                visibility,
                type_annotation,
                initializer,
            }),
            DeclarationKind::Static => Ok(Stmt::Static {
                name,
                visibility,
                type_annotation,
                initializer,
            }),
        }
    }

    fn parse_expression(&mut self, precedence: i32) -> Result<Expr, ParseError> {
        self.enter_recursion()?;
        let result = {
            let mut left = self.parse_prefix()?;
            while precedence < self.get_precedence(&self.current.token) {
                left = self.parse_infix(left)?;
            }
            Ok(left)
        };

        self.exit_recursion();
        return result;
    }

    fn parse_prefix(&mut self) -> Result<Expr, ParseError> {
        match &self.current.token {
            Token::Async => self.parse_async_prefix(),

            Token::If => self.parse_if_expression(),

            Token::Minus | Token::Not => {
                let operator = self.current.token.clone();
                self.advance();
                let operand = Box::new(self.parse_expression(PRECEDENCE_PREFIX)?);
                Ok(Expr::Unary { operator, operand })
            }

            Token::LeftBracket => {
                self.advance(); // consume [
                let mut elements = Vec::new();

                while self.current.token != Token::RightBracket {
                    if !elements.is_empty() {
                        self.expect(Token::Comma)?;
                    }
                    elements.push(self.parse_expression(0)?);
                }

                self.expect(Token::RightBracket)?;
                Ok(Expr::Array(elements))
            }

            Token::LeftBrace => {
                self.advance();
                self.parse_block_expression_with_async(false)
            }

            Token::BitOr => self.parse_closure_expression_with_async(false),

            Token::Integer(n, type_suffix) => {
                let n = *n;
                let type_suffix = type_suffix.clone();
                self.advance();
                Ok(Expr::Integer(n, type_suffix))
            }

            Token::Float(n, type_suffix) => {
                let n = *n;
                let type_suffix = type_suffix.clone();
                self.advance();
                Ok(Expr::Float(n, type_suffix))
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
                let first_segment = name.clone();
                self.advance();

                if self.current.token == Token::LeftBrace && self.is_struct_type(&first_segment) {
                    self.advance(); // consume {
                    let fields = self.parse_struct_init_fields()?;
                    self.expect(Token::RightBrace)?;
                    return Ok(Expr::StructInit { struct_name: first_segment, fields });
                }

                match first_segment.as_str() {
                    "Ok" => {
                        self.expect(Token::LeftParen)?;
                        let expr = if self.current.token == Token::RightParen { Expr::Unit } else { self.parse_expression(0)? };
                        self.expect(Token::RightParen)?;
                        Ok(Expr::Ok(Box::new(expr)))
                    }

                    "Err" => {
                        self.expect(Token::LeftParen)?;
                        let expr = if self.current.token == Token::RightParen { Expr::Unit } else { self.parse_expression(0)? };
                        self.expect(Token::RightParen)?;
                        Ok(Expr::Err(Box::new(expr)))
                    }

                    "Some" => {
                        self.expect(Token::LeftParen)?;
                        let expr = self.parse_expression(0)?;
                        self.expect(Token::RightParen)?;
                        Ok(Expr::Some(Box::new(expr)))
                    }

                    "None" => {
                        if self.current.token == Token::LeftParen {
                            self.expect(Token::LeftParen)?;
                            self.expect(Token::RightParen)?;
                        }
                        Ok(Expr::None)
                    }
                    _ => {
                        if self.current.token == Token::DoubleColon {
                            self.advance(); // consume '::'
                            let mut segments = vec![first_segment];

                            loop {
                                segments.push(self.expect_identifier()?);

                                if self.current.token != Token::DoubleColon {
                                    break;
                                }
                                self.advance(); // consume '::'
                            }

                            let path = Path { segments };

                            if self.current.token == Token::LeftParen {
                                self.advance(); // consume '('
                                let mut arguments = Vec::new();

                                while self.current.token != Token::RightParen {
                                    if !arguments.is_empty() {
                                        self.expect(Token::Comma)?;
                                    }
                                    arguments.push(self.parse_expression(0)?);
                                }

                                self.expect(Token::RightParen)?;
                                Ok(Expr::Call {
                                    function: Box::new(Expr::Path(path)),
                                    arguments,
                                })
                            } else {
                                Ok(Expr::Path(path))
                            }
                        } else {
                            self.parse_identifier_expression(first_segment)
                        }
                    }
                }
            }

            _ => {
                return Err(ParseError::ExpectedIdentifier {
                    location: self.current.location.to_owned(),
                })
            }
        }
    }

    fn parse_async_prefix(&mut self) -> Result<Expr, ParseError> {
        self.advance(); // consume the async token
        let is_async = true;
        match self.current.token {
            Token::LeftBrace => {
                self.advance(); // consume '{'
                self.parse_block_expression_with_async(is_async)
            }
            Token::BitOr => self.parse_closure_expression_with_async(is_async),
            _ => Err(ParseError::Custom {
                message: "Expected async block or async closure after 'async'".to_string(),
                location: self.current.location.clone(),
            }),
        }
    }

    fn parse_block_expression_with_async(&mut self, is_async: bool) -> Result<Expr, ParseError> {
        self.enter_recursion()?;
        let mut statements = Vec::new();
        let mut returns = false;
        while self.current.token != Token::RightBrace {
            let stmt = self.parse_statement()?;
            if matches!(stmt, Stmt::Return(_)) {
                returns = true;
            }
            statements.push(stmt);
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

        self.expect(Token::RightBrace)?; // consume '}'
        self.exit_recursion();
        Ok(Expr::Block { statements, value, returns, is_async })
    }

    fn parse_closure_expression_with_async(&mut self, is_async: bool) -> Result<Expr, ParseError> {
        self.advance(); // consume the first '|'
        let mut params: Vec<(String, Option<Type>)> = Vec::new();

        if self.current.token == Token::BitOr {
            self.advance(); // consume the second '|'
        } else {
            while self.current.token != Token::BitOr {
                if !params.is_empty() {
                    self.expect(Token::Comma)?;
                }
                let param_name = self.expect_identifier()?;
                let param_type = if self.current.token == Token::Colon {
                    self.advance(); // consume ':'
                    Some(self.parse_type()?)
                } else {
                    None
                };

                params.push((param_name, param_type));
            }
            self.expect(Token::BitOr)?; // consume the closing '|'
        }

        let body = if self.current.token == Token::LeftBrace {
            self.advance(); // consume '{'
            self.parse_block_expression_with_async(false)?
        } else {
            self.parse_expression(0)?
        };

        Ok(Expr::Closure {
            params,
            body: Box::new(body),
            is_async,
        })
    }

    fn parse_identifier_expression(&mut self, name: String) -> Result<Expr, ParseError> {
        match self.current.token {
            Token::LeftAngle | Token::DoubleColon => {
                let mut segments = vec![name];

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
                }

                if self.current.token == Token::DoubleColon {
                    self.advance();
                    loop {
                        segments.push(self.expect_identifier()?);

                        if self.current.token != Token::DoubleColon {
                            break;
                        }
                        self.advance();
                    }
                }

                if self.current.token == Token::LeftParen {
                    self.advance();
                    let mut arguments = Vec::new();

                    while self.current.token != Token::RightParen {
                        if !arguments.is_empty() {
                            self.expect(Token::Comma)?;
                        }
                        arguments.push(self.parse_expression(0)?);
                    }

                    self.expect(Token::RightParen)?;
                    Ok(Expr::Call {
                        function: Box::new(Expr::Path(Path { segments })),
                        arguments,
                    })
                } else {
                    Ok(Expr::Path(Path { segments }))
                }
            }

            Token::Assign => {
                self.advance();
                let value = Box::new(self.parse_expression(0)?);
                Ok(Expr::Assignment { target: name, value })
            }

            Token::LeftParen => {
                self.advance();
                let mut arguments = Vec::new();

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

    fn parse_infix(&mut self, left: Expr) -> Result<Expr, ParseError> {
        match &self.current.token {
            Token::LeftParen => {
                self.advance(); // consume (
                let mut arguments = Vec::new();

                while self.current.token != Token::RightParen {
                    if !arguments.is_empty() {
                        self.expect(Token::Comma)?;
                    }
                    arguments.push(self.parse_expression(0)?);
                }

                self.expect(Token::RightParen)?;

                Ok(Expr::Call { function: Box::new(left), arguments })
            }

            Token::Dot => {
                self.advance(); // consume .

                match self.current.token {
                    Token::Await => {
                        self.advance(); // consume 'await'
                        Ok(Expr::Await(Box::new(left)))
                    }
                    _ => {
                        let method = self.expect_identifier()?;
                        self.parse_member_access(left, method)
                    }
                }
            }

            Token::As => {
                self.advance(); // consume 'as'
                let target_type = self.parse_type()?;
                Ok(Expr::Cast { expr: Box::new(left), target_type })
            }

            Token::LeftBracket => {
                self.advance(); // consume [
                let index = self.parse_expression(0)?;
                self.expect(Token::RightBracket)?;

                Ok(Expr::Index {
                    array: Box::new(left),
                    index: Box::new(index),
                })
            }

            Token::Question => {
                self.advance(); // consume ?
                Ok(Expr::Try(Box::new(left)))
            }

            Token::BitOr => {
                if self.peek.token == Token::BitOr {
                    self.advance(); // consume the first '|'
                    self.advance(); // consume the second '|'

                    let operator = Token::Or;
                    let precedence = self.get_precedence(&operator);
                    let right = self.parse_expression(precedence)?;
                    Ok(Expr::Binary {
                        left: Box::new(left),
                        operator,
                        right: Box::new(right),
                    })
                } else {
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
            }

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
            | Token::Or
            | Token::BitAnd
            | Token::BitXor
            | Token::Rem => {
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

    fn parse_struct_init_fields(&mut self) -> Result<Vec<(String, Expr, bool)>, ParseError> {
        let mut fields = Vec::new();

        while self.current.token != Token::RightBrace {
            if !fields.is_empty() {
                self.expect(Token::Comma)?;

                if self.current.token == Token::RightBrace {
                    break;
                }
            }

            let field_name = self.expect_identifier()?;

            let (field_value, is_shorthand) = if self.current.token == Token::Colon {
                self.advance(); // consume :
                (self.parse_expression(0)?, false)
            } else {
                // shorthand case
                (Expr::Identifier(field_name.clone()), true)
            };

            fields.push((field_name, field_value, is_shorthand));
        }

        Ok(fields)
    }

    fn parse_member_access(&mut self, object: Expr, member: String) -> Result<Expr, ParseError> {
        match self.current.token {
            Token::Assign => {
                self.advance(); // consume =
                let value = Box::new(self.parse_expression(0)?);
                Ok(Expr::MemberAssignment {
                    object: Box::new(object),
                    member,
                    value,
                })
            }

            Token::LeftParen => {
                self.advance(); // consume (
                let mut arguments = Vec::new();

                while self.current.token != Token::RightParen {
                    if !arguments.is_empty() {
                        self.expect(Token::Comma)?;
                    }
                    arguments.push(self.parse_expression(0)?);
                }

                self.expect(Token::RightParen)?;

                Ok(Expr::MethodCall {
                    object: Box::new(object),
                    method: member,
                    arguments,
                })
            }

            _ => Ok(Expr::MemberAccess { object: Box::new(object), member }),
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

    fn parse_block_expression(&mut self) -> Result<Expr, ParseError> { self.parse_block_expression_with_async(false) }

    fn get_precedence(&self, token: &Token) -> i32 {
        match token {
            Token::Dot => PRECEDENCE_MEMBER,
            Token::LeftParen => PRECEDENCE_CALL,
            Token::Question => PRECEDENCE_QUESTION,
            Token::Equals | Token::NotEquals => PRECEDENCE_EQUALS,
            Token::LeftAngle | Token::RightAngle | Token::LessEquals | Token::GreaterEquals => PRECEDENCE_COMPARE,
            Token::Or | Token::BitOr => PRECEDENCE_OR,
            Token::And | Token::BitAnd => PRECEDENCE_AND,
            Token::Plus | Token::Minus => PRECEDENCE_SUM,
            Token::Star | Token::Slash | Token::Rem => PRECEDENCE_PRODUCT,
            Token::BitXor => PRECEDENCE_OR,
            Token::Not => PRECEDENCE_PREFIX,
            Token::LeftBracket => PRECEDENCE_INDEX,
            Token::As => PRECEDENCE_CALL,
            _ => PRECEDENCE_LOWEST,
        }
    }
}
