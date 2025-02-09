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
    current_impl_target: Option<String>,
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
            current_impl_target: None,
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

        let attributes = self.parse_attributes()?;

        let visibility = if matches!(self.current.token, Token::Pub) {
            self.advance(); // consume 'pub'
            true
        } else {
            false
        };

        let result = match self.current.token {
            Token::Return => self.parse_return_statement(),
            Token::Let => self.parse_let_statement(attributes),
            Token::Impl => self.parse_impl_block(attributes),

            Token::Use => self.parse_use_statement(visibility, attributes),
            Token::Type => self.parse_type_alias(visibility, attributes),
            Token::Enum => self.parse_enum_declaration(visibility, attributes),
            Token::Module => self.parse_module_statement(visibility, attributes),
            Token::Struct => self.parse_struct_declaration(visibility, attributes),
            Token::Fn => self.parse_function_statement(visibility, attributes),
            Token::MacroRules => self.parse_macro_definition(attributes, visibility),

            Token::Const => self.parse_const_static_statement(DeclarationKind::Const, visibility, attributes),
            Token::Static => self.parse_const_static_statement(DeclarationKind::Static, visibility, attributes),

            Token::LeftBrace => {
                self.advance();
                let block = self.parse_block_expression()?;
                Ok(Stmt::ExpressionStmt(block))
            }

            Token::Match | Token::If => {
                let expr = self.parse_expression(0)?;
                Ok(Stmt::ExpressionValue(expr))
            }

            Token::Async => {
                if self.peek.token == Token::Fn {
                    self.parse_function_statement(visibility, attributes)
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

    fn parse_struct_declaration(&mut self, visibility: bool, attributes: Vec<Attribute>) -> Result<Stmt, ParseError> {
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
            attributes,
        })
    }

    fn parse_impl_block(&mut self, attributes: Vec<Attribute>) -> Result<Stmt, ParseError> {
        self.advance(); // consume 'impl'
        let target = self.parse_path()?;

        self.expect(Token::LeftBrace)?;
        self.current_impl_target = Some(target.segments.last().unwrap().ident.clone());

        let mut items = Vec::new();

        while self.current.token != Token::RightBrace {
            let method_attributes = self.parse_attributes()?;

            let visibility = if self.current.token == Token::Pub {
                self.advance(); // consume 'pub'
                true
            } else {
                false
            };

            if self.current.token != Token::Fn && self.current.token != Token::Async {
                return Err(ParseError::UnexpectedToken {
                    found: self.current.clone(),
                    expected: Some("function definition in impl block".to_string()),
                });
            }

            let method = self.parse_function_statement(visibility, method_attributes)?;
            items.push(method);
        }

        self.expect(Token::RightBrace)?;
        self.current_impl_target = None;

        Ok(Stmt::Impl { target, items, attributes })
    }

    fn parse_function_parameters(&mut self) -> Result<Vec<(Pattern, Type)>, ParseError> {
        let mut params = Vec::new();
        self.expect(Token::LeftParen)?;

        while self.current.token != Token::RightParen {
            if !params.is_empty() {
                self.expect(Token::Comma)?;
            }
            let (pattern, opt_type) = self.parse_parameter()?;
            let ty = match opt_type {
                Some(t) => t,
                None => {
                    if let Some(target) = &self.current_impl_target {
                        Type::Simple(target.clone())
                    } else {
                        return Err(ParseError::Custom {
                            message: "Self parameter outside of impl block".to_string(),
                            location: self.current.location.clone(),
                        });
                    }
                }
            };
            params.push((pattern, ty));
        }
        self.expect(Token::RightParen)?;
        Ok(params)
    }

    fn parse_match_expression(&mut self) -> Result<Expr, ParseError> {
        self.advance(); // consume 'match'
        let value = Box::new(self.parse_expression(0)?);

        self.expect(Token::LeftBrace)?;
        let mut arms = Vec::new();

        while self.current.token != Token::RightBrace {
            arms.push(self.parse_match_arm()?);

            if self.current.token == Token::Comma {
                self.advance();
            }
        }

        self.expect(Token::RightBrace)?;
        Ok(Expr::Match { value, arms })
    }

    fn parse_match_arm(&mut self) -> Result<MatchArm, ParseError> {
        let pattern = self.parse_pattern()?;

        let guard = if self.current.token == Token::If {
            self.advance();
            Some(self.parse_expression(0)?)
        } else {
            None
        };

        self.expect(Token::Fat)?; // consume '=>'

        let body = if self.current.token == Token::LeftBrace {
            self.advance();
            let expr = self.parse_block_expression()?;
            expr
        } else {
            let expr = self.parse_expression(0)?;
            if self.current.token == Token::Comma {
                self.advance();
            }
            expr
        };

        Ok(MatchArm { pattern, guard, body })
    }

    fn parse_parameter(&mut self) -> Result<(Pattern, Option<Type>), ParseError> {
        if self.current.token == Token::BitAnd {
            self.advance();
            let mut is_mut_ref = false;
            if self.current.token == Token::Mut {
                is_mut_ref = true;
                self.advance();
            }

            if let Token::Identifier(id) = &self.current.token {
                if id == "self" {
                    self.advance();
                    return Ok((
                        Pattern::Reference {
                            mutable: is_mut_ref,
                            pattern: Box::new(Pattern::Identifier {
                                name: "self".to_string(),
                                mutable: false,
                            }),
                        },
                        None,
                    ));
                } else {
                }
            } else {
                return Err(ParseError::ExpectedIdentifier {
                    location: self.current.location.clone(),
                });
            }
        }

        if let Token::Identifier(id) = &self.current.token {
            if id == "self" {
                self.advance();
                return Ok((
                    Pattern::Identifier {
                        name: "self".to_string(),
                        mutable: false,
                    },
                    None,
                ));
            }
        }

        let mut is_mut_binding = false;
        if self.current.token == Token::Mut {
            is_mut_binding = true;
            self.advance();
        }
        let pattern = if let Token::Identifier(id) = &self.current.token {
            let ident = id.clone();
            self.advance();
            Pattern::Identifier { name: ident, mutable: is_mut_binding }
        } else {
            return Err(ParseError::ExpectedIdentifier {
                location: self.current.location.clone(),
            });
        };

        self.expect(Token::Colon)?;
        let param_type = self.parse_type()?;

        Ok((pattern, Some(param_type)))
    }

    fn parse_path(&mut self) -> Result<Path, ParseError> {
        let mut segments = Vec::new();
        segments.push(self.parse_path_segment()?);

        while self.current.token == Token::DoubleColon {
            self.advance(); // consume '::'
            segments.push(self.parse_path_segment()?);
        }

        Ok(Path { segments })
    }

    fn parse_path_segment(&mut self) -> Result<PathSegment, ParseError> {
        let ident = self.expect_identifier()?;
        let mut generics = Vec::new();

        if self.current.token == Token::LeftAngle {
            self.advance(); // consume '<'
            while self.current.token != Token::RightAngle {
                if !generics.is_empty() {
                    self.expect(Token::Comma)?;
                }
                generics.push(self.parse_type()?);
            }
            self.expect(Token::RightAngle)?;
        }

        Ok(PathSegment { ident, generics })
    }

    fn parse_use_statement(&mut self, visibility: bool, attributes: Vec<Attribute>) -> Result<Stmt, ParseError> {
        self.advance(); // consume 'use'

        let path = self.parse_use_path()?;

        let alias = if self.current.token == Token::As {
            self.advance(); // consume 'as'
            Some(self.expect_identifier()?)
        } else {
            None
        };

        self.expect(Token::Semicolon)?;

        Ok(Stmt::Use { path, alias, visibility, attributes })
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

    fn parse_module_statement(&mut self, visibility: bool, attributes: Vec<Attribute>) -> Result<Stmt, ParseError> {
        self.advance(); // consume 'module'

        let name = self.expect_identifier()?;

        self.expect(Token::LeftBrace)?;

        let mut body = Vec::new();
        while self.current.token != Token::RightBrace {
            body.push(self.parse_statement()?);
        }

        self.expect(Token::RightBrace)?;

        Ok(Stmt::Module { name, visibility, body, attributes })
    }

    fn parse_function_statement(&mut self, visibility: bool, attributes: Vec<Attribute>) -> Result<Stmt, ParseError> {
        let is_async = if self.current.token == Token::Async {
            self.advance(); // consume 'async'
            true
        } else {
            false
        };

        self.expect(Token::Fn)?; // consume 'fn'

        let name = self.expect_identifier()?;

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

        let params = self.parse_function_parameters()?;

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
            attributes,
        })
    }

    fn parse_enum_declaration(&mut self, visibility: bool, attributes: Vec<Attribute>) -> Result<Stmt, ParseError> {
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
            attributes,
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

    fn parse_type_alias(&mut self, visibility: bool, attributes: Vec<Attribute>) -> Result<Stmt, ParseError> {
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

        Ok(Stmt::TypeAlias {
            name,
            visibility,
            type_params,
            ty,
            attributes,
        })
    }

    fn parse_type(&mut self) -> Result<Type, ParseError> {
        match &self.current.token {
            Token::BitAnd => {
                self.advance(); // consume '&'
                let mutable = if self.current.token == Token::Mut {
                    self.advance();
                    true
                } else {
                    false
                };
                let inner = self.parse_type()?;
                Ok(Type::Reference { mutable, inner: Box::new(inner) })
            }

            Token::Star => {
                self.advance(); // consume '*'
                let inner = self.parse_type()?;
                Ok(Type::Pointer { inner: Box::new(inner) })
            }

            Token::LeftParen => {
                self.advance(); // consume '('

                if self.current.token == Token::RightParen {
                    self.advance(); // consume ')'
                    return Ok(Type::Unit);
                }

                let mut types = Vec::new();

                while self.current.token != Token::RightParen {
                    if !types.is_empty() {
                        self.expect(Token::Comma)?;

                        if self.current.token == Token::RightParen {
                            break;
                        }
                    }

                    types.push(self.parse_type()?);
                }

                self.expect(Token::RightParen)?;

                if types.len() == 1 && self.current.token != Token::Comma {
                    Ok(types.remove(0))
                } else {
                    Ok(Type::Tuple(types))
                }
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

                if path.segments.len() == 1 && path.segments[0].ident == "Self" {
                    if let Some(target) = &self.current_impl_target {
                        return Ok(Type::Simple(target.clone()));
                    } else {
                        return Err(ParseError::Custom {
                            message: "Cannot use Self type outside of an impl block".to_string(),
                            location: self.current.location.clone(),
                        });
                    }
                }

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

    fn parse_let_statement(&mut self, attributes: Vec<Attribute>) -> Result<Stmt, ParseError> {
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
            attributes,
        })
    }

    fn parse_const_static_statement(&mut self, kind: DeclarationKind, visibility: bool, attributes: Vec<Attribute>) -> Result<Stmt, ParseError> {
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
                attributes,
            }),
            DeclarationKind::Static => Ok(Stmt::Static {
                name,
                visibility,
                type_annotation,
                initializer,
                attributes,
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
            Token::Match => self.parse_match_expression(),

            Token::Async => self.parse_async_prefix(),

            Token::If => self.parse_if_expression(),

            Token::Minus | Token::Not => {
                let operator = self.current.token.clone();
                self.advance();
                let operand = Box::new(self.parse_expression(PRECEDENCE_PREFIX)?);
                Ok(Expr::Unary { operator, operand })
            }

            Token::BitAnd => {
                self.advance();
                let mutable = if self.current.token == Token::Mut {
                    self.advance();
                    true
                } else {
                    false
                };

                let operand = Box::new(self.parse_expression(PRECEDENCE_PREFIX)?);
                Ok(Expr::Reference { mutable, operand })
            }

            Token::Star => {
                self.advance(); // consume '*'
                let operand = Box::new(self.parse_expression(PRECEDENCE_PREFIX)?);
                Ok(Expr::Dereference { operand })
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
                self.advance(); // consume '('

                if self.current.token == Token::RightParen {
                    self.advance();
                    return Ok(Expr::Unit);
                }

                let mut elements = Vec::new();

                while self.current.token != Token::RightParen {
                    if !elements.is_empty() {
                        self.expect(Token::Comma)?;

                        if self.current.token == Token::RightParen {
                            break;
                        }
                    }

                    elements.push(self.parse_expression(0)?);
                }

                self.expect(Token::RightParen)?;

                if elements.len() == 1 && self.current.token != Token::Comma {
                    Ok(elements.remove(0))
                } else {
                    Ok(Expr::Tuple(elements))
                }
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

                            let mut segments = vec![PathSegment {
                                ident: first_segment,
                                generics: Vec::new(),
                            }];

                            loop {
                                let ident = self.expect_identifier()?;
                                segments.push(PathSegment { ident, generics: Vec::new() });

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
        if self.current.token == Token::Not {
            return self.parse_macro_invocation(name);
        }

        if self.current.token == Token::LeftBrace {
            let struct_name = if name == "Self" {
                match &self.current_impl_target {
                    Some(target) => target.clone(),
                    None => {
                        return Err(ParseError::Custom {
                            message: "Cannot use Self outside of an impl block".to_string(),
                            location: self.current.location.clone(),
                        })
                    }
                }
            } else {
                name.clone()
            };

            if self.is_struct_type(&struct_name) {
                self.advance(); // consume {
                let fields = self.parse_struct_init_fields()?;
                self.expect(Token::RightBrace)?;
                return Ok(Expr::StructInit { struct_name, fields });
            }
        }

        if self.current.token == Token::DoubleColon {
            self.advance(); // consume '::'
            let mut segments = vec![PathSegment { ident: name, generics: Vec::new() }];
            let ident = self.expect_identifier()?;
            segments.push(PathSegment { ident, generics: Vec::new() });

            while self.current.token == Token::DoubleColon {
                self.advance(); // consume '::'
                let ident = self.expect_identifier()?;
                segments.push(PathSegment { ident, generics: Vec::new() });
            }

            if self.current.token == Token::LeftAngle {
                self.advance(); // consume '<'
                let mut generics = Vec::new();
                while self.current.token != Token::RightAngle {
                    if !generics.is_empty() {
                        self.expect(Token::Comma)?;
                    }
                    generics.push(self.parse_type()?);
                }
                self.expect(Token::RightAngle)?;
                if let Some(last_segment) = segments.last_mut() {
                    last_segment.generics = generics;
                }
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
                return Ok(Expr::Call {
                    function: Box::new(Expr::Path(path)),
                    arguments,
                });
            } else {
                return Ok(Expr::Path(path));
            }
        }

        match self.current.token {
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

    fn parse_macro_invocation(&mut self, macro_name: String) -> Result<Expr, ParseError> {
        self.advance(); // consume '!'

        let delimiter = match self.current.token {
            Token::LeftParen => MacroDelimiter::Paren,
            Token::LeftBracket => MacroDelimiter::Bracket,
            Token::LeftBrace => MacroDelimiter::Brace,
            _ => {
                return Err(ParseError::UnexpectedToken {
                    found: self.current.clone(),
                    expected: Some("an opening delimiter ('(', '[', or '{')".to_string()),
                });
            }
        };

        let tokens = self.parse_macro_delimited_tokens(delimiter.clone())?;

        Ok(Expr::MacroInvocation { name: macro_name, delimiter, tokens })
    }

    fn parse_attributes(&mut self) -> Result<Vec<Attribute>, ParseError> {
        let mut attrs = Vec::new();
        while self.current.token == Token::Pound {
            attrs.push(self.parse_attribute()?);
        }
        Ok(attrs)
    }

    fn parse_attribute(&mut self) -> Result<Attribute, ParseError> {
        self.expect(Token::Pound)?;

        let is_inner = if self.current.token == Token::Not {
            self.advance();
            true
        } else {
            false
        };

        self.expect(Token::LeftBracket)?;

        let mut tokens = Vec::new();
        while self.current.token != Token::RightBracket {
            if self.current.token == Token::EOF {
                return Err(ParseError::Custom {
                    message: "Unterminated attribute, expected ']'".to_string(),
                    location: self.current.location.clone(),
                });
            }
            tokens.push(self.current.clone());
            self.advance();
        }
        self.expect(Token::RightBracket)?;

        let name = if let Some(first) = tokens.first() {
            match &first.token {
                Token::Identifier(id) => id.clone(),
                _ => "<unknown>".to_string(),
            }
        } else {
            "<empty>".to_string()
        };

        Ok(Attribute { is_inner, name, tokens })
    }

    fn parse_macro_definition(&mut self, attributes: Vec<Attribute>, visibility: bool) -> Result<Stmt, ParseError> {
        self.advance();

        if self.current.token != Token::Not {
            return Err(ParseError::UnexpectedToken {
                found: self.current.clone(),
                expected: Some("expected '!' after macro_rules".to_string()),
            });
        }

        self.advance();
        let name = self.expect_identifier()?;

        let delimiter = match self.current.token {
            Token::LeftBrace => MacroDelimiter::Brace,
            Token::LeftParen => MacroDelimiter::Paren,
            Token::LeftBracket => MacroDelimiter::Bracket,
            _ => {
                return Err(ParseError::UnexpectedToken {
                    found: self.current.clone(),
                    expected: Some("expected an opening delimiter ('{', '(' or '[') after the macro name".to_string()),
                });
            }
        };

        let tokens = self.parse_macro_delimited_tokens(delimiter)?;

        Ok(Stmt::MacroDefinition { attributes, visibility, name, tokens })
    }

    fn parse_macro_delimited_tokens(&mut self, delimiter: MacroDelimiter) -> Result<Vec<TokenInfo>, ParseError> {
        let (open_token, close_token) = match delimiter {
            MacroDelimiter::Paren => (Token::LeftParen, Token::RightParen),
            MacroDelimiter::Bracket => (Token::LeftBracket, Token::RightBracket),
            MacroDelimiter::Brace => (Token::LeftBrace, Token::RightBrace),
        };

        if self.current.token != open_token {
            return Err(ParseError::UnexpectedToken {
                found: self.current.clone(),
                expected: Some(format!("expected opening delimiter {:?}", open_token)),
            });
        }
        self.advance();

        let mut tokens = Vec::new();
        let mut nesting = 1;

        while nesting > 0 {
            if self.current.token == Token::EOF {
                return Err(ParseError::Custom {
                    message: "Unterminated macro definition".to_string(),
                    location: self.current.location.clone(),
                });
            }

            if self.current.token == open_token {
                nesting += 1;
            } else if self.current.token == close_token {
                nesting -= 1;
                if nesting == 0 {
                    self.advance();
                    break;
                }
            }

            tokens.push(self.current.clone());
            self.advance();
        }

        if nesting != 0 {
            return Err(ParseError::Custom {
                message: "Unbalanced macro delimiters".to_string(),
                location: self.current.location.clone(),
            });
        }

        Ok(tokens)
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

    fn parse_pattern(&mut self) -> Result<Pattern, ParseError> {
        match &self.current.token {
            Token::Identifier(name) => {
                let name = name.clone();
                self.advance();

                if name == "_" {
                    return Ok(Pattern::Wildcard);
                }

                match name.as_str() {
                    "Ok" | "Err" | "Some" => {
                        if self.current.token == Token::LeftParen {
                            self.advance(); // consume (
                            let inner = self.parse_pattern()?;
                            self.expect(Token::RightParen)?;
                            let path = Path {
                                segments: vec![PathSegment { ident: name, generics: Vec::new() }],
                            };

                            return Ok(Pattern::TupleStruct { path, elements: vec![inner] });
                        }
                    }
                    "None" => {
                        if self.current.token == Token::LeftParen {
                            self.advance(); // consume (
                            self.expect(Token::RightParen)?;
                        }

                        let path = Path {
                            segments: vec![PathSegment { ident: name, generics: Vec::new() }],
                        };

                        return Ok(Pattern::Path(path));
                    }
                    _ => {}
                }

                if self.current.token == Token::DoubleColon {
                    let mut segments = vec![PathSegment { ident: name, generics: Vec::new() }];

                    while self.current.token == Token::DoubleColon {
                        self.advance();
                        let next_ident = self.expect_identifier()?;
                        segments.push(PathSegment {
                            ident: next_ident,
                            generics: Vec::new(),
                        });
                    }

                    let path = Path { segments };

                    match self.current.token {
                        Token::LeftParen => {
                            self.advance();
                            let mut elements = Vec::new();

                            while self.current.token != Token::RightParen {
                                if !elements.is_empty() {
                                    self.expect(Token::Comma)?;
                                }
                                elements.push(self.parse_pattern()?);
                            }

                            self.expect(Token::RightParen)?;
                            Ok(Pattern::TupleStruct { path, elements })
                        }

                        Token::LeftBrace => {
                            self.advance();
                            let mut fields = Vec::new();
                            let mut rest = false;

                            while self.current.token != Token::RightBrace {
                                if self.current.token == Token::Dot {
                                    self.advance();
                                    self.expect(Token::Dot)?;
                                    rest = true;
                                    break;
                                }

                                if !fields.is_empty() {
                                    self.expect(Token::Comma)?;
                                }

                                let field_name = self.expect_identifier()?;
                                let pattern = if self.current.token == Token::Colon {
                                    self.advance();
                                    self.parse_pattern()?
                                } else {
                                    Pattern::Identifier {
                                        name: field_name.clone(),
                                        mutable: false,
                                    }
                                };

                                fields.push((field_name, pattern));
                            }

                            self.expect(Token::RightBrace)?;
                            Ok(Pattern::Struct { path, fields, rest })
                        }
                        _ => Ok(Pattern::Path(path)),
                    }
                } else {
                    Ok(Pattern::Identifier { name, mutable: false })
                }
            }

            Token::Integer(n, t) => {
                let n = *n;
                let t = t.clone();
                self.advance();
                Ok(Pattern::Literal(Expr::Integer(n, t)))
            }

            Token::Float(n, t) => {
                let n = *n;
                let t = t.clone();
                self.advance();
                Ok(Pattern::Literal(Expr::Float(n, t)))
            }

            Token::Minus => {
                self.advance(); // consume the minus
                match &self.current.token {
                    Token::Integer(n, t) => {
                        let n = -n;
                        let t = t.clone();
                        self.advance();
                        Ok(Pattern::Literal(Expr::Integer(n, t)))
                    }
                    Token::Float(n, t) => {
                        let n = -n;
                        let t = t.clone();
                        self.advance();
                        Ok(Pattern::Literal(Expr::Float(n, t)))
                    }
                    _ => Err(ParseError::ExpectedExpression {
                        location: self.current.location.clone(),
                    }),
                }
            }

            Token::String(s) => {
                let s = s.clone();
                self.advance();
                Ok(Pattern::Literal(Expr::String(s)))
            }

            Token::True => {
                self.advance();
                Ok(Pattern::Literal(Expr::Boolean(true)))
            }

            Token::False => {
                self.advance();
                Ok(Pattern::Literal(Expr::Boolean(false)))
            }

            Token::BitOr => {
                self.advance();
                let mut patterns = Vec::new();
                patterns.push(self.parse_pattern()?);

                while self.current.token == Token::BitOr {
                    self.advance();
                    patterns.push(self.parse_pattern()?);
                }

                Ok(Pattern::Or(patterns))
            }

            _ => Err(ParseError::UnexpectedToken {
                found: self.current.clone(),
                expected: Some("pattern".to_string()),
            }),
        }
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
