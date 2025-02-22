pub mod ast;
pub mod lexer;

use ast::*;
use lexer::*;

use crate::util;
use std::collections::{HashMap, HashSet};

enum DeclarationKind {
    Const,
    Static,
}

const MAX_RECURSION_DEPTH: i32 = 500;
const PRECEDENCE_LOWEST: i32 = 0;
const PRECEDENCE_ASSIGN: i32 = 1;
const PRECEDENCE_RANGE: i32 = 1;
const PRECEDENCE_EQUALS: i32 = 6;
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
    current_trait_target: Option<String>,
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
            current_trait_target: None,
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

        let label = if let Token::Lifetime(l) = &self.current.token {
            if self.peek.token == Token::Colon {
                let lab = l.clone();
                self.advance(); // consume lifetime
                self.advance(); // consume ':'
                Some(lab)
            } else {
                None
            }
        } else {
            None
        };

        let result = match self.current.token {
            Token::Break => self.parse_break_statement(),
            Token::Return => self.parse_return_statement(),
            Token::Continue => self.parse_continue_statement(),

            Token::Let => self.parse_let_statement(attributes),
            Token::Impl => self.parse_impl_block(attributes),

            Token::Use => self.parse_use_statement(visibility, attributes),
            Token::Type => self.parse_type_alias(visibility, attributes),
            Token::Enum => self.parse_enum_declaration(visibility, attributes),
            Token::Module => self.parse_module_statement(visibility, attributes),
            Token::Struct => self.parse_struct_declaration(visibility, attributes),
            Token::Trait => self.parse_trait_declaration(visibility, attributes),
            Token::Fn => self.parse_function_statement(visibility, attributes, false),
            Token::MacroRules => self.parse_macro_definition(attributes, visibility),

            Token::Loop => Ok(Stmt::ExpressionStmt(self.parse_loop_expression(label)?)),
            Token::While => Ok(Stmt::ExpressionStmt(self.parse_while_expression(label)?)),
            Token::For => Ok(Stmt::ExpressionStmt(self.parse_for_expression(label)?)),

            Token::Const => self.parse_const_static_statement(DeclarationKind::Const, visibility, attributes),
            Token::Static => self.parse_const_static_statement(DeclarationKind::Static, visibility, attributes),

            Token::LeftBrace => {
                self.advance();
                let block = self.parse_block_expression()?;
                Ok(Stmt::ExpressionStmt(block))
            }

            Token::Match | Token::If => {
                let expr = self.parse_expression(0)?;
                if self.current.token == Token::Semicolon {
                    self.advance();
                    Ok(Stmt::ExpressionStmt(expr))
                } else {
                    Ok(Stmt::ExpressionValue(expr))
                }
            }

            Token::Async => {
                if self.peek.token == Token::Fn {
                    self.parse_function_statement(visibility, attributes, false)
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

        let mut fields = HashMap::new();

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

            fields.insert(field_name, (field_type, field_visibility));
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

    fn parse_trait_declaration(&mut self, visibility: bool, attributes: Vec<Attribute>) -> Result<Stmt, ParseError> {
        self.advance(); // consume 'trait'
        let name = self.expect_identifier()?;

        self.current_trait_target = Some(name.clone());
        self.expect(Token::LeftBrace)?;

        let mut items = Vec::new();

        while self.current.token != Token::RightBrace {
            let method_attributes = self.parse_attributes()?;
            if self.current.token != Token::Fn && self.current.token != Token::Async {
                return Err(ParseError::UnexpectedToken {
                    found: self.current.clone(),
                    expected: Some("function prototype in trait declaration".to_string()),
                });
            }

            let method = self.parse_function_statement(false, method_attributes, true)?;
            if self.current.token == Token::Semicolon {
                self.advance();
            }
            items.push(method);
        }

        self.expect(Token::RightBrace)?;
        self.current_trait_target = None;

        Ok(Stmt::Trait { name, items, visibility, attributes })
    }

    fn parse_impl_block(&mut self, attributes: Vec<Attribute>) -> Result<Stmt, ParseError> {
        self.advance(); // consume 'impl'

        let first_path = self.parse_path()?;

        let (trait_path, target) = if self.current.token == Token::For {
            self.advance(); // consume 'for'
            let trait_path = first_path;
            let target = self.parse_path()?;
            (Some(trait_path), target)
        } else {
            (None, first_path)
        };

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

            let method = self.parse_function_statement(visibility, method_attributes, false)?;
            items.push(method);
        }

        self.expect(Token::RightBrace)?;
        self.current_impl_target = None;

        if let Some(trait_path) = trait_path {
            Ok(Stmt::TraitImpl {
                trait_path,
                target,
                items,
                attributes,
            })
        } else {
            Ok(Stmt::Impl { target, items, attributes })
        }
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
                    if util::is_self_parameter(&pattern) {
                        if let Some(target) = &self.current_impl_target {
                            Type::Simple(target.clone())
                        } else if let Some(trait_target) = &self.current_trait_target {
                            Type::Path(Path {
                                segments: vec![PathSegment {
                                    ident: trait_target.clone(),
                                    generics: Vec::new(),
                                }],
                            })
                        } else {
                            return Err(ParseError::Custom {
                                message: "Self parameter outside of impl or trait".to_string(),
                                location: self.current.location.clone(),
                            });
                        }
                    } else {
                        return Err(ParseError::Custom {
                            message: "Missing type annotation".to_string(),
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

        if self.current.token == Token::Mut {
            self.advance(); // consume 'mut'
            if let Token::Identifier(id) = &self.current.token {
                if id == "self" {
                    self.advance();
                    return Ok((
                        Pattern::Identifier {
                            name: "self".to_string(),
                            mutable: true,
                        },
                        None,
                    ));
                }
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

    fn parse_generic_arguments(&mut self) -> Result<Vec<Type>, ParseError> {
        self.expect(Token::LeftAngle)?;
        let mut generics = Vec::new();
        while self.current.token != Token::RightAngle {
            if !generics.is_empty() {
                self.expect(Token::Comma)?;
            }
            generics.push(self.parse_type()?);
        }
        self.expect(Token::RightAngle)?;
        Ok(generics)
    }

    fn parse_path_segment(&mut self) -> Result<PathSegment, ParseError> {
        let ident = self.expect_identifier()?;
        let generics = if self.current.token == Token::LeftAngle { self.parse_generic_arguments()? } else { Vec::new() };

        Ok(PathSegment { ident, generics })
    }

    fn parse_path_starting_with(&mut self, first: String) -> Result<Path, ParseError> {
        let mut segments = Vec::new();
        segments.push(PathSegment { ident: first, generics: Vec::new() });

        while self.current.token == Token::DoubleColon {
            self.advance(); // consume '::'
            if self.current.token == Token::LeftAngle {
                let generics = self.parse_generic_arguments()?;
                segments.last_mut().unwrap().generics = generics;
            } else {
                let ident = self.expect_identifier()?;
                let mut segment = PathSegment { ident, generics: Vec::new() };
                if self.current.token == Token::LeftAngle {
                    segment.generics = self.parse_generic_arguments()?;
                }
                segments.push(segment);
            }
        }
        Ok(Path { segments })
    }

    fn parse_path(&mut self) -> Result<Path, ParseError> {
        let mut segments = Vec::new();
        segments.push(self.parse_path_segment()?);

        while self.current.token == Token::DoubleColon {
            self.advance(); // consume '::'
            if self.current.token == Token::LeftAngle {
                let generics = self.parse_generic_arguments()?;
                segments.last_mut().unwrap().generics = generics;
            } else {
                segments.push(self.parse_path_segment()?);
            }
        }

        Ok(Path { segments })
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

        let (body, is_external) = match self.current.token {
            Token::LeftBrace => {
                self.advance(); // consume '{'
                let mut statements = Vec::new();
                while self.current.token != Token::RightBrace {
                    statements.push(self.parse_statement()?);
                }
                self.expect(Token::RightBrace)?;
                (statements, false)
            }
            Token::Semicolon => {
                self.advance(); // consume ';'
                (Vec::new(), true)
            }
            _ => {
                return Err(ParseError::UnexpectedToken {
                    found: self.current.clone(),
                    expected: Some("'{' or ';'".to_string()),
                });
            }
        };

        Ok(Stmt::Module {
            name,
            visibility,
            body,
            attributes,
            is_external,
        })
    }

    fn parse_function_statement(&mut self, visibility: bool, attributes: Vec<Attribute>, is_trait: bool) -> Result<Stmt, ParseError> {
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

        if is_trait && self.current.token == Token::Semicolon {
            self.advance();
            return Ok(Stmt::Function {
                name,
                visibility,
                is_async,
                type_params,
                params,
                return_type,
                body: Vec::new(),
                attributes,
            });
        }

        self.expect(Token::LeftBrace)?; // consume '{'
        let mut body = Vec::new();
        while self.current.token != Token::RightBrace {
            body.push(self.parse_statement()?);
        }
        self.expect(Token::RightBrace)?; // consume '}'

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
                            });
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

    fn parse_continue_statement(&mut self) -> Result<Stmt, ParseError> {
        self.advance(); // consume 'continue'
        let label = if let Token::Lifetime(lbl) = &self.current.token {
            let l = lbl.clone();
            self.advance();
            Some(l)
        } else {
            None
        };
        self.expect(Token::Semicolon)?;
        Ok(Stmt::Continue(label))
    }

    fn parse_break_statement(&mut self) -> Result<Stmt, ParseError> {
        self.advance(); // consume 'break'

        let label = if let Token::Lifetime(lbl) = &self.current.token {
            let l = lbl.clone();
            self.advance();
            Some(l)
        } else {
            None
        };

        let value = if self.current.token != Token::Semicolon { Some(self.parse_expression(0)?) } else { None };

        self.expect(Token::Semicolon)?;
        Ok(Stmt::Break(label, value))
    }

    pub fn parse_let_statement(&mut self, attributes: Vec<Attribute>) -> Result<Stmt, ParseError> {
        self.advance(); // consume 'let'
        let pattern = self.parse_pattern_binding()?;
        let type_annotation = if self.current.token == Token::Colon {
            self.advance();
            Some(self.parse_type()?)
        } else {
            None
        };
        let initializer = if self.current.token == Token::Assign {
            self.advance();
            Some(Box::new(self.parse_expression(0)?))
        } else {
            None
        };
        self.expect(Token::Semicolon)?;
        Ok(Stmt::Let {
            pattern,
            type_annotation,
            initializer,
            attributes,
        })
    }

    fn parse_pattern_binding(&mut self) -> Result<Pattern, ParseError> {
        if self.current.token == Token::Mut {
            self.advance();
            let pat = self.parse_pattern()?;
            match pat {
                Pattern::Identifier { name, .. } => Ok(Pattern::Identifier { name, mutable: true }),
                _ => Ok(pat),
            }
        } else {
            self.parse_pattern()
        }
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
                });
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

    pub fn parse_expression(&mut self, precedence: i32) -> Result<Expr, ParseError> {
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
        if let Token::Lifetime(label) = &self.current.token {
            if self.peek.token == Token::Colon {
                let label_str = label.clone();
                self.advance(); // consume lifetime
                self.advance(); // consume :

                match self.current.token {
                    Token::Loop => return self.parse_loop_expression(Some(label_str)),
                    Token::While => return self.parse_while_expression(Some(label_str)),
                    Token::For => return self.parse_for_expression(Some(label_str)),
                    _ => {
                        return Err(ParseError::UnexpectedToken {
                            found: self.current.clone(),
                            expected: Some("loop, while or for after label".to_string()),
                        });
                    }
                }
            }
        }

        if matches!(self.current.token, Token::Range | Token::RangeInclusive) {
            let inclusive = if let Token::RangeInclusive = self.current.token {
                self.advance();
                true
            } else {
                self.advance();
                false
            };

            let right = if self.token_starts_expression(&self.current.token) {
                Some(Box::new(self.parse_expression(PRECEDENCE_RANGE)?))
            } else {
                None
            };

            return Ok(Expr::Range { start: None, end: right, inclusive });
        }

        match &self.current.token {
            Token::Match => self.parse_match_expression(),

            Token::Async => self.parse_async_prefix(),

            Token::If => self.parse_if_expression(),

            Token::Loop => return self.parse_loop_expression(None),

            Token::While => return self.parse_while_expression(None),

            Token::For => return self.parse_for_expression(None),

            Token::Minus | Token::Not | Token::BitNot => {
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

                if self.current.token == Token::RightBracket {
                    self.advance(); // consume ]
                    return Ok(Expr::Array(vec![]));
                }

                let first_expr = self.parse_expression(0)?;

                if self.current.token == Token::Semicolon {
                    self.advance(); // consume ;
                    let count_expr = self.parse_expression(0)?;
                    self.expect(Token::RightBracket)?;

                    Ok(Expr::ArrayRepeat {
                        value: Box::new(first_expr),
                        count: Box::new(count_expr),
                    })
                } else {
                    let mut elements = vec![first_expr];

                    while self.current.token == Token::Comma {
                        self.advance(); // consume ,
                        if self.current.token == Token::RightBracket {
                            break;
                        }
                        elements.push(self.parse_expression(0)?);
                    }

                    self.expect(Token::RightBracket)?;
                    Ok(Expr::Array(elements))
                }
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
                let ident = name.clone();
                self.advance();
                return self.parse_identifier_expression(ident);
            }

            _ => {
                return Err(ParseError::ExpectedIdentifier {
                    location: self.current.location.to_owned(),
                });
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
            if let Stmt::ExpressionValue(_) = statements.last().unwrap() {
                if let Stmt::ExpressionValue(expr) = statements.pop().unwrap() {
                    Some(Box::new(expr))
                } else {
                    None
                }
            } else {
                None
            }
        } else {
            None
        };

        self.expect(Token::RightBrace)?;
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
                        });
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
            let path = self.parse_path_starting_with(name)?;

            if self.current.token == Token::LeftBrace {
                self.advance(); // consume {
                let fields = self.parse_struct_init_fields()?;
                self.expect(Token::RightBrace)?;
                return Ok(Expr::StructInit {
                    struct_name: path.segments.last().unwrap().ident.clone(),
                    fields,
                });
            }

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
            }
            return Ok(Expr::Path(path));
        }

        match self.current.token {
            Token::Assign => {
                self.advance();
                let value = Box::new(self.parse_expression(0)?);

                Ok(Expr::Assignment {
                    target: Box::new(Expr::Identifier(name)),
                    value,
                })
            }

            Token::RemAssign
            | Token::BitAndAssign
            | Token::StarEquals
            | Token::PlusEquals
            | Token::MinusEquals
            | Token::SlashEquals
            | Token::ShlAssign
            | Token::ShrAssign
            | Token::BitXorAssign
            | Token::BitOrAssign => {
                let operator = self.current.token.clone();
                self.advance();
                let value = Box::new(self.parse_expression(0)?);
                Ok(Expr::CompoundAssignment {
                    target: Box::new(Expr::Identifier(name)),
                    operator,
                    value,
                })
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

    fn token_starts_expression(&self, token: &Token) -> bool {
        match token {
            Token::Identifier(_)
            | Token::Integer(_, _)
            | Token::Float(_, _)
            | Token::String(_)
            | Token::True
            | Token::False
            | Token::LeftParen
            | Token::LeftBracket
            | Token::LeftBrace
            | Token::Minus
            | Token::Not
            | Token::BitNot
            | Token::Async
            | Token::If
            | Token::Match
            | Token::Loop
            | Token::While
            | Token::For
            | Token::Range
            | Token::RangeInclusive
            | Token::BitAnd => true,
            _ => false,
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

            Token::Range => {
                let precedence = self.get_precedence(&Token::Range);
                self.advance(); // consume ..

                let right = if self.token_starts_expression(&self.current.token) {
                    Some(Box::new(self.parse_expression(precedence)?))
                } else {
                    None
                };

                Ok(Expr::Range {
                    start: Some(Box::new(left)),
                    end: right,
                    inclusive: false,
                })
            }

            Token::RangeInclusive => {
                let precedence = self.get_precedence(&Token::RangeInclusive);
                self.advance(); // consume ..=

                let right = if self.token_starts_expression(&self.current.token) {
                    Some(Box::new(self.parse_expression(precedence)?))
                } else {
                    None
                };

                Ok(Expr::Range {
                    start: Some(Box::new(left)),
                    end: right,
                    inclusive: true,
                })
            }

            Token::Dot => {
                self.advance(); // consume .

                match self.current.token {
                    Token::Await => {
                        self.advance(); // consume 'await'
                        Ok(Expr::Await(Box::new(left)))
                    }
                    Token::Integer(n, _) if n >= 0 => {
                        let index = n.to_string();
                        self.advance(); // consume the integer
                        self.parse_member_access(left, index)
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
            | Token::Rem
            | Token::BitNot => {
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

            Token::PlusEquals
            | Token::MinusEquals
            | Token::StarEquals
            | Token::SlashEquals
            | Token::RemAssign
            | Token::BitAndAssign
            | Token::BitOrAssign
            | Token::BitXorAssign
            | Token::ShlAssign
            | Token::ShrAssign
            | Token::BitNotEquals => {
                let operator = self.current.token.clone();
                self.advance();
                let value = self.parse_expression(PRECEDENCE_ASSIGN - 1)?;
                Ok(Expr::CompoundAssignment {
                    target: Box::new(left),
                    operator,
                    value: Box::new(value),
                })
            }

            Token::LeftBracket => {
                self.advance(); // consume [
                let index = self.parse_expression(0)?;
                self.expect(Token::RightBracket)?;

                let indexed_expr = Expr::Index {
                    array: Box::new(left),
                    index: Box::new(index),
                };

                match self.current.token {
                    Token::Assign => {
                        self.advance(); // consume =
                        let value = self.parse_expression(0)?;
                        Ok(Expr::Assignment {
                            target: Box::new(indexed_expr),
                            value: Box::new(value),
                        })
                    }

                    Token::PlusEquals
                    | Token::MinusEquals
                    | Token::StarEquals
                    | Token::SlashEquals
                    | Token::RemAssign
                    | Token::BitAndAssign
                    | Token::BitOrAssign
                    | Token::BitXorAssign
                    | Token::ShlAssign
                    | Token::ShrAssign
                    | Token::BitNotEquals => {
                        let operator = self.current.token.clone();
                        self.advance();
                        let value = self.parse_expression(0)?;
                        Ok(Expr::CompoundAssignment {
                            target: Box::new(indexed_expr),
                            operator,
                            value: Box::new(value),
                        })
                    }

                    _ => Ok(indexed_expr),
                }
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

    fn parse_loop_expression(&mut self, label: Option<String>) -> Result<Expr, ParseError> {
        self.advance(); // consume 'loop'
        self.expect(Token::LeftBrace)?;
        let body = self.parse_block_expression()?;
        Ok(Expr::Loop { label, body: Box::new(body) })
    }

    fn parse_while_expression(&mut self, label: Option<String>) -> Result<Expr, ParseError> {
        self.advance(); // consume 'while'
        let condition = if self.current.token == Token::Let {
            self.advance(); // consume 'let'
            let pattern = self.parse_pattern()?;
            self.expect(Token::Assign)?;
            let expr = self.parse_expression(0)?;
            WhileCondition::Let(pattern, Box::new(expr))
        } else {
            let expr = self.parse_expression(0)?;
            WhileCondition::Expression(Box::new(expr))
        };
        self.expect(Token::LeftBrace)?;
        let body = self.parse_block_expression()?;
        Ok(Expr::While {
            label,
            condition,
            body: Box::new(body),
        })
    }

    fn parse_for_expression(&mut self, label: Option<String>) -> Result<Expr, ParseError> {
        self.advance(); // consume 'for'
        let pattern = self.parse_pattern()?;

        self.expect(Token::In)?;
        let iterable = self.parse_expression(0)?;

        self.expect(Token::LeftBrace)?;
        let body = self.parse_block_expression()?;

        Ok(Expr::For {
            label,
            pattern,
            iterable: Box::new(iterable),
            body: Box::new(body),
        })
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

    fn parse_struct_init_fields(&mut self) -> Result<HashMap<String, (Expr, bool)>, ParseError> {
        let mut fields = HashMap::new();

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

            fields.insert(field_name, (field_value, is_shorthand));
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

            Token::RemAssign
            | Token::BitAndAssign
            | Token::StarEquals
            | Token::PlusEquals
            | Token::MinusEquals
            | Token::SlashEquals
            | Token::ShlAssign
            | Token::ShrAssign
            | Token::BitXorAssign
            | Token::BitOrAssign => {
                let operator = self.current.token.clone();
                self.advance();
                let value = Box::new(self.parse_expression(0)?);
                Ok(Expr::CompoundAssignment {
                    target: Box::new(Expr::MemberAccess { object: Box::new(object), member }),
                    operator,
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

        if self.current.token == Token::Let {
            self.advance();
            let pattern = self.parse_pattern()?;

            self.expect(Token::Assign)?;
            let value = Box::new(self.parse_expression(0)?);

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

            return Ok(Expr::IfLet {
                pattern,
                value,
                then_branch,
                else_branch,
            });
        } else {
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
    }

    fn parse_pattern(&mut self) -> Result<Pattern, ParseError> {
        match &self.current.token {
            Token::LeftParen => {
                self.advance(); // consume '('

                if self.current.token == Token::RightParen {
                    self.advance(); // consume ')'
                    return Ok(Pattern::Tuple(vec![]));
                }

                let mut patterns = Vec::new();
                patterns.push(self.parse_pattern()?);

                while self.current.token == Token::Comma {
                    self.advance(); // consume ','
                    if self.current.token == Token::RightParen {
                        break;
                    }
                    patterns.push(self.parse_pattern()?);
                }

                self.expect(Token::RightParen)?; // consume ')'
                return Ok(Pattern::Tuple(patterns));
            }

            Token::Mut => {
                self.advance(); // consume 'mut'
                let inner = self.parse_pattern()?;
                match inner {
                    Pattern::Identifier { name, .. } => Ok(Pattern::Identifier { name, mutable: true }),
                    _ => {
                        return Err(ParseError::Custom {
                            message: "'mut' is only allowed with identifier patterns".to_string(),
                            location: self.current.location.clone(),
                        });
                    }
                }
            }

            Token::Integer(n, t) => {
                let n = *n;
                let t = t.clone();
                self.advance();
                Ok(Pattern::Literal(Box::new(Expr::Integer(n, t))))
            }

            Token::Float(n, t) => {
                let n = *n;
                let t = t.clone();
                self.advance();
                Ok(Pattern::Literal(Box::new(Expr::Float(n, t))))
            }

            Token::String(s) => {
                let s = s.clone();
                self.advance();
                Ok(Pattern::Literal(Box::new(Expr::String(s))))
            }

            Token::True => {
                self.advance();
                Ok(Pattern::Literal(Box::new(Expr::Boolean(true))))
            }

            Token::False => {
                self.advance();
                Ok(Pattern::Literal(Box::new(Expr::Boolean(false))))
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

            Token::Minus => {
                self.advance(); // consume the minus
                match &self.current.token {
                    Token::Integer(n, t) => {
                        let n = -n;
                        let t = t.clone();
                        self.advance();
                        Ok(Pattern::Literal(Box::new(Expr::Integer(n, t))))
                    }
                    Token::Float(n, t) => {
                        let n = -n;
                        let t = t.clone();
                        self.advance();
                        Ok(Pattern::Literal(Box::new(Expr::Float(n, t))))
                    }
                    _ => Err(ParseError::ExpectedExpression {
                        location: self.current.location.clone(),
                    }),
                }
            }

            Token::Identifier(name) => {
                let name = name.clone();
                self.advance();

                if name == "_" {
                    return Ok(Pattern::Wildcard);
                }

                if self.current.token == Token::LeftParen {
                    self.advance(); // consume '('

                    let mut patterns = Vec::new();
                    if self.current.token != Token::RightParen {
                        patterns.push(self.parse_pattern()?);
                        while self.current.token == Token::Comma {
                            self.advance(); // consume ','
                            if self.current.token == Token::RightParen {
                                break;
                            }
                            patterns.push(self.parse_pattern()?);
                        }
                    }
                    self.expect(Token::RightParen)?; // consume ')'

                    let path = Path {
                        segments: vec![PathSegment { ident: name, generics: Vec::new() }],
                    };

                    return Ok(Pattern::TupleStruct { path, elements: patterns });
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

                    match &self.current.token {
                        Token::LeftParen => {
                            self.advance(); // consume '('

                            if self.current.token == Token::RightParen {
                                self.advance(); // consume ')'
                                return Ok(Pattern::TupleStruct { path, elements: vec![] });
                            }

                            let mut patterns = Vec::new();
                            patterns.push(self.parse_pattern()?);

                            while self.current.token == Token::Comma {
                                self.advance(); // consume ','
                                if self.current.token == Token::RightParen {
                                    break;
                                }
                                patterns.push(self.parse_pattern()?);
                            }

                            self.expect(Token::RightParen)?; // consume ')'
                            Ok(Pattern::TupleStruct { path, elements: patterns })
                        }

                        Token::Mut => {
                            self.advance(); // consume 'mut'
                            let inner = self.parse_pattern()?;
                            match inner {
                                Pattern::Identifier { name, .. } => Ok(Pattern::Identifier { name, mutable: true }),
                                _ => {
                                    return Err(ParseError::Custom {
                                        message: "'mut' is only allowed with identifier patterns".to_string(),
                                        location: self.current.location.clone(),
                                    });
                                }
                            }
                        }

                        Token::Identifier(name) => {
                            let name = name.clone();
                            self.advance();
                            if name == "_" { Ok(Pattern::Wildcard) } else { Ok(Pattern::Identifier { name, mutable: false }) }
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

            _ => Err(ParseError::UnexpectedToken {
                found: self.current.clone(),
                expected: Some("pattern".to_string()),
            }),
        }
    }

    fn parse_block_expression(&mut self) -> Result<Expr, ParseError> { self.parse_block_expression_with_async(false) }

    fn get_precedence(&self, token: &Token) -> i32 {
        match token {
            Token::Range | Token::RangeInclusive => PRECEDENCE_RANGE,
            Token::Dot => PRECEDENCE_MEMBER,
            Token::LeftParen => PRECEDENCE_CALL,
            Token::Question => PRECEDENCE_QUESTION,
            Token::Equals | Token::NotEquals | Token::BitNotEquals => PRECEDENCE_EQUALS,
            Token::LeftAngle | Token::RightAngle | Token::LessEquals | Token::GreaterEquals => PRECEDENCE_COMPARE,
            Token::Or | Token::BitOr => PRECEDENCE_OR,
            Token::And | Token::BitAnd => PRECEDENCE_AND,
            Token::Plus | Token::Minus => PRECEDENCE_SUM,
            Token::Star | Token::Slash | Token::Rem => PRECEDENCE_PRODUCT,
            Token::BitXor => PRECEDENCE_OR,
            Token::Not | Token::BitNot => PRECEDENCE_PREFIX,
            Token::LeftBracket => PRECEDENCE_INDEX,
            Token::As => PRECEDENCE_CALL,
            Token::RemAssign
            | Token::BitAndAssign
            | Token::StarEquals
            | Token::PlusEquals
            | Token::MinusEquals
            | Token::SlashEquals
            | Token::ShlAssign
            | Token::ShrAssign
            | Token::BitXorAssign
            | Token::BitOrAssign => PRECEDENCE_ASSIGN,
            _ => PRECEDENCE_LOWEST,
        }
    }
}
