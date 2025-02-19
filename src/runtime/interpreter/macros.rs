use super::*;
use crate::parser::ast::NumericType;

#[derive(Clone, Debug)]
pub struct RepetitionBlock {
    variable: String,
    tokens: Vec<TokenInfo>,
    separator: Option<Token>,
}

impl<'st> Interpreter<'st> {
    pub fn handle_macro_definition(&mut self, name: &str, tokens: &[TokenInfo]) -> Result<(), String> {
        let delimiter = self.extract_macro_delimiter(tokens)?;
        self.mcs.insert(name.to_string(), (delimiter, tokens.to_vec()));
        Ok(())
    }

    pub fn expand_macro_with_recursion_check(&mut self, name: &str, delimiter: &MacroDelimiter, tokens: &[TokenInfo], depth: usize) -> Result<Expr, String> {
        if depth > 100 {
            return Err("Maximum macro recursion depth exceeded".to_string());
        }

        let expanded = self.expand_macro(name, delimiter, tokens)?;
        self.process_nested_macros(&expanded, depth + 1)
    }

    fn expand_macro(&mut self, name: &str, delimiter: &MacroDelimiter, tokens: &[TokenInfo]) -> Result<Expr, String> {
        match self.handle_procedural_macro(name, tokens) {
            Ok(expanded_tokens) => return self.parse_expanded_tokens(expanded_tokens),
            Err(e) if e.is_none() => {}
            Err(e) => return Err(e.unwrap()),
        }

        if let Some((def_delimiter, def_tokens)) = self.mcs.get(name) {
            if delimiter != def_delimiter {
                return Err(format!("Macro '{}' invoked with wrong delimiter", name));
            }

            let params = self.extract_macro_parameters(def_tokens)?;
            let args = self.extract_macro_arguments(tokens)?;

            if params.len() != args.len() {
                return Err(format!("Macro '{}' expected {} arguments, got {}", name, params.len(), args.len()));
            }

            let processed_tokens = self.process_repetition_pattern(def_tokens, &args)?;
            let expanded_tokens = self.substitute_macro_tokens(&processed_tokens, &params, &args)?;
            self.parse_expanded_tokens(expanded_tokens)
        } else {
            Err(format!("Macro '{}' not found", name))
        }
    }

    fn handle_procedural_macro(&mut self, name: &str, tokens: &[TokenInfo]) -> Result<Vec<TokenInfo>, Option<String>> {
        match name {
            "stringify" => {
                let content = tokens_to_string(tokens);
                let string_token = TokenInfo {
                    token: Token::String(content),
                    location: tokens.first().map_or_else(|| Location { line: 0, column: 0 }, |t| t.location.clone()),
                };

                Ok(vec![string_token])
            }

            "concat" => {
                let mut result = String::new();
                for token in tokens {
                    if let Token::String(s) = &token.token {
                        result.push_str(s);
                    }
                }

                let concat_token = TokenInfo {
                    token: Token::String(result),
                    location: tokens.first().map_or_else(|| Location { line: 0, column: 0 }, |t| t.location.clone()),
                };

                Ok(vec![concat_token])
            }

            "include_str" => {
                if tokens.len() != 1 {
                    return Err(Some("include_str! requires exactly one file path argument".to_string()));
                }

                if let Token::String(path) = &tokens[0].token {
                    match std::fs::read_to_string(path) {
                        Ok(content) => {
                            let include_token = TokenInfo {
                                token: Token::String(content),
                                location: tokens[0].location.clone(),
                            };
                            Ok(vec![include_token])
                        }
                        Err(e) => Err(Some(format!("Failed to read file '{}': {}", path, e))),
                    }
                } else {
                    Err(Some("include_str! argument must be a string literal".to_string()))
                }
            }

            "format" => {
                if tokens.is_empty() {
                    return Ok(vec![TokenInfo {
                        token: Token::String("".to_string()),
                        location: Location { line: 0, column: 0 },
                    }]);
                }

                let fmt_token = &tokens[0];
                let fmt_str = if let Token::String(ref s) = fmt_token.token {
                    s.clone()
                } else {
                    return Err(Some("format! requires a string literal as its first argument".to_string()));
                };

                let mut parts = Vec::new();
                let mut placeholders = 0;
                let mut current_part = String::new();
                let mut i = 0;

                while i < fmt_str.len() {
                    if fmt_str[i..].starts_with("{}") {
                        parts.push(current_part);
                        current_part = String::new();
                        placeholders += 1;
                        i += 2;
                    } else {
                        current_part.push(fmt_str.chars().nth(i).unwrap());
                        i += 1;
                    }
                }

                parts.push(current_part);

                let args = self.extract_macro_arguments(&tokens[1..])?;
                if placeholders != args.len() {
                    return Err(Some(format!("format! expected {} arguments, got {}", placeholders, args.len())));
                }

                let mut new_tokens = Vec::new();
                let first_location = fmt_token.location.clone();

                new_tokens.push(TokenInfo {
                    token: Token::Identifier("core".to_string()),
                    location: first_location.clone(),
                });

                new_tokens.push(TokenInfo {
                    token: Token::DoubleColon,
                    location: first_location.clone(),
                });

                new_tokens.push(TokenInfo {
                    token: Token::Identifier("concat".to_string()),
                    location: first_location.clone(),
                });

                new_tokens.push(TokenInfo {
                    token: Token::LeftParen,
                    location: first_location.clone(),
                });

                for (i, part) in parts.iter().enumerate() {
                    new_tokens.push(TokenInfo {
                        token: Token::String(part.clone()),
                        location: first_location.clone(),
                    });

                    if i < args.len() {
                        new_tokens.push(TokenInfo {
                            token: Token::Comma,
                            location: first_location.clone(),
                        });

                        new_tokens.extend(args[i].clone());
                    }

                    if i < parts.len() - 1 {
                        new_tokens.push(TokenInfo {
                            token: Token::Comma,
                            location: first_location.clone(),
                        });
                    }
                }

                new_tokens.push(TokenInfo {
                    token: Token::RightParen,
                    location: first_location.clone(),
                });

                Ok(new_tokens)
            }

            "println" => {
                let mut format_invocation = Vec::new();
                let first_location = tokens.first().map_or(Location { line: 0, column: 0 }, |t| t.location.clone());

                format_invocation.push(TokenInfo {
                    token: Token::Identifier("format".to_string()),
                    location: first_location.clone(),
                });

                format_invocation.push(TokenInfo {
                    token: Token::Not,
                    location: first_location.clone(),
                });

                format_invocation.push(TokenInfo {
                    token: Token::LeftParen,
                    location: first_location.clone(),
                });

                format_invocation.extend_from_slice(tokens);
                format_invocation.push(TokenInfo {
                    token: Token::RightParen,
                    location: first_location.clone(),
                });

                // io::print( format!( ... ) )
                let mut result_tokens = Vec::new();
                result_tokens.push(TokenInfo {
                    token: Token::Identifier("io".to_string()),
                    location: first_location.clone(),
                });
                result_tokens.push(TokenInfo {
                    token: Token::DoubleColon,
                    location: first_location.clone(),
                });
                result_tokens.push(TokenInfo {
                    token: Token::Identifier("println".to_string()),
                    location: first_location.clone(),
                });
                result_tokens.push(TokenInfo {
                    token: Token::LeftParen,
                    location: first_location.clone(),
                });
                result_tokens.extend(format_invocation);
                result_tokens.push(TokenInfo {
                    token: Token::RightParen,
                    location: first_location,
                });
                Ok(result_tokens)
            }

            // add more built-in procedural macros later
            _ => Err(None),
        }
    }

    fn extract_macro_delimiter(&self, tokens: &[TokenInfo]) -> Result<MacroDelimiter, String> {
        for token_info in tokens {
            match token_info.token {
                Token::LeftParen => return Ok(MacroDelimiter::Paren),
                Token::LeftBracket => return Ok(MacroDelimiter::Bracket),
                Token::LeftBrace => return Ok(MacroDelimiter::Brace),
                _ => continue,
            }
        }

        Err("Macro definition missing delimiter".to_string())
    }

    fn extract_macro_parameters(&self, tokens: &[TokenInfo]) -> Result<Vec<String>, String> {
        let mut params = Vec::new();
        for token_info in tokens {
            if let Token::Identifier(name) = &token_info.token {
                if name.starts_with('$') {
                    params.push(name[1..].to_string());
                }
            }
        }
        Ok(params)
    }

    fn parse_expanded_tokens(&self, tokens: Vec<TokenInfo>) -> Result<Expr, String> {
        let input = tokens_to_string(&tokens);
        let lexer = Lexer::new(input);
        let mut parser = crate::parser::Parser::new(lexer);

        parser.parse_expression(0).map_err(|e| format!("Failed to parse expanded macro: {}", e))
    }

    fn extract_macro_arguments(&self, tokens: &[TokenInfo]) -> Result<Vec<Vec<TokenInfo>>, String> {
        // simple implementation - just splits by commas
        let mut args = Vec::new();
        let mut current_arg = Vec::new();
        let mut nesting = 0;

        for token_info in tokens {
            match token_info.token {
                Token::LeftParen | Token::LeftBrace | Token::LeftBracket => {
                    nesting += 1;
                    current_arg.push(token_info.clone());
                }
                Token::RightParen | Token::RightBrace | Token::RightBracket => {
                    nesting -= 1;
                    current_arg.push(token_info.clone());
                }
                Token::Comma if nesting == 0 => {
                    if !current_arg.is_empty() {
                        args.push(current_arg);
                        current_arg = Vec::new();
                    }
                }
                _ => current_arg.push(token_info.clone()),
            }
        }

        if !current_arg.is_empty() {
            args.push(current_arg);
        }

        Ok(args)
    }

    fn substitute_macro_tokens(&self, def_tokens: &[TokenInfo], params: &[String], args: &[Vec<TokenInfo>]) -> Result<Vec<TokenInfo>, String> {
        let mut result = Vec::new();

        for token_info in def_tokens {
            if let Token::Identifier(name) = &token_info.token {
                if name.starts_with('$') {
                    let param_name = &name[1..];
                    if let Some(index) = params.iter().position(|p| p == param_name) {
                        // insert the argument tokens
                        result.extend_from_slice(&args[index]);
                    } else {
                        // keep the token as is
                        result.push(token_info.clone());
                    }
                } else {
                    // regular identifier
                    result.push(token_info.clone());
                }
            } else {
                // non-identifier token
                result.push(token_info.clone());
            }
        }

        Ok(result)
    }

    fn process_nested_macros(&mut self, expr: &Expr, depth: usize) -> Result<Expr, String> {
        match expr {
            Expr::MacroInvocation { name, delimiter, tokens } => self.expand_macro_with_recursion_check(name, delimiter, tokens, depth),

            Expr::Block { statements, value, returns, is_async } => {
                let mut processed_stmts = Vec::new();

                for stmt in statements {
                    let processed_stmt = self.process_nested_macros_in_stmt(stmt, depth)?;
                    processed_stmts.push(processed_stmt);
                }

                let processed_value = if let Some(val) = value { Some(Box::new(self.process_nested_macros(val, depth)?)) } else { None };

                Ok(Expr::Block {
                    statements: processed_stmts,
                    value: processed_value,
                    returns: *returns,
                    is_async: *is_async,
                })
            }

            // process other expression types recursively
            // ... handle other expression types
            _ => Ok(expr.clone()),
        }
    }

    fn process_nested_macros_in_stmt(&mut self, stmt: &Stmt, depth: usize) -> Result<Stmt, String> {
        match stmt {
            Stmt::ExpressionStmt(expr) => {
                let processed = self.process_nested_macros(expr, depth)?;
                Ok(Stmt::ExpressionStmt(processed))
            }

            Stmt::Let {
                pattern,
                type_annotation,
                initializer,
                attributes,
            } => {
                let processed_init = if let Some(init) = initializer {
                    Some(Box::new(self.process_nested_macros(init, depth)?))
                } else {
                    None
                };

                Ok(Stmt::Let {
                    pattern: pattern.clone(),
                    type_annotation: type_annotation.clone(),
                    initializer: processed_init,
                    attributes: attributes.clone(),
                })
            }

            // process other statement types
            // ... handle other statement types
            _ => Ok(stmt.clone()),
        }
    }

    fn process_repetition_pattern(&self, tokens: &[TokenInfo], args: &[Vec<TokenInfo>]) -> Result<Vec<TokenInfo>, String> {
        let mut result = Vec::new();
        let mut repetition_blocks = self.find_repetition_blocks(tokens)?;

        for block in &mut repetition_blocks {
            let expanded = self.expand_repetition_block(block, args)?;
            result.extend(expanded);
        }

        Ok(result)
    }

    fn find_repetition_blocks(&self, tokens: &[TokenInfo]) -> Result<Vec<RepetitionBlock>, String> {
        let mut blocks = Vec::new();
        let mut i = 0;

        while i < tokens.len() {
            if let Token::Identifier(name) = &tokens[i].token {
                if name.starts_with('$') && i + 3 < tokens.len() {
                    if matches!(tokens[i + 1].token, Token::Colon) {
                        let var_name = name[1..].to_string();
                        let (block_start, block_end) = self.find_repetition_bounds(&tokens[i + 2..])?;

                        blocks.push(RepetitionBlock {
                            variable: var_name,
                            tokens: tokens[i + 2 + block_start..i + 2 + block_end].to_vec(),
                            separator: self.find_separator(&tokens[i + 2 + block_end..])?,
                        });

                        i = i + 2 + block_end;
                    }
                }
            }
            i += 1;
        }

        Ok(blocks)
    }

    fn find_repetition_bounds(&self, tokens: &[TokenInfo]) -> Result<(usize, usize), String> {
        let mut nesting = 0;
        let mut start = 0;

        while start < tokens.len() {
            match tokens[start].token {
                Token::LeftParen | Token::LeftBrace | Token::LeftBracket => {
                    nesting = 1;
                    break;
                }
                _ => start += 1,
            }
        }

        if start >= tokens.len() {
            return Err("Missing repetition block start delimiter".to_string());
        }

        let mut end = start + 1;
        while end < tokens.len() {
            match tokens[end].token {
                Token::LeftParen | Token::LeftBrace | Token::LeftBracket => nesting += 1,
                Token::RightParen | Token::RightBrace | Token::RightBracket => {
                    nesting -= 1;
                    if nesting == 0 {
                        return Ok((start, end + 1));
                    }
                }
                _ => (),
            }
            end += 1;
        }

        Err("Unmatched repetition block delimiter".to_string())
    }

    fn find_separator(&self, tokens: &[TokenInfo]) -> Result<Option<Token>, String> {
        if tokens.len() >= 2 && matches!(tokens[0].token, Token::Star) {
            // $(...) * token
            Ok(Some(tokens[1].token.clone()))
        } else {
            Ok(None) // no separator
        }
    }

    fn expand_repetition_block(&self, block: &RepetitionBlock, args: &[Vec<TokenInfo>]) -> Result<Vec<TokenInfo>, String> {
        let mut result = Vec::new();

        let matched_args = args.iter().filter(|arg| self.contains_variable(&block.variable, arg)).collect::<Vec<_>>();

        for (i, arg) in matched_args.iter().enumerate() {
            if i > 0 {
                if let Some(separator) = &block.separator {
                    result.push(TokenInfo {
                        token: separator.clone(),
                        location: block.tokens.first().unwrap().location.clone(),
                    });
                }
            }

            let expanded = self.substitute_variables_in_block(&block.tokens, &block.variable, arg)?;
            result.extend(expanded);
        }

        Ok(result)
    }

    fn contains_variable(&self, var_name: &str, arg: &[TokenInfo]) -> bool { arg.iter().any(|token| if let Token::Identifier(name) = &token.token { name == var_name } else { false }) }

    fn substitute_variables_in_block(&self, block_tokens: &[TokenInfo], var_name: &str, arg_tokens: &[TokenInfo]) -> Result<Vec<TokenInfo>, String> {
        let mut result = Vec::new();

        for token in block_tokens {
            if let Token::Identifier(name) = &token.token {
                if name == var_name {
                    result.extend_from_slice(arg_tokens);
                } else {
                    result.push(token.clone());
                }
            } else {
                result.push(token.clone());
            }
        }

        Ok(result)
    }
}

fn tokens_to_string(tokens: &[TokenInfo]) -> String {
    let mut result = String::new();

    for token_info in tokens {
        result.push_str(&token_to_string(&token_info.token));
        result.push(' ');
    }

    result.trim_end().to_string()
}

fn token_to_string(token: &Token) -> String {
    match token {
        Token::Pound => "#".to_string(),
        Token::Dollar => "$".to_string(),
        Token::MacroRules => "macro_rules!".to_string(),

        Token::Let => "let".to_string(),
        Token::Mut => "mut".to_string(),
        Token::Const => "const".to_string(),
        Token::Static => "static".to_string(),
        Token::Struct => "struct".to_string(),
        Token::Trait => "trait".to_string(),
        Token::Impl => "impl".to_string(),
        Token::Enum => "enum".to_string(),
        Token::Fn => "fn".to_string(),
        Token::Return => "return".to_string(),
        Token::Continue => "continue".to_string(),
        Token::Break => "break".to_string(),
        Token::If => "if".to_string(),
        Token::Else => "else".to_string(),
        Token::True => "true".to_string(),
        Token::False => "false".to_string(),
        Token::Type => "type".to_string(),
        Token::Pub => "pub".to_string(),
        Token::Async => "async".to_string(),
        Token::Await => "await".to_string(),
        Token::Match => "match".to_string(),
        Token::Loop => "loop".to_string(),
        Token::While => "while".to_string(),
        Token::For => "for".to_string(),
        Token::In => "in".to_string(),

        Token::Use => "use".to_string(),
        Token::Module => "mod".to_string(),
        Token::As => "as".to_string(),
        Token::DoubleColon => "::".to_string(),

        Token::Dot => ".".to_string(),
        Token::Range => "..".to_string(),
        Token::LeftParen => "(".to_string(),
        Token::RightParen => ")".to_string(),
        Token::LeftBracket => "[".to_string(),
        Token::RightBracket => "]".to_string(),
        Token::LeftBrace => "{".to_string(),
        Token::RightBrace => "}".to_string(),
        Token::Semicolon => ";".to_string(),
        Token::Colon => ":".to_string(),
        Token::Comma => ",".to_string(),
        Token::Arrow => "->".to_string(),
        Token::LeftAngle => "<".to_string(),
        Token::RightAngle => ">".to_string(),
        Token::Question => "?".to_string(),
        Token::Fat => "=>".to_string(),

        Token::Plus => "+".to_string(),
        Token::Minus => "-".to_string(),
        Token::Star => "*".to_string(),
        Token::Slash => "/".to_string(),
        Token::Assign => "=".to_string(),
        Token::Equals => "==".to_string(),

        Token::Not => "!".to_string(),
        Token::NotEquals => "!=".to_string(),
        Token::Rem => "%".to_string(),
        Token::RemAssign => "%=".to_string(),
        Token::BitAnd => "&".to_string(),
        Token::BitAndAssign => "&=".to_string(),
        Token::And => "&&".to_string(),
        Token::StarEquals => "*=".to_string(),
        Token::PlusEquals => "+=".to_string(),
        Token::MinusEquals => "-=".to_string(),
        Token::SlashEquals => "/=".to_string(),
        Token::Shl => "<<".to_string(),
        Token::ShlAssign => "<<=".to_string(),
        Token::LessEquals => "<=".to_string(),
        Token::GreaterEquals => ">=".to_string(),
        Token::Shr => ">>".to_string(),
        Token::ShrAssign => ">>=".to_string(),
        Token::BitXor => "^".to_string(),
        Token::BitXorAssign => "^=".to_string(),
        Token::BitOr => "|".to_string(),
        Token::BitOrAssign => "|=".to_string(),
        Token::Or => "||".to_string(),

        Token::Identifier(s) => s.clone(),
        Token::String(s) => format!("\"{}\"", s),
        Token::Lifetime(s) => s.clone(),

        Token::Integer(val, maybe_suffix) => {
            let mut s = val.to_string();
            if let Some(suffix) = maybe_suffix {
                match suffix {
                    NumericType::I8 => s.push_str("i8"),
                    NumericType::I16 => s.push_str("i16"),
                    NumericType::I32 => s.push_str("i32"),
                    NumericType::I64 => s.push_str("i64"),
                    NumericType::I128 => s.push_str("i128"),
                    NumericType::ISize => s.push_str("isize"),

                    NumericType::U8 => s.push_str("u8"),
                    NumericType::U16 => s.push_str("u16"),
                    NumericType::U32 => s.push_str("u32"),
                    NumericType::U64 => s.push_str("u64"),
                    NumericType::U128 => s.push_str("u128"),
                    NumericType::USize => s.push_str("usize"),

                    NumericType::F32 => s.push_str("f32"),
                    NumericType::F64 => s.push_str("f64"),
                }
            }
            s
        }

        Token::Float(val, maybe_suffix) => {
            let mut s = val.to_string();
            if let Some(suffix) = maybe_suffix {
                match suffix {
                    NumericType::I8 => s.push_str("i8"),
                    NumericType::I16 => s.push_str("i16"),
                    NumericType::I32 => s.push_str("i32"),
                    NumericType::I64 => s.push_str("i64"),
                    NumericType::I128 => s.push_str("i128"),
                    NumericType::ISize => s.push_str("isize"),

                    NumericType::U8 => s.push_str("u8"),
                    NumericType::U16 => s.push_str("u16"),
                    NumericType::U32 => s.push_str("u32"),
                    NumericType::U64 => s.push_str("u64"),
                    NumericType::U128 => s.push_str("u128"),
                    NumericType::USize => s.push_str("usize"),

                    NumericType::F32 => s.push_str("f32"),
                    NumericType::F64 => s.push_str("f64"),
                }
            }
            s
        }

        Token::EOF => "".to_string(),
    }
}
