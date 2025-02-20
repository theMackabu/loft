mod proc;
mod str;

use super::*;
use crate::parser::ast::NumericType;

#[derive(Clone, Debug)]
pub struct RepetitionBlock {
    variable: String,
    tokens: Vec<TokenInfo>,
    separator: Option<Token>,
}

enum Placeholder {
    Named(String),
    Positional(Option<usize>),
}

impl<'st> Interpreter<'st> {
    pub fn handle_macro_definition(&mut self, name: &str, tokens: &[TokenInfo]) -> Result<(), String> {
        let delimiter = extract_macro_delimiter(tokens)?;
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
        match proc::handle_procedural_macro(name, tokens) {
            Ok(expanded_tokens) => return self.parse_expanded_tokens(expanded_tokens),
            Err(e) if e.is_none() => {}
            Err(e) => return Err(e.unwrap()),
        }

        if let Some((def_delimiter, def_tokens)) = self.mcs.get(name) {
            if delimiter != def_delimiter {
                return Err(format!("Macro '{}' invoked with wrong delimiter", name));
            }

            let params = self.extract_macro_parameters(def_tokens)?;
            let args = extract_macro_arguments(tokens)?;

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

    fn extract_macro_parameters(&self, tokens: &[TokenInfo]) -> Result<Vec<String>, String> {
        let mut params = Vec::new();
        let mut i = 0;

        while i < tokens.len() {
            if let Token::Fat = tokens[i].token {
                break;
            }

            if let Token::Dollar = tokens[i].token {
                if i + 1 < tokens.len() {
                    if let Token::Identifier(ref name) = tokens[i + 1].token {
                        params.push(name.clone());
                        i += 2;
                        continue;
                    } else {
                        return Err("Expected identifier after '$' in macro parameters".to_string());
                    }
                }
            }

            i += 1;
        }

        Ok(params)
    }

    fn parse_expanded_tokens(&self, tokens: Vec<TokenInfo>) -> Result<Expr, String> {
        let input = str::tokens_to_string(&tokens);
        let lexer = Lexer::new(input);
        let mut parser = crate::parser::Parser::new(lexer);

        parser.parse_expression(0).map_err(|e| format!("Failed to parse expanded macro: {}", e))
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

fn extract_macro_arguments(tokens: &[TokenInfo]) -> Result<Vec<Vec<TokenInfo>>, String> {
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

fn extract_macro_delimiter(tokens: &[TokenInfo]) -> Result<MacroDelimiter, String> {
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
