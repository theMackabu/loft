mod proc;
mod str;

use super::*;
use crate::parser::ast::NumericType;

macro_rules! debug {
    ($($arg:tt)*) => {{
        let blue = "\x1b[38;5;27m";
        let reset = "\x1b[0m";
        eprintln!("{}[DEBUG] {}{}", blue, format_args!($($arg)*), reset);
    }};
}

macro_rules! warn {
    ($($arg:tt)*) => {{
        let yellow = "\x1b[38;5;11m";
        let reset = "\x1b[0m";
        eprintln!("{}[WARN] {}{}", yellow, format_args!($($arg)*), reset);
    }};
}

macro_rules! trace {
    ($($arg:tt)*) => {{
        let pink = "\x1b[38;5;13m";
        let reset = "\x1b[0m";
        eprintln!("{}[TRACE] {}{}", pink, format_args!($($arg)*), reset);
    }};
}

#[derive(Clone, Debug)]
pub struct RepetitionBlock {
    variable: String,
    tokens: Vec<TokenInfo>,
    separator: Option<Token>,
}

impl<'st> Interpreter<'st> {
    pub fn handle_macro_definition(&mut self, name: &str, tokens: &[TokenInfo]) -> Result<(), String> {
        debug!("Handling macro definition: '{}'", name);
        let delimiter = extract_macro_delimiter(tokens)?;
        debug!("Extracted delimiter: {:?}", delimiter);
        self.mcs.insert(name.to_string(), (delimiter, tokens.to_vec()));
        debug!("Macro '{}' successfully defined", name);
        Ok(())
    }

    pub fn expand_macro(&mut self, name: &str, delimiter: &MacroDelimiter, tokens: &[TokenInfo], depth: usize) -> Result<Expr, String> {
        debug!("Expanding macro '{}' at depth {}", name, depth);
        if depth > 100 {
            debug!("Maximum macro recursion depth exceeded for '{}'", name);
            return Err("Maximum macro recursion depth exceeded".to_string());
        }

        let expanded = self.expand_macro_inner(name, delimiter, tokens)?;
        debug!("Macro '{}' expanded, processing nested macros", name);
        self.process_nested_macros(&expanded, depth + 1)
    }

    fn parse_expanded_tokens(&self, tokens: &[TokenInfo]) -> Result<Expr, String> {
        debug!("Parsing expanded tokens (count: {})", tokens.len());
        let input = str::tokens_to_string(tokens);
        trace!("Expanded tokens as string: {}", input);
        let lexer = Lexer::new(input);
        let mut parser = crate::parser::Parser::new(lexer);

        let result = parser.parse_expression(0).map_err(|e| {
            warn!("Failed to parse expanded macro: {}", e);
            format!("Failed to parse expanded macro: {}", e)
        });
        debug!("Expanded tokens parsed successfully");
        result
    }

    fn expand_macro_inner(&mut self, name: &str, delimiter: &MacroDelimiter, tokens: &[TokenInfo]) -> Result<Expr, String> {
        debug!("Expanding macro inner: '{}'", name);
        match proc::handle_procedural_macro(name, tokens) {
            Ok(expanded_tokens) => {
                debug!("Procedural macro '{}' handled successfully", name);
                return self.parse_expanded_tokens(&expanded_tokens);
            }
            Err(e) if e.is_none() => {
                debug!("Not a procedural macro, continuing with regular macro expansion");
            }
            Err(e) => {
                warn!("Procedural macro '{}' failed: {:?}", name, e);
                return Err(e.unwrap());
            }
        }

        if let Some((def_delimiter, def_tokens)) = self.mcs.get(name) {
            if delimiter != def_delimiter {
                warn!("Macro '{}' invoked with wrong delimiter: expected {:?}, got {:?}", name, def_delimiter, delimiter);
                return Err(format!("Macro '{}' invoked with wrong delimiter", name));
            }

            debug!("Extracting macro parameters");
            let params = self.extract_macro_parameters(def_tokens)?;
            debug!("Extracted {} parameters: {:?}", params.len(), params);

            debug!("Extracting macro arguments");
            let args = extract_macro_arguments(tokens)?;
            debug!("Extracted {} arguments", args.len());

            if params.len() != args.len() {
                warn!("Parameter/argument count mismatch for '{}': expected {}, got {}", name, params.len(), args.len());
                return Err(format!("Macro '{}' expected {} arguments, got {}", name, params.len(), args.len()));
            }

            debug!("Processing repetition patterns");
            let processed_tokens = self.process_repetition_pattern(def_tokens, &args)?;
            debug!("Processed {} tokens with repetition patterns", processed_tokens.len());

            debug!("Substituting macro tokens");
            let expanded_tokens = self.substitute_macro_tokens(&processed_tokens, &params, &args)?;
            debug!("Substituted tokens, final expansion has {} tokens", expanded_tokens.len());

            self.parse_expanded_tokens(&expanded_tokens)
        } else {
            warn!("Macro '{}' not found", name);
            Err(format!("Macro '{}' not found", name))
        }
    }

    fn extract_macro_parameters(&self, tokens: &[TokenInfo]) -> Result<Vec<String>, String> {
        debug!("Extracting macro parameters from {} tokens", tokens.len());
        let mut params = Vec::new();
        let mut i = 0;

        while i < tokens.len() {
            if let Token::Fat = tokens[i].token {
                debug!("Found fat arrow token at position {}, ending parameter extraction", i);
                break;
            }

            if let Token::Dollar = tokens[i].token {
                if i + 1 < tokens.len() {
                    if let Token::Identifier(ref name) = tokens[i + 1].token {
                        debug!("Found parameter '{}' at position {}", name, i);
                        params.push(name.clone());
                        i += 2;
                        continue;
                    } else {
                        warn!("Expected identifier after '$' at position {}", i);
                        return Err("Expected identifier after '$' in macro parameters".to_string());
                    }
                }
            }

            i += 1;
        }

        debug!("Extracted {} parameters: {:?}", params.len(), params);
        Ok(params)
    }

    fn substitute_macro_tokens(&self, def_tokens: &[TokenInfo], params: &[String], args: &[Vec<TokenInfo>]) -> Result<Vec<TokenInfo>, String> {
        debug!("Substituting macro tokens: {} parameters, {} arguments", params.len(), args.len());
        let mut result = Vec::new();

        for (i, token_info) in def_tokens.iter().enumerate() {
            if let Token::Identifier(name) = &token_info.token {
                if name.starts_with('$') {
                    let param_name = &name[1..];
                    if let Some(index) = params.iter().position(|p| p == param_name) {
                        debug!("Substituting parameter '{}' at position {} with argument {}", param_name, i, index);
                        // insert the argument tokens
                        result.extend_from_slice(&args[index]);
                    } else {
                        // keep the token as is
                        trace!("Unknown parameter '{}' at position {}, keeping as is", param_name, i);
                        result.push(token_info.clone());
                    }
                } else {
                    // regular identifier
                    trace!("Regular identifier '{}' at position {}", name, i);
                    result.push(token_info.clone());
                }
            } else {
                // non-identifier token
                trace!("Non-identifier token at position {}: {:?}", i, token_info.token);
                result.push(token_info.clone());
            }
        }

        debug!("Substituted tokens: {} original tokens expanded to {} tokens", def_tokens.len(), result.len());
        Ok(result)
    }

    fn process_nested_macros(&mut self, expr: &Expr, depth: usize) -> Result<Expr, String> {
        debug!("Processing nested macros at depth {}", depth);
        match expr {
            Expr::MacroInvocation { name, delimiter, tokens } => {
                debug!("Found nested macro invocation '{}' at depth {}", name, depth);
                self.expand_macro(name, delimiter, tokens, depth)
            }

            Expr::Block { statements, value, returns, is_async } => {
                debug!("Processing nested macros in block with {} statements", statements.len());
                let mut processed_stmts = Vec::new();

                for (i, stmt) in statements.iter().enumerate() {
                    debug!("Processing statement {} in block", i);
                    let processed_stmt = self.process_nested_macros_in_stmt(stmt, depth)?;
                    processed_stmts.push(processed_stmt);
                }

                let processed_value = if let Some(val) = value {
                    debug!("Processing block value expression");
                    Some(Box::new(self.process_nested_macros(val, depth)?))
                } else {
                    None
                };

                debug!("Block processed with {} statements", processed_stmts.len());
                Ok(Expr::Block {
                    statements: processed_stmts,
                    value: processed_value,
                    returns: *returns,
                    is_async: *is_async,
                })
            }

            // process other expression types recursively
            // ... handle other expression types
            _ => {
                trace!("Skipping non-macro expression type: {:?}", expr);
                Ok(expr.clone())
            }
        }
    }

    fn process_nested_macros_in_stmt(&mut self, stmt: &Stmt, depth: usize) -> Result<Stmt, String> {
        debug!("Processing nested macros in statement at depth {}", depth);
        match stmt {
            Stmt::ExpressionStmt(expr) => {
                debug!("Processing expression statement");
                let processed = self.process_nested_macros(expr, depth)?;
                Ok(Stmt::ExpressionStmt(processed))
            }

            Stmt::Let {
                pattern,
                type_annotation,
                initializer,
                attributes,
            } => {
                debug!("Processing let statement with pattern: {:?}", pattern);
                let processed_init = if let Some(init) = initializer {
                    debug!("Processing let statement initializer");
                    Some(Box::new(self.process_nested_macros(init, depth)?))
                } else {
                    debug!("Let statement has no initializer");
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
            _ => {
                trace!("Skipping non-macro statement type: {:?}", stmt);
                Ok(stmt.clone())
            }
        }
    }

    fn process_repetition_pattern(&self, tokens: &[TokenInfo], args: &[Vec<TokenInfo>]) -> Result<Vec<TokenInfo>, String> {
        debug!("Processing repetition patterns in {} tokens", tokens.len());
        let mut result = Vec::new();
        let mut repetition_blocks = self.find_repetition_blocks(tokens)?;
        debug!("Found {} repetition blocks", repetition_blocks.len());

        for (i, block) in repetition_blocks.iter_mut().enumerate() {
            debug!("Expanding repetition block {} with variable '{}'", i, block.variable);
            let expanded = self.expand_repetition_block(block, args)?;
            debug!("Block {} expanded to {} tokens", i, expanded.len());
            result.extend(expanded);
        }

        debug!("Processed repetition patterns: {} tokens", result.len());
        Ok(result)
    }

    fn find_repetition_blocks(&self, tokens: &[TokenInfo]) -> Result<Vec<RepetitionBlock>, String> {
        debug!("Finding repetition blocks in {} tokens", tokens.len());
        let mut blocks = Vec::new();
        let mut i = 0;

        while i < tokens.len() {
            if let Token::Identifier(name) = &tokens[i].token {
                if name.starts_with('$') && i + 3 < tokens.len() {
                    if matches!(tokens[i + 1].token, Token::Colon) {
                        let var_name = name[1..].to_string();
                        debug!("Found potential repetition block with variable '{}' at position {}", var_name, i);

                        let (block_start, block_end) = self.find_repetition_bounds(&tokens[i + 2..])?;
                        debug!("Found repetition bounds: start={}, end={}", block_start, block_end);

                        let separator = self.find_separator(&tokens[i + 2 + block_end..])?;
                        debug!("Repetition separator: {:?}", separator);

                        blocks.push(RepetitionBlock {
                            variable: var_name,
                            tokens: tokens[i + 2 + block_start..i + 2 + block_end].to_vec(),
                            separator,
                        });

                        i = i + 2 + block_end;
                        continue;
                    }
                }
            }
            i += 1;
        }

        debug!("Found {} repetition blocks", blocks.len());
        Ok(blocks)
    }

    fn find_repetition_bounds(&self, tokens: &[TokenInfo]) -> Result<(usize, usize), String> {
        debug!("Finding repetition bounds in {} tokens", tokens.len());
        let mut nesting = 0;
        let mut start = 0;

        while start < tokens.len() {
            match tokens[start].token {
                Token::LeftParen | Token::LeftBrace | Token::LeftBracket => {
                    debug!("Found starting delimiter at position {}: {:?}", start, tokens[start].token);
                    nesting = 1;
                    break;
                }
                _ => start += 1,
            }
        }

        if start >= tokens.len() {
            warn!("Missing repetition block start delimiter");
            return Err("Missing repetition block start delimiter".to_string());
        }

        let mut end = start + 1;
        while end < tokens.len() {
            match tokens[end].token {
                Token::LeftParen | Token::LeftBrace | Token::LeftBracket => {
                    nesting += 1;
                    trace!("Nesting increased to {} at position {}", nesting, end);
                }
                Token::RightParen | Token::RightBrace | Token::RightBracket => {
                    nesting -= 1;
                    trace!("Nesting decreased to {} at position {}", nesting, end);
                    if nesting == 0 {
                        debug!("Found matching delimiter at position {}", end);
                        return Ok((start, end + 1));
                    }
                }
                _ => (),
            }
            end += 1;
        }

        warn!("Unmatched repetition block delimiter");
        Err("Unmatched repetition block delimiter".to_string())
    }

    fn find_separator(&self, tokens: &[TokenInfo]) -> Result<Option<Token>, String> {
        debug!("Looking for separator in {} tokens", tokens.len());
        if tokens.len() >= 2 && matches!(tokens[0].token, Token::Star) {
            // $(...) * token
            debug!("Found separator: {:?}", tokens[1].token);
            Ok(Some(tokens[1].token.clone()))
        } else {
            debug!("No separator found");
            Ok(None) // no separator
        }
    }

    fn expand_repetition_block(&self, block: &RepetitionBlock, args: &[Vec<TokenInfo>]) -> Result<Vec<TokenInfo>, String> {
        debug!("Expanding repetition block with variable '{}', {} tokens", block.variable, block.tokens.len());
        let mut result = Vec::new();

        let matched_args = args.iter().filter(|arg| self.contains_variable(&block.variable, arg)).collect::<Vec<_>>();
        debug!("Found {} matching arguments for variable '{}'", matched_args.len(), block.variable);

        for (i, arg) in matched_args.iter().enumerate() {
            if i > 0 {
                if let Some(separator) = &block.separator {
                    debug!("Adding separator between repetitions {}-{}", i - 1, i);
                    result.push(TokenInfo {
                        token: separator.clone(),
                        location: block.tokens.first().unwrap().location.clone(),
                    });
                }
            }

            debug!("Substituting variables in repetition {} for '{}'", i, block.variable);
            let expanded = self.substitute_variables_in_block(&block.tokens, &block.variable, arg)?;
            debug!("Repetition {} expanded to {} tokens", i, expanded.len());
            result.extend(expanded);
        }

        debug!("Expanded repetition block to {} tokens", result.len());
        Ok(result)
    }

    fn contains_variable(&self, var_name: &str, arg: &[TokenInfo]) -> bool {
        let contains = arg.iter().any(|token| if let Token::Identifier(name) = &token.token { name == var_name } else { false });
        trace!("Checking if argument contains variable '{}': {}", var_name, contains);
        contains
    }

    fn substitute_variables_in_block(&self, block_tokens: &[TokenInfo], var_name: &str, arg_tokens: &[TokenInfo]) -> Result<Vec<TokenInfo>, String> {
        debug!("Substituting variable '{}' in {} block tokens", var_name, block_tokens.len());
        let mut result = Vec::new();

        for (i, token) in block_tokens.iter().enumerate() {
            if let Token::Identifier(name) = &token.token {
                if name == var_name {
                    debug!("Replacing variable '{}' at position {} with {} argument tokens", var_name, i, arg_tokens.len());
                    result.extend_from_slice(arg_tokens);
                } else {
                    trace!("Keeping identifier '{}' at position {}", name, i);
                    result.push(token.clone());
                }
            } else {
                trace!("Keeping non-identifier token at position {}", i);
                result.push(token.clone());
            }
        }

        debug!("Substituted {} block tokens to {} result tokens", block_tokens.len(), result.len());
        Ok(result)
    }
}

fn extract_macro_arguments(tokens: &[TokenInfo]) -> Result<Vec<Vec<TokenInfo>>, String> {
    debug!("Extracting macro arguments from {} tokens", tokens.len());
    // simple implementation - just splits by commas
    let mut args = Vec::new();
    let mut current_arg = Vec::new();
    let mut nesting = 0;

    for (i, token_info) in tokens.iter().enumerate() {
        match token_info.token {
            Token::LeftParen | Token::LeftBrace | Token::LeftBracket => {
                nesting += 1;
                trace!("Nesting increased to {} at position {}", nesting, i);
                current_arg.push(token_info.clone());
            }
            Token::RightParen | Token::RightBrace | Token::RightBracket => {
                nesting -= 1;
                trace!("Nesting decreased to {} at position {}", nesting, i);
                current_arg.push(token_info.clone());
            }
            Token::Comma if nesting == 0 => {
                if !current_arg.is_empty() {
                    debug!("Found argument with {} tokens at position {}", current_arg.len(), i);
                    args.push(current_arg);
                    current_arg = Vec::new();
                }
            }
            _ => current_arg.push(token_info.clone()),
        }
    }

    if !current_arg.is_empty() {
        debug!("Found final argument with {} tokens", current_arg.len());
        args.push(current_arg);
    }

    debug!("Extracted {} total arguments", args.len());
    Ok(args)
}

fn extract_macro_delimiter(tokens: &[TokenInfo]) -> Result<MacroDelimiter, String> {
    debug!("Extracting macro delimiter from {} tokens", tokens.len());
    for (i, token_info) in tokens.iter().enumerate() {
        match token_info.token {
            Token::LeftParen => {
                debug!("Found parenthesis delimiter at position {}", i);
                return Ok(MacroDelimiter::Paren);
            }
            Token::LeftBracket => {
                debug!("Found bracket delimiter at position {}", i);
                return Ok(MacroDelimiter::Bracket);
            }
            Token::LeftBrace => {
                debug!("Found brace delimiter at position {}", i);
                return Ok(MacroDelimiter::Brace);
            }
            _ => continue,
        }
    }

    warn!("Macro definition missing delimiter");
    Err("Macro definition missing delimiter".to_string())
}
