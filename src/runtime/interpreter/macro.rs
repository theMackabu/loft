mod parse;
mod proc;
mod str;

use super::*;
use crate::parser::ast::NumericType;
use crate::{debug, trace, warn};
use parse::{MacroParamKind, MacroParameter};

#[derive(Clone, Debug)]
pub struct MacroBranch {
    literal_tokens: Vec<Token>,
    params: Vec<MacroParameter>,
    body_tokens: Vec<TokenInfo>,
}

#[derive(Clone, Debug)]
pub struct RepetitionBlock {
    variable: String,
    tokens: Vec<TokenInfo>,
    separator: Option<Token>,
}

impl<'st> Interpreter<'st> {
    pub fn handle_macro_definition(&mut self, name: &str, tokens: &[TokenInfo]) -> Result<(), String> {
        let delimiter = extract_macro_delimiter(tokens)?;
        let branches = extract_macro_branches(tokens)?;

        self.mcs.insert(name.to_string(), (delimiter, tokens.to_vec(), branches));
        Ok(())
    }

    pub fn expand_macro(&mut self, name: &str, delimiter: &MacroDelimiter, tokens: &[TokenInfo], depth: usize) -> Result<Expr, String> {
        if depth > 100 {
            return Err("Maximum macro recursion depth exceeded".to_string());
        }

        let expanded = self.expand_macro_inner(name, delimiter, tokens)?;
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

        if let Some((def_delimiter, _, branches)) = self.mcs.get(name) {
            if delimiter != def_delimiter {
                warn!("Macro '{}' invoked with wrong delimiter: expected {:?}, got {:?}", name, def_delimiter, delimiter);
                return Err(format!("Macro '{}' invoked with wrong delimiter", name));
            }

            debug!("Extracting macro arguments");
            let args = extract_macro_arguments(tokens)?;
            debug!("Extracted {} arguments", args.len());

            for (i, branch) in branches.iter().enumerate() {
                debug!("Trying to match branch {} of macro '{}'", i, name);
                if match_branch_literals(&branch.literal_tokens, &args) && validate_branch_params(&branch.params, &args).is_ok() {
                    debug!("Branch {} matched successfully", i);

                    debug!("Processing repetition patterns");
                    let processed_tokens = self.process_repetition_pattern(&branch.body_tokens, &args)?;
                    debug!("Processed {} tokens with repetition patterns", processed_tokens.len());

                    debug!("Substituting macro tokens");
                    let expanded_tokens = self.substitute_macro_tokens(&processed_tokens, &branch.params, &args)?;
                    debug!("Substituted tokens, final expansion has {} tokens", expanded_tokens.len());

                    return self.parse_expanded_tokens(&expanded_tokens);
                }
            }

            warn!("No matching branch found for macro '{}'", name);
            return Err(format!("No matching branch found for macro '{}'", name));
        } else {
            warn!("Macro '{}' not found", name);
            Err(format!("Macro '{}' not found", name))
        }
    }

    fn substitute_macro_tokens(&self, def_tokens: &[TokenInfo], params: &[MacroParameter], args: &[Vec<TokenInfo>]) -> Result<Vec<TokenInfo>, String> {
        debug!(
            "Substituting macro tokens: {} parameters, {} arguments in {} definition tokens",
            params.len(),
            args.len(),
            def_tokens.len()
        );
        let mut result = Vec::new();
        let mut i = 0;

        let format_args = if args.len() > 1 { &args[1..] } else { &[] };

        while i < def_tokens.len() {
            if def_tokens[i].token == Token::Dollar {
                if i + 1 < def_tokens.len() {
                    match &def_tokens[i + 1].token {
                        Token::LeftParen => {
                            let (group_tokens, group_len) = self.extract_repetition_group(&def_tokens[i..])?;
                            let expanded = self.expand_repetition_group(&group_tokens, params, format_args)?;
                            result.extend(expanded);
                            i += group_len;
                            continue;
                        }
                        Token::Identifier(ref name) => {
                            if let Some(index) = params.iter().position(|p| p.name == *name) {
                                if params[index].kind != MacroParamKind::Expr {
                                    return Err(format!("Unsupported macro parameter kind for ${}", params[index].name));
                                }
                                if index < format_args.len() {
                                    debug!("Substituting parameter '{}' with {} argument tokens", name, format_args[index].len());
                                    result.extend_from_slice(&format_args[index]);
                                }
                                i += 2; // skip '$' and the identifier
                                continue;
                            }
                        }
                        _ => {}
                    }
                }
            }

            result.push(def_tokens[i].clone());
            i += 1;
        }

        debug!("Substituted tokens: {} original tokens expanded to {} tokens", def_tokens.len(), result.len());
        Ok(result)
    }

    fn expand_repetition_group(&self, group_tokens: &[TokenInfo], params: &[MacroParameter], args: &[Vec<TokenInfo>]) -> Result<Vec<TokenInfo>, String> {
        if group_tokens.len() < 3 {
            return Err("Invalid repetition group tokens.".to_string());
        }

        let mut nesting = 0;
        let mut body_end = None;
        for (i, token_info) in group_tokens.iter().enumerate().skip(1) {
            match token_info.token {
                Token::LeftParen => nesting += 1,
                Token::RightParen => {
                    nesting -= 1;
                    if nesting == 0 {
                        body_end = Some(i);
                        break;
                    }
                }
                _ => {}
            }
        }

        let body_end = body_end.ok_or("Failed to find matching ')' in repetition group.".to_string())?;
        let body_tokens = &group_tokens[2..body_end];

        let separator = if group_tokens.len() > body_end + 2 {
            if group_tokens[body_end + 1].token == Token::Comma {
                Some(group_tokens[body_end + 1].token.clone())
            } else {
                None
            }
        } else {
            None
        };

        let mut repeated_param_index = None;
        for i in 0..body_tokens.len() {
            if body_tokens[i].token == Token::Dollar {
                if i + 1 < body_tokens.len() {
                    if let Token::Identifier(ref name) = body_tokens[i + 1].token {
                        if let Some(idx) = params.iter().position(|p| p.name == *name && p.repeated) {
                            repeated_param_index = Some(idx);
                            break;
                        }
                    }
                }
            }
        }
        let param_index = repeated_param_index.ok_or("No repeated parameter found in repetition group body.".to_string())?;

        let mut expanded = Vec::new();
        for (iter_index, arg_tokens) in args.iter().enumerate() {
            if iter_index > 0 {
                if let Some(ref sep) = separator {
                    expanded.push(TokenInfo {
                        token: sep.clone(),
                        location: group_tokens[0].location.clone(),
                    });
                }
            }

            let mut iteration_expansion = Vec::new();
            let mut j = 0;
            while j < body_tokens.len() {
                if body_tokens[j].token == Token::Dollar {
                    if j + 1 < body_tokens.len() {
                        if let Token::Identifier(ref id_name) = body_tokens[j + 1].token {
                            if id_name == &params[param_index].name {
                                iteration_expansion.extend_from_slice(arg_tokens);
                                j += 2;
                                continue;
                            }
                        }
                    }
                }
                iteration_expansion.push(body_tokens[j].clone());
                j += 1;
            }
            expanded.extend(iteration_expansion);
        }
        Ok(expanded)
    }

    fn extract_repetition_group(&self, tokens: &[TokenInfo]) -> Result<(Vec<TokenInfo>, usize), String> {
        if tokens.is_empty() || tokens[0].token != Token::Dollar {
            return Err("Expected '$' at beginning of repetition group.".to_string());
        }
        let mut collected = Vec::new();
        let mut i = 0;
        collected.push(tokens[i].clone());
        i += 1;
        if i >= tokens.len() || tokens[i].token != Token::LeftParen {
            return Err("Expected '(' after '$' in repetition group.".to_string());
        }
        collected.push(tokens[i].clone());
        i += 1;

        let mut nesting = 1;
        while i < tokens.len() {
            let t = &tokens[i];
            match t.token {
                Token::LeftParen => {
                    nesting += 1;
                }
                Token::RightParen => {
                    nesting -= 1;
                    collected.push(t.clone());
                    i += 1;
                    if nesting == 0 {
                        break;
                    }
                    continue;
                }
                _ => {}
            }
            collected.push(t.clone());
            i += 1;
        }

        if nesting != 0 {
            return Err("Unmatched '(' in repetition group.".to_string());
        }

        if i < tokens.len() && tokens[i].token == Token::Comma {
            collected.push(tokens[i].clone());
            i += 1;
        }

        if i < tokens.len() {
            match tokens[i].token {
                Token::Star | Token::Plus => {
                    collected.push(tokens[i].clone());
                    i += 1;
                }
                _ => {
                    return Err("Expected repetition operator '*' or '+' after repetition group.".to_string());
                }
            }
        } else {
            return Err("Unexpected end of tokens during parsing repetition group.".to_string());
        }
        Ok((collected, i))
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
        let mut repetition_blocks = self.find_repetition_blocks(tokens)?;
        debug!("Found {} repetition blocks", repetition_blocks.len());

        if repetition_blocks.is_empty() {
            debug!("No repetition blocks found, returning original tokens");
            return Ok(tokens.to_vec());
        }

        let mut result = Vec::new();
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

fn extract_macro_branches(tokens: &[TokenInfo]) -> Result<Vec<MacroBranch>, String> {
    debug!("Extracting macro branches from {} tokens", tokens.len());
    let mut branches = Vec::new();
    let mut branch_start = 0;

    while branch_start < tokens.len() {
        let (literals, params, arrow_pos) = extract_branch_pattern(&tokens[branch_start..])?;
        if arrow_pos == 0 {
            break;
        }

        let absolute_arrow_pos = branch_start + arrow_pos;
        let (body_tokens, next_branch_start) = extract_branch_body(&tokens[absolute_arrow_pos + 1..], branch_start + arrow_pos + 1)?;

        branches.push(MacroBranch {
            literal_tokens: literals,
            params,
            body_tokens,
        });

        branch_start = next_branch_start;

        if branch_start < tokens.len() && tokens[branch_start].token == Token::Semicolon {
            branch_start += 1;
        }
    }

    if branches.is_empty() {
        return Err("No valid branches found in macro definition".to_string());
    }

    debug!("Extracted {} macro branches", branches.len());
    Ok(branches)
}

fn extract_branch_pattern(tokens: &[TokenInfo]) -> Result<(Vec<Token>, Vec<MacroParameter>, usize), String> {
    debug!("Extracting branch pattern from {} tokens", tokens.len());
    let mut arrow_pos = 0;

    for (i, token_info) in tokens.iter().enumerate() {
        if token_info.token == Token::Fat {
            arrow_pos = i;
            break;
        }
    }

    if arrow_pos == 0 {
        debug!("No fat arrow found, no more branches");
        return Ok((Vec::new(), Vec::new(), 0));
    }

    let mut literal_tokens = Vec::new();
    let mut params = Vec::new();
    let mut i = 0;

    if i < arrow_pos && (tokens[i].token == Token::LeftParen || tokens[i].token == Token::LeftBrace || tokens[i].token == Token::LeftBracket) {
        i += 1;
    }

    while i < arrow_pos {
        if tokens[i].token == Token::Dollar {
            let pattern_tokens = &tokens[i..arrow_pos];
            let parser = parse::MacroParamParser::new(pattern_tokens);
            let pattern_params = parser.parse()?;
            params.extend(pattern_params);
            break;
        } else if tokens[i].token != Token::Comma {
            debug!("Found literal token in branch pattern: {:?}", tokens[i].token);
            literal_tokens.push(tokens[i].token.clone());
        }

        i += 1;
    }

    debug!("Extracted {} literal tokens and {} parameters for branch", literal_tokens.len(), params.len());
    Ok((literal_tokens, params, arrow_pos))
}

fn extract_branch_body(tokens: &[TokenInfo], start_pos: usize) -> Result<(Vec<TokenInfo>, usize), String> {
    debug!("Extracting branch body from {} tokens", tokens.len());
    let mut body_tokens = Vec::new();
    let mut nesting = 0;
    let mut end_pos = 0;

    for (i, token_info) in tokens.iter().enumerate() {
        match token_info.token {
            Token::LeftParen | Token::LeftBrace | Token::LeftBracket => {
                nesting += 1;
                body_tokens.push(token_info.clone());
            }
            Token::RightParen | Token::RightBrace | Token::RightBracket => {
                nesting -= 1;
                body_tokens.push(token_info.clone());

                if nesting == 0 {
                    end_pos = i + 1;
                    break;
                }
            }
            Token::Semicolon if nesting == 0 => {
                end_pos = i;
                break;
            }
            _ => body_tokens.push(token_info.clone()),
        }
    }

    if end_pos == 0 && !tokens.is_empty() {
        end_pos = tokens.len();
    }

    debug!("Extracted {} body tokens for branch", body_tokens.len());
    Ok((body_tokens, start_pos + end_pos))
}

fn match_branch_literals(literals: &[Token], args: &[Vec<TokenInfo>]) -> bool {
    debug!("Matching {} literal tokens against {} arguments", literals.len(), args.len());

    if literals.len() == 1 && args.is_empty() {
        if let Token::RightParen = &literals[0] {
            debug!("Matched empty macro invocation pattern");
            return true;
        }
    }

    if literals.is_empty() {
        return true;
    }

    if args.is_empty() {
        return false;
    }

    for (i, literal) in literals.iter().enumerate() {
        if i >= args.len() {
            return false;
        }

        if args[i].len() != 1 {
            return false;
        }

        let arg_token = &args[i][0].token;

        match literal {
            Token::Identifier(lit_name) => {
                if let Token::Identifier(arg_name) = arg_token {
                    if lit_name != arg_name {
                        debug!("Identifier mismatch: expected '{}', got '{}'", lit_name, arg_name);
                        return false;
                    }
                } else {
                    debug!("Token type mismatch: expected Identifier, got {:?}", arg_token);
                    return false;
                }
            }
            _ => {
                if arg_token != literal {
                    debug!("Literal token mismatch: expected {:?}, got {:?}", literal, arg_token);
                    return false;
                }
            }
        }
    }

    debug!("All literal tokens matched successfully");
    true
}

fn validate_branch_params(params: &[MacroParameter], args: &[Vec<TokenInfo>]) -> Result<(), String> {
    let mut required_count = 0;
    let mut has_repeated = false;

    for param in params {
        if param.repeated {
            has_repeated = true;
        } else {
            required_count += 1;
        }
    }

    let valid = match has_repeated {
        true => args.len() >= required_count,
        false => args.len() == required_count,
    };

    if !valid {
        Err(format!(
            "Expected {} arguments, got {}",
            if has_repeated { format!("at least {}", required_count) } else { required_count.to_string() },
            args.len()
        ))
    } else {
        Ok(())
    }
}
