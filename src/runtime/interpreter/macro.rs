mod builtin;
mod parse;
mod str;

use super::*;
use crate::parser::ast::NumericType;
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
        let input = str::tokens_to_string(tokens);
        let lexer = Lexer::new(input);
        let mut parser = crate::parser::Parser::new(lexer);

        parser.parse_expression(0).map_err(|e| format!("Failed to parse expanded macro: {}", e))
    }

    fn expand_macro_inner(&mut self, name: &str, delimiter: &MacroDelimiter, tokens: &[TokenInfo]) -> Result<Expr, String> {
        match builtin::handle_procedural_macro(name, tokens) {
            Ok(expanded_tokens) => {
                let tokens = self.parse_expanded_tokens(&expanded_tokens);
                println!("{tokens:?}");
                return tokens;
            }
            Err(e) if e.is_none() => {}
            Err(e) => return Err(e.unwrap()),
        }

        if let Some((def_delimiter, _, branches)) = self.mcs.get(name) {
            if delimiter != def_delimiter {
                return Err(format!("Macro '{}' invoked with wrong delimiter", name));
            }

            let args = extract_macro_arguments(tokens)?;

            for branch in branches.iter() {
                if match_branch_literals(&branch.literal_tokens, &args) && validate_branch_params(&branch.params, &args).is_ok() {
                    let substitution_args = if !branch.literal_tokens.is_empty() && !args.is_empty() { &args[1..] } else { &args };
                    let processed_tokens = self.process_repetition_pattern(&branch.body_tokens, &args)?;
                    let expanded_tokens = self.substitute_macro_tokens(&processed_tokens, &branch.params, substitution_args)?;

                    return self.parse_expanded_tokens(&expanded_tokens);
                }
            }

            return Err(format!("No matching branch found for macro '{}'", name));
        } else {
            Err(format!("Macro '{}' not found", name))
        }
    }

    fn substitute_macro_tokens(&self, def_tokens: &[TokenInfo], params: &[MacroParameter], args: &[Vec<TokenInfo>]) -> Result<Vec<TokenInfo>, String> {
        let mut result = Vec::new();
        let mut i = 0;

        while i < def_tokens.len() {
            if def_tokens[i].token == Token::Dollar {
                if i + 1 < def_tokens.len() {
                    match &def_tokens[i + 1].token {
                        Token::LeftParen => {
                            let (group_tokens, group_len) = self.extract_repetition_group(&def_tokens[i..])?;
                            let expanded = self.expand_repetition_group(&group_tokens, params, args)?;
                            result.extend(expanded);
                            i += group_len;
                            continue;
                        }
                        Token::Identifier(name) => {
                            if let Some(param_idx) = params.iter().position(|p| p.name == *name) {
                                if params[param_idx].kind != MacroParamKind::Expr {
                                    return Err(format!("Unsupported macro parameter kind for ${}", params[param_idx].name));
                                }
                                if let Some(arg_tokens) = args.get(param_idx) {
                                    result.extend_from_slice(arg_tokens);
                                }
                                i += 2;
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
        match expr {
            Expr::MacroInvocation { name, delimiter, tokens } => self.expand_macro(name, delimiter, tokens, depth),

            Expr::Block { statements, value, returns, is_async } => {
                let mut processed_stmts = Vec::new();

                for stmt in statements.iter() {
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
            Stmt::ExpressionStmt(expr) => Ok(Stmt::ExpressionStmt(self.process_nested_macros(expr, depth)?)),

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
        let mut repetition_blocks = self.find_repetition_blocks(tokens)?;

        if repetition_blocks.is_empty() {
            return Ok(tokens.to_vec());
        }

        let mut result = Vec::new();
        for block in repetition_blocks.iter_mut() {
            result.extend(self.expand_repetition_block(block, args)?);
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
                        let separator = self.find_separator(&tokens[i + 2 + block_end..])?;

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
                Token::LeftParen | Token::LeftBrace | Token::LeftBracket => {
                    nesting += 1;
                }
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

            result.extend(self.substitute_variables_in_block(&block.tokens, &block.variable, arg)?);
        }

        Ok(result)
    }

    fn contains_variable(&self, var_name: &str, arg: &[TokenInfo]) -> bool { arg.iter().any(|token| if let Token::Identifier(name) = &token.token { name == var_name } else { false }) }

    fn substitute_variables_in_block(&self, block_tokens: &[TokenInfo], var_name: &str, arg_tokens: &[TokenInfo]) -> Result<Vec<TokenInfo>, String> {
        let mut result = Vec::new();

        for token in block_tokens.iter() {
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
    let mut args = Vec::new();
    let mut current_arg = Vec::new();
    let mut nesting = 0;

    fn is_delimiter(token: &Token) -> bool {
        matches!(
            token,
            Token::At | Token::Arrow | Token::Comma | Token::LeftAngle | Token::RightAngle | Token::Question | Token::Pound | Token::Colon | Token::Semicolon | Token::Fat
        )
    }

    let tokens = if !tokens.is_empty() && tokens[0].token == Token::Not { &tokens[1..] } else { tokens };

    for (i, token_info) in tokens.iter().enumerate() {
        match &token_info.token {
            Token::LeftParen | Token::LeftBrace | Token::LeftBracket => {
                nesting += 1;
                current_arg.push(token_info.clone());
            }
            Token::RightParen | Token::RightBrace | Token::RightBracket => {
                nesting -= 1;
                current_arg.push(token_info.clone());
            }
            token if nesting == 0 && is_delimiter(token) => {
                if matches!(token, Token::Comma) {
                    if !current_arg.is_empty() {
                        args.push(current_arg);
                        current_arg = Vec::new();
                    }
                } else {
                    current_arg.push(token_info.clone());
                    if !current_arg.is_empty() {
                        args.push(current_arg);
                        current_arg = Vec::new();
                    }
                }
            }
            _ if nesting == 0 => {
                if i > 0 && !current_arg.is_empty() {
                    let prev_token_end = if let Some(prev) = tokens.get(i - 1) {
                        match &prev.token {
                            Token::Identifier(s) => prev.location.column + s.len(),
                            Token::String(s) => prev.location.column + s.len() + 2,
                            token if is_delimiter(token) => prev.location.column + 1,
                            _ => prev.location.column + 1,
                        }
                    } else {
                        0
                    };

                    if token_info.location.column > prev_token_end + 1 {
                        if !current_arg.is_empty() {
                            args.push(current_arg);
                            current_arg = Vec::new();
                        }
                    }
                }
                current_arg.push(token_info.clone());
            }
            _ => {
                current_arg.push(token_info.clone());
            }
        }
    }

    if !current_arg.is_empty() {
        args.push(current_arg);
    }

    Ok(args.into_iter().filter(|arg| !arg.is_empty()).collect())
}

fn extract_macro_delimiter(tokens: &[TokenInfo]) -> Result<MacroDelimiter, String> {
    for token_info in tokens.iter() {
        match token_info.token {
            Token::LeftParen => return Ok(MacroDelimiter::Paren),
            Token::LeftBracket => return Ok(MacroDelimiter::Bracket),
            Token::LeftBrace => return Ok(MacroDelimiter::Brace),
            _ => continue,
        }
    }

    Err("Macro definition missing delimiter".to_string())
}

fn extract_macro_branches(tokens: &[TokenInfo]) -> Result<Vec<MacroBranch>, String> {
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

    Ok(branches)
}

fn extract_branch_pattern(tokens: &[TokenInfo]) -> Result<(Vec<Token>, Vec<MacroParameter>, usize), String> {
    let mut arrow_pos = 0;

    for (i, token_info) in tokens.iter().enumerate() {
        if token_info.token == Token::Fat {
            arrow_pos = i;
            break;
        }
    }

    if arrow_pos == 0 {
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
            literal_tokens.push(tokens[i].token.clone());
        }

        i += 1;
    }

    Ok((literal_tokens, params, arrow_pos))
}

fn extract_branch_body(tokens: &[TokenInfo], start_pos: usize) -> Result<(Vec<TokenInfo>, usize), String> {
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

    Ok((body_tokens, start_pos + end_pos))
}

fn match_branch_literals(literals: &[Token], args: &[Vec<TokenInfo>]) -> bool {
    if literals.len() == 1 && args.is_empty() {
        if let Token::RightParen = &literals[0] {
            return true;
        }
    }

    if literals.is_empty() {
        return true;
    }

    if args.is_empty() {
        return false;
    }

    let arg_tokens: Vec<&Token> = args[0].iter().map(|t| &t.token).collect();

    if literals.len() == arg_tokens.len() {
        let matches = literals.iter().zip(arg_tokens.iter()).all(|(lit, arg)| match_token(lit, arg));
        return matches;
    }

    if literals.len() == 1 && arg_tokens.len() == 1 {
        let matches = match_token(&literals[0], arg_tokens[0]);
        return matches;
    }

    false
}

fn match_token(expected: &Token, actual: &Token) -> bool {
    match (expected, actual) {
        (Token::Identifier(lit_name), Token::Identifier(arg_name)) => {
            let matches = lit_name == arg_name;
            if !matches {}
            matches
        }
        _ => {
            let matches = expected == actual;
            if !matches {}
            matches
        }
    }
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
