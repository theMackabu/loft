use super::{str, *};

enum Placeholder {
    Named(String),
    Positional(Option<usize>),
}

pub fn handle_procedural_macro(name: &str, tokens: &[TokenInfo]) -> Result<Vec<TokenInfo>, Option<String>> {
    match name {
        "stringify" => {
            let content = str::tokens_to_string(tokens);
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

        "panic" => {
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

            // core::panic( format!( ... ) )
            let mut result_tokens = Vec::new();
            result_tokens.push(TokenInfo {
                token: Token::Identifier("core".to_string()),
                location: first_location.clone(),
            });
            result_tokens.push(TokenInfo {
                token: Token::DoubleColon,
                location: first_location.clone(),
            });
            result_tokens.push(TokenInfo {
                token: Token::Identifier("panic".to_string()),
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

        "println" => {
            let mut format_invocation = Vec::new();
            let first_location = tokens.first().map_or(Location { line: 0, column: 0 }, |t| t.location.clone());

            format_invocation.push(TokenInfo {
                token: Token::Identifier("print".to_string()),
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

            if tokens.is_empty() {
                format_invocation.push(TokenInfo {
                    token: Token::String("\n".to_string()),
                    location: first_location.clone(),
                });
            } else if let Some(TokenInfo { token: Token::String(s), location }) = tokens.first() {
                let mut new_format_string = s.clone();
                new_format_string.push('\n');

                format_invocation.push(TokenInfo {
                    token: Token::String(new_format_string),
                    location: location.clone(),
                });

                if tokens.len() > 1 {
                    format_invocation.push(TokenInfo {
                        token: Token::Comma,
                        location: location.clone(),
                    });
                    format_invocation.extend_from_slice(&tokens[1..]);
                }
            } else {
                return Err(Some("println! requires a string literal as its first argument".to_string()));
            }

            format_invocation.push(TokenInfo {
                token: Token::RightParen,
                location: first_location.clone(),
            });

            Ok(format_invocation)
        }

        "print" => {
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
                token: Token::Identifier("print".to_string()),
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
            let mut placeholders = Vec::new();
            let mut current_part = String::new();
            let mut i = 0;

            while i < fmt_str.len() {
                if fmt_str[i..].starts_with("{}") {
                    parts.push(current_part);
                    current_part = String::new();
                    placeholders.push(Placeholder::Positional(None));
                    i += 2;
                } else if fmt_str[i..].starts_with('{') {
                    if let Some(end) = fmt_str[i..].find('}') {
                        let content = &fmt_str[i + 1..i + end];
                        parts.push(current_part);
                        current_part = String::new();

                        if let Ok(index) = content.parse::<usize>() {
                            placeholders.push(Placeholder::Positional(Some(index)));
                        } else {
                            placeholders.push(Placeholder::Named(content.to_string()));
                        }

                        i += end + 1;
                    } else {
                        return Err(Some("Unmatched '{' in format string".to_string()));
                    }
                } else {
                    current_part.push(fmt_str.chars().nth(i).unwrap());
                    i += 1;
                }
            }

            parts.push(current_part);

            let args = extract_macro_arguments(&tokens[1..])?;
            let mut named_args = std::collections::HashMap::new();

            for arg in &args {
                if let Some(Token::Identifier(name)) = arg.first().map(|t| &t.token) {
                    named_args.insert(name.clone(), arg.clone());
                }
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

            let mut positional_index = 0;

            for (i, part) in parts.iter().enumerate() {
                new_tokens.push(TokenInfo {
                    token: Token::String(part.clone()),
                    location: first_location.clone(),
                });

                if i < placeholders.len() {
                    match &placeholders[i] {
                        Placeholder::Positional(Some(index)) => {
                            if *index < args.len() {
                                new_tokens.push(TokenInfo {
                                    token: Token::Comma,
                                    location: first_location.clone(),
                                });
                                new_tokens.extend(args[*index].clone());
                            } else {
                                return Err(Some(format!("format! expected argument at index {}, but got {} arguments", index, args.len())));
                            }
                        }
                        Placeholder::Positional(None) => {
                            if positional_index < args.len() {
                                new_tokens.push(TokenInfo {
                                    token: Token::Comma,
                                    location: first_location.clone(),
                                });
                                new_tokens.extend(args[positional_index].clone());
                                positional_index += 1;
                            } else {
                                return Err(Some(format!("format! expected {} arguments, got {}", positional_index + 1, args.len())));
                            }
                        }
                        Placeholder::Named(name) => {
                            if let Some(arg_tokens) = named_args.get(name) {
                                new_tokens.push(TokenInfo {
                                    token: Token::Comma,
                                    location: first_location.clone(),
                                });
                                new_tokens.extend(arg_tokens.clone());
                            } else {
                                new_tokens.push(TokenInfo {
                                    token: Token::Comma,
                                    location: first_location.clone(),
                                });
                                new_tokens.push(TokenInfo {
                                    token: Token::Identifier(name.clone()),
                                    location: first_location.clone(),
                                });
                            }
                        }
                    }
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

        // add more built-in procedural macros later
        _ => Err(None),
    }
}
