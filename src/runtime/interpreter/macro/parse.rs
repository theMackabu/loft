use self::str::token_to_string;
use super::*;

#[derive(Clone, Debug, PartialEq)]
pub enum MacroParamKind {
    Expr,
    Type,
    Ident,
    TokenTree,
    // later add other kinds like Block, etc.
}

#[derive(Clone, Debug)]
pub struct MacroParameter {
    pub name: String,
    pub kind: MacroParamKind,
    pub repeated: bool,
}

#[derive(Clone, Debug)]
pub struct MacroParamParser<'a> {
    tokens: &'a [TokenInfo],
    position: usize,
    params: Vec<MacroParameter>,
    in_repetition: bool,
    delimiter_stack: Vec<Token>,
}

impl<'a> MacroParamParser<'a> {
    pub fn new(tokens: &'a [TokenInfo]) -> Self {
        MacroParamParser {
            tokens,
            position: 0,
            params: Vec::new(),
            in_repetition: false,
            delimiter_stack: Vec::new(),
        }
    }

    pub fn parse(mut self) -> Result<Vec<MacroParameter>, String> {
        while self.position < self.tokens.len() {
            self.parse_next()?;
        }
        Ok(self.params)
    }

    fn parse_next(&mut self) -> Result<(), String> {
        let current = self.current_token().ok_or("Unexpected end of tokens")?;

        match &current.token {
            Token::Dollar => self.handle_dollar(),
            Token::LeftParen | Token::LeftBrace | Token::LeftBracket => self.handle_opening_delimiter(current.token.clone()),
            Token::RightParen | Token::RightBrace | Token::RightBracket => self.handle_closing_delimiter(current.token.clone()),
            _ => {
                self.advance();
                Ok(())
            }
        }
    }

    fn handle_dollar(&mut self) -> Result<(), String> {
        if self.peek_next() == Some(&Token::LeftParen) {
            self.handle_repetition_pattern()
        } else {
            self.handle_single_parameter()
        }
    }

    fn handle_single_parameter(&mut self) -> Result<(), String> {
        self.advance(); // skip $

        let ident = match self.current_token() {
            Some(t) => t.token.as_identifier().ok_or("Expected identifier after '$'")?.to_string(),
            None => return Err("Unexpected end of tokens".into()),
        };

        self.advance(); // skip identifier

        let mut kind = MacroParamKind::Expr;
        if self.current_token_is(Token::Colon) {
            self.advance(); // skip :
            kind = self.parse_param_kind()?;
        }

        self.params.push(MacroParameter {
            name: ident,
            kind,
            repeated: self.in_repetition,
        });

        Ok(())
    }

    fn handle_repetition_pattern(&mut self) -> Result<(), String> {
        self.advance(); // skip $
        self.advance(); // skip (
        self.delimiter_stack.push(Token::LeftParen);
        self.in_repetition = true;

        while self.position < self.tokens.len() {
            let current = self.current_token().ok_or("Unexpected end of repetition")?;

            if current.token == Token::RightParen && self.delimiter_stack.last() == Some(&Token::LeftParen) {
                self.delimiter_stack.pop();
                self.advance(); // skip )
                self.in_repetition = false;
                break;
            }

            if current.token == Token::Dollar {
                self.handle_single_parameter()?;
            } else {
                self.advance();
            }
        }

        Ok(())
    }

    fn parse_param_kind(&mut self) -> Result<MacroParamKind, String> {
        match self.current_token() {
            Some(t) if t.token.is_identifier() => {
                let kind = match t.token.as_identifier().unwrap() {
                    "expr" => MacroParamKind::Expr,
                    "ty" => MacroParamKind::Type,
                    "ident" => MacroParamKind::Ident,
                    "tt" => MacroParamKind::TokenTree,
                    _ => return Err(format!("Unsupported parameter kind: {}", token_to_string(&t.token))),
                };
                self.advance();
                Ok(kind)
            }
            _ => Err("Expected parameter kind after colon".to_string()),
        }
    }

    fn handle_opening_delimiter(&mut self, delim: Token) -> Result<(), String> {
        if self.in_repetition {
            self.delimiter_stack.push(delim);
        }
        self.advance();
        Ok(())
    }

    fn handle_closing_delimiter(&mut self, delim: Token) -> Result<(), String> {
        if self.in_repetition {
            let expected = match delim {
                Token::RightParen => Token::LeftParen,
                Token::RightBrace => Token::LeftBrace,
                Token::RightBracket => Token::LeftBracket,
                _ => return Err("Unexpected closing delimiter".to_string()),
            };

            if self.delimiter_stack.last() == Some(&expected) {
                self.delimiter_stack.pop();
                if self.delimiter_stack.is_empty() {
                    self.in_repetition = false;
                }
            }
        }
        self.advance();
        Ok(())
    }

    fn advance(&mut self) { self.position += 1; }

    fn current_token(&self) -> Option<&TokenInfo> { self.tokens.get(self.position) }

    fn current_token_is(&self, token: Token) -> bool { self.current_token().map(|t| t.token == token).unwrap_or(false) }

    fn peek_next(&self) -> Option<&Token> { self.tokens.get(self.position + 1).map(|t| &t.token) }
}
