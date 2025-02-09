#[derive(Debug, PartialEq, Clone)]
pub enum Token {
    // keywords
    Let,
    Fn,
    Return,
    If,
    Else,

    // symbols
    LeftParen,
    RightParen,
    LeftBrace,
    RightBrace,
    Semicolon,
    Colon,
    Comma,
    Arrow,

    // operators
    Plus,
    Minus,
    Star,
    Slash,
    Assign,
    Equals,

    // literals
    Identifier(String),
    Integer(i64),
    String(String),

    EOF,
}

pub struct Lexer {
    input: Vec<char>,
    position: usize,
    current_char: Option<char>,
}

impl Lexer {
    pub fn new(input: String) -> Self {
        let chars: Vec<char> = input.chars().collect();
        let current = chars.get(0).copied();
        Self {
            input: chars,
            position: 0,
            current_char: current,
        }
    }

    fn advance(&mut self) {
        self.position += 1;
        self.current_char = self.input.get(self.position).copied();
    }

    fn peek(&self) -> Option<char> { self.input.get(self.position + 1).copied() }

    fn skip_whitespace(&mut self) {
        while let Some(c) = self.current_char {
            if !c.is_whitespace() {
                break;
            }
            self.advance();
        }
    }

    fn read_identifier(&mut self) -> String {
        let mut identifier = String::new();

        while let Some(c) = self.current_char {
            if c.is_alphanumeric() || c == '_' {
                identifier.push(c);
                self.advance();
            } else {
                break;
            }
        }

        identifier
    }

    fn read_number(&mut self) -> i64 {
        let mut number = String::new();

        while let Some(c) = self.current_char {
            if c.is_digit(10) {
                number.push(c);
                self.advance();
            } else {
                break;
            }
        }

        number.parse().unwrap_or(0)
    }

    pub fn next_token(&mut self) -> Token {
        self.skip_whitespace();

        let token = match self.current_char {
            None => Token::EOF,
            Some(c) => match c {
                '(' => {
                    self.advance();
                    Token::LeftParen
                }
                ')' => {
                    self.advance();
                    Token::RightParen
                }
                '{' => {
                    self.advance();
                    Token::LeftBrace
                }
                '}' => {
                    self.advance();
                    Token::RightBrace
                }
                ';' => {
                    self.advance();
                    Token::Semicolon
                }
                ':' => {
                    self.advance();
                    Token::Colon
                }
                ',' => {
                    self.advance();
                    Token::Comma
                }
                '+' => {
                    self.advance();
                    Token::Plus
                }
                '-' => {
                    if self.peek() == Some('>') {
                        self.advance();
                        self.advance();
                        Token::Arrow
                    } else {
                        self.advance();
                        Token::Minus
                    }
                }
                '*' => {
                    self.advance();
                    Token::Star
                }
                '/' => {
                    self.advance();
                    Token::Slash
                }
                '=' => {
                    if self.peek() == Some('=') {
                        self.advance();
                        self.advance();
                        Token::Equals
                    } else {
                        self.advance();
                        Token::Assign
                    }
                }
                c if c.is_alphabetic() => {
                    let ident = self.read_identifier();
                    match ident.as_str() {
                        "let" => Token::Let,
                        "fn" => Token::Fn,
                        "return" => Token::Return,
                        "if" => Token::If,
                        "else" => Token::Else,
                        _ => Token::Identifier(ident),
                    }
                }
                c if c.is_digit(10) => Token::Integer(self.read_number()),
                _ => {
                    self.advance();
                    Token::EOF
                }
            },
        };

        token
    }
}
