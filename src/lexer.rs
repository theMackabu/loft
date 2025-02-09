#[derive(Debug, PartialEq, Clone)]
pub enum Token {
    // keywords
    Let,    // let
    Fn,     // fn
    Return, // return
    If,     // if
    Else,   // else
    True,   // true
    False,  // false

    // symbols
    LeftParen,  //(
    RightParen, //)
    LeftBrace,  // {
    RightBrace, // }
    Semicolon,  // ;
    Colon,      // :
    Comma,      // ,
    Arrow,      // ->

    // operators
    Plus,   // +
    Minus,  // -
    Star,   // *
    Slash,  // /
    Assign, // =
    Equals, // ==

    // advanced operators
    Not,           // !
    NotEquals,     // !=
    Rem,           // %
    RemAssign,     // %=
    BitAnd,        // &
    BitAndAssign,  // &=
    And,           // &&
    StarEquals,    // *=
    PlusEquals,    // +=
    MinusEquals,   // -=
    SlashEquals,   // /=
    Shl,           // <<
    ShlAssign,     // <<=
    LessThan,      // <
    LessEquals,    // <=
    GreaterThan,   // >
    GreaterEquals, // >=
    Shr,           // >>
    ShrAssign,     // >>=
    BitXor,        // ^
    BitXorAssign,  // ^=
    BitOr,         // |
    BitOrAssign,   // |=
    Or,            // ||

    // literals
    Identifier(String), // ident
    Integer(i64),       // add i32, u32, etc
    String(String),     // ""

    EOF,
}

#[derive(Debug, Clone)]
pub struct Location {
    pub line: usize,
    pub column: usize,
}

#[derive(Debug, Clone)]
pub struct TokenInfo {
    pub token: Token,
    pub location: Location,
}

#[derive(Clone)]
pub struct Lexer {
    input: Vec<char>,
    position: usize,
    current_char: Option<char>,

    // error info
    line: usize,
    column: usize,
}

impl Lexer {
    pub fn new(input: String) -> Self {
        let chars: Vec<char> = input.chars().collect();
        let current = chars.get(0).copied();

        Self {
            input: chars,
            position: 0,
            current_char: current,
            line: 1,
            column: 1,
        }
    }

    fn advance(&mut self) {
        if let Some(c) = self.current_char {
            if c == '\n' {
                self.line += 1;
                self.column = 1;
            } else {
                self.column += 1;
            }
        }
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

    fn read_string(&mut self) -> Result<String, &'static str> {
        let mut string = String::new();
        self.advance(); // Skip the opening quote

        while let Some(c) = self.current_char {
            match c {
                '"' => {
                    self.advance();
                    return Ok(string);
                }
                '\\' => {
                    self.advance();
                    match self.current_char {
                        Some('n') => string.push('\n'),
                        Some('t') => string.push('\t'),
                        Some('r') => string.push('\r'),
                        Some('"') => string.push('"'),
                        Some('\\') => string.push('\\'),
                        _ => return Err("Invalid escape sequence"),
                    }
                    self.advance();
                }
                _ => {
                    string.push(c);
                    self.advance();
                }
            }
        }

        Err("Unterminated string literal")
    }

    pub fn next_token(&mut self) -> TokenInfo {
        self.skip_whitespace();

        let location = Location { line: self.line, column: self.column };

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
                    if self.peek() == Some('=') {
                        self.advance();
                        Token::PlusEquals
                    } else {
                        Token::Plus
                    }
                }

                '-' => {
                    self.advance();
                    if self.peek() == Some('=') {
                        self.advance();
                        Token::MinusEquals
                    } else if self.peek() == Some('>') {
                        self.advance();
                        Token::Arrow
                    } else {
                        Token::Minus
                    }
                }

                '*' => {
                    self.advance();
                    if self.peek() == Some('=') {
                        self.advance();
                        Token::StarEquals
                    } else {
                        Token::Star
                    }
                }

                '/' => {
                    self.advance();
                    if self.peek() == Some('=') {
                        self.advance();
                        Token::SlashEquals
                    } else {
                        Token::Slash
                    }
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

                '"' => match self.read_string() {
                    Ok(s) => Token::String(s),
                    Err(_) => Token::EOF, // implement better error handling
                },

                '!' => {
                    self.advance();
                    if self.peek() == Some('=') {
                        self.advance();
                        Token::NotEquals
                    } else {
                        Token::Not
                    }
                }

                '%' => {
                    self.advance();
                    if self.peek() == Some('=') {
                        self.advance();
                        Token::RemAssign
                    } else {
                        Token::Rem
                    }
                }

                '&' => {
                    self.advance();
                    match self.peek() {
                        Some('&') => {
                            self.advance();
                            Token::And
                        }
                        Some('=') => {
                            self.advance();
                            Token::BitAndAssign
                        }
                        _ => Token::BitAnd,
                    }
                }

                '^' => {
                    self.advance();
                    if self.peek() == Some('=') {
                        self.advance();
                        Token::BitXorAssign
                    } else {
                        Token::BitXor
                    }
                }

                '<' => {
                    self.advance();
                    match self.peek() {
                        Some('<') => {
                            self.advance();
                            if self.peek() == Some('=') {
                                self.advance();
                                Token::ShlAssign
                            } else {
                                Token::Shl
                            }
                        }
                        Some('=') => {
                            self.advance();
                            Token::LessEquals
                        }
                        _ => Token::LessThan,
                    }
                }

                '>' => {
                    self.advance();
                    match self.peek() {
                        Some('>') => {
                            self.advance();
                            if self.peek() == Some('=') {
                                self.advance();
                                Token::ShrAssign
                            } else {
                                Token::Shr
                            }
                        }
                        Some('=') => {
                            self.advance();
                            Token::GreaterEquals
                        }
                        _ => Token::GreaterThan,
                    }
                }

                '|' => {
                    self.advance();
                    match self.peek() {
                        Some('|') => {
                            self.advance();
                            Token::Or
                        }
                        Some('=') => {
                            self.advance();
                            Token::BitOrAssign
                        }
                        _ => Token::BitOr,
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
                        "true" => Token::True,
                        "false" => Token::False,
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

        TokenInfo { token, location }
    }
}
