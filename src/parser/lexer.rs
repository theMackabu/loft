use super::ast::NumericType;
use std::fmt;

#[derive(Debug, PartialEq, Clone)]
pub enum Token {
    // definitions
    Pound,      // #
    At,         // @
    Dollar,     // $
    MacroRules, // macro_rules!

    // keywords
    Let,      // let
    Mut,      // mut
    Const,    // const
    Static,   // static,
    Struct,   // struct
    Trait,    // trait
    Impl,     // impl
    Enum,     // enum
    Fn,       // fn
    Return,   // return
    Continue, // continue
    Break,    // break
    If,       // if
    Else,     // else
    True,     // true
    False,    // false
    Type,     // type
    Pub,      // pub
    Async,    // async
    Await,    // await
    Match,    // match
    Loop,     // loop
    While,    // while
    For,      // for
    In,       // in

    // project
    Use,         // use
    Module,      // mod
    As,          // as
    DoubleColon, // ::

    // symbols
    Dot,            // .
    Range,          // ..
    RangeInclusive, // ..=
    LeftParen,      //(
    RightParen,     //)
    LeftBracket,    // [
    RightBracket,   // ]
    LeftBrace,      // {
    RightBrace,     // }
    Semicolon,      // ;
    Colon,          // :
    Comma,          // ,
    Arrow,          // ->
    LeftAngle,      // <
    RightAngle,     // >
    Question,       // ?
    Fat,            // =>

    // operators
    Plus,   // +
    Minus,  // -
    Star,   // *
    Slash,  // /
    Assign, // =
    Equals, // ==

    // advanced operators
    Not,           // !
    BitNot,        // ~
    NotEquals,     // !=
    BitNotEquals,  // ~=
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
    LessEquals,    // <=
    GreaterEquals, // >=
    Shr,           // >>
    ShrAssign,     // >>=
    BitXor,        // ^
    BitXorAssign,  // ^=
    BitOr,         // |
    BitOrAssign,   // |=
    Or,            // ||

    // literals
    Identifier(String),
    String(String),
    Lifetime(String),

    // numbers
    Integer(i64, Option<NumericType>),
    Float(f64, Option<NumericType>),

    EOF,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Location {
    pub line: usize,
    pub column: usize,
}

#[derive(Clone, Debug, PartialEq)]
pub struct TokenInfo {
    pub token: Token,
    pub location: Location,
}

#[derive(Clone)]
pub struct Lexer {
    input: Vec<char>,
    current_char: Option<char>,
    token_buffer: Vec<TokenInfo>,

    line: usize,
    column: usize,
    position: usize,
}

impl Lexer {
    pub fn new(input: String) -> Self {
        let chars: Vec<char> = input.chars().collect();
        let current = chars.get(0).copied();

        Self {
            input: chars,
            current_char: current,
            token_buffer: Vec::new(),

            line: 1,
            column: 1,
            position: 0,
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

    fn skip_whitespace_and_comments(&mut self) {
        while let Some(c) = self.current_char {
            match c {
                c if c.is_whitespace() => {
                    self.advance();
                }

                '/' if self.peek() == Some('/') => {
                    while let Some(c) = self.current_char {
                        if c == '\n' {
                            self.advance();
                            break;
                        }
                        self.advance();
                    }
                }

                '/' if self.peek() == Some('*') => {
                    self.advance(); // consume '/'
                    self.advance(); // consume '*'

                    let mut depth = 1;

                    while depth > 0 {
                        match self.current_char {
                            None => break,

                            Some('*') if self.peek() == Some('/') => {
                                self.advance(); // consume '*'
                                self.advance(); // consume '/'
                                depth -= 1;
                            }

                            Some('/') if self.peek() == Some('*') => {
                                self.advance(); // consume '/'
                                self.advance(); // consume '*'
                                depth += 1;
                            }

                            Some(_) => {
                                self.advance();
                            }
                        }
                    }
                }

                _ => break,
            }
        }
    }

    fn read_identifier(&mut self) -> String {
        let mut identifier = String::new();

        if let Some(c) = self.current_char {
            if c == '_' || c.is_alphabetic() {
                identifier.push(c);
                self.advance();
            }
        }

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

    fn read_number(&mut self) -> Token {
        let mut number = String::new();
        let mut is_float = false;

        while let Some(c) = self.current_char {
            if c.is_digit(10) {
                number.push(c);
                self.advance();
            } else {
                break;
            }
        }

        if self.current_char == Some('.') {
            if let Some(next_char) = self.peek() {
                if next_char.is_digit(10) {
                    is_float = true;
                    number.push('.');
                    self.advance();

                    while let Some(c) = self.current_char {
                        if c.is_digit(10) {
                            number.push(c);
                            self.advance();
                        } else {
                            break;
                        }
                    }
                }
            }
        }

        if let Some('e' | 'E') = self.current_char {
            is_float = true;
            number.push('e');
            self.advance();

            if let Some('+' | '-') = self.current_char {
                number.push(self.current_char.unwrap());
                self.advance();
            }

            let mut has_digits = false;
            while let Some(c) = self.current_char {
                if c.is_digit(10) {
                    has_digits = true;
                    number.push(c);
                    self.advance();
                } else {
                    break;
                }
            }

            if !has_digits {
                return Token::Integer(0, None); // improve error handling
            }
        }

        let type_suffix = match self.current_char {
            Some('i') => {
                self.advance();
                match self.read_numeric_suffix().as_str() {
                    "8" => Some(NumericType::I8),
                    "16" => Some(NumericType::I16),
                    "32" => Some(NumericType::I32),
                    "64" => Some(NumericType::I64),
                    "128" => Some(NumericType::I128),
                    _ => None,
                }
            }
            Some('u') => {
                self.advance();
                match self.read_numeric_suffix().as_str() {
                    "8" => Some(NumericType::U8),
                    "16" => Some(NumericType::U16),
                    "32" => Some(NumericType::U32),
                    "64" => Some(NumericType::U64),
                    "128" => Some(NumericType::U128),
                    _ => None,
                }
            }
            Some('f') => {
                self.advance();
                match self.read_numeric_suffix().as_str() {
                    "32" => Some(NumericType::F32),
                    "64" => Some(NumericType::F64),
                    _ => None,
                }
            }
            _ => None,
        };

        if is_float {
            match number.parse::<f64>() {
                Ok(value) => Token::Float(value, type_suffix),
                Err(_) => Token::Float(0.0, None), // improve error handling
            }
        } else {
            match number.parse::<i64>() {
                Ok(value) => Token::Integer(value, type_suffix),
                Err(_) => Token::Integer(0, None), // improve error handling
            }
        }
    }

    fn read_numeric_suffix(&mut self) -> String {
        let mut suffix = String::new();
        while let Some(c) = self.current_char {
            if c.is_digit(10) {
                suffix.push(c);
                self.advance();
            } else {
                break;
            }
        }
        suffix
    }

    fn read_string(&mut self) -> Result<String, &'static str> {
        let mut string = String::new();
        self.advance(); // consume '"'

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
                        Some('x') => {
                            self.advance(); // consume 'x'
                            let mut hex = String::new();
                            for _ in 0..2 {
                                match self.current_char {
                                    Some(c) if c.is_ascii_hexdigit() => {
                                        hex.push(c);
                                        self.advance();
                                    }
                                    _ => return Err("Invalid hex escape sequence"),
                                }
                            }
                            if let Ok(value) = u8::from_str_radix(&hex, 16) {
                                string.push(value as char);
                            } else {
                                return Err("Invalid hex escape sequence");
                            }
                            continue;
                        }
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

    fn read_raw_string(&mut self) -> Result<String, &'static str> {
        if self.current_char != Some('"') {
            return Err("Expected opening quote in raw string literal");
        }

        self.advance(); // consume '"'

        let mut string = String::new();
        while let Some(c) = self.current_char {
            if c == '"' {
                self.advance();
                return Ok(string);
            } else {
                string.push(c);
                self.advance();
            }
        }
        Err("Unterminated raw string literal")
    }

    pub fn next_token(&mut self) -> TokenInfo {
        if let Some(token) = self.token_buffer.pop() {
            return token;
        }

        self.skip_whitespace_and_comments();

        let location = Location { line: self.line, column: self.column };

        let token = match self.current_char {
            None => Token::EOF,
            Some(c) => match c {
                '#' => {
                    self.advance();
                    Token::Pound
                }

                '@' => {
                    self.advance();
                    Token::At
                }

                '?' => {
                    self.advance();
                    Token::Question
                }

                '$' => {
                    self.advance();
                    Token::Dollar
                }

                '(' => {
                    self.advance();
                    Token::LeftParen
                }

                ')' => {
                    self.advance();
                    Token::RightParen
                }

                '[' => {
                    self.advance();
                    Token::LeftBracket
                }

                ']' => {
                    self.advance();
                    Token::RightBracket
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

                ',' => {
                    self.advance();
                    Token::Comma
                }

                '.' => {
                    self.advance();
                    if self.current_char == Some('.') {
                        self.advance();
                        if self.current_char == Some('=') {
                            self.advance();
                            Token::RangeInclusive
                        } else {
                            Token::Range
                        }
                    } else {
                        Token::Dot
                    }
                }

                '+' => {
                    self.advance();
                    if self.current_char == Some('=') {
                        self.advance();
                        Token::PlusEquals
                    } else {
                        Token::Plus
                    }
                }

                '-' => {
                    self.advance();
                    if self.current_char == Some('=') {
                        self.advance();
                        Token::MinusEquals
                    } else if self.current_char == Some('>') {
                        self.advance();
                        Token::Arrow
                    } else {
                        Token::Minus
                    }
                }

                ':' => {
                    if self.peek() == Some(':') {
                        self.advance();
                        self.advance();
                        Token::DoubleColon
                    } else {
                        self.advance();
                        Token::Colon
                    }
                }

                '*' => {
                    self.advance();
                    if self.current_char == Some('=') {
                        self.advance();
                        Token::StarEquals
                    } else {
                        Token::Star
                    }
                }

                '/' => {
                    self.advance();
                    if self.current_char == Some('=') {
                        self.advance();
                        Token::SlashEquals
                    } else {
                        Token::Slash
                    }
                }

                '=' => {
                    self.advance();
                    match self.current_char {
                        Some('=') => {
                            self.advance();
                            Token::Equals
                        }
                        Some('>') => {
                            self.advance();
                            Token::Fat
                        }
                        _ => Token::Assign,
                    }
                }

                '\'' => {
                    self.advance();
                    let mut lifetime = String::from("'");
                    while let Some(c) = self.current_char {
                        if c.is_alphanumeric() || c == '_' {
                            lifetime.push(c);
                            self.advance();
                        } else {
                            break;
                        }
                    }
                    Token::Lifetime(lifetime)
                }

                '"' => match self.read_string() {
                    Ok(s) => Token::String(s),
                    Err(_) => Token::EOF, // implement better error handling
                },

                'r' if self.peek() == Some('"') => {
                    self.advance();

                    match self.read_raw_string() {
                        Ok(s) => Token::String(s),
                        Err(_) => Token::EOF, // implement better error handling
                    }
                }

                '!' => {
                    self.advance();
                    if self.current_char == Some('=') {
                        self.advance();
                        Token::NotEquals
                    } else {
                        Token::Not
                    }
                }

                '~' => {
                    self.advance();
                    if self.current_char == Some('=') {
                        self.advance();
                        Token::BitNotEquals
                    } else {
                        Token::BitNot
                    }
                }

                '&' => {
                    self.advance();
                    if self.current_char == Some('&') {
                        self.advance();
                        Token::And
                    } else if self.current_char == Some('=') {
                        self.advance();
                        Token::BitAndAssign
                    } else {
                        Token::BitAnd
                    }
                }

                '|' => {
                    self.advance();
                    if self.current_char == Some('|') {
                        self.advance();
                        self.token_buffer.push(TokenInfo {
                            token: Token::BitOr,
                            location: location.clone(),
                        });
                        Token::BitOr
                    } else if self.current_char == Some('=') {
                        self.advance();
                        Token::BitOrAssign
                    } else {
                        Token::BitOr
                    }
                }

                '%' => {
                    self.advance();
                    if self.current_char == Some('=') {
                        self.advance();
                        Token::RemAssign
                    } else {
                        Token::Rem
                    }
                }

                '^' => {
                    self.advance();
                    if self.current_char == Some('=') {
                        self.advance();
                        Token::BitXorAssign
                    } else {
                        Token::BitXor
                    }
                }

                '<' => {
                    self.advance();
                    match self.current_char {
                        Some('<') => {
                            self.advance();
                            if self.current_char == Some('=') {
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
                        _ => Token::LeftAngle,
                    }
                }

                '>' => {
                    self.advance();
                    match self.current_char {
                        Some('>') => {
                            self.advance();
                            if self.current_char == Some('=') {
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
                        _ => Token::RightAngle,
                    }
                }

                c if c == '_' || c.is_alphabetic() => {
                    let ident = self.read_identifier();

                    match ident.as_str() {
                        "let" => Token::Let,
                        "mut" => Token::Mut,
                        "const" => Token::Const,
                        "static" => Token::Static,
                        "struct" => Token::Struct,
                        "trait" => Token::Trait,
                        "impl" => Token::Impl,
                        "enum" => Token::Enum,
                        "fn" => Token::Fn,
                        "return" => Token::Return,
                        "continue" => Token::Continue,
                        "break" => Token::Break,
                        "if" => Token::If,
                        "else" => Token::Else,
                        "true" => Token::True,
                        "false" => Token::False,
                        "type" => Token::Type,
                        "pub" => Token::Pub,
                        "async" => Token::Async,
                        "await" => Token::Await,
                        "use" => Token::Use,
                        "mod" => Token::Module,
                        "as" => Token::As,
                        "match" => Token::Match,
                        "loop" => Token::Loop,
                        "while" => Token::While,
                        "for" => Token::For,
                        "in" => Token::In,
                        "macro_rules" => Token::MacroRules,
                        _ => Token::Identifier(ident),
                    }
                }

                c if c.is_digit(10) => self.read_number(),

                _ => {
                    self.advance();
                    Token::EOF
                }
            },
        };

        TokenInfo { token, location }
    }
}

impl Token {
    pub fn is_identifier(&self) -> bool { matches!(self, Token::Identifier(_)) }

    pub fn as_identifier(&self) -> Option<&str> { if let Token::Identifier(s) = self { Some(s.as_str()) } else { None } }
}

impl TokenInfo {
    pub fn new(token: Token, location: Location) -> Self { Self { token, location } }
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let token = match self {
            Token::Pound => "#".to_string(),
            Token::At => "@".to_string(),
            Token::Dollar => "$".to_string(),
            Token::MacroRules => "macro_rules!".to_string(),

            Token::Let => "let ".to_string(),
            Token::Mut => "mut ".to_string(),
            Token::Const => "const ".to_string(),
            Token::Static => "static ".to_string(),
            Token::Struct => "struct ".to_string(),
            Token::Trait => "trait ".to_string(),
            Token::Impl => "impl ".to_string(),
            Token::Enum => "enum ".to_string(),
            Token::Fn => "fn ".to_string(),
            Token::Return => "return ".to_string(),
            Token::Continue => "continue ".to_string(),
            Token::Break => "break ".to_string(),
            Token::If => "if ".to_string(),
            Token::Else => "else ".to_string(),
            Token::Type => "type ".to_string(),
            Token::Pub => "pub ".to_string(),
            Token::Async => "async ".to_string(),
            Token::Await => "await ".to_string(),
            Token::Match => "match ".to_string(),
            Token::Loop => "loop ".to_string(),
            Token::While => "while ".to_string(),
            Token::For => "for ".to_string(),
            Token::In => "in ".to_string(),

            Token::True => "true".to_string(),
            Token::False => "false".to_string(),

            Token::Use => "use ".to_string(),
            Token::Module => "mod ".to_string(),
            Token::As => "as ".to_string(),
            Token::DoubleColon => "::".to_string(),

            Token::Dot => ".".to_string(),
            Token::Range => "..".to_string(),
            Token::RangeInclusive => "..=".to_string(),
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
            Token::BitNot => "~".to_string(),
            Token::NotEquals => "!=".to_string(),
            Token::BitNotEquals => "~=".to_string(),
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

            Token::Lifetime(s) => s.clone(),
            Token::Identifier(s) => s.clone(),

            Token::String(s) => {
                let mut escaped = String::new();
                for c in s.chars() {
                    match c {
                        '\0' => escaped.push_str("\\0"),     // null
                        '\t' => escaped.push_str("\\t"),     // tab
                        '\n' => escaped.push_str("\\n"),     // line feed
                        '\r' => escaped.push_str("\\r"),     // carriage return
                        '\"' => escaped.push_str("\\\""),    // double quote
                        '\\' => escaped.push_str("\\\\"),    // backslash
                        '\x1b' => escaped.push_str("\\x1b"), // ESC
                        '\x7f' => escaped.push_str("\\x7f"), // DEL

                        c if c.is_ascii_control() => {
                            escaped.push_str(&format!("\\x{:02x}", c as u8));
                        }

                        c => escaped.push(c),
                    }
                }
                format!("\"{}\"", escaped)
            }

            Token::Integer(val, has_suffix) => {
                let mut int = val.to_string();

                if let Some(suffix) = has_suffix {
                    int.push_str(match suffix {
                        NumericType::I8 => "i8",
                        NumericType::I16 => "i16",
                        NumericType::I32 => "i32",
                        NumericType::I64 => "i64",
                        NumericType::I128 => "i128",
                        NumericType::ISize => "isize",

                        NumericType::U8 => "u8",
                        NumericType::U16 => "u16",
                        NumericType::U32 => "u32",
                        NumericType::U64 => "u64",
                        NumericType::U128 => "u128",
                        NumericType::USize => "usize",

                        NumericType::F32 => "f32",
                        NumericType::F64 => "f64",
                    })
                }

                int
            }

            Token::Float(val, has_suffix) => {
                let mut num = val.to_string();

                if !num.contains('.') && !num.contains('e') {
                    num.push_str(".0");
                }

                let mut flt = String::with_capacity(num.len());
                flt.push_str(&num);

                if let Some(suffix) = has_suffix {
                    flt.push_str(match suffix {
                        NumericType::I8 => "i8",
                        NumericType::I16 => "i16",
                        NumericType::I32 => "i32",
                        NumericType::I64 => "i64",
                        NumericType::I128 => "i128",
                        NumericType::ISize => "isize",
                        NumericType::U8 => "u8",
                        NumericType::U16 => "u16",
                        NumericType::U32 => "u32",
                        NumericType::U64 => "u64",
                        NumericType::U128 => "u128",
                        NumericType::USize => "usize",
                        NumericType::F32 => "f32",
                        NumericType::F64 => "f64",
                    });
                }

                flt
            }

            Token::EOF => "".to_string(),
        };

        f.write_str(&token)
    }
}
