use super::*;

pub fn tokens_to_string(tokens: &[TokenInfo]) -> String {
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
