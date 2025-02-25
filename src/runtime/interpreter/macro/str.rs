use super::*;

pub fn tokens_to_string(tokens: &[TokenInfo]) -> String {
    let mut result = String::new();

    for token_info in tokens {
        result.push_str(&token_to_string(&token_info.token));
    }

    result.trim_end().to_string()
}

pub fn token_to_string(token: &Token) -> String {
    match token {
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

            return int;
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

            return flt;
        }

        Token::EOF => "".to_string(),
    }
}
