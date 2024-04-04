use std::fmt;

use ecow::EcoString;

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Token {
    Name { name: EcoString },
    UpName { name: EcoString },
    DiscardName { name: EcoString },
    Int { value: EcoString },
    Float { value: EcoString },
    String { value: EcoString },
    CommentDoc { content: String },
    // Groupings
    LeftParen,   // (
    RightParen,  // )
    LeftSquare,  // [
    RightSquare, // }
    LeftBrace,   // {
    RightBrace,  // }
    // Int Operators
    Plus,
    Minus,
    Star,
    Slash,
    Less,
    Greater,
    LessEqual,
    GreaterEqual,
    Percent,
    // Float Operators
    PlusDot,         // '+.'
    MinusDot,        // '-.'
    StarDot,         // '*.'
    SlashDot,        // '/.'
    LessDot,         // '<.'
    GreaterDot,      // '>.'
    LessEqualDot,    // '<=.'
    GreaterEqualDot, // '>=.'
    // String Operators
    LtGt, // '<>'
    // Other Punctuation
    Colon,
    Comma,
    Hash, // '#'
    Bang, // '!'
    Equal,
    EqualEqual, // '=='
    NotEqual,   // '!='
    Vbar,       // '|'
    VbarVbar,   // '||'
    AmperAmper, // '&&'
    LtLt,       // '<<'
    GtGt,       // '>>'
    Pipe,       // '|>'
    Dot,        // '.'
    RArrow,     // '->'
    LArrow,     // '<-'
    DotDot,     // '..'
    At,         // '@'
    EndOfFile,
    // Extra
    CommentNormal,
    CommentModule,
    EmptyLine,
    // Keywords (alphabetically):
    As,
    Assert,
    Auto,
    Case,
    Const,
    Delegate,
    Derive,
    Echo,
    Else,
    Fn,
    If,
    Implement,
    Import,
    Let,
    Macro,
    Opaque,
    Panic,
    Pub,
    Test,
    Todo,
    Type,
    Use,
}

impl Token {
    pub fn guard_precedence(&self) -> Option<u8> {
        match self {
            Self::VbarVbar => Some(1),

            Self::AmperAmper => Some(2),

            Self::EqualEqual | Self::NotEqual => Some(3),

            Self::Less
            | Self::LessEqual
            | Self::LessDot
            | Self::LessEqualDot
            | Self::GreaterEqual
            | Self::Greater
            | Self::GreaterEqualDot
            | Self::GreaterDot => Some(4),

            _ => None,
        }
    }
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let s = match self {
            Token::Name { name } | Token::UpName { name } | Token::DiscardName { name } => {
                name.as_str()
            }
            Token::Int { value } | Token::Float { value } | Token::String { value } => {
                value.as_str()
            }
            Token::AmperAmper => "&&",
            Token::As => "as",
            Token::Assert => "assert",
            Token::At => "@",
            Token::Auto => "auto",
            Token::Bang => "!",
            Token::Case => "case",
            Token::Colon => ":",
            Token::Comma => ",",
            Token::CommentDoc { .. } => "///",
            Token::CommentModule => "////",
            Token::CommentNormal => "//",
            Token::Const => "const",
            Token::Delegate => "delegate",
            Token::Derive => "derive",
            Token::Dot => ".",
            Token::DotDot => "..",
            Token::Echo => "echo",
            Token::Else => "else",
            Token::EmptyLine => "EMPTYLINE",
            Token::EndOfFile => "EOF",
            Token::Equal => "=",
            Token::EqualEqual => "==",
            Token::Fn => "fn",
            Token::Greater => ">",
            Token::GreaterDot => ">.",
            Token::GreaterEqual => ">=",
            Token::GreaterEqualDot => ">=.",
            Token::GtGt => ">>",
            Token::Hash => "#",
            Token::If => "if",
            Token::Implement => "implement",
            Token::Import => "import",
            Token::LArrow => "<-",
            Token::LeftBrace => "{",
            Token::LeftParen => "(",
            Token::LeftSquare => "[",
            Token::Less => "<",
            Token::LessDot => "<.",
            Token::LessEqual => "<=",
            Token::LessEqualDot => "<=.",
            Token::Let => "let",
            Token::LtGt => "<>",
            Token::LtLt => "<<",
            Token::Macro => "macro",
            Token::Minus => "-",
            Token::MinusDot => "-.",
            Token::NotEqual => "!=",
            Token::Opaque => "opaque",
            Token::Panic => "panic",
            Token::Percent => "%",
            Token::Pipe => "|>",
            Token::Plus => "+",
            Token::PlusDot => "+.",
            Token::Pub => "pub",
            Token::RArrow => "->",
            Token::RightBrace => "}",
            Token::RightParen => ")",
            Token::RightSquare => "]",
            Token::Slash => "/",
            Token::SlashDot => "/.",
            Token::Star => "*",
            Token::StarDot => "*.",
            Token::Test => "test",
            Token::Todo => "todo",
            Token::Type => "type",
            Token::Use => "use",
            Token::Vbar => "|",
            Token::VbarVbar => "||",
        };
        write!(f, "\"{s}\"")
    }
}
