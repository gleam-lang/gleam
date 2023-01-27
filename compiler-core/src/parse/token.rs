use std::fmt;

use smol_str::SmolStr;

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Token {
    Name { name: SmolStr },
    UpName { name: SmolStr },
    DiscardName { name: SmolStr },
    Int { value: SmolStr },
    Float { value: SmolStr },
    String { value: SmolStr },
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
    EndOfFile,
    // Extra
    CommentNormal,
    CommentDoc,
    CommentModule,
    EmptyLine,
    // Keywords (alphabetically):
    As,
    Assert,
    Case,
    Const,
    External,
    Fn,
    If,
    Import,
    Let,
    Opaque,
    Pub,
    Todo,
    Try,
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
            Token::LeftParen => "(",
            Token::RightParen => ")",
            Token::LeftSquare => "[",
            Token::RightSquare => "]",
            Token::LeftBrace => "{",
            Token::RightBrace => "}",
            Token::Plus => "+",
            Token::Minus => "-",
            Token::Star => "*",
            Token::Slash => "/",
            Token::Less => "<",
            Token::Greater => ">",
            Token::LessEqual => "<=",
            Token::GreaterEqual => ">=",
            Token::Percent => "%",
            Token::PlusDot => "+.",
            Token::MinusDot => "-.",
            Token::StarDot => "*.",
            Token::SlashDot => "/.",
            Token::LessDot => "<.",
            Token::GreaterDot => ">.",
            Token::LessEqualDot => "<=.",
            Token::GreaterEqualDot => ">=.",
            Token::Colon => ":",
            Token::Comma => ",",
            Token::Hash => "#",
            Token::Bang => "!",
            Token::Equal => "=",
            Token::EqualEqual => "==",
            Token::NotEqual => "!=",
            Token::Vbar => "|",
            Token::VbarVbar => "||",
            Token::AmperAmper => "&&",
            Token::LtLt => "<<",
            Token::GtGt => ">>",
            Token::Pipe => "|>",
            Token::Dot => ".",
            Token::RArrow => "->",
            Token::LArrow => "<-",
            Token::DotDot => "..",
            Token::EndOfFile => "EOF",
            Token::CommentNormal => "//",
            Token::CommentDoc => "///",
            Token::CommentModule => "////",
            Token::EmptyLine => "EMPTYLINE",
            Token::As => "as",
            Token::Assert => "assert",
            Token::Case => "case",
            Token::Const => "const",
            Token::External => "external",
            Token::Fn => "fn",
            Token::If => "if",
            Token::Import => "import",
            Token::Let => "let",
            Token::Opaque => "opaque",
            Token::Pub => "pub",
            Token::Todo => "todo",
            Token::Try => "try",
            Token::Use => "use",
            Token::Type => "type",
            Token::LtGt => "<>",
        };
        write!(f, "\"{s}\"")
    }
}
