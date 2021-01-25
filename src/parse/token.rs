use std::fmt;

#[derive(Clone, Debug, PartialEq)]
pub enum Tok {
    Name { name: String },
    UpName { name: String },
    DiscardName { name: String },
    Int { value: String },
    Float { value: String },
    String { value: String },
    // Groupings
    Lpar,
    Rpar,
    Lsqb,
    Rsqb,
    Lbrace,
    Rbrace,
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
    // Other Punctuation
    Colon,
    Comma,
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
    DotDot,     // '..'
    ListNil,    // '[]'
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
    Tuple,
    Type,
}

impl Tok {
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

impl fmt::Display for Tok {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let s = match self {
            Tok::Name { name } | Tok::UpName { name } | Tok::DiscardName { name } => name,
            Tok::Int { value } | Tok::Float { value } | Tok::String { value } => value,
            Tok::Lpar => "(",
            Tok::Rpar => ")",
            Tok::Lsqb => "[",
            Tok::Rsqb => "]",
            Tok::Lbrace => "{",
            Tok::Rbrace => "}",
            Tok::Plus => "+",
            Tok::Minus => "-",
            Tok::Star => "*",
            Tok::Slash => "/",
            Tok::Less => "<",
            Tok::Greater => ">",
            Tok::LessEqual => "<=",
            Tok::GreaterEqual => ">=",
            Tok::Percent => "%",
            Tok::PlusDot => "+.",
            Tok::MinusDot => "-.",
            Tok::StarDot => "*.",
            Tok::SlashDot => "/.",
            Tok::LessDot => "<.",
            Tok::GreaterDot => ">.",
            Tok::LessEqualDot => "<=.",
            Tok::GreaterEqualDot => ">=.",
            Tok::Colon => ":",
            Tok::Comma => ",",
            Tok::Equal => "=",
            Tok::EqualEqual => "==",
            Tok::NotEqual => "!=",
            Tok::Vbar => "|",
            Tok::VbarVbar => "||",
            Tok::AmperAmper => "&&",
            Tok::LtLt => "<<",
            Tok::GtGt => ">>",
            Tok::Pipe => "|>",
            Tok::Dot => ".",
            Tok::RArrow => "->",
            Tok::DotDot => "..",
            Tok::ListNil => "[]",
            Tok::EndOfFile => "EOF",
            Tok::CommentNormal => "//",
            Tok::CommentDoc => "///",
            Tok::CommentModule => "////",
            Tok::EmptyLine => "EMPTYLINE",
            Tok::As => "as",
            Tok::Assert => "assert",
            Tok::Case => "case",
            Tok::Const => "const",
            Tok::External => "external",
            Tok::Fn => "fn",
            Tok::If => "if",
            Tok::Import => "import",
            Tok::Let => "let",
            Tok::Opaque => "opaque",
            Tok::Pub => "pub",
            Tok::Todo => "todo",
            Tok::Try => "try",
            Tok::Tuple => "tuple",
            Tok::Type => "type",
        };
        write!(f, "\"{}\"", s)
    }
}
