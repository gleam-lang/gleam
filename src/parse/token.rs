#[derive(Clone, Debug, PartialEq)]
pub enum Tok {
    Name { name: String },
    UpName { name: String },
    DiscardName { name: String },
    // Int { value: BigInt },
    // Float { value: f64 },
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
