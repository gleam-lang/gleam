use super::*;

#[derive(Debug, PartialEq, Clone)]
pub enum UntypedExpr {
    Int {
        location: SrcSpan,
        value: String,
    },

    Float {
        location: SrcSpan,
        value: String,
    },

    String {
        location: SrcSpan,
        value: String,
    },

    Sequence {
        location: SrcSpan,
        expressions: Vec<Self>,
    },

    Var {
        location: SrcSpan,
        name: String,
    },

    Fn {
        location: SrcSpan,
        is_capture: bool,
        arguments: Vec<Arg<()>>,
        body: Box<Self>,
        return_annotation: Option<TypeAst>,
    },

    List {
        location: SrcSpan,
        elements: Vec<Self>,
        tail: Option<Box<Self>>,
    },

    Call {
        location: SrcSpan,
        fun: Box<Self>,
        arguments: Vec<CallArg<Self>>,
    },

    BinOp {
        location: SrcSpan,
        name: BinOp,
        left: Box<Self>,
        right: Box<Self>,
    },

    Pipe {
        location: SrcSpan,
        left: Box<Self>,
        right: Box<Self>,
    },

    Assignment {
        location: SrcSpan,
        value: Box<Self>,
        pattern: Pattern<(), ()>,
        kind: AssignmentKind,
        annotation: Option<TypeAst>,
    },

    Try {
        location: SrcSpan,
        value: Box<Self>,
        pattern: Pattern<(), ()>,
        then: Box<Self>,
        annotation: Option<TypeAst>,
    },

    Case {
        location: SrcSpan,
        subjects: Vec<Self>,
        clauses: Vec<Clause<Self, (), (), ()>>,
    },

    FieldAccess {
        location: SrcSpan,
        label: String,
        container: Box<Self>,
    },

    Tuple {
        location: SrcSpan,
        elems: Vec<Self>,
    },

    TupleIndex {
        location: SrcSpan,
        index: u64,
        tuple: Box<Self>,
    },

    Todo {
        location: SrcSpan,
        label: Option<String>,
    },

    BitString {
        location: SrcSpan,
        segments: Vec<UntypedExprBitStringSegment>,
    },

    RecordUpdate {
        location: SrcSpan,
        constructor: Box<Self>,
        spread: RecordUpdateSpread,
        arguments: Vec<UntypedRecordUpdateArg>,
    },
}

impl UntypedExpr {
    pub fn location(&self) -> SrcSpan {
        match self {
            Self::Try { then, .. } => then.location(),
            Self::Pipe { right, .. } => right.location(),
            Self::Fn { location, .. }
            | Self::Var { location, .. }
            | Self::Int { location, .. }
            | Self::Todo { location, .. }
            | Self::Case { location, .. }
            | Self::Call { location, .. }
            | Self::List { location, .. }
            | Self::Float { location, .. }
            | Self::BinOp { location, .. }
            | Self::Tuple { location, .. }
            | Self::String { location, .. }
            | Self::BitString { location, .. }
            | Self::Assignment { location, .. }
            | Self::TupleIndex { location, .. }
            | Self::FieldAccess { location, .. }
            | Self::RecordUpdate { location, .. } => *location,
            Self::Sequence {
                location,
                expressions,
                ..
            } => expressions.last().map(Self::location).unwrap_or(*location),
        }
    }

    pub fn append_in_sequence(self, next: Self) -> Self {
        match self {
            Self::Sequence {
                location,
                mut expressions,
            } => {
                expressions.push(next);
                Self::Sequence {
                    location,
                    expressions,
                }
            }
            _ => Self::Sequence {
                location: next.location(),
                expressions: vec![self, next],
            },
        }
    }

    pub fn start_byte_index(&self) -> usize {
        match self {
            Self::Sequence {
                expressions,
                location,
                ..
            } => expressions
                .first()
                .map(|e| e.start_byte_index())
                .unwrap_or(location.start),
            Self::Pipe { left, .. } => left.start_byte_index(),
            Self::Assignment { location, .. } => location.start,
            _ => self.location().start,
        }
    }

    pub fn binop_precedence(&self) -> u8 {
        match self {
            Self::BinOp { name, .. } => name.precedence(),
            Self::Pipe { .. } => 5,
            _ => std::u8::MAX,
        }
    }

    pub fn is_simple_constant(&self) -> bool {
        matches!(
            self,
            Self::String { .. } | Self::Int { .. } | Self::Float { .. }
        )
    }
}

impl HasLocation for UntypedExpr {
    fn location(&self) -> SrcSpan {
        self.location()
    }
}
