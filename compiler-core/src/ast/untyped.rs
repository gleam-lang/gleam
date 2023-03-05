use vec1::Vec1;

use super::*;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum UntypedExpr {
    Int {
        location: SrcSpan,
        value: SmolStr,
    },

    Float {
        location: SrcSpan,
        value: SmolStr,
    },

    String {
        location: SrcSpan,
        value: SmolStr,
    },

    Block {
        location: SrcSpan,
        expressions: Vec<Self>,
    },

    Var {
        location: SrcSpan,
        name: SmolStr,
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

    PipeLine {
        expressions: Vec1<Self>,
    },

    Assignment {
        location: SrcSpan,
        value: Box<Self>,
        pattern: Pattern<(), ()>,
        kind: AssignmentKind,
        annotation: Option<TypeAst>,
    },

    Use(Use),

    Case {
        location: SrcSpan,
        subjects: Vec<Self>,
        clauses: Vec<Clause<Self, (), (), ()>>,
    },

    FieldAccess {
        location: SrcSpan,
        label: SmolStr,
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
        kind: TodoKind,
        location: SrcSpan,
        label: Option<SmolStr>,
    },

    Panic {
        location: SrcSpan,
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

    NegateBool {
        location: SrcSpan,
        value: Box<Self>,
    },

    NegateInt {
        location: SrcSpan,
        value: Box<Self>,
    },
}

impl UntypedExpr {
    pub fn location(&self) -> SrcSpan {
        match self {
            Self::PipeLine { expressions, .. } => expressions.last().location(),
            Self::Fn { location, .. }
            | Self::Use(Use { location, .. })
            | Self::Var { location, .. }
            | Self::Int { location, .. }
            | Self::Todo { location, .. }
            | Self::Case { location, .. }
            | Self::Call { location, .. }
            | Self::List { location, .. }
            | Self::Float { location, .. }
            | Self::BinOp { location, .. }
            | Self::Tuple { location, .. }
            | Self::Panic { location, .. }
            | Self::String { location, .. }
            | Self::BitString { location, .. }
            | Self::Assignment { location, .. }
            | Self::TupleIndex { location, .. }
            | Self::FieldAccess { location, .. }
            | Self::RecordUpdate { location, .. }
            | Self::NegateBool { location, .. }
            | Self::NegateInt { location, .. } => *location,
            Self::Block {
                location,
                expressions,
                ..
            } => expressions.last().map(Self::location).unwrap_or(*location),
        }
    }

    pub fn start_byte_index(&self) -> u32 {
        match self {
            Self::Block {
                expressions,
                location,
                ..
            } => expressions
                .first()
                .map(|e| e.start_byte_index())
                .unwrap_or(location.start),
            Self::PipeLine { expressions, .. } => expressions.first().start_byte_index(),
            Self::Assignment { location, .. } => location.start,
            _ => self.location().start,
        }
    }

    pub fn binop_precedence(&self) -> u8 {
        match self {
            Self::BinOp { name, .. } => name.precedence(),
            Self::PipeLine { .. } => 5,
            _ => std::u8::MAX,
        }
    }

    pub fn is_simple_constant(&self) -> bool {
        matches!(
            self,
            Self::String { .. } | Self::Int { .. } | Self::Float { .. }
        )
    }

    /// Returns `true` if the untyped expr is [`Call`].
    ///
    /// [`Call`]: UntypedExpr::Call
    #[must_use]
    pub fn is_call(&self) -> bool {
        matches!(self, Self::Call { .. })
    }
}

impl HasLocation for UntypedExpr {
    fn location(&self) -> SrcSpan {
        self.location()
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Use {
    pub location: SrcSpan,
    pub call: Box<UntypedExpr>,
    pub assignments: Vec<UntypedPattern>,
}
