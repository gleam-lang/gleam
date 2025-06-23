use vec1::Vec1;

use super::*;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum UntypedExpr {
    Int {
        location: SrcSpan,
        value: EcoString,
        int_value: BigInt,
    },

    Float {
        location: SrcSpan,
        value: EcoString,
    },

    String {
        location: SrcSpan,
        value: EcoString,
    },

    Block {
        location: SrcSpan,
        statements: Vec1<UntypedStatement>,
    },

    Var {
        location: SrcSpan,
        name: EcoString,
    },

    // TODO: create new variant for captures specifically
    Fn {
        /// For anonymous functions, this is the location of the entire function including the end of the body.
        /// For named functions, this is the location of the function head.
        location: SrcSpan,
        kind: FunctionLiteralKind,
        /// The byte location of the end of the function head before the opening bracket
        end_of_head_byte_index: u32,
        arguments: Vec<UntypedArg>,
        body: Vec1<UntypedStatement>,
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
        name_location: SrcSpan,
        left: Box<Self>,
        right: Box<Self>,
    },

    PipeLine {
        expressions: Vec1<Self>,
    },

    Case {
        location: SrcSpan,
        subjects: Vec<Self>,
        // None if the case expression is missing a body.
        clauses: Option<Vec<Clause<Self, (), ()>>>,
    },

    FieldAccess {
        // This is the location of the whole record and field
        //   user.name
        //   ^^^^^^^^^
        location: SrcSpan,
        // This is the location of just the field access (ignoring the `.`)
        //   user.name
        //        ^^^^
        label_location: SrcSpan,
        label: EcoString,
        container: Box<Self>,
    },

    Tuple {
        location: SrcSpan,
        elements: Vec<Self>,
    },

    TupleIndex {
        location: SrcSpan,
        index: u64,
        tuple: Box<Self>,
    },

    Todo {
        kind: TodoKind,
        location: SrcSpan,
        message: Option<Box<Self>>,
    },

    Panic {
        location: SrcSpan,
        message: Option<Box<Self>>,
    },

    Echo {
        location: SrcSpan,
        expression: Option<Box<Self>>,
    },

    BitArray {
        location: SrcSpan,
        segments: Vec<UntypedExprBitArraySegment>,
    },

    RecordUpdate {
        location: SrcSpan,
        constructor: Box<Self>,
        record: RecordBeingUpdated,
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

    /// A placeholder used when parsing is incomplete or when a function body is
    /// missing due to an external implementation being given for the function
    /// instead.
    /// TODO: This variant should be removed in future, but it requires some
    /// rework of the type inference code to be able to handle functions that do
    /// not have a body.
    Placeholder {
        location: SrcSpan,
    },
}

impl UntypedExpr {
    pub fn location(&self) -> SrcSpan {
        match self {
            Self::PipeLine { expressions, .. } => expressions
                .first()
                .location()
                .merge(&expressions.last().location()),

            Self::Fn { location, .. }
            | Self::Var { location, .. }
            | Self::Int { location, .. }
            | Self::Todo { location, .. }
            | Self::Echo { location, .. }
            | Self::Case { location, .. }
            | Self::Call { location, .. }
            | Self::List { location, .. }
            | Self::Float { location, .. }
            | Self::Block { location, .. }
            | Self::BinOp { location, .. }
            | Self::Tuple { location, .. }
            | Self::Panic { location, .. }
            | Self::String { location, .. }
            | Self::BitArray { location, .. }
            | Self::NegateInt { location, .. }
            | Self::NegateBool { location, .. }
            | Self::TupleIndex { location, .. }
            | Self::Placeholder { location, .. }
            | Self::FieldAccess { location, .. }
            | Self::RecordUpdate { location, .. } => *location,
        }
    }

    pub fn start_byte_index(&self) -> u32 {
        match self {
            Self::Block { location, .. } => location.start,
            Self::PipeLine { expressions, .. } => expressions.first().start_byte_index(),
            _ => self.location().start,
        }
    }

    pub fn bin_op_precedence(&self) -> u8 {
        match self {
            Self::BinOp { name, .. } => name.precedence(),
            Self::PipeLine { .. } => 5,
            _ => u8::MAX,
        }
    }

    pub fn bin_op_name(&self) -> Option<&BinOp> {
        match self {
            UntypedExpr::BinOp { name, .. } => Some(name),
            _ => None,
        }
    }

    pub fn can_have_multiple_per_line(&self) -> bool {
        match self {
            UntypedExpr::Int { .. }
            | UntypedExpr::Float { .. }
            | UntypedExpr::String { .. }
            | UntypedExpr::Var { .. } => true,

            UntypedExpr::NegateBool { value, .. }
            | UntypedExpr::NegateInt { value, .. }
            | UntypedExpr::FieldAccess {
                container: value, ..
            } => value.can_have_multiple_per_line(),

            UntypedExpr::Block { .. }
            | UntypedExpr::Fn { .. }
            | UntypedExpr::List { .. }
            | UntypedExpr::Call { .. }
            | UntypedExpr::BinOp { .. }
            | UntypedExpr::PipeLine { .. }
            | UntypedExpr::Case { .. }
            | UntypedExpr::Tuple { .. }
            | UntypedExpr::TupleIndex { .. }
            | UntypedExpr::Todo { .. }
            | UntypedExpr::Panic { .. }
            | UntypedExpr::Echo { .. }
            | UntypedExpr::BitArray { .. }
            | UntypedExpr::RecordUpdate { .. }
            | UntypedExpr::Placeholder { .. } => false,
        }
    }

    pub fn is_tuple(&self) -> bool {
        match self {
            UntypedExpr::Tuple { .. } => true,
            _ => false,
        }
    }

    /// Returns `true` if the untyped expr is [`Call`].
    ///
    /// [`Call`]: UntypedExpr::Call
    #[must_use]
    pub fn is_call(&self) -> bool {
        matches!(self, Self::Call { .. })
    }

    /// Returns `true` if the untyped expr is [`Placeholder`].
    ///
    /// [`Placeholder`]: UntypedExpr::Placeholder
    #[must_use]
    pub fn is_placeholder(&self) -> bool {
        matches!(self, Self::Placeholder { .. })
    }

    #[must_use]
    pub fn is_binop(&self) -> bool {
        matches!(self, Self::BinOp { .. })
    }

    #[must_use]
    pub fn is_pipeline(&self) -> bool {
        matches!(self, Self::PipeLine { .. })
    }

    #[must_use]
    pub fn is_todo(&self) -> bool {
        matches!(self, Self::Todo { .. })
    }

    #[must_use]
    pub fn is_panic(&self) -> bool {
        matches!(self, Self::Panic { .. })
    }
}

impl HasLocation for UntypedExpr {
    fn location(&self) -> SrcSpan {
        self.location()
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum FunctionLiteralKind {
    Capture { hole: SrcSpan },
    Anonymous { head: SrcSpan },
    Use { location: SrcSpan },
}

impl FunctionLiteralKind {
    pub fn is_capture(&self) -> bool {
        match self {
            FunctionLiteralKind::Capture { .. } => true,
            FunctionLiteralKind::Anonymous { .. } | FunctionLiteralKind::Use { .. } => false,
        }
    }
}
