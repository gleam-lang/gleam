use vec1::Vec1;

use super::*;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum UntypedExpr {
    Int {
        location: SrcSpan,
        value: EcoString,
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
        location: SrcSpan,
        is_capture: bool,
        arguments: Vec<Arg<()>>,
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
        left: Box<Self>,
        right: Box<Self>,
    },

    PipeLine {
        expressions: Vec1<Self>,
    },

    Case {
        location: SrcSpan,
        subjects: Vec<Self>,
        clauses: Vec<Clause<Self, (), ()>>,
    },

    FieldAccess {
        // This is the location of the whole record and field
        //   user.name
        //   ^^^^^^^^^
        location: SrcSpan,
        // This is the location of just the field access
        //   user.name
        //       ^^^^^
        label_location: SrcSpan,
        label: EcoString,
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
        message: Option<Box<Self>>,
    },

    Panic {
        location: SrcSpan,
        message: Option<Box<Self>>,
    },

    BitArray {
        location: SrcSpan,
        segments: Vec<UntypedExprBitArraySegment>,
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
            Self::PipeLine { expressions, .. } => expressions.last().location(),

            Self::Fn { location, .. }
            | Self::Var { location, .. }
            | Self::Int { location, .. }
            | Self::Todo { location, .. }
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
            _ => std::u8::MAX,
        }
    }

    pub fn bin_op_name(&self) -> Option<&BinOp> {
        match self {
            UntypedExpr::BinOp { name, .. } => Some(name),
            _ => None,
        }
    }

    pub fn is_simple_constant(&self) -> bool {
        matches!(
            self,
            Self::String { .. } | Self::Int { .. } | Self::Float { .. }
        )
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

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Use {
    /// This is the expression with the untyped/typed code of the use callback
    /// function.
    ///
    pub call: Box<UntypedExpr>,

    /// This is the location of the whole use line, starting from the `use`
    /// keyword and ending with the function call on the right hand side of
    /// `<-`.
    ///
    /// ```gleam
    /// use a <- reult.try(result)
    /// ^^^^^^^^^^^^^^^^^^^^^^^^^^
    /// ```
    ///
    pub location: SrcSpan,

    /// This is the SrcSpan of the patterns you find on the left hand side of
    /// `<-` in a use expression.
    ///
    /// ```gleam
    /// use pattern1, pattern2 <- todo
    ///     ^^^^^^^^^^^^^^^^^^
    /// ```
    ///
    /// In case there's no patterns it will be corresponding to the SrcSpan of
    /// the `use` keyword itself.
    ///
    pub assignments_location: SrcSpan,

    /// The patterns on the left hand side of `<-` in a use expression.
    ///
    pub assignments: Vec<UseAssignment>,
}
