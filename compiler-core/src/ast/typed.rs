use super::*;
use crate::type_::{bool, HasType, Type};

use lazy_static::lazy_static;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TypedExpr {
    Int {
        location: SrcSpan,
        typ: Arc<Type>,
        value: SmolStr,
    },

    Float {
        location: SrcSpan,
        typ: Arc<Type>,
        value: SmolStr,
    },

    String {
        location: SrcSpan,
        typ: Arc<Type>,
        value: SmolStr,
    },

    Block {
        location: SrcSpan,
        statements: Vec1<TypedStatement>,
    },

    /// A chain of pipe expressions.
    /// By this point the type checker has expanded it into a series of
    /// assignments and function calls, but we still have a Pipeline AST node as
    /// even though it is identical to `Block` we want to use different
    /// locations when showing it in error messages, etc.
    Pipeline {
        location: SrcSpan,
        assignments: Vec<TypedAssignment>,
        finally: Box<Self>,
    },

    Var {
        location: SrcSpan,
        constructor: ValueConstructor,
        name: SmolStr,
    },

    Fn {
        location: SrcSpan,
        typ: Arc<Type>,
        is_capture: bool,
        args: Vec<Arg<Arc<Type>>>,
        body: Vec1<Statement<Arc<Type>, Self>>,
        return_annotation: Option<TypeAst>,
    },

    List {
        location: SrcSpan,
        typ: Arc<Type>,
        elements: Vec<Self>,
        tail: Option<Box<Self>>,
    },

    Call {
        location: SrcSpan,
        typ: Arc<Type>,
        fun: Box<Self>,
        args: Vec<CallArg<Self>>,
    },

    BinOp {
        location: SrcSpan,
        typ: Arc<Type>,
        name: BinOp,
        left: Box<Self>,
        right: Box<Self>,
    },

    Case {
        location: SrcSpan,
        typ: Arc<Type>,
        subjects: Vec<Self>,
        clauses: Vec<Clause<Self, Arc<Type>, SmolStr>>,
    },

    RecordAccess {
        location: SrcSpan,
        typ: Arc<Type>,
        label: SmolStr,
        index: u64,
        record: Box<Self>,
    },

    ModuleSelect {
        location: SrcSpan,
        typ: Arc<Type>,
        label: SmolStr,
        module_name: SmolStr,
        module_alias: SmolStr,
        constructor: ModuleValueConstructor,
    },

    Tuple {
        location: SrcSpan,
        typ: Arc<Type>,
        elems: Vec<Self>,
    },

    TupleIndex {
        location: SrcSpan,
        typ: Arc<Type>,
        index: u64,
        tuple: Box<Self>,
    },

    Todo {
        location: SrcSpan,
        message: Option<SmolStr>,
        type_: Arc<Type>,
    },

    Panic {
        location: SrcSpan,
        message: Option<SmolStr>,
        type_: Arc<Type>,
    },

    BitString {
        location: SrcSpan,
        typ: Arc<Type>,
        segments: Vec<TypedExprBitStringSegment>,
    },

    RecordUpdate {
        location: SrcSpan,
        typ: Arc<Type>,
        spread: Box<Self>,
        args: Vec<TypedRecordUpdateArg>,
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

impl TypedExpr {
    // This could be optimised in places to exit early if the first of a series
    // of expressions is after the byte index.
    pub fn find_node(&self, byte_index: u32) -> Option<Located<'_>> {
        // arguments of a function may not be in the same location as the function
        // e.g. in "use" expressions, so we handle it separately
        if !self.location().contains(byte_index) && !matches!(self, Self::Fn { .. }) {
            return None;
        }

        match self {
            Self::Var { .. }
            | Self::Int { .. }
            | Self::Todo { .. }
            | Self::Panic { .. }
            | Self::Float { .. }
            | Self::String { .. }
            | Self::ModuleSelect { .. } => Some(self.into()),

            Self::Pipeline {
                assignments,
                finally,
                ..
            } => assignments
                .iter()
                .find_map(|e| e.find_node(byte_index))
                .or_else(|| finally.find_node(byte_index)),

            Self::Block { statements, .. } => {
                statements.iter().find_map(|e| e.find_node(byte_index))
            }

            Self::Tuple {
                elems: expressions, ..
            }
            | Self::List {
                elements: expressions,
                ..
            } => expressions
                .iter()
                .find_map(|e| e.find_node(byte_index))
                .or(Some(self.into())),

            Self::NegateBool { value, .. } => value.find_node(byte_index).or(Some(self.into())),

            Self::NegateInt { value, .. } => value.find_node(byte_index).or(Some(self.into())),

            Self::Fn { body, args, .. } => args
                .iter()
                .find_map(|arg| arg.find_node(byte_index))
                .or_else(|| {
                    body.iter()
                        .find_map(|statement| statement.find_node(byte_index))
                        .or_else(|| {
                            if self.location().contains(byte_index) {
                                Some(self.into())
                            } else {
                                None
                            }
                        })
                }),

            Self::Call { fun, args, .. } => args
                .iter()
                .find_map(|arg| arg.find_node(byte_index))
                .or_else(|| fun.find_node(byte_index))
                .or(Some(self.into())),

            Self::BinOp { left, right, .. } => left
                .find_node(byte_index)
                .or_else(|| right.find_node(byte_index)),

            Self::Case {
                subjects, clauses, ..
            } => subjects
                .iter()
                .find_map(|subject| subject.find_node(byte_index))
                .or_else(|| {
                    clauses
                        .iter()
                        .find_map(|clause| clause.find_node(byte_index))
                })
                .or(Some(self.into())),

            Self::RecordAccess {
                record: expression, ..
            }
            | Self::TupleIndex {
                tuple: expression, ..
            } => expression.find_node(byte_index).or(Some(self.into())),

            Self::BitString { segments, .. } => segments
                .iter()
                .find_map(|arg| arg.find_node(byte_index))
                .or(Some(self.into())),

            Self::RecordUpdate { spread, args, .. } => args
                .iter()
                .find_map(|arg| arg.find_node(byte_index))
                .or_else(|| spread.find_node(byte_index))
                .or(Some(self.into())),
        }
    }

    pub fn non_zero_compile_time_number(&self) -> bool {
        use regex::Regex;
        lazy_static! {
            static ref NON_ZERO: Regex = Regex::new(r"[1-9]").expect("NON_ZERO regex");
        }

        matches!(
            self,
            Self::Int{ value, .. } | Self::Float { value, .. } if NON_ZERO.is_match(value)
        )
    }

    pub fn location(&self) -> SrcSpan {
        match self {
            Self::Fn { location, .. }
            | Self::Int { location, .. }
            | Self::Var { location, .. }
            | Self::Todo { location, .. }
            | Self::Case { location, .. }
            | Self::Call { location, .. }
            | Self::List { location, .. }
            | Self::Float { location, .. }
            | Self::BinOp { location, .. }
            | Self::Tuple { location, .. }
            | Self::Panic { location, .. }
            | Self::Block { location, .. }
            | Self::String { location, .. }
            | Self::NegateBool { location, .. }
            | Self::NegateInt { location, .. }
            | Self::Pipeline { location, .. }
            | Self::BitString { location, .. }
            | Self::TupleIndex { location, .. }
            | Self::ModuleSelect { location, .. }
            | Self::RecordAccess { location, .. }
            | Self::RecordUpdate { location, .. } => *location,
        }
    }

    pub fn type_defining_location(&self) -> SrcSpan {
        match self {
            Self::Fn { location, .. }
            | Self::Int { location, .. }
            | Self::Var { location, .. }
            | Self::Todo { location, .. }
            | Self::Case { location, .. }
            | Self::Call { location, .. }
            | Self::List { location, .. }
            | Self::Float { location, .. }
            | Self::BinOp { location, .. }
            | Self::Tuple { location, .. }
            | Self::String { location, .. }
            | Self::Panic { location, .. }
            | Self::NegateBool { location, .. }
            | Self::NegateInt { location, .. }
            | Self::Pipeline { location, .. }
            | Self::BitString { location, .. }
            | Self::TupleIndex { location, .. }
            | Self::ModuleSelect { location, .. }
            | Self::RecordAccess { location, .. }
            | Self::RecordUpdate { location, .. } => *location,
            Self::Block { statements, .. } => statements.last().location(),
        }
    }

    pub fn definition_location(&self) -> Option<DefinitionLocation<'_>> {
        match self {
            TypedExpr::Fn { .. }
            | TypedExpr::Int { .. }
            | TypedExpr::List { .. }
            | TypedExpr::Call { .. }
            | TypedExpr::Case { .. }
            | TypedExpr::Todo { .. }
            | TypedExpr::Panic { .. }
            | TypedExpr::BinOp { .. }
            | TypedExpr::Float { .. }
            | TypedExpr::Tuple { .. }
            | TypedExpr::NegateBool { .. }
            | TypedExpr::NegateInt { .. }
            | TypedExpr::String { .. }
            | TypedExpr::Block { .. }
            | TypedExpr::Pipeline { .. }
            | TypedExpr::BitString { .. }
            | TypedExpr::TupleIndex { .. }
            | TypedExpr::RecordAccess { .. } => None,

            // TODO: test
            // TODO: definition
            TypedExpr::RecordUpdate { .. } => None,

            // TODO: test
            TypedExpr::ModuleSelect {
                module_name,
                constructor,
                ..
            } => Some(DefinitionLocation {
                module: Some(module_name.as_str()),
                span: constructor.location(),
            }),

            // TODO: test
            TypedExpr::Var { constructor, .. } => Some(constructor.definition_location()),
        }
    }

    pub fn type_(&self) -> Arc<Type> {
        match self {
            Self::NegateBool { .. } => bool(),
            Self::NegateInt { value, .. } => value.type_(),
            Self::Var { constructor, .. } => constructor.type_.clone(),
            Self::Fn { typ, .. }
            | Self::Int { typ, .. }
            | Self::Todo { type_: typ, .. }
            | Self::Case { typ, .. }
            | Self::List { typ, .. }
            | Self::Call { typ, .. }
            | Self::Float { typ, .. }
            | Self::Panic { type_: typ, .. }
            | Self::BinOp { typ, .. }
            | Self::Tuple { typ, .. }
            | Self::String { typ, .. }
            | Self::BitString { typ, .. }
            | Self::TupleIndex { typ, .. }
            | Self::ModuleSelect { typ, .. }
            | Self::RecordAccess { typ, .. }
            | Self::RecordUpdate { typ, .. } => typ.clone(),
            Self::Pipeline { finally, .. } => finally.type_(),
            Self::Block { statements, .. } => statements.last().type_(),
        }
    }

    pub fn is_literal(&self) -> bool {
        matches!(
            self,
            Self::Int { .. }
                | Self::List { .. }
                | Self::Float { .. }
                | Self::Tuple { .. }
                | Self::String { .. }
                | Self::BitString { .. }
        )
    }

    /// Returns `true` if the typed expr is [`Var`].
    ///
    /// [`Var`]: TypedExpr::Var
    #[must_use]
    pub fn is_var(&self) -> bool {
        matches!(self, Self::Var { .. })
    }

    pub(crate) fn get_documentation(&self) -> Option<&str> {
        match self {
            TypedExpr::Var { constructor, .. } => constructor.get_documentation(),
            TypedExpr::ModuleSelect { constructor, .. } => constructor.get_documentation(),

            TypedExpr::Int { .. }
            | TypedExpr::Float { .. }
            | TypedExpr::String { .. }
            | TypedExpr::Block { .. }
            | TypedExpr::Pipeline { .. }
            | TypedExpr::Fn { .. }
            | TypedExpr::List { .. }
            | TypedExpr::Call { .. }
            | TypedExpr::BinOp { .. }
            | TypedExpr::Case { .. }
            | TypedExpr::Tuple { .. }
            | TypedExpr::TupleIndex { .. }
            | TypedExpr::Todo { .. }
            | TypedExpr::Panic { .. }
            | TypedExpr::BitString { .. }
            | TypedExpr::RecordUpdate { .. }
            | TypedExpr::RecordAccess { .. }
            | TypedExpr::NegateBool { .. }
            | TypedExpr::NegateInt { .. } => None,
        }
    }

    /// Returns `true` if the typed expr is [`Case`].
    ///
    /// [`Case`]: TypedExpr::Case
    #[must_use]
    pub fn is_case(&self) -> bool {
        matches!(self, Self::Case { .. })
    }

    /// Returns `true` if the typed expr is [`Pipeline`].
    ///
    /// [`Pipeline`]: TypedExpr::Pipeline
    #[must_use]
    pub fn is_pipeline(&self) -> bool {
        matches!(self, Self::Pipeline { .. })
    }
}

impl<'a> From<&'a TypedExpr> for Located<'a> {
    fn from(value: &'a TypedExpr) -> Self {
        Located::Expression(value)
    }
}

impl HasLocation for TypedExpr {
    fn location(&self) -> SrcSpan {
        self.location()
    }
}

impl HasType for TypedExpr {
    fn type_(&self) -> Arc<Type> {
        self.type_()
    }
}

impl crate::bit_string::GetLiteralValue for TypedExpr {
    fn as_int_literal(&self) -> Option<i64> {
        if let TypedExpr::Int { value: val, .. } = self {
            if let Ok(val) = val.parse::<i64>() {
                return Some(val);
            }
        }
        None
    }
}
