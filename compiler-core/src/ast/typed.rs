use std::sync::OnceLock;

use type_::{FieldMap, TypedCallArg};

use super::*;
use crate::{
    build::ExpressionPosition,
    exhaustiveness::CompiledCase,
    parse::LiteralFloatValue,
    type_::{HasType, Type, ValueConstructorVariant, bool},
};

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TypedExpr {
    Int {
        location: SrcSpan,
        type_: Arc<Type>,
        value: EcoString,
        int_value: BigInt,
    },

    Float {
        location: SrcSpan,
        type_: Arc<Type>,
        value: EcoString,
        float_value: LiteralFloatValue,
    },

    String {
        location: SrcSpan,
        type_: Arc<Type>,
        value: EcoString,
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
        first_value: TypedPipelineAssignment,
        assignments: Vec<(TypedPipelineAssignment, PipelineAssignmentKind)>,
        finally: Box<Self>,
        finally_kind: PipelineAssignmentKind,
    },

    Var {
        location: SrcSpan,
        constructor: ValueConstructor,
        name: EcoString,
    },

    Fn {
        location: SrcSpan,
        type_: Arc<Type>,
        kind: FunctionLiteralKind,
        arguments: Vec<TypedArg>,
        body: Vec1<TypedStatement>,
        return_annotation: Option<TypeAst>,
        purity: Purity,
    },

    List {
        location: SrcSpan,
        type_: Arc<Type>,
        elements: Vec<Self>,
        tail: Option<Box<Self>>,
    },

    Call {
        location: SrcSpan,
        type_: Arc<Type>,
        fun: Box<Self>,
        arguments: Vec<CallArg<Self>>,
    },

    BinOp {
        location: SrcSpan,
        type_: Arc<Type>,
        name: BinOp,
        name_location: SrcSpan,
        left: Box<Self>,
        right: Box<Self>,
    },

    Case {
        location: SrcSpan,
        type_: Arc<Type>,
        subjects: Vec<Self>,
        clauses: Vec<Clause<Self, Arc<Type>, EcoString>>,
        compiled_case: CompiledCase,
    },

    RecordAccess {
        location: SrcSpan,
        field_start: u32,
        type_: Arc<Type>,
        label: EcoString,
        index: u64,
        record: Box<Self>,
        documentation: Option<EcoString>,
    },

    /// Generated internally for accessing unlabelled fields of a custom type,
    /// such as for record updates.
    PositionalAccess {
        location: SrcSpan,
        type_: Arc<Type>,
        index: u64,
        record: Box<Self>,
    },

    ModuleSelect {
        location: SrcSpan,
        field_start: u32,
        type_: Arc<Type>,
        label: EcoString,
        module_name: EcoString,
        module_alias: EcoString,
        constructor: ModuleValueConstructor,
    },

    Tuple {
        location: SrcSpan,
        type_: Arc<Type>,
        elements: Vec<Self>,
    },

    TupleIndex {
        location: SrcSpan,
        type_: Arc<Type>,
        index: u64,
        tuple: Box<Self>,
    },

    Todo {
        location: SrcSpan,
        message: Option<Box<Self>>,
        kind: TodoKind,
        type_: Arc<Type>,
    },

    Panic {
        location: SrcSpan,
        message: Option<Box<Self>>,
        type_: Arc<Type>,
    },

    Echo {
        location: SrcSpan,
        type_: Arc<Type>,
        expression: Option<Box<Self>>,
        message: Option<Box<Self>>,
    },

    BitArray {
        location: SrcSpan,
        type_: Arc<Type>,
        segments: Vec<TypedExprBitArraySegment>,
    },

    /// A record update gets desugared to a block expression of the form
    ///
    /// {
    ///   let _record = record
    ///   Constructor(explicit_arg: explicit_value(), implicit_arg: _record.implicit_arg)
    /// }
    ///
    /// We still keep a separate `RecordUpdate` AST node for the same reasons as
    /// we do for pipelines.
    RecordUpdate {
        location: SrcSpan,
        type_: Arc<Type>,
        /// If the record is an expression that is not a variable we will need to assign to a
        /// variable so it can be referred multiple times.
        record_assignment: Option<Box<TypedAssignment>>,
        constructor: Box<Self>,
        arguments: Vec<CallArg<Self>>,
    },

    NegateBool {
        location: SrcSpan,
        value: Box<Self>,
    },

    NegateInt {
        location: SrcSpan,
        value: Box<Self>,
    },

    /// A placeholder expression used to allow module analysis to continue
    /// even when there are type errors. Should never end up in generated code.
    Invalid {
        location: SrcSpan,
        type_: Arc<Type>,
        /// Extra information about the invalid expression, useful for providing
        /// addition help or information, such as code actions to fix invalid
        /// states.
        extra_information: Option<InvalidExpression>,
    },
}

impl TypedExpr {
    pub fn is_println(&self) -> bool {
        let fun = if let TypedExpr::Call { fun, arguments, .. } = self
            && arguments.len() == 1
        {
            fun.as_ref()
        } else {
            return false;
        };

        if let TypedExpr::ModuleSelect {
            label, module_name, ..
        } = fun
        {
            label == "println" && module_name == "gleam/io"
        } else {
            false
        }
    }

    pub fn find_node(&self, byte_index: u32) -> Option<Located<'_>> {
        match self {
            Self::Var { .. }
            | Self::Int { .. }
            | Self::Float { .. }
            | Self::String { .. }
            | Self::Invalid { .. }
            | Self::PositionalAccess { .. } => self.self_if_contains_location(byte_index),

            Self::ModuleSelect {
                location,
                field_start,
                module_name,
                module_alias,
                ..
            } => {
                // We want to return the `ModuleSelect` only when we're hovering
                // over the selected field, not on the module part.
                let field_span = SrcSpan {
                    start: *field_start,
                    end: location.end,
                };

                let module_span =
                    SrcSpan::new(location.start, location.start + (module_alias.len() as u32));

                if field_span.contains(byte_index) {
                    Some(self.into())
                } else if SrcSpan::new(location.start, field_start - 1).contains(byte_index) {
                    Some(Located::ModuleName {
                        location: module_span,
                        module_name: module_name.clone(),
                        module_alias: module_alias.clone(),
                        layer: Layer::Value,
                    })
                } else {
                    None
                }
            }

            Self::Echo {
                expression,
                message,
                ..
            } => expression
                .as_ref()
                .and_then(|expression| expression.find_node(byte_index))
                .or_else(|| {
                    message
                        .as_ref()
                        .and_then(|message| message.find_node(byte_index))
                })
                .or_else(|| self.self_if_contains_location(byte_index)),

            Self::Panic { message, .. } => message
                .as_ref()
                .and_then(|message| message.find_node(byte_index))
                .or_else(|| self.self_if_contains_location(byte_index)),

            Self::Todo { kind, message, .. } => match kind {
                TodoKind::Keyword => message
                    .as_ref()
                    .and_then(|message| message.find_node(byte_index))
                    .or_else(|| self.self_if_contains_location(byte_index)),
                // We don't want to match on todos that were implicitly inserted
                // by the compiler as it would result in confusing suggestions
                // from the LSP.
                TodoKind::EmptyFunction { .. } | TodoKind::EmptyBlock | TodoKind::IncompleteUse => {
                    None
                }
            },

            Self::Pipeline {
                first_value,
                assignments,
                finally,
                ..
            } => first_value
                .find_node(byte_index)
                .or_else(|| {
                    assignments
                        .iter()
                        .find_map(|(e, _)| e.find_node(byte_index))
                })
                .or_else(|| finally.find_node(byte_index)),

            // Exit the search and return None if during iteration a statement
            // is found with a start index beyond the index under search.
            Self::Block { statements, .. } => {
                for statement in statements {
                    if statement.location().start > byte_index {
                        break;
                    }

                    if let Some(located) = statement.find_node(byte_index) {
                        return Some(located);
                    }
                }

                None
            }

            // Exit the search and return the encompassing type (e.g., list or tuple)
            // if during iteration, an element is encountered with a start index
            // beyond the index under search.
            Self::Tuple {
                elements: expressions,
                ..
            } => {
                for expression in expressions {
                    if expression.location().start > byte_index {
                        break;
                    }

                    if let Some(located) = expression.find_node(byte_index) {
                        return Some(located);
                    }
                }

                self.self_if_contains_location(byte_index)
            }

            Self::List {
                elements: expressions,
                tail,
                ..
            } => {
                for expression in expressions {
                    if expression.location().start > byte_index {
                        break;
                    }

                    if let Some(located) = expression.find_node(byte_index) {
                        return Some(located);
                    }
                }

                if let Some(tail) = tail
                    && let Some(node) = tail.find_node(byte_index)
                {
                    return Some(node);
                }
                self.self_if_contains_location(byte_index)
            }

            Self::NegateBool { value, .. } | Self::NegateInt { value, .. } => value
                .find_node(byte_index)
                .or_else(|| self.self_if_contains_location(byte_index)),

            Self::Fn {
                body, arguments, ..
            } => arguments
                .iter()
                .find_map(|arg| arg.find_node(byte_index))
                .or_else(|| body.iter().find_map(|s| s.find_node(byte_index)))
                .or_else(|| self.self_if_contains_location(byte_index)),

            Self::Call { fun, arguments, .. } => arguments
                .iter()
                .find_map(|argument| argument.find_node(byte_index, fun, arguments))
                .or_else(|| fun.find_node(byte_index))
                .or_else(|| self.self_if_contains_location(byte_index)),

            Self::BinOp { left, right, .. } => left
                .find_node(byte_index)
                .or_else(|| right.find_node(byte_index)),

            Self::Case {
                subjects, clauses, ..
            } => subjects
                .iter()
                .find_map(|subject| subject.find_node(byte_index))
                .or_else(|| clauses.iter().find_map(|c| c.find_node(byte_index)))
                .or_else(|| self.self_if_contains_location(byte_index)),

            Self::RecordAccess {
                record: expression, ..
            }
            | Self::TupleIndex {
                tuple: expression, ..
            } => expression
                .find_node(byte_index)
                .or_else(|| self.self_if_contains_location(byte_index)),

            Self::BitArray { segments, .. } => segments
                .iter()
                .find_map(|arg| arg.find_node(byte_index))
                .or_else(|| self.self_if_contains_location(byte_index)),

            Self::RecordUpdate {
                record_assignment,
                constructor,
                arguments,
                ..
            } => arguments
                .iter()
                .filter(|argument| argument.implicit.is_none())
                .find_map(|argument| argument.find_node(byte_index, constructor, arguments))
                .or_else(|| constructor.find_node(byte_index))
                .or_else(|| {
                    record_assignment
                        .as_ref()
                        .and_then(|assignment| assignment.find_node(byte_index))
                })
                .or_else(|| self.self_if_contains_location(byte_index)),
        }
    }

    pub fn find_statement(&self, byte_index: u32) -> Option<&TypedStatement> {
        match self {
            Self::Var { .. }
            | Self::Int { .. }
            | Self::Float { .. }
            | Self::String { .. }
            | Self::ModuleSelect { .. }
            | Self::Invalid { .. }
            | Self::PositionalAccess { .. } => None,

            Self::Pipeline {
                first_value,
                assignments,
                finally,
                ..
            } => first_value
                .find_statement(byte_index)
                .or_else(|| {
                    assignments
                        .iter()
                        .find_map(|(e, _)| e.find_statement(byte_index))
                })
                .or_else(|| finally.find_statement(byte_index)),

            // Exit the search and return None if during iteration a statement
            // is found with a start index beyond the index under search.
            Self::Block { statements, .. } => {
                for statement in statements {
                    if statement.location().start > byte_index {
                        break;
                    }

                    if let Some(located) = statement.find_statement(byte_index) {
                        return Some(located);
                    }
                }

                None
            }

            // Exit the search and return the encompassing type (e.g., list or tuple)
            // if during iteration, an element is encountered with a start index
            // beyond the index under search.
            Self::Tuple {
                elements: expressions,
                ..
            } => {
                for expression in expressions {
                    if expression.location().start > byte_index {
                        break;
                    }

                    if let Some(located) = expression.find_statement(byte_index) {
                        return Some(located);
                    }
                }

                None
            }

            Self::List {
                elements: expressions,
                tail,
                ..
            } => {
                for expression in expressions {
                    if expression.location().start > byte_index {
                        break;
                    }

                    if let Some(located) = expression.find_statement(byte_index) {
                        return Some(located);
                    }
                }

                if let Some(tail) = tail
                    && let Some(node) = tail.find_statement(byte_index)
                {
                    return Some(node);
                }
                None
            }

            Self::NegateBool { value, .. } | Self::NegateInt { value, .. } => {
                value.find_statement(byte_index)
            }

            Self::Fn { body, .. } => body.iter().find_map(|s| s.find_statement(byte_index)),

            Self::Call { fun, arguments, .. } => arguments
                .iter()
                .find_map(|argument| argument.find_statement(byte_index))
                .or_else(|| fun.find_statement(byte_index)),

            Self::BinOp { left, right, .. } => left
                .find_statement(byte_index)
                .or_else(|| right.find_statement(byte_index)),

            Self::Case {
                subjects, clauses, ..
            } => subjects
                .iter()
                .find_map(|subject| subject.find_statement(byte_index))
                .or_else(|| {
                    clauses
                        .iter()
                        .find_map(|c| c.then.find_statement(byte_index))
                }),

            Self::RecordAccess {
                record: expression, ..
            }
            | Self::TupleIndex {
                tuple: expression, ..
            } => expression.find_statement(byte_index),

            Self::Echo {
                expression,
                message,
                ..
            } => expression
                .as_ref()
                .and_then(|expression| expression.find_statement(byte_index))
                .or_else(|| {
                    message
                        .as_ref()
                        .and_then(|message| message.find_statement(byte_index))
                }),

            Self::Todo { message, kind, .. } => match kind {
                TodoKind::EmptyFunction { .. } | TodoKind::IncompleteUse | TodoKind::EmptyBlock => {
                    None
                }
                TodoKind::Keyword => message
                    .as_ref()
                    .and_then(|message| message.find_statement(byte_index)),
            },

            Self::Panic { message, .. } => message
                .as_ref()
                .and_then(|message| message.find_statement(byte_index)),

            Self::BitArray { segments, .. } => segments
                .iter()
                .find_map(|arg| arg.value.find_statement(byte_index)),

            Self::RecordUpdate {
                record_assignment,
                arguments,
                ..
            } => arguments
                .iter()
                .filter(|arg| arg.implicit.is_none())
                .find_map(|arg| arg.find_statement(byte_index))
                .or_else(|| {
                    record_assignment
                        .as_ref()
                        .and_then(|r| r.value.find_statement(byte_index))
                }),
        }
    }

    fn self_if_contains_location(&self, byte_index: u32) -> Option<Located<'_>> {
        if self.location().contains(byte_index) {
            Some(self.into())
        } else {
            None
        }
    }

    pub fn is_non_zero_compile_time_number(&self) -> bool {
        match self {
            Self::Int { int_value, .. } => int_value != &BigInt::ZERO,
            Self::Float { value, .. } => is_non_zero_number(value),
            Self::String { .. }
            | Self::Block { .. }
            | Self::Pipeline { .. }
            | Self::Var { .. }
            | Self::Fn { .. }
            | Self::List { .. }
            | Self::Call { .. }
            | Self::BinOp { .. }
            | Self::Case { .. }
            | Self::RecordAccess { .. }
            | Self::PositionalAccess { .. }
            | Self::ModuleSelect { .. }
            | Self::Tuple { .. }
            | Self::TupleIndex { .. }
            | Self::Todo { .. }
            | Self::Panic { .. }
            | Self::Echo { .. }
            | Self::BitArray { .. }
            | Self::RecordUpdate { .. }
            | Self::NegateBool { .. }
            | Self::NegateInt { .. }
            | Self::Invalid { .. } => false,
        }
    }

    pub fn is_zero_compile_time_number(&self) -> bool {
        match self {
            Self::Int { int_value, .. } => int_value == &BigInt::ZERO,
            Self::Float { value, .. } => !is_non_zero_number(value),
            Self::String { .. }
            | Self::Block { .. }
            | Self::Pipeline { .. }
            | Self::Var { .. }
            | Self::Fn { .. }
            | Self::List { .. }
            | Self::Call { .. }
            | Self::BinOp { .. }
            | Self::Case { .. }
            | Self::RecordAccess { .. }
            | Self::PositionalAccess { .. }
            | Self::ModuleSelect { .. }
            | Self::Tuple { .. }
            | Self::TupleIndex { .. }
            | Self::Todo { .. }
            | Self::Panic { .. }
            | Self::Echo { .. }
            | Self::BitArray { .. }
            | Self::RecordUpdate { .. }
            | Self::NegateBool { .. }
            | Self::NegateInt { .. }
            | Self::Invalid { .. } => false,
        }
    }

    pub fn location(&self) -> SrcSpan {
        match self {
            Self::Fn { location, .. }
            | Self::Int { location, .. }
            | Self::Var { location, .. }
            | Self::Todo { location, .. }
            | Self::Echo { location, .. }
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
            | Self::BitArray { location, .. }
            | Self::TupleIndex { location, .. }
            | Self::ModuleSelect { location, .. }
            | Self::RecordAccess { location, .. }
            | Self::PositionalAccess { location, .. }
            | Self::RecordUpdate { location, .. }
            | Self::Invalid { location, .. } => *location,
        }
    }

    pub fn type_defining_location(&self) -> SrcSpan {
        match self {
            Self::Fn { location, .. }
            | Self::Int { location, .. }
            | Self::Var { location, .. }
            | Self::Todo { location, .. }
            | Self::Echo { location, .. }
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
            | Self::BitArray { location, .. }
            | Self::TupleIndex { location, .. }
            | Self::ModuleSelect { location, .. }
            | Self::RecordAccess { location, .. }
            | Self::PositionalAccess { location, .. }
            | Self::RecordUpdate { location, .. }
            | Self::Invalid { location, .. } => *location,
            Self::Block { statements, .. } => statements.last().location(),
        }
    }

    pub fn definition_location(&self) -> Option<DefinitionLocation> {
        match self {
            TypedExpr::Fn { .. }
            | TypedExpr::Int { .. }
            | TypedExpr::List { .. }
            | TypedExpr::Call { .. }
            | TypedExpr::Case { .. }
            | TypedExpr::Todo { .. }
            | TypedExpr::Echo { .. }
            | TypedExpr::Panic { .. }
            | TypedExpr::BinOp { .. }
            | TypedExpr::Float { .. }
            | TypedExpr::Tuple { .. }
            | TypedExpr::NegateBool { .. }
            | TypedExpr::NegateInt { .. }
            | TypedExpr::String { .. }
            | TypedExpr::Block { .. }
            | TypedExpr::Pipeline { .. }
            | TypedExpr::BitArray { .. }
            | TypedExpr::TupleIndex { .. }
            | TypedExpr::RecordAccess { .. }
            | TypedExpr::PositionalAccess { .. }
            | Self::Invalid { .. } => None,

            // TODO: test
            // TODO: definition
            TypedExpr::RecordUpdate { .. } => None,

            // TODO: test
            TypedExpr::ModuleSelect {
                module_name,
                constructor,
                ..
            } => Some(DefinitionLocation {
                module: Some(module_name.clone()),
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
            Self::Fn { type_, .. }
            | Self::Int { type_, .. }
            | Self::Todo { type_, .. }
            | Self::Echo { type_, .. }
            | Self::Case { type_, .. }
            | Self::List { type_, .. }
            | Self::Call { type_, .. }
            | Self::Float { type_, .. }
            | Self::Panic { type_, .. }
            | Self::BinOp { type_, .. }
            | Self::Tuple { type_, .. }
            | Self::String { type_, .. }
            | Self::BitArray { type_, .. }
            | Self::TupleIndex { type_, .. }
            | Self::ModuleSelect { type_, .. }
            | Self::RecordAccess { type_, .. }
            | Self::PositionalAccess { type_, .. }
            | Self::RecordUpdate { type_, .. }
            | Self::Invalid { type_, .. } => type_.clone(),
            Self::Pipeline { finally, .. } => finally.type_(),
            Self::Block { statements, .. } => statements.last().type_(),
        }
    }

    pub fn is_literal(&self) -> bool {
        match self {
            Self::Int { .. } | Self::Float { .. } | Self::String { .. } => true,

            Self::List { elements, .. } | Self::Tuple { elements, .. } => {
                elements.iter().all(|value| value.is_literal())
            }

            Self::BitArray { segments, .. } => {
                segments.iter().all(|segment| segment.value.is_literal())
            }

            // Calls are literals if they are records and all the arguemnts are also literals.
            Self::Call { fun, arguments, .. } => {
                fun.is_record_literal()
                    && arguments.iter().all(|argument| argument.value.is_literal())
            }

            // Variables are literals if they are record constructors that take no arguments.
            Self::Var {
                constructor:
                    ValueConstructor {
                        variant: ValueConstructorVariant::Record { arity: 0, .. },
                        ..
                    },
                ..
            } => true,

            Self::Block { .. }
            | Self::Pipeline { .. }
            | Self::Var { .. }
            | Self::Fn { .. }
            | Self::BinOp { .. }
            | Self::Case { .. }
            | Self::RecordAccess { .. }
            | Self::PositionalAccess { .. }
            | Self::ModuleSelect { .. }
            | Self::TupleIndex { .. }
            | Self::Todo { .. }
            | Self::Panic { .. }
            | Self::Echo { .. }
            | Self::RecordUpdate { .. }
            | Self::NegateBool { .. }
            | Self::NegateInt { .. }
            | Self::Invalid { .. } => false,
        }
    }

    pub fn is_known_bool(&self) -> bool {
        match self {
            TypedExpr::BinOp {
                left, right, name, ..
            } if name.is_bool_operator() => left.is_known_bool() && right.is_known_bool(),
            TypedExpr::NegateBool { value, .. } => value.is_known_bool(),
            TypedExpr::Int { .. }
            | TypedExpr::Float { .. }
            | TypedExpr::String { .. }
            | TypedExpr::Block { .. }
            | TypedExpr::Pipeline { .. }
            | TypedExpr::Var { .. }
            | TypedExpr::Fn { .. }
            | TypedExpr::List { .. }
            | TypedExpr::Call { .. }
            | TypedExpr::BinOp { .. }
            | TypedExpr::Case { .. }
            | TypedExpr::RecordAccess { .. }
            | TypedExpr::PositionalAccess { .. }
            | TypedExpr::ModuleSelect { .. }
            | TypedExpr::Tuple { .. }
            | TypedExpr::TupleIndex { .. }
            | TypedExpr::Todo { .. }
            | TypedExpr::Panic { .. }
            | TypedExpr::Echo { .. }
            | TypedExpr::BitArray { .. }
            | TypedExpr::RecordUpdate { .. }
            | TypedExpr::NegateInt { .. }
            | TypedExpr::Invalid { .. } => self.is_literal(),
        }
    }

    pub fn is_literal_string(&self) -> bool {
        matches!(self, Self::String { .. })
    }

    /// Returns `true` if the typed expr is [`Var`].
    ///
    /// [`Var`]: TypedExpr::Var
    #[must_use]
    pub fn is_var(&self) -> bool {
        matches!(self, Self::Var { .. })
    }

    pub fn get_documentation(&self) -> Option<&str> {
        match self {
            TypedExpr::Var { constructor, .. } => constructor.get_documentation(),
            TypedExpr::ModuleSelect { constructor, .. } => constructor.get_documentation(),
            TypedExpr::RecordAccess { documentation, .. } => documentation.as_deref(),

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
            | TypedExpr::Echo { .. }
            | TypedExpr::Panic { .. }
            | TypedExpr::BitArray { .. }
            | TypedExpr::RecordUpdate { .. }
            | TypedExpr::NegateBool { .. }
            | TypedExpr::NegateInt { .. }
            | TypedExpr::PositionalAccess { .. }
            | TypedExpr::Invalid { .. } => None,
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

    pub fn is_pure_value_constructor(&self) -> bool {
        match self {
            TypedExpr::Int { .. }
            | TypedExpr::Float { .. }
            | TypedExpr::String { .. }
            | TypedExpr::List { .. }
            | TypedExpr::Tuple { .. }
            | TypedExpr::BitArray { .. }
            | TypedExpr::Var { .. }
            | TypedExpr::BinOp { .. }
            | TypedExpr::RecordAccess { .. }
            | TypedExpr::PositionalAccess { .. }
            | TypedExpr::TupleIndex { .. }
            | TypedExpr::RecordUpdate { .. }
            | TypedExpr::Fn { .. } => true,

            TypedExpr::NegateBool { value, .. } | TypedExpr::NegateInt { value, .. } => {
                value.is_pure_value_constructor()
            }

            // Just selecting a value from a module never has any effects. The
            // selected thing might be a function but it has no side effects as
            // long as it's not called!
            TypedExpr::ModuleSelect { .. } => true,

            // A pipeline is a pure value constructor if its last step is a record builder,
            // or a call to a pure function. For example:
            //  - `wibble() |> wobble() |> Ok`
            //  - `"hello" |> fn(s) { s <> " world!" }`
            TypedExpr::Pipeline {
                first_value,
                assignments,
                finally,
                ..
            } => {
                first_value.value.is_pure_value_constructor()
                    && assignments
                        .iter()
                        .all(|(assignment, _)| assignment.value.is_pure_value_constructor())
                    && finally.is_pure_value_constructor()
            }

            TypedExpr::Call { fun, arguments, .. } => {
                (fun.is_record_literal() || fun.called_function_purity().is_pure())
                    && arguments
                        .iter()
                        .all(|argument| argument.value.is_pure_value_constructor())
            }

            // A block is pure if all the statements it's made of are pure.
            // For example `{ True 1 }`
            TypedExpr::Block { statements, .. } => {
                statements.iter().all(|s| s.is_pure_value_constructor())
            }

            // A case is pure if its subject and all its branches are.
            // For example:
            // ```gleam
            // case 1 + 1 {
            //   0 -> 1
            //   _ -> 2
            // }
            // ```
            TypedExpr::Case {
                subjects, clauses, ..
            } => {
                subjects.iter().all(|s| s.is_pure_value_constructor())
                    && clauses.iter().all(|c| c.then.is_pure_value_constructor())
            }

            // `panic`, `todo`, and placeholders are never considered pure value constructors,
            // we don't want to raise a warning for an unused value if it's one
            // of those.
            TypedExpr::Todo { .. }
            | TypedExpr::Panic { .. }
            | TypedExpr::Echo { .. }
            | TypedExpr::Invalid { .. } => false,
        }
    }

    /// Returns the purity of the left hand side of a function call. For example:
    ///
    /// ```gleam
    /// io.println("Hello, world!")
    /// ```
    ///
    /// Here, the left hand side is `io.println`, which is an impure function,
    /// so we would return `Purity::Impure`.
    ///
    /// This does not check whether an expression is pure on its own; for that
    /// see `is_pure_value_constructor`.
    ///
    pub fn called_function_purity(&self) -> Purity {
        match self {
            TypedExpr::Var { constructor, .. } => constructor.called_function_purity(),
            TypedExpr::ModuleSelect { constructor, .. } => constructor.called_function_purity(),
            TypedExpr::Fn { purity, .. } => *purity,

            // While we can infer the purity of some of these expressions, such
            // as `Case`, in this example:
            //  ```gleam
            // case x {
            //   True -> io.println
            //   False -> function.identity
            // }("Hello")
            // ```
            //
            // This kind of code is rare in real Gleam applications, and as this
            // system is just used for warnings, it is unlikely that supporting
            // them will provide any significant benefit to developer experience,
            // so we just return `Unknown` for simplicity.
            //
            TypedExpr::Block { .. }
            | TypedExpr::Pipeline { .. }
            | TypedExpr::Call { .. }
            | TypedExpr::Case { .. }
            | TypedExpr::RecordAccess { .. }
            | TypedExpr::TupleIndex { .. }
            | TypedExpr::Echo { .. } => Purity::Unknown,

            // The following expressions are all invalid on the left hand side
            // of a call expression: `10()` is not valid Gleam. Therefore, we
            // don't really care about any of these as they shouldn't appear in
            // well typed Gleam code, and so we can just return `Unknown`.
            TypedExpr::Int { .. }
            | TypedExpr::Float { .. }
            | TypedExpr::String { .. }
            | TypedExpr::List { .. }
            | TypedExpr::BinOp { .. }
            | TypedExpr::Tuple { .. }
            | TypedExpr::Todo { .. }
            | TypedExpr::Panic { .. }
            | TypedExpr::BitArray { .. }
            | TypedExpr::RecordUpdate { .. }
            | TypedExpr::NegateBool { .. }
            | TypedExpr::NegateInt { .. }
            | TypedExpr::PositionalAccess { .. }
            | TypedExpr::Invalid { .. } => Purity::Unknown,
        }
    }

    #[must_use]
    /// Returns true if the value is a literal record builder like
    /// `Wibble(1, 2)`, `module.Wobble("a")`
    ///
    pub fn is_record_literal(&self) -> bool {
        match self {
            TypedExpr::Call { fun, .. } => fun.is_record_literal(),
            TypedExpr::Var { constructor, .. } => constructor.variant.is_record(),
            TypedExpr::ModuleSelect {
                constructor: ModuleValueConstructor::Record { .. },
                ..
            } => true,
            TypedExpr::Int { .. }
            | TypedExpr::Float { .. }
            | TypedExpr::String { .. }
            | TypedExpr::Block { .. }
            | TypedExpr::Pipeline { .. }
            | TypedExpr::Fn { .. }
            | TypedExpr::List { .. }
            | TypedExpr::BinOp { .. }
            | TypedExpr::Case { .. }
            | TypedExpr::RecordAccess { .. }
            | TypedExpr::PositionalAccess { .. }
            | TypedExpr::ModuleSelect { .. }
            | TypedExpr::Tuple { .. }
            | TypedExpr::TupleIndex { .. }
            | TypedExpr::Todo { .. }
            | TypedExpr::Panic { .. }
            | TypedExpr::Echo { .. }
            | TypedExpr::BitArray { .. }
            | TypedExpr::RecordUpdate { .. }
            | TypedExpr::NegateBool { .. }
            | TypedExpr::NegateInt { .. }
            | TypedExpr::Invalid { .. } => false,
        }
    }

    /// Returns true if the expression is a record constructor function/record constructor
    /// with non-zero arity.
    ///
    pub fn is_record_constructor_function(&self) -> bool {
        match self {
            TypedExpr::Var {
                constructor:
                    ValueConstructor {
                        variant: ValueConstructorVariant::Record { arity, .. },
                        ..
                    },
                ..
            } => *arity > 0,

            TypedExpr::ModuleSelect {
                constructor: ModuleValueConstructor::Record { arity, .. },
                ..
            } => *arity > 0,

            TypedExpr::Int { .. }
            | TypedExpr::Float { .. }
            | TypedExpr::String { .. }
            | TypedExpr::Block { .. }
            | TypedExpr::Pipeline { .. }
            | TypedExpr::Var { .. }
            | TypedExpr::Fn { .. }
            | TypedExpr::List { .. }
            | TypedExpr::Call { .. }
            | TypedExpr::BinOp { .. }
            | TypedExpr::Case { .. }
            | TypedExpr::RecordAccess { .. }
            | TypedExpr::PositionalAccess { .. }
            | TypedExpr::ModuleSelect { .. }
            | TypedExpr::Tuple { .. }
            | TypedExpr::TupleIndex { .. }
            | TypedExpr::Todo { .. }
            | TypedExpr::Panic { .. }
            | TypedExpr::Echo { .. }
            | TypedExpr::BitArray { .. }
            | TypedExpr::RecordUpdate { .. }
            | TypedExpr::NegateBool { .. }
            | TypedExpr::NegateInt { .. }
            | TypedExpr::Invalid { .. } => false,
        }
    }

    /// If the given expression is a literal record builder, this will return
    /// index of the variant being built.
    ///
    pub fn variant_index(&self) -> Option<u16> {
        match self {
            TypedExpr::Call { fun, .. } => fun.variant_index(),
            TypedExpr::Var {
                constructor:
                    ValueConstructor {
                        variant: ValueConstructorVariant::Record { variant_index, .. },
                        ..
                    },
                ..
            }
            | TypedExpr::ModuleSelect {
                constructor: ModuleValueConstructor::Record { variant_index, .. },
                ..
            } => Some(*variant_index),
            TypedExpr::Int { .. }
            | TypedExpr::Float { .. }
            | TypedExpr::String { .. }
            | TypedExpr::Block { .. }
            | TypedExpr::Pipeline { .. }
            | TypedExpr::Var { .. }
            | TypedExpr::Fn { .. }
            | TypedExpr::List { .. }
            | TypedExpr::BinOp { .. }
            | TypedExpr::Case { .. }
            | TypedExpr::RecordAccess { .. }
            | TypedExpr::PositionalAccess { .. }
            | TypedExpr::ModuleSelect { .. }
            | TypedExpr::Tuple { .. }
            | TypedExpr::TupleIndex { .. }
            | TypedExpr::Todo { .. }
            | TypedExpr::Panic { .. }
            | TypedExpr::Echo { .. }
            | TypedExpr::BitArray { .. }
            | TypedExpr::RecordUpdate { .. }
            | TypedExpr::NegateBool { .. }
            | TypedExpr::NegateInt { .. }
            | TypedExpr::Invalid { .. } => None,
        }
    }

    #[must_use]
    /// If `self` is a record constructor, returns the number of arguments it
    /// needs to be called. Otherwise, returns `None`.
    ///
    pub fn record_constructor_arity(&self) -> Option<u16> {
        match self {
            TypedExpr::Call { fun, .. } => fun.record_constructor_arity(),
            TypedExpr::Var {
                constructor:
                    ValueConstructor {
                        variant: ValueConstructorVariant::Record { arity, .. },
                        ..
                    },
                ..
            } => Some(*arity),
            TypedExpr::Int { .. }
            | TypedExpr::Float { .. }
            | TypedExpr::String { .. }
            | TypedExpr::Block { .. }
            | TypedExpr::Pipeline { .. }
            | TypedExpr::Var { .. }
            | TypedExpr::Fn { .. }
            | TypedExpr::List { .. }
            | TypedExpr::BinOp { .. }
            | TypedExpr::Case { .. }
            | TypedExpr::RecordAccess { .. }
            | TypedExpr::PositionalAccess { .. }
            | TypedExpr::ModuleSelect { .. }
            | TypedExpr::Tuple { .. }
            | TypedExpr::TupleIndex { .. }
            | TypedExpr::Todo { .. }
            | TypedExpr::Panic { .. }
            | TypedExpr::Echo { .. }
            | TypedExpr::BitArray { .. }
            | TypedExpr::RecordUpdate { .. }
            | TypedExpr::NegateBool { .. }
            | TypedExpr::NegateInt { .. }
            | TypedExpr::Invalid { .. } => None,
        }
    }

    pub fn var_constructor(&self) -> Option<(&ValueConstructor, &EcoString)> {
        if let TypedExpr::Var {
            constructor, name, ..
        } = self
        {
            Some((constructor, name))
        } else {
            None
        }
    }

    #[must_use]
    pub(crate) fn is_panic(&self) -> bool {
        matches!(self, TypedExpr::Panic { .. })
    }

    pub(crate) fn call_arguments(&self) -> Option<&Vec<TypedCallArg>> {
        if let TypedExpr::Call { arguments, .. } = self {
            Some(arguments)
        } else {
            None
        }
    }

    pub(crate) fn fn_expression_body(&self) -> Option<&Vec1<TypedStatement>> {
        if let TypedExpr::Fn { body, .. } = self {
            Some(body)
        } else {
            None
        }
    }

    // If the expression is a fn or a block then returns the location of its
    // last element, otherwise it returns the location of the whole expression.
    pub fn last_location(&self) -> SrcSpan {
        match self {
            TypedExpr::Int { location, .. }
            | TypedExpr::Float { location, .. }
            | TypedExpr::String { location, .. }
            | TypedExpr::Var { location, .. }
            | TypedExpr::List { location, .. }
            | TypedExpr::Call { location, .. }
            | TypedExpr::BinOp { location, .. }
            | TypedExpr::Case { location, .. }
            | TypedExpr::RecordAccess { location, .. }
            | TypedExpr::PositionalAccess { location, .. }
            | TypedExpr::ModuleSelect { location, .. }
            | TypedExpr::Tuple { location, .. }
            | TypedExpr::TupleIndex { location, .. }
            | TypedExpr::Todo { location, .. }
            | TypedExpr::Panic { location, .. }
            | TypedExpr::BitArray { location, .. }
            | TypedExpr::RecordUpdate { location, .. }
            | TypedExpr::NegateBool { location, .. }
            | TypedExpr::NegateInt { location, .. }
            | TypedExpr::Invalid { location, .. }
            | TypedExpr::Echo { location, .. }
            | TypedExpr::Pipeline { location, .. } => *location,

            TypedExpr::Block { statements, .. } => statements.last().last_location(),
            TypedExpr::Fn { body, .. } => body.last().last_location(),
        }
    }

    pub fn field_map(&self) -> Option<&FieldMap> {
        match self {
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
            | TypedExpr::RecordAccess { .. }
            | TypedExpr::PositionalAccess { .. }
            | TypedExpr::Tuple { .. }
            | TypedExpr::TupleIndex { .. }
            | TypedExpr::Todo { .. }
            | TypedExpr::Panic { .. }
            | TypedExpr::Echo { .. }
            | TypedExpr::BitArray { .. }
            | TypedExpr::RecordUpdate { .. }
            | TypedExpr::NegateBool { .. }
            | TypedExpr::NegateInt { .. }
            | TypedExpr::Invalid { .. } => None,

            TypedExpr::Var { constructor, .. } => constructor.field_map(),
            TypedExpr::ModuleSelect { constructor, .. } => match constructor {
                ModuleValueConstructor::Record { field_map, .. }
                | ModuleValueConstructor::Fn { field_map, .. } => field_map.as_ref(),
                ModuleValueConstructor::Constant { .. } => None,
            },
        }
    }

    pub fn is_invalid(&self) -> bool {
        matches!(self, TypedExpr::Invalid { .. })
    }

    /// Checks that two expressions are written in the same (ignoring
    /// whitespace).
    ///
    /// This is useful for the language server to know when it is possible to
    /// merge two blocks of code together because they are the same.
    /// Simply checking for equality of the AST nodes wouldn't work as those
    /// also contain the source location (meaning that two expression that look
    /// the same but are in different places would be considered different)!
    ///
    pub fn syntactically_eq(&self, other: &TypedExpr) -> bool {
        match (self, other) {
            (TypedExpr::Int { int_value: n, .. }, TypedExpr::Int { int_value: m, .. }) => n == m,
            (TypedExpr::Int { .. }, _) => false,

            (TypedExpr::Float { float_value: n, .. }, TypedExpr::Float { float_value: m, .. }) => {
                n == m
            }
            (TypedExpr::Float { .. }, _) => false,

            (TypedExpr::String { value, .. }, TypedExpr::String { value: other, .. }) => {
                value == other
            }
            (TypedExpr::String { .. }, _) => false,

            (
                TypedExpr::Block { statements, .. },
                TypedExpr::Block {
                    statements: other, ..
                },
            ) => pairwise_all(statements, other, |(one, other)| {
                one.syntactically_eq(other)
            }),

            (TypedExpr::Block { .. }, _) => false,

            (
                TypedExpr::List { elements, tail, .. },
                TypedExpr::List {
                    elements: other_elements,
                    tail: other_tail,
                    ..
                },
            ) => {
                let tails_are_equal = match (tail, other_tail) {
                    (Some(one), Some(other)) => one.syntactically_eq(other),
                    (None, Some(_)) | (Some(_), None) => false,
                    (None, None) => true,
                };

                tails_are_equal
                    && pairwise_all(elements, other_elements, |(one, other)| {
                        one.syntactically_eq(other)
                    })
            }
            (TypedExpr::List { .. }, _) => false,

            (TypedExpr::Var { name, .. }, TypedExpr::Var { name: other, .. }) => name == other,
            (TypedExpr::Var { .. }, _) => false,

            (
                TypedExpr::Fn {
                    arguments, body, ..
                },
                TypedExpr::Fn {
                    arguments: other_arguments,
                    body: other_body,
                    ..
                },
            ) => {
                let arguments_are_equal =
                    pairwise_all(arguments, other_arguments, |(one, other)| {
                        one.get_variable_name() == other.get_variable_name()
                    });

                let bodies_are_equal =
                    pairwise_all(body, other_body, |(one, other)| one.syntactically_eq(other));

                arguments_are_equal && bodies_are_equal
            }
            (TypedExpr::Fn { .. }, _) => false,

            (
                TypedExpr::Pipeline {
                    first_value,
                    assignments,
                    finally,
                    ..
                },
                TypedExpr::Pipeline {
                    first_value: other_first_value,
                    assignments: other_assignments,
                    finally: other_finally,
                    ..
                },
            ) => {
                first_value.value.syntactically_eq(&other_first_value.value)
                    && pairwise_all(assignments, other_assignments, |(one, other)| {
                        one.0.value.syntactically_eq(&other.0.value)
                    })
                    && finally.syntactically_eq(other_finally)
            }
            (TypedExpr::Pipeline { .. }, _) => false,

            (
                TypedExpr::Call { fun, arguments, .. },
                TypedExpr::Call {
                    fun: other_fun,
                    arguments: other_arguments,
                    ..
                },
            ) => {
                fun.syntactically_eq(other_fun)
                    && pairwise_all(arguments, other_arguments, |(one, other)| {
                        one.label == other.label && one.value.syntactically_eq(&other.value)
                    })
            }
            (TypedExpr::Call { .. }, _) => false,

            (
                TypedExpr::BinOp {
                    name, left, right, ..
                },
                TypedExpr::BinOp {
                    name: other_name,
                    left: other_left,
                    right: other_right,
                    ..
                },
            ) => {
                name == other_name
                    && left.syntactically_eq(other_left)
                    && right.syntactically_eq(other_right)
            }
            (TypedExpr::BinOp { .. }, _) => false,

            (
                TypedExpr::Case {
                    subjects, clauses, ..
                },
                TypedExpr::Case {
                    subjects: other_subjects,
                    clauses: other_clauses,
                    ..
                },
            ) => {
                pairwise_all(subjects, other_subjects, |(one, other)| {
                    one.syntactically_eq(other)
                }) && pairwise_all(clauses, other_clauses, |(one, other)| {
                    one.syntactically_eq(other)
                })
            }
            (TypedExpr::Case { .. }, _) => false,

            (
                TypedExpr::RecordAccess { label, record, .. },
                TypedExpr::RecordAccess {
                    label: other_label,
                    record: other_record,
                    ..
                },
            ) => label == other_label && record.syntactically_eq(other_record),
            (TypedExpr::RecordAccess { .. }, _) => false,

            (
                TypedExpr::ModuleSelect {
                    label,
                    module_alias,
                    ..
                },
                TypedExpr::ModuleSelect {
                    label: other_label,
                    module_alias: other_module_alias,
                    ..
                },
            ) => label == other_label && module_alias == other_module_alias,
            (TypedExpr::ModuleSelect { .. }, _) => false,

            (
                TypedExpr::Tuple { elements, .. },
                TypedExpr::Tuple {
                    elements: other_elements,
                    ..
                },
            ) => pairwise_all(elements, other_elements, |(one, other)| {
                one.syntactically_eq(other)
            }),
            (TypedExpr::Tuple { .. }, _) => false,

            (
                TypedExpr::TupleIndex { index, tuple, .. },
                TypedExpr::TupleIndex {
                    index: other_index,
                    tuple: other_tuple,
                    ..
                },
            ) => index == other_index && tuple.syntactically_eq(other_tuple),
            (TypedExpr::TupleIndex { .. }, _) => false,

            (
                TypedExpr::Todo { message, kind, .. },
                TypedExpr::Todo {
                    message: other_message,
                    kind: other_kind,
                    ..
                },
            ) => {
                let messages_are_equal = match (message, other_message) {
                    (Some(one), Some(other)) => one.syntactically_eq(other),
                    (None, None) => true,
                    (None, Some(_)) | (Some(_), None) => false,
                };
                messages_are_equal && kind == other_kind
            }
            (TypedExpr::Todo { .. }, _) => false,

            (
                TypedExpr::Panic { message, .. },
                TypedExpr::Panic {
                    message: message_other,
                    ..
                },
            ) => match (message, message_other) {
                (None, None) => true,
                (None, Some(_)) | (Some(_), None) => false,
                (Some(one), Some(other)) => one.syntactically_eq(other),
            },
            (TypedExpr::Panic { .. }, _) => false,

            (
                TypedExpr::Echo {
                    expression,
                    message,
                    ..
                },
                TypedExpr::Echo {
                    expression: other_expression,
                    message: other_message,
                    ..
                },
            ) => {
                let messages_are_equal = match (message, other_message) {
                    (None, None) => true,
                    (None, Some(_)) | (Some(_), None) => false,
                    (Some(one), Some(other)) => one.syntactically_eq(other),
                };
                let expressions_are_equal = match (expression, other_expression) {
                    (None, None) => true,
                    (None, Some(_)) | (Some(_), None) => false,
                    (Some(one), Some(other)) => one.syntactically_eq(other),
                };
                messages_are_equal && expressions_are_equal
            }
            (TypedExpr::Echo { .. }, _) => false,

            (
                TypedExpr::BitArray { segments, .. },
                TypedExpr::BitArray {
                    segments: other_segments,
                    ..
                },
            ) => pairwise_all(segments, other_segments, |(one, other)| {
                one.syntactically_eq(other)
            }),
            (TypedExpr::BitArray { .. }, _) => false,

            (
                TypedExpr::RecordUpdate {
                    constructor,
                    arguments,
                    ..
                },
                TypedExpr::RecordUpdate {
                    constructor: other_constructor,
                    arguments: other_arguments,
                    ..
                },
            ) => {
                constructor.syntactically_eq(other_constructor)
                    && pairwise_all(arguments, other_arguments, |(one, other)| {
                        one.label == other.label && one.value.syntactically_eq(&other.value)
                    })
            }
            (TypedExpr::RecordUpdate { .. }, _) => false,

            (
                TypedExpr::NegateBool { value, .. },
                TypedExpr::NegateBool {
                    value: other_value, ..
                },
            ) => value.syntactically_eq(other_value),
            (TypedExpr::NegateBool { .. }, _) => false,

            (TypedExpr::NegateInt { value: n, .. }, TypedExpr::NegateInt { value: m, .. }) => {
                n.syntactically_eq(m)
            }
            (TypedExpr::NegateInt { .. }, _) => false,

            (TypedExpr::PositionalAccess { .. }, _) => false,
            (TypedExpr::Invalid { .. }, _) => false,
        }
    }

    pub fn is_todo_with_no_message(&self) -> bool {
        matches!(self, TypedExpr::Todo { message: None, .. })
    }
}

/// Checks that two slices have the same number of item and that the given
/// predicate holds for all pairs of items.
///
pub(crate) fn pairwise_all<A>(one: &[A], other: &[A], function: impl Fn((&A, &A)) -> bool) -> bool {
    one.len() == other.len() && one.iter().zip(other).all(function)
}

fn is_non_zero_number(value: &EcoString) -> bool {
    use regex::Regex;
    static NON_ZERO: OnceLock<Regex> = OnceLock::new();

    NON_ZERO
        .get_or_init(|| Regex::new(r"[1-9]").expect("NON_ZERO regex"))
        .is_match(value)
}

impl<'a> From<&'a TypedExpr> for Located<'a> {
    fn from(expression: &'a TypedExpr) -> Self {
        Located::Expression {
            expression,
            position: ExpressionPosition::Expression,
        }
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

impl bit_array::GetLiteralValue for TypedExpr {
    fn as_int_literal(&self) -> Option<BigInt> {
        if let TypedExpr::Int { int_value, .. } = self {
            Some(int_value.clone())
        } else {
            None
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum InvalidExpression {
    ModuleSelect {
        module_name: EcoString,
        label: EcoString,
    },
    UnknownVariable {
        name: EcoString,
    },
}
