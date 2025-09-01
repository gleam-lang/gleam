use std::sync::OnceLock;

use type_::{FieldMap, TypedCallArg};

use super::*;
use crate::{
    build::ExpressionPosition,
    exhaustiveness::CompiledCase,
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
        let fun = match self {
            TypedExpr::Call { fun, arguments, .. } if arguments.len() == 1 => fun.as_ref(),
            _ => return false,
        };

        match fun {
            TypedExpr::ModuleSelect {
                label, module_name, ..
            } => label == "println" && module_name == "gleam/io",
            _ => false,
        }
    }

    pub fn find_node(&self, byte_index: u32) -> Option<Located<'_>> {
        match self {
            Self::Var { .. }
            | Self::Int { .. }
            | Self::Float { .. }
            | Self::String { .. }
            | Self::Invalid { .. } => self.self_if_contains_location(byte_index),

            Self::ModuleSelect {
                location,
                field_start,
                module_name,
                ..
            } => {
                // We want to return the `ModuleSelect` only when we're hovering
                // over the selected field, not on the module part.
                let field_span = SrcSpan {
                    start: *field_start,
                    end: location.end,
                };

                // We subtract 1 so the location doesn't include the `.` character.
                let module_span = SrcSpan::new(location.start, field_start - 1);

                if field_span.contains(byte_index) {
                    Some(self.into())
                } else if module_span.contains(byte_index) {
                    Some(Located::ModuleName {
                        location: module_span,
                        name: module_name,
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
            | Self::Invalid { .. } => None,

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

    pub fn non_zero_compile_time_number(&self) -> bool {
        match self {
            Self::Int { int_value, .. } => int_value != &BigInt::ZERO,
            Self::Float { value, .. } => is_non_zero_number(value),
            _ => false,
        }
    }

    pub fn zero_compile_time_number(&self) -> bool {
        match self {
            Self::Int { int_value, .. } => int_value == &BigInt::ZERO,
            Self::Float { value, .. } => !is_non_zero_number(value),
            _ => false,
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
                fun.is_record_builder()
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

            _ => false,
        }
    }

    pub fn is_known_bool(&self) -> bool {
        match self {
            TypedExpr::BinOp {
                left, right, name, ..
            } if name.is_bool_operator() => left.is_known_bool() && right.is_known_bool(),
            TypedExpr::NegateBool { value, .. } => value.is_known_bool(),
            _ => self.is_literal(),
        }
    }

    pub fn is_literal_string(&self) -> bool {
        match self {
            Self::String { .. } => true,
            _ => false,
        }
    }

    /// Returns `true` if the typed expr is [`Var`].
    ///
    /// [`Var`]: TypedExpr::Var
    #[must_use]
    pub fn is_var(&self) -> bool {
        match self {
            Self::Var { .. } => true,
            _ => false,
        }
    }

    pub(crate) fn get_documentation(&self) -> Option<&str> {
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
            | TypedExpr::Invalid { .. } => None,
        }
    }

    /// Returns `true` if the typed expr is [`Case`].
    ///
    /// [`Case`]: TypedExpr::Case
    #[must_use]
    pub fn is_case(&self) -> bool {
        match self {
            Self::Case { .. } => true,
            _ => false,
        }
    }

    /// Returns `true` if the typed expr is [`Pipeline`].
    ///
    /// [`Pipeline`]: TypedExpr::Pipeline
    #[must_use]
    pub fn is_pipeline(&self) -> bool {
        match self {
            Self::Pipeline { .. } => true,
            _ => false,
        }
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
                (fun.is_record_builder() || fun.called_function_purity().is_pure())
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
            | TypedExpr::Invalid { .. } => Purity::Unknown,
        }
    }

    #[must_use]
    /// Returns true if the value is a literal record builder like
    /// `Wibble(1, 2)`, `module.Wobble("a")`
    ///
    pub fn is_record_builder(&self) -> bool {
        match self {
            TypedExpr::Call { fun, .. } => fun.is_record_builder(),
            TypedExpr::Var { constructor, .. } => constructor.variant.is_record(),
            TypedExpr::ModuleSelect {
                constructor: ModuleValueConstructor::Record { .. },
                ..
            } => true,
            _ => false,
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
            _ => None,
        }
    }

    #[must_use]
    /// If `self` is a record constructor, returns the nuber of arguments it
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
            _ => None,
        }
    }

    #[must_use]
    pub(crate) fn is_panic(&self) -> bool {
        match self {
            TypedExpr::Panic { .. } => true,
            _ => false,
        }
    }

    pub(crate) fn call_arguments(&self) -> Option<&Vec<TypedCallArg>> {
        match self {
            TypedExpr::Call { arguments, .. } => Some(arguments),
            _ => None,
        }
    }

    pub(crate) fn fn_expression_body(&self) -> Option<&Vec1<TypedStatement>> {
        match self {
            TypedExpr::Fn { body, .. } => Some(body),
            _ => None,
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

    pub(crate) fn is_invalid(&self) -> bool {
        match self {
            TypedExpr::Invalid { .. } => true,
            _ => false,
        }
    }
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
}
