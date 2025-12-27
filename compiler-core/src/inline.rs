//! This module implements the function inlining optimisation. This allows
//! function calls to be inlined at the callsite, and replaced with the contents
//! of the function which is being called.
//!
//! Function inlining is useful for two main reasons:
//! - It removes the overhead of calling other functions and jumping around
//!   execution too much
//! - It removes the barrier of the function call between the code around the
//!   call, and the code inside the called function.
//!
//! For example, the following Gleam code make heavy use of `use` sugar and higher
//! order functions:
//!
//! ```gleam
//! pub fn try_sum(list: List(Result(String, Nil)), sum: Int) -> Result(Int, Nil) {
//!   use <- bool.guard(when: sum >= 1000, return: Ok(sum))
//!   case list {
//!     [] -> Ok(sum)
//!     [first, ..rest] -> {
//!       use number <- result.try(int.parse(first))
//!       try_sum(rest, sum + number)
//!     }
//!   }
//! }
//! ```
//!
//! This can make the code easier to read, but it normally would have a performance
//! cost. There are two called functions, and two implicit anonymous functions.
//! This function is also not tail recursive, as it uses higher order functions
//! inside its body.
//!
//! However, with function inlining, the above code can be optimised to:
//!
//! ```gleam
//! pub fn try_sum(list: List(Result(String, Nil)), sum: Int) -> Result(Int, Nil) {
//!   case sum >= 1000 {
//!     True -> Ok(sum)
//!     False -> case list {
//!       [] -> Ok(sum)
//!       [first, ..rest] -> {
//!         case int.parse(first) {
//!           Ok(number) -> try_sum(rest, sum + number)
//!           Error(error) -> Error(error)
//!         }
//!       }
//!     }
//!   }
//! }
//! ```
//!
//! Which now has no extra function calls, and is tail recursive!
//!
//! The process of function inlining is quite simple really. It is implemented
//! using an AST folder, which traverses each node of the AST, and potentially
//! alters it as it goes.
//!
//! Every time we encounter a function call, we decide whether or not we can
//! inline it. For now, the criteria for inlining is very simple, although a
//! more complex heuristic-based approach will likely be implemented in the
//! future. For now though, a function can be inlined if:
//! It is a standard library function within the hardcoded list - which can be
//! found in the `inline_function` function - or, it is an anonymous function.
//!
//! Inlining anonymous functions allows us to:
//! - Remove calls to parameters of higher-order functions once those higher-
//!   order functions have been inlined. For example, the following example using
//!   `result.map`:
//!   ```gleam
//!   result.map(Ok(10), fn(x) { x + 1 })
//!   ```
//!
//!   Without inlining of anonymous function would be turned into:
//!   ```gleam
//!   case Ok(10) {
//!     Ok(value) -> Ok(fn(x) { x + 1 }(value))
//!     Error(error) -> Error(error)
//!   }
//!   ```
//!
//!   However if we inline anonymous functions also, we remove every call, and
//!   so it becomes:
//!
//!   ```gleam
//!   case Ok(10) {
//!     Ok(value) -> Ok(value + 1)
//!     Error(error) -> Error(error)
//!   }
//!   ```
//!
//! - Remove calls to anonymous functions in pipelines. Sometimes, an anonymous
//!   function is used in a pipeline, which can sometimes be the result of an
//!   expanded function capture. For example:
//!
//!   ```gleam
//!   "10" |> int.parse |> result.unwrap(0) |> fn(x) { x * x } |> something_else
//!   ```
//!
//!   This can now be desugared to:
//!   ```gleam
//!   let _pipe1 = "10"
//!   let _pipe2 = int.parse(_pipe1)
//!   let _pipe3 = result.unwrap(_pipe2, 0)
//!   let _pipe4 = _pipe3 * _pipe3
//!   something_else(_pipe4)
//!   ```
//!
//! See documentation of individual functions to explain better how the process
//! works.
//!

#![allow(dead_code)]

use std::{
    collections::{HashMap, HashSet},
    sync::Arc,
};

use ecow::{EcoString, eco_format};
use itertools::Itertools;
use vec1::Vec1;

use crate::{
    STDLIB_PACKAGE_NAME,
    analyse::Inferred,
    ast::{
        self, ArgNames, Assert, AssignName, Assignment, AssignmentKind, BitArrayOption,
        BitArraySegment, BitArraySize, CallArg, Clause, FunctionLiteralKind, Pattern,
        PipelineAssignmentKind, Publicity, SrcSpan, Statement, TailPattern, TypedArg, TypedAssert,
        TypedAssignment, TypedBitArraySize, TypedClause, TypedDefinitions, TypedExpr,
        TypedExprBitArraySegment, TypedFunction, TypedModule, TypedPattern,
        TypedPipelineAssignment, TypedStatement, TypedUse, visit::Visit,
    },
    exhaustiveness::{Body, CompiledCase, Decision},
    type_::{
        self, Deprecation, ModuleInterface, ModuleValueConstructor, PRELUDE_MODULE_NAME,
        PatternConstructor, Type, TypedCallArg, ValueConstructor, ValueConstructorVariant,
        collapse_links,
        error::VariableOrigin,
        expression::{Implementations, Purity},
    },
};

/// Perform function inlining across an entire module, applying it to each
/// individual function.
pub fn module(
    mut module: TypedModule,
    modules: &im::HashMap<EcoString, ModuleInterface>,
) -> TypedModule {
    let mut inliner = Inliner::new(modules);

    module.definitions = TypedDefinitions {
        functions: module
            .definitions
            .functions
            .into_iter()
            .map(|function| inliner.function(function))
            .collect(),
        ..module.definitions
    };

    module
}

struct Inliner<'a> {
    /// Importable modules, containing information about functions which can be
    /// inlined
    modules: &'a im::HashMap<EcoString, ModuleInterface>,
    /// Any variables which can be inlined. This is used when inlining the body
    /// of function calls. Let's look at an example inlinable function:
    /// ```gleam
    /// pub fn add(a, b) {
    ///   a + b
    /// }
    /// ```
    /// If it is called - `add(1, 2)` - it can be inlined to the following:
    /// ```gleam
    /// {
    ///   let a = 1
    ///   let b = 2
    ///   a + b
    /// }
    /// ```
    ///
    /// However, this can be inlined further. Since `a` and `b` are only used
    /// once each in the body, the whole expression can be reduced to `1 + 2`.
    ///
    /// In the above example, this variable would contain `{a: 1, b: 2}`,
    /// indicating the names of the variables to be inlined, as well as the
    /// values to replace them with.
    inline_variables: HashMap<EcoString, TypedExpr>,

    /// The number we append to variable names in order to ensure uniqueness.
    variable_number: usize,
    /// Set of in-scope variables, used to determine when a conflict between
    /// variable names occurs during inlining.
    in_scope: HashSet<EcoString>,
    /// If two variables conflict in names during inlining, we need to rename
    /// one to avoid the conflict. Any variables renamed this way are stored
    /// here.
    renamed_variables: im::HashMap<EcoString, EcoString>,
    /// The current position, whether we are inside the body of an inlined
    /// function or not.
    position: Position,
}

#[derive(Debug, Clone, Copy, PartialEq)]
enum Position {
    RegularFunction,
    InlinedFunction,
}

impl Inliner<'_> {
    fn new(modules: &im::HashMap<EcoString, ModuleInterface>) -> Inliner<'_> {
        Inliner {
            modules,
            inline_variables: HashMap::new(),
            variable_number: 0,
            renamed_variables: im::HashMap::new(),
            in_scope: HashSet::new(),
            position: Position::RegularFunction,
        }
    }

    /// Defines a variable in the current scope, renaming it if necessary.
    /// Currently, this duplicates work performed in the code generators, where
    /// variables are renamed in a similar way. But since inlining can change
    /// scope boundaries, it needs to be performed here too. Ideally, we would
    /// move all the deduplicating logic from the code generators to here where
    /// we perform inlining, but that is a fairly large item of work.
    fn define_variable(&mut self, name: EcoString) -> EcoString {
        let unique_in_scope = self.in_scope.insert(name.clone());
        // If the variable name is already defined, and we are inlining a function,
        // that means there is a potential conflict in names and we need to rename
        // the variable.
        if !unique_in_scope && self.position == Position::InlinedFunction {
            // Prefixing the variable name with `_inline_` ensures it does
            // not conflict with other defined variables.
            let new_name = eco_format!("_inline_{name}_{}", self.variable_number);
            self.variable_number += 1;
            _ = self.renamed_variables.insert(name, new_name.clone());
            new_name
        } else {
            name
        }
    }

    /// Get the name we are using for a variable, in case it is renamed.
    fn variable_name(&self, name: EcoString) -> EcoString {
        self.renamed_variables.get(&name).cloned().unwrap_or(name)
    }

    fn function(&mut self, mut function: TypedFunction) -> TypedFunction {
        for argument in function.arguments.iter() {
            match &argument.names {
                ArgNames::Discard { .. } | ArgNames::LabelledDiscard { .. } => {}
                ArgNames::Named { name, .. } | ArgNames::NamedLabelled { name, .. } => {
                    _ = self.in_scope.insert(name.clone());
                }
            }
        }

        function.body = function
            .body
            .into_iter()
            .map(|statement| self.statement(statement))
            .collect_vec();
        function
    }

    fn statement(&mut self, statement: TypedStatement) -> TypedStatement {
        match statement {
            Statement::Expression(expression_ast) => {
                Statement::Expression(self.expression(expression_ast))
            }
            Statement::Assignment(assignment_ast) => {
                Statement::Assignment(Box::new(self.assignment(*assignment_ast)))
            }
            Statement::Use(use_ast) => Statement::Use(self.use_(use_ast)),
            Statement::Assert(assert_ast) => Statement::Assert(self.assert(assert_ast)),
        }
    }

    fn assert(&mut self, assert: TypedAssert) -> TypedAssert {
        let Assert {
            location,
            value,
            message,
        } = assert;

        Assert {
            location,
            value: self.expression(value),
            message: message.map(|expression| self.expression(expression)),
        }
    }

    fn use_(&mut self, mut use_: TypedUse) -> TypedUse {
        use_.call = self.boxed_expression(use_.call);
        use_
    }

    fn assignment(&mut self, assignment: TypedAssignment) -> TypedAssignment {
        let Assignment {
            location,
            value,
            pattern,
            kind,
            annotation,
            compiled_case,
        } = assignment;

        Assignment {
            location,
            value: self.expression(value),
            pattern: self.register_pattern_variables(pattern),
            kind: self.assignment_kind(kind),
            annotation,
            compiled_case,
        }
    }

    /// Register variables defined in a pattern so we correctly keep track of
    /// the scope, and rename any which conflict with existing variables.
    fn register_pattern_variables(&mut self, pattern: TypedPattern) -> TypedPattern {
        match pattern {
            Pattern::Int { .. }
            | Pattern::Float { .. }
            | Pattern::String { .. }
            | Pattern::Discard { .. }
            | Pattern::Invalid { .. } => pattern,

            Pattern::Variable {
                location,
                name,
                type_,
                origin,
            } => Pattern::Variable {
                location,
                name: self.define_variable(name),
                type_,
                origin,
            },
            Pattern::BitArraySize(size) => Pattern::BitArraySize(self.bit_array_size(size)),
            Pattern::Assign {
                name,
                location,
                pattern,
            } => Pattern::Assign {
                name: self.define_variable(name),
                location,
                pattern: Box::new(self.register_pattern_variables(*pattern)),
            },
            Pattern::List {
                location,
                elements,
                tail,
                type_,
            } => Pattern::List {
                location,
                elements: elements
                    .into_iter()
                    .map(|element| self.register_pattern_variables(element))
                    .collect(),
                tail: tail.map(|tail| {
                    Box::new(TailPattern {
                        location: tail.location,
                        pattern: self.register_pattern_variables(tail.pattern),
                    })
                }),
                type_,
            },
            Pattern::Constructor {
                location,
                name_location,
                name,
                arguments,
                module,
                constructor,
                spread,
                type_,
            } => Pattern::Constructor {
                location,
                name_location,
                name,
                arguments: arguments
                    .into_iter()
                    .map(
                        |CallArg {
                             label,
                             location,
                             value,
                             implicit,
                         }| CallArg {
                            label,
                            location,
                            value: self.register_pattern_variables(value),
                            implicit,
                        },
                    )
                    .collect(),
                module,
                constructor,
                spread,
                type_,
            },
            Pattern::Tuple { location, elements } => Pattern::Tuple {
                location,
                elements: elements
                    .into_iter()
                    .map(|element| self.register_pattern_variables(element))
                    .collect(),
            },
            Pattern::BitArray { location, segments } => Pattern::BitArray {
                location,
                segments: segments
                    .into_iter()
                    .map(|segment| {
                        self.bit_array_segment(segment, Self::register_pattern_variables)
                    })
                    .collect(),
            },
            Pattern::StringPrefix {
                location,
                left_location,
                left_side_assignment,
                right_location,
                left_side_string,
                right_side_assignment,
            } => Pattern::StringPrefix {
                location,
                left_location,
                left_side_assignment: left_side_assignment
                    .map(|(name, location)| (self.define_variable(name), location)),
                right_location,
                left_side_string,
                right_side_assignment: match right_side_assignment {
                    AssignName::Variable(name) => AssignName::Variable(self.define_variable(name)),
                    AssignName::Discard(name) => AssignName::Discard(name),
                },
            },
        }
    }

    fn bit_array_size(&mut self, size: TypedBitArraySize) -> TypedBitArraySize {
        match size {
            BitArraySize::Int { .. } => size,
            BitArraySize::Variable {
                location,
                name,
                constructor,
                type_,
            } => BitArraySize::Variable {
                location,
                name: self.variable_name(name),
                constructor,
                type_,
            },
            BitArraySize::BinaryOperator {
                location,
                operator,
                left,
                right,
            } => BitArraySize::BinaryOperator {
                location,
                operator,
                left: Box::new(self.bit_array_size(*left)),
                right: Box::new(self.bit_array_size(*right)),
            },
            BitArraySize::Block { location, inner } => BitArraySize::Block {
                location,
                inner: Box::new(self.bit_array_size(*inner)),
            },
        }
    }

    fn assignment_kind(&mut self, kind: AssignmentKind<TypedExpr>) -> AssignmentKind<TypedExpr> {
        match kind {
            AssignmentKind::Let | AssignmentKind::Generated => kind,
            AssignmentKind::Assert {
                location,
                assert_keyword_start,
                message,
            } => AssignmentKind::Assert {
                location,
                assert_keyword_start,
                message: message.map(|expression| self.expression(expression)),
            },
        }
    }

    fn boxed_expression(&mut self, boxed: Box<TypedExpr>) -> Box<TypedExpr> {
        Box::new(self.expression(*boxed))
    }

    fn expressions(&mut self, expressions: Vec<TypedExpr>) -> Vec<TypedExpr> {
        expressions
            .into_iter()
            .map(|expression| self.expression(expression))
            .collect()
    }

    /// Perform inlining over an expression. This function is recursive, as
    /// expressions can be deeply nested. Most expressions just recursively
    /// call this function on each of their component parts, but some have
    /// special handling.
    fn expression(&mut self, mut expression: TypedExpr) -> TypedExpr {
        match expression {
            TypedExpr::Int { .. }
            | TypedExpr::Float { .. }
            | TypedExpr::String { .. }
            | TypedExpr::Fn { .. }
            | TypedExpr::ModuleSelect { .. }
            | TypedExpr::Invalid { .. } => expression,

            TypedExpr::Var {
                ref constructor,
                ref mut name,
                ..
            } => match &constructor.variant {
                // If this variable can be inlined, replace it with its value.
                // See the `inline_variables` documentation for an explanation.
                ValueConstructorVariant::LocalVariable { .. } => {
                    // We remove the variable as inlined variables can only be
                    // inlined once. `inline_variables` only contains variables
                    // which we have already checked are possible to inline, as
                    // we check for variables which are only used once when converting
                    // to an `InlinableFunction`.
                    match self.inline_variables.remove(name) {
                        Some(inlined_expression) => inlined_expression,
                        None => match self.renamed_variables.get(name) {
                            Some(new_name) => {
                                *name = new_name.clone();
                                expression
                            }
                            None => expression,
                        },
                    }
                }
                ValueConstructorVariant::ModuleConstant { .. }
                | ValueConstructorVariant::ModuleFn { .. }
                | ValueConstructorVariant::Record { .. } => expression,
            },

            TypedExpr::Block {
                location,
                statements,
            } => TypedExpr::Block {
                location,
                statements: statements.mapped(|statement| self.statement(statement)),
            },

            TypedExpr::NegateBool { location, value } => TypedExpr::NegateBool {
                location,
                value: self.boxed_expression(value),
            },

            TypedExpr::NegateInt { location, value } => TypedExpr::NegateInt {
                location,
                value: self.boxed_expression(value),
            },

            TypedExpr::Pipeline {
                location,
                first_value,
                assignments,
                finally,
                finally_kind,
            } => self.pipeline(location, first_value, assignments, finally, finally_kind),

            TypedExpr::List {
                location,
                type_,
                elements,
                tail,
            } => TypedExpr::List {
                location,
                type_,
                elements: self.expressions(elements),
                tail: tail.map(|boxed_expression| self.boxed_expression(boxed_expression)),
            },

            TypedExpr::Call {
                location,
                type_,
                fun,
                arguments,
                arguments_start,
            } => self.call(location, type_, fun, arguments, arguments_start),

            TypedExpr::BinOp {
                location,
                type_,
                name,
                name_location,
                left,
                right,
            } => TypedExpr::BinOp {
                location,
                type_,
                name,
                name_location,
                left: self.boxed_expression(left),
                right: self.boxed_expression(right),
            },

            TypedExpr::Case {
                location,
                type_,
                subjects,
                clauses,
                compiled_case,
            } => self.case(location, type_, subjects, clauses, compiled_case),

            TypedExpr::RecordAccess {
                location,
                field_start,
                type_,
                label,
                index,
                record,
                documentation,
            } => TypedExpr::RecordAccess {
                location,
                field_start,
                type_,
                label,
                index,
                record: self.boxed_expression(record),
                documentation,
            },

            TypedExpr::PositionalAccess {
                location,
                type_,
                index,
                record,
            } => TypedExpr::PositionalAccess {
                location,
                type_,
                index,
                record: self.boxed_expression(record),
            },

            TypedExpr::Tuple {
                location,
                type_,
                elements,
            } => TypedExpr::Tuple {
                location,
                type_,
                elements: self.expressions(elements),
            },

            TypedExpr::TupleIndex {
                location,
                type_,
                index,
                tuple,
            } => TypedExpr::TupleIndex {
                location,
                type_,
                index,
                tuple: self.boxed_expression(tuple),
            },

            TypedExpr::Todo {
                location,
                message,
                kind,
                type_,
            } => TypedExpr::Todo {
                location,
                message: message.map(|boxed_expression| self.boxed_expression(boxed_expression)),
                kind,
                type_,
            },

            TypedExpr::Panic {
                location,
                message,
                type_,
            } => TypedExpr::Panic {
                location,
                message: message.map(|boxed_expression| self.boxed_expression(boxed_expression)),
                type_,
            },

            TypedExpr::Echo {
                location,
                type_,
                expression,
                message,
            } => TypedExpr::Echo {
                location,
                expression: expression.map(|expression| self.boxed_expression(expression)),
                message: message.map(|message| self.boxed_expression(message)),
                type_,
            },

            TypedExpr::BitArray {
                location,
                type_,
                segments,
            } => self.bit_array(location, type_, segments),

            TypedExpr::RecordUpdate {
                location,
                type_,
                record_assignment,
                constructor,
                arguments,
            } => TypedExpr::RecordUpdate {
                location,
                type_,
                record_assignment: record_assignment
                    .map(|assignment| Box::new(self.assignment(*assignment))),
                constructor: self.boxed_expression(constructor),
                arguments: self.arguments(arguments),
            },
        }
    }

    fn arguments(&mut self, arguments: Vec<TypedCallArg>) -> Vec<TypedCallArg> {
        arguments
            .into_iter()
            .map(
                |TypedCallArg {
                     label,
                     location,
                     value,
                     implicit,
                 }| TypedCallArg {
                    label,
                    location,
                    value: self.expression(value),
                    implicit,
                },
            )
            .collect()
    }

    /// Where the magic happens. First, we check the left-hand side of the call
    /// to see if it's something we can inline. If not, we continue to walk the
    /// tree like all the other expressions do. If it can be inlined, we follow
    /// a three-step process:
    ///
    /// - Inlining: Here, we replace the reference to the function with an
    ///   anonymous function with the same contents. If the left-hand side is
    ///   already an anonymous function, we skip this step.
    ///
    /// - Beta reduction: The call to the anonymous function it transformed into
    ///   a block with assignments for each argument at the beginning
    ///
    /// - Optimisation: We then recursively optimise the block. This allows us
    ///   to, for example, inline anonymous functions passed to higher-order
    ///   functions.
    ///
    /// Here is an example of inlining `result.map`:
    ///
    /// Initial code:
    /// ```gleam
    /// let x = Ok(10)
    /// result.map(x, fn(x) {
    ///   let y = x + 4
    ///   int.to_string(y)
    /// })
    /// ```
    ///
    /// After inlining:
    /// ```gleam
    /// let x = Ok(10)
    /// fn(result, function) {
    ///   case result {
    ///     Ok(value) -> Ok(function(value))
    ///     Error(error) -> Error(error)
    ///   }
    /// }(x, fn(x) {
    ///   let y = x + 4
    ///   int.to_string(y)
    /// })
    /// ```
    ///
    /// After beta reduction:
    /// ```gleam
    /// let x = Ok(10)
    /// {
    ///   let result = x
    ///   let function = fn(x) {
    ///     let y = x + 4
    ///     int.to_string(y)
    ///   }
    ///   case result {
    ///     Ok(value) -> Ok(function(value))
    ///     Error(error) -> Error(error)
    ///   }
    /// }
    /// ```
    ///
    /// And finally, after the final optimising pass, where this inlining process
    /// is repeated:
    /// ```gleam
    /// let x = Ok(10)
    /// case x {
    ///   Ok(value) -> Ok({
    ///     let y = x + 4
    ///     int.to_string(y)
    ///   })
    ///   Error(error) -> Error(error)
    /// }
    /// ```
    ///
    fn call(
        &mut self,
        location: SrcSpan,
        type_: Arc<Type>,
        function: Box<TypedExpr>,
        arguments: Vec<TypedCallArg>,
        arguments_start: Option<u32>,
    ) -> TypedExpr {
        let arguments = self.arguments(arguments);

        // First, we traverse the left-hand side of this call. If this is called
        // inside another inlined function, this could potentially inline an
        // argument, allowing further inlining.
        let function = self.expression(*function);

        // If the left-hand side is in a block for some reason, for example
        // `{ fn(x) { x + 1 } }(10)`, we still want to be able to inline it.
        let function = expand_block(function);

        let function = match function {
            TypedExpr::Var {
                ref constructor,
                ref name,
                ..
            } => match &constructor.variant {
                ValueConstructorVariant::ModuleFn { module, .. } => {
                    // If the function is in the list of inlinable functions in
                    // the module it belongs to, we can inline it!
                    if let Some(function) = self
                        .modules
                        .get(module)
                        .and_then(|module| module.inline_functions.get(name))
                    {
                        // First, we do the actual inlining, by converting it to
                        // an anonymous function.
                        let (parameters, body) = function.to_anonymous_function();
                        // Then, we perform beta reduction, inlining the call to
                        // the anonymous function.
                        return self.inline_anonymous_function_call(
                            &parameters,
                            arguments,
                            body,
                            &function.inlinable_parameters,
                        );
                    } else {
                        function
                    }
                }
                // We cannot inline local variables or constants, as we do not
                // have enough information to inline them. Records are not actually
                // function calls, so they also cannot be inlined.
                ValueConstructorVariant::LocalVariable { .. }
                | ValueConstructorVariant::ModuleConstant { .. }
                | ValueConstructorVariant::Record { .. } => function,
            },
            TypedExpr::ModuleSelect {
                ref constructor,
                label: ref name,
                ref module_name,
                ..
            } => match constructor {
                // We use the same logic here as for `TypedExpr::Var` above.
                ModuleValueConstructor::Fn { .. } => {
                    if let Some(function) = self
                        .modules
                        .get(module_name)
                        .and_then(|module| module.inline_functions.get(name))
                    {
                        let (parameters, body) = function.to_anonymous_function();
                        return self.inline_anonymous_function_call(
                            &parameters,
                            arguments,
                            body,
                            &function.inlinable_parameters,
                        );
                    } else {
                        function
                    }
                }
                ModuleValueConstructor::Record { .. } | ModuleValueConstructor::Constant { .. } => {
                    function
                }
            },
            // Direct calls to anonymous functions can always be inlined
            TypedExpr::Fn {
                arguments: parameters,
                body,
                ..
            } => {
                let inlinable_parameters = find_inlinable_parameters(&parameters, &body);
                return self.inline_anonymous_function_call(
                    &parameters,
                    arguments,
                    body,
                    &inlinable_parameters,
                );
            }
            TypedExpr::Int { .. }
            | TypedExpr::Float { .. }
            | TypedExpr::String { .. }
            | TypedExpr::Block { .. }
            | TypedExpr::Pipeline { .. }
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
            | TypedExpr::Invalid { .. } => function,
        };

        TypedExpr::Call {
            location,
            type_,
            fun: Box::new(function),
            arguments,
            arguments_start,
        }
    }

    /// Turn a call to an anonymous function into a block with assignments.
    fn inline_anonymous_function_call(
        &mut self,
        parameters: &[TypedArg],
        arguments: Vec<TypedCallArg>,
        body: Vec1<TypedStatement>,
        inlinable_parameters: &[EcoString],
    ) -> TypedExpr {
        // Arguments to this call that can be inlined, and do not need an assignment.
        let mut inline = HashMap::new();

        // We start by collecting all the assignments for parameters which cannot
        // be inlined.
        let mut statements = parameters
            .iter()
            .zip(arguments)
            .filter_map(|(parameter, argument)| {
                let name = parameter.get_variable_name().cloned().unwrap_or("_".into());

                // An argument can be inlined if it is only used once (stored in
                // the `InlineFunction` structure), and it is pure. Sometime impure
                // arguments can be inlined, but for simplicity we avoid inlining
                // all impure arguments for now. This heuristic can be improved
                // later.
                if inlinable_parameters.contains(&name)
                    && argument.value.is_pure_value_constructor()
                {
                    _ = inline.insert(name, argument.value);
                    return None;
                }

                let type_ = argument.value.type_();

                // Register the variable in scope, so that it is renamed if
                // necessary.
                let name = self.define_variable(name);

                // Otherwise, we make an assignment which assigns the value of
                // the argument to the correct parameter name.
                Some(Statement::Assignment(Box::new(Assignment {
                    location: BLANK_LOCATION,
                    value: argument.value,
                    pattern: TypedPattern::Variable {
                        location: BLANK_LOCATION,
                        name: name.clone(),
                        type_: type_.clone(),
                        origin: VariableOrigin::generated(),
                    },
                    kind: AssignmentKind::Generated,
                    compiled_case: CompiledCase::simple_variable_assignment(name, type_),
                    annotation: None,
                })))
            })
            .collect_vec();

        // If we are performing inlining within an already inlined function, there
        // might be inlinable variables in the outer scope. However, these cannot be
        // inlined inside a nested function, so they are saved and restored afterwards.
        let inline_variables = std::mem::replace(&mut self.inline_variables, inline);
        let position = self.position;
        self.position = Position::InlinedFunction;
        let variables = self.renamed_variables.clone();

        // Perform inlining on each of the statements in this function's body,
        // potentially inlining parameters and function calls inside this function.
        statements.extend(body.into_iter().map(|statement| self.statement(statement)));

        // Restore scope
        self.inline_variables = inline_variables;
        self.position = position;
        self.renamed_variables = variables;

        // We try to expand this block, so a function which is inlined as a
        // single expression does not get wrapped unnecessarily
        expand_block(TypedExpr::Block {
            location: BLANK_LOCATION,
            statements: statements
                .try_into()
                .expect("Type checking ensures there is at least one statement"),
        })
    }

    fn pipeline(
        &mut self,
        location: SrcSpan,
        first_value: TypedPipelineAssignment,
        assignments: Vec<(TypedPipelineAssignment, PipelineAssignmentKind)>,
        finally: Box<TypedExpr>,
        finally_kind: PipelineAssignmentKind,
    ) -> TypedExpr {
        let first_value = self.pipeline_assignment(first_value);
        let assignments = assignments
            .into_iter()
            .map(|(assignment, kind)| (self.pipeline_assignment(assignment), kind))
            .collect();
        let finally = self.boxed_expression(finally);

        TypedExpr::Pipeline {
            location,
            first_value,
            assignments,
            finally,
            finally_kind,
        }
    }

    fn pipeline_assignment(
        &mut self,
        assignment: TypedPipelineAssignment,
    ) -> TypedPipelineAssignment {
        let TypedPipelineAssignment {
            location,
            name,
            value,
        } = assignment;

        TypedPipelineAssignment {
            location,
            name,
            value: self.boxed_expression(value),
        }
    }

    fn bit_array(
        &mut self,
        location: SrcSpan,
        type_: Arc<Type>,
        segments: Vec<TypedExprBitArraySegment>,
    ) -> TypedExpr {
        let segments = segments
            .into_iter()
            .map(|segment| self.bit_array_segment(segment, Self::expression))
            .collect();

        TypedExpr::BitArray {
            location,
            type_,
            segments,
        }
    }

    fn bit_array_segment<Value>(
        &mut self,
        segment: BitArraySegment<Value, Arc<Type>>,
        function: fn(&mut Self, Value) -> Value,
    ) -> BitArraySegment<Value, Arc<Type>> {
        let BitArraySegment {
            location,
            value,
            options,
            type_,
        } = segment;

        BitArraySegment {
            location,
            value: Box::new(function(self, *value)),
            options: options
                .into_iter()
                .map(|option| self.bit_array_option(option, function))
                .collect(),
            type_,
        }
    }

    fn bit_array_option<Value>(
        &mut self,
        option: BitArrayOption<Value>,
        function: fn(&mut Self, Value) -> Value,
    ) -> BitArrayOption<Value> {
        match option {
            BitArrayOption::Bytes { .. }
            | BitArrayOption::Int { .. }
            | BitArrayOption::Float { .. }
            | BitArrayOption::Bits { .. }
            | BitArrayOption::Utf8 { .. }
            | BitArrayOption::Utf16 { .. }
            | BitArrayOption::Utf32 { .. }
            | BitArrayOption::Utf8Codepoint { .. }
            | BitArrayOption::Utf16Codepoint { .. }
            | BitArrayOption::Utf32Codepoint { .. }
            | BitArrayOption::Signed { .. }
            | BitArrayOption::Unsigned { .. }
            | BitArrayOption::Big { .. }
            | BitArrayOption::Little { .. }
            | BitArrayOption::Native { .. }
            | BitArrayOption::Unit { .. } => option,
            BitArrayOption::Size {
                location,
                value,
                short_form,
            } => BitArrayOption::Size {
                location,
                value: Box::new(function(self, *value)),
                short_form,
            },
        }
    }

    fn case(
        &mut self,
        location: SrcSpan,
        type_: Arc<Type>,
        subjects: Vec<TypedExpr>,
        clauses: Vec<TypedClause>,
        compiled_case: CompiledCase,
    ) -> TypedExpr {
        let subjects = self.expressions(subjects);
        let clauses = clauses
            .into_iter()
            .map(|clause| self.case_clause(clause))
            .collect();

        // Since JavaScript code generation uses the decision tree to generate
        // code for `case` expressions, we need to rename the variables bound
        // in the decision tree too. Because we have already renamed the variables
        // in the pattern, we can simply look up the rebound names.
        let compiled_case = CompiledCase {
            tree: self.decision(compiled_case.tree),
            subject_variables: compiled_case.subject_variables,
        };

        TypedExpr::Case {
            location,
            type_,
            subjects,
            clauses,
            compiled_case,
        }
    }

    fn case_clause(&mut self, clause: TypedClause) -> TypedClause {
        let Clause {
            location,
            pattern,
            alternative_patterns,
            guard,
            then,
        } = clause;

        let pattern = pattern
            .into_iter()
            .map(|pattern| self.register_pattern_variables(pattern))
            .collect();

        let alternative_patterns = alternative_patterns
            .into_iter()
            .map(|patterns| {
                patterns
                    .into_iter()
                    .map(|pattern| self.register_pattern_variables(pattern))
                    .collect()
            })
            .collect();

        let then = self.expression(then);

        Clause {
            location,
            pattern,
            alternative_patterns,
            guard,
            then,
        }
    }

    fn decision(&self, decision: Decision) -> Decision {
        match decision {
            Decision::Run { body } => Decision::Run {
                body: self.case_body(body),
            },
            Decision::Guard {
                guard,
                if_true,
                if_false,
            } => Decision::Guard {
                guard,
                if_true: self.case_body(if_true),
                if_false: Box::new(self.decision(*if_false)),
            },
            Decision::Switch {
                var,
                choices,
                fallback,
                fallback_check,
            } => Decision::Switch {
                var,
                choices: choices
                    .into_iter()
                    .map(|(check, decision)| (check, self.decision(decision)))
                    .collect(),
                fallback: Box::new(self.decision(*fallback)),
                fallback_check,
            },
            Decision::Fail => Decision::Fail,
        }
    }

    fn case_body(&self, body: Body) -> Body {
        let Body {
            bindings,
            clause_index,
        } = body;

        let bindings = bindings
            .into_iter()
            // We do this after renaming the variables in the pattern, so we can
            // just lookup the new name, rather than renaming again.
            .map(|(name, value)| (self.variable_name(name), value))
            .collect();

        Body {
            bindings,
            clause_index,
        }
    }
}

fn find_inlinable_parameters(parameters: &[TypedArg], body: &[TypedStatement]) -> Vec<EcoString> {
    let mut parameter_map = HashMap::new();
    for parameter in parameters {
        let (name, location) = match &parameter.names {
            ArgNames::Discard { .. } | ArgNames::LabelledDiscard { .. } => continue,
            ArgNames::Named { name, location }
            | ArgNames::NamedLabelled {
                name,
                name_location: location,
                ..
            } => (name, location),
        };
        _ = parameter_map.insert((name.clone(), *location), false);
    }

    let mut finder = FindInlinableParameters {
        parameters: parameter_map,
        position: FunctionPosition::Body,
    };
    for statement in body {
        finder.visit_typed_statement(statement);
    }

    // Inlinable parameters are those that are used exactly once. Any parameters
    // used more than once will be removed from this map and so will not be
    // considered inlinable.
    finder
        .parameters
        .into_iter()
        .filter_map(|((name, _), used)| used.then_some(name))
        .collect()
}

/// A struct for finding the inlinable parameters of an anonymous function. Since
/// we want to inline all anonymous functions, not just a subset of them like we
/// do with regular functions, this must be implemented slightly differently, but
/// it means we can take advantage of the AST visitor, since we don't need to
/// transform the anonymous function into an intermediate representation.
struct FindInlinableParameters {
    parameters: HashMap<(EcoString, SrcSpan), bool>,
    position: FunctionPosition,
}

#[derive(Debug, Clone, Copy)]
enum FunctionPosition {
    Body,
    NestedFunction,
}

impl FindInlinableParameters {
    fn register_reference(&mut self, name: &EcoString, location: SrcSpan) {
        let key = (name.clone(), location);

        match self.position {
            FunctionPosition::Body => {}
            // We don't inline any parameters which are referenced in nested
            // anonymous function, as our system for inlining parameters cannot
            // properly handle that case; it requires more complex rewriting of
            // the code in some cases.
            FunctionPosition::NestedFunction => {
                _ = self.parameters.remove(&key);
                return;
            }
        }

        match self.parameters.get_mut(&key) {
            Some(true) => _ = self.parameters.remove(&key),
            Some(used @ false) => {
                *used = true;
            }
            None => {}
        }
    }
}

impl<'ast> Visit<'ast> for FindInlinableParameters {
    fn visit_typed_expr_var(
        &mut self,
        _location: &'ast SrcSpan,
        constructor: &'ast ValueConstructor,
        name: &'ast EcoString,
    ) {
        if let ValueConstructorVariant::LocalVariable { location, .. } = constructor.variant {
            self.register_reference(name, location);
        }
    }

    fn visit_typed_clause_guard_var(
        &mut self,
        _location: &'ast SrcSpan,
        name: &'ast EcoString,
        _type_: &'ast Arc<Type>,
        location: &'ast SrcSpan,
    ) {
        self.register_reference(name, *location);
    }

    fn visit_typed_bit_array_size_variable(
        &mut self,
        _location: &'ast SrcSpan,
        name: &'ast EcoString,
        constructor: &'ast Option<Box<ValueConstructor>>,
        _type_: &'ast Arc<Type>,
    ) {
        let variant = match constructor {
            Some(constructor) => &constructor.variant,
            None => return,
        };
        if let ValueConstructorVariant::LocalVariable { location, .. } = variant {
            self.register_reference(name, *location);
        }
    }

    fn visit_typed_expr_fn(
        &mut self,
        location: &'ast SrcSpan,
        type_: &'ast Arc<Type>,
        kind: &'ast FunctionLiteralKind,
        args: &'ast [TypedArg],
        body: &'ast Vec1<TypedStatement>,
        return_annotation: &'ast Option<ast::TypeAst>,
    ) {
        let previous_position = self.position;
        self.position = FunctionPosition::NestedFunction;

        ast::visit::visit_typed_expr_fn(self, location, type_, kind, args, body, return_annotation);

        self.position = previous_position;
    }
}

/// Removes any blocks which are acting as brackets (they hold a single expression)
fn expand_block(expression: TypedExpr) -> TypedExpr {
    match expression {
        TypedExpr::Block {
            location,
            statements,
        } if statements.len() == 1 => {
            let (first, _rest) = statements.split_off_first();

            match first {
                // If this is several blocks inside each other, we want to
                // expand them all.
                Statement::Expression(inner) => expand_block(inner),
                Statement::Assignment(_) | Statement::Use(_) | Statement::Assert(_) => {
                    TypedExpr::Block {
                        location,
                        statements: Vec1::new(first),
                    }
                }
            }
        }
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
        | TypedExpr::Invalid { .. } => expression,
    }
}

/// Converts a function from the Gleam AST into a special "inlinable function",
/// which is a simplified version, containing just enough information for us to
/// perform inlining, while keeping the cache files to a minimum size.
///
/// This function also determines whether a function is inlinable. Currently this
/// just checks it against a list of stdlib functions we want to prioritise
/// inlining, but later it will be changed to a more complicated heuristic.
///
pub fn function_to_inlinable(
    package: &str,
    module: &str,
    function: &TypedFunction,
) -> Option<InlinableFunction> {
    let (_, name) = function.name.as_ref()?;

    if !is_inlinable(package, module, name) {
        return None;
    }

    let parameters = function
        .arguments
        .iter()
        .map(|argument| match &argument.names {
            ArgNames::Discard { name, .. } | ArgNames::Named { name, .. } => InlinableParameter {
                label: None,
                name: name.clone(),
            },
            ArgNames::LabelledDiscard { label, name, .. }
            | ArgNames::NamedLabelled { label, name, .. } => InlinableParameter {
                label: Some(label.clone()),
                name: name.clone(),
            },
        })
        .collect();

    let mut converter = FunctionToInlinable::new(&function.arguments);

    let body = function
        .body
        .iter()
        .map(|statement| converter.statement(statement))
        .collect::<Option<_>>()?;

    // Figure out which parameters can be inlined within the body of this function.
    // When we inline a function, we convert it to a block with assignments for
    // the parameters. Then, if those parameters contain no side effects and are
    // only referenced once within the body, they can be inlined into the place
    // they are referenced.
    //
    // This code checks for the parameters which are used exactly once, which
    // can then be inlined. We can't inline parameters which are never used, as
    // there is nowhere to inline them to, so it doesn't make sense. We still
    // need to evaluate them though, as there could be side effects caused by
    // the values passed to them.
    let inlinable_parameters = converter
        .parameter_references
        .into_iter()
        .filter_map(|((name, _), used)| used.then_some(name))
        .collect();

    Some(InlinableFunction {
        parameters,
        body,
        inlinable_parameters,
    })
}

/// The heuristic to determine whether a function is inlinable. For now, this
/// just checks against a list of standard library functions.
fn is_inlinable(package: &str, module: &str, name: &str) -> bool {
    // For now we only offer inlining of standard library functions
    if package != STDLIB_PACKAGE_NAME {
        return false;
    }

    match (module, name) {
        // These are the functions which we currently inline
        ("gleam/bool", "guard") => true,
        ("gleam/bool", "lazy_guard") => true,
        ("gleam/result", "try") => true,
        ("gleam/result", "map") => true,
        ("gleam/result", "map_error") => true,
        // For testing purposes it's useful to have a function which will always
        // be inlined, which we can define however we want. We only inline this
        // when we are in test mode though, because we wouldn't want this detail
        // leaking out into real-world code and causing unexpected behaviour.
        ("testing", "always_inline") => cfg!(test),
        _ => false,
    }
}

/// Holds state for converting a `TypedFunction` into an `InlinableFunction`.
struct FunctionToInlinable {
    /// A map of parameters to a boolean of whether they have been used. Since
    /// Gleam has variable shadowing, we must also store the definition location
    /// of each parameter to ensure that it is not a variable shadowing the parameter
    /// name.
    /// If a parameter is used more than once, it is removed from the map, so it
    /// is no longer tracked as an inlinable parameter.
    parameter_references: HashMap<(EcoString, SrcSpan), bool>,
}

impl FunctionToInlinable {
    fn new(arguments: &[TypedArg]) -> Self {
        let parameter_references = arguments
            .iter()
            .filter_map(|argument| {
                let (name, location) = match &argument.names {
                    ArgNames::Discard { .. } | ArgNames::LabelledDiscard { .. } => return None,
                    ArgNames::Named { name, location } => (name.clone(), *location),
                    ArgNames::NamedLabelled {
                        name,
                        name_location,
                        ..
                    } => (name.clone(), *name_location),
                };

                Some(((name, location), false))
            })
            .collect();

        Self {
            parameter_references,
        }
    }

    fn statement(&mut self, statement: &TypedStatement) -> Option<InlinableExpression> {
        match statement {
            Statement::Expression(expression) => self.expression(expression),
            Statement::Assignment(_) | Statement::Use(_) | Statement::Assert(_) => None,
        }
    }

    /// Converts an expression to an `InlinableExpression`. We only convert a
    /// small subset of the AST for now, enough to compile our desired inlinable
    /// stdlib functions. Anything else returns `None`, indicating a function
    /// cannot be inlined.
    fn expression(&mut self, expression: &TypedExpr) -> Option<InlinableExpression> {
        match expression {
            TypedExpr::Case {
                subjects,
                clauses,
                compiled_case,
                type_,
                ..
            } => {
                let subjects = subjects
                    .iter()
                    .map(|expression| self.expression(expression))
                    .collect::<Option<_>>()?;
                let clauses = clauses
                    .iter()
                    .map(|clause| self.clause(clause))
                    .collect::<Option<_>>()?;

                Some(InlinableExpression::Case {
                    subjects,
                    clauses,
                    compiled_case: Box::new(compiled_case.clone()),
                    type_: self.type_(type_),
                })
            }
            TypedExpr::Var {
                constructor, name, ..
            } => {
                match &constructor.variant {
                    ValueConstructorVariant::LocalVariable { location, .. } => {
                        let key = (name.clone(), *location);
                        match self.parameter_references.get_mut(&key) {
                            Some(true) => {
                                _ = self.parameter_references.remove(&key);
                            }
                            Some(usage) => *usage = true,
                            None => {}
                        }
                    }
                    ValueConstructorVariant::ModuleConstant { .. }
                    | ValueConstructorVariant::ModuleFn { .. }
                    | ValueConstructorVariant::Record { .. } => {}
                }

                Some(InlinableExpression::Variable {
                    name: name.clone(),
                    constructor: self.value_constructor(constructor)?,
                    type_: self.type_(&constructor.type_),
                })
            }
            TypedExpr::Call {
                fun,
                arguments,
                type_,
                ..
            } => {
                let function = self.expression(fun)?;
                let arguments = arguments
                    .iter()
                    .map(|argument| {
                        Some(InlinableArgument {
                            label: argument.label.clone(),
                            value: self.expression(&argument.value)?,
                        })
                    })
                    .collect::<Option<_>>()?;

                Some(InlinableExpression::Call {
                    function: Box::new(function),
                    arguments,
                    type_: self.type_(type_),
                })
            }

            TypedExpr::Int { .. }
            | TypedExpr::Float { .. }
            | TypedExpr::String { .. }
            | TypedExpr::Block { .. }
            | TypedExpr::Pipeline { .. }
            | TypedExpr::Fn { .. }
            | TypedExpr::List { .. }
            | TypedExpr::BinOp { .. }
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

    fn type_(&self, type_: &Arc<Type>) -> InlinableType {
        match collapse_links(type_.clone()).as_ref() {
            Type::Fn { arguments, return_ } => InlinableType::Function {
                arguments: arguments
                    .iter()
                    .map(|argument| self.type_(argument))
                    .collect(),
                return_: Box::new(self.type_(return_)),
            },
            Type::Named {
                module,
                name,
                arguments,
                ..
            } if module == PRELUDE_MODULE_NAME => self.prelude_type(name, arguments),
            Type::Named { .. } | Type::Var { .. } | Type::Tuple { .. } => InlinableType::Other,
        }
    }

    fn prelude_type(&self, name: &str, arguments: &[Arc<Type>]) -> InlinableType {
        match (name, arguments) {
            ("BitArray", _) => InlinableType::BitArray,
            ("Bool", _) => InlinableType::Bool,
            ("Float", _) => InlinableType::Float,
            ("Int", _) => InlinableType::Int,
            ("List", [element]) => InlinableType::List(Box::new(self.type_(element))),
            ("Nil", _) => InlinableType::Nil,
            ("Result", [ok, error]) => InlinableType::Result {
                ok: Box::new(self.type_(ok)),
                error: Box::new(self.type_(error)),
            },
            ("String", _) => InlinableType::String,
            ("UtfCodepoint", _) => InlinableType::UtfCodepoint,
            _ => InlinableType::Other,
        }
    }

    fn value_constructor(
        &mut self,
        constructor: &ValueConstructor,
    ) -> Option<InlinableValueConstructor> {
        match &constructor.variant {
            ValueConstructorVariant::LocalVariable { .. } => {
                Some(InlinableValueConstructor::LocalVariable)
            }
            ValueConstructorVariant::ModuleConstant { .. } => None,
            ValueConstructorVariant::ModuleFn { name, module, .. } => {
                Some(InlinableValueConstructor::Function {
                    name: name.clone(),
                    module: module.clone(),
                })
            }
            ValueConstructorVariant::Record { name, module, .. } => {
                Some(InlinableValueConstructor::Record {
                    name: name.clone(),
                    module: module.clone(),
                })
            }
        }
    }

    fn clause(&mut self, clause: &TypedClause) -> Option<InlinableClause> {
        let pattern = clause
            .pattern
            .iter()
            .map(Self::pattern)
            .collect::<Option<_>>()?;
        let body = self.expression(&clause.then)?;
        Some(InlinableClause { pattern, body })
    }

    fn pattern(pattern: &TypedPattern) -> Option<InlinablePattern> {
        match pattern {
            TypedPattern::Variable { name, .. } => {
                Some(InlinablePattern::Variable { name: name.clone() })
            }
            TypedPattern::Constructor {
                name,
                arguments,
                constructor: Inferred::Known(inferred),
                ..
            } => {
                let arguments = arguments
                    .iter()
                    .map(|argument| {
                        Some(InlinableArgument {
                            label: argument.label.clone(),
                            value: Self::pattern(&argument.value)?,
                        })
                    })
                    .collect::<Option<_>>()?;

                Some(InlinablePattern::Constructor {
                    name: name.clone(),
                    module: inferred.module.clone(),
                    arguments,
                })
            }

            TypedPattern::Constructor {
                constructor: Inferred::Unknown,
                ..
            } => None,

            TypedPattern::Int { .. }
            | TypedPattern::Float { .. }
            | TypedPattern::String { .. }
            | TypedPattern::BitArraySize { .. }
            | TypedPattern::Assign { .. }
            | TypedPattern::Discard { .. }
            | TypedPattern::List { .. }
            | TypedPattern::Tuple { .. }
            | TypedPattern::BitArray { .. }
            | TypedPattern::StringPrefix { .. }
            | TypedPattern::Invalid { .. } => None,
        }
    }
}

/// A simplified version of a `TypedFunction`.
#[derive(Debug, Clone, PartialEq, Eq, serde::Serialize, serde::Deserialize)]
pub struct InlinableFunction {
    pub parameters: Vec<InlinableParameter>,
    pub body: Vec<InlinableExpression>,
    /// A list of parameters which are only referenced once and can therefore
    /// be inlined within the body of this function.
    pub inlinable_parameters: Vec<EcoString>,
}

/// Location information is not stored for inlinable functions, to reduce cache
/// size. The only reason we should need location information is for generating
/// code for panicking keywords, like `panic` or `todo`.
///
/// Those are not supported yet, and when they are they will likely require some
/// more thought as to how they are implemented, as inlining a function completely
/// changes its location in the codebase.
const BLANK_LOCATION: SrcSpan = SrcSpan { start: 0, end: 0 };

impl InlinableFunction {
    /// Converts an `InlinableFunction` to an anonymous function, which can then
    /// be inlined within another function.
    fn to_anonymous_function(&self) -> (Vec<TypedArg>, Vec1<TypedStatement>) {
        let parameters = self
            .parameters
            .iter()
            .map(|parameter| parameter.to_typed_arg())
            .collect();

        let body = self
            .body
            .iter()
            .map(|ast| Statement::Expression(ast.to_expression()))
            .collect_vec();

        (
            parameters,
            body.try_into()
                .expect("Type-checking ensured that the body has at least 1 statement"),
        )
    }
}

#[derive(Debug, Clone, PartialEq, Eq, serde::Serialize, serde::Deserialize)]
pub enum InlinableExpression {
    Case {
        subjects: Vec<InlinableExpression>,
        clauses: Vec<InlinableClause>,
        compiled_case: Box<CompiledCase>,
        type_: InlinableType,
    },

    Variable {
        name: EcoString,
        constructor: InlinableValueConstructor,
        type_: InlinableType,
    },

    Call {
        function: Box<InlinableExpression>,
        arguments: Vec<InlinableArgument<InlinableExpression>>,
        type_: InlinableType,
    },
}

impl InlinableExpression {
    fn to_expression(&self) -> TypedExpr {
        match self {
            InlinableExpression::Case {
                subjects,
                clauses,
                compiled_case,
                type_,
            } => TypedExpr::Case {
                location: BLANK_LOCATION,
                type_: type_.to_type(),
                subjects: subjects
                    .iter()
                    .map(|subject| subject.to_expression())
                    .collect(),
                clauses: clauses
                    .iter()
                    .map(|clause| clause.to_typed_clause())
                    .collect(),
                compiled_case: compiled_case.as_ref().clone(),
            },
            InlinableExpression::Variable {
                name,
                constructor,
                type_,
            } => TypedExpr::Var {
                location: BLANK_LOCATION,
                constructor: constructor.to_value_constructor(type_.to_type()),
                name: name.clone(),
            },
            InlinableExpression::Call {
                function,
                arguments,
                type_,
            } => TypedExpr::Call {
                location: BLANK_LOCATION,
                type_: type_.to_type(),
                fun: Box::new(function.to_expression()),
                arguments: arguments
                    .iter()
                    .map(|argument| argument.to_call_arg(Self::to_expression))
                    .collect(),
                arguments_start: None,
            },
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, serde::Serialize, serde::Deserialize)]
pub struct InlinableClause {
    pub pattern: Vec<InlinablePattern>,
    pub body: InlinableExpression,
}

impl InlinableClause {
    fn to_typed_clause(&self) -> TypedClause {
        TypedClause {
            location: BLANK_LOCATION,
            pattern: self
                .pattern
                .iter()
                .map(|pattern| pattern.to_typed_pattern())
                .collect(),
            alternative_patterns: Vec::new(),
            guard: None,
            then: self.body.to_expression(),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, serde::Serialize, serde::Deserialize)]
pub enum InlinablePattern {
    Constructor {
        name: EcoString,
        module: EcoString,
        arguments: Vec<InlinableArgument<InlinablePattern>>,
    },

    Variable {
        name: EcoString,
    },
}

impl InlinablePattern {
    fn to_typed_pattern(&self) -> TypedPattern {
        match self {
            InlinablePattern::Constructor {
                name,
                module,
                arguments,
            } => TypedPattern::Constructor {
                location: BLANK_LOCATION,
                name_location: BLANK_LOCATION,
                name: name.clone(),
                arguments: arguments
                    .iter()
                    .map(|argument| argument.to_call_arg(Self::to_typed_pattern))
                    .collect(),
                module: None,
                constructor: Inferred::Known(PatternConstructor {
                    name: name.clone(),
                    field_map: None,
                    documentation: None,
                    module: module.clone(),
                    location: BLANK_LOCATION,
                    constructor_index: 0,
                }),
                spread: None,
                type_: unknown_type(),
            },
            InlinablePattern::Variable { name } => TypedPattern::Variable {
                location: BLANK_LOCATION,
                name: name.clone(),
                type_: unknown_type(),
                origin: VariableOrigin::generated(),
            },
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, serde::Serialize, serde::Deserialize)]
pub enum InlinableValueConstructor {
    LocalVariable,
    Function { name: EcoString, module: EcoString },
    Record { name: EcoString, module: EcoString },
}

impl InlinableValueConstructor {
    fn to_value_constructor(&self, type_: Arc<Type>) -> ValueConstructor {
        let variant = match self {
            InlinableValueConstructor::LocalVariable => ValueConstructorVariant::LocalVariable {
                location: BLANK_LOCATION,
                origin: VariableOrigin::generated(),
            },
            InlinableValueConstructor::Function { name, module } => {
                ValueConstructorVariant::ModuleFn {
                    name: name.clone(),
                    field_map: None,
                    module: module.clone(),
                    arity: 0,
                    location: BLANK_LOCATION,
                    documentation: None,
                    implementations: Implementations::supporting_all(),
                    external_erlang: None,
                    external_javascript: None,
                    purity: Purity::Unknown,
                }
            }
            InlinableValueConstructor::Record { name, module } => ValueConstructorVariant::Record {
                name: name.clone(),
                arity: 0,
                field_map: None,
                location: BLANK_LOCATION,
                module: module.clone(),
                variants_count: 0,
                variant_index: 0,
                documentation: None,
            },
        };
        ValueConstructor {
            publicity: Publicity::Private,
            deprecation: Deprecation::NotDeprecated,
            variant,
            type_,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, serde::Serialize, serde::Deserialize)]
pub struct InlinableArgument<T> {
    pub label: Option<EcoString>,
    pub value: T,
}

impl<T> InlinableArgument<T> {
    fn to_call_arg<U, F>(&self, convert_value: F) -> CallArg<U>
    where
        F: FnOnce(&T) -> U,
    {
        CallArg {
            label: self.label.clone(),
            location: BLANK_LOCATION,
            value: convert_value(&self.value),
            implicit: None,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, serde::Serialize, serde::Deserialize)]
pub struct InlinableParameter {
    pub label: Option<EcoString>,
    pub name: EcoString,
}

impl InlinableParameter {
    fn to_typed_arg(&self) -> TypedArg {
        let is_discard = self.name.starts_with('_');

        let names = match &self.label {
            Some(label) if is_discard => ArgNames::LabelledDiscard {
                label: label.clone(),
                label_location: BLANK_LOCATION,
                name: self.name.clone(),
                name_location: BLANK_LOCATION,
            },
            Some(label) => ArgNames::NamedLabelled {
                label: label.clone(),
                label_location: BLANK_LOCATION,
                name: self.name.clone(),
                name_location: BLANK_LOCATION,
            },
            None if is_discard => ArgNames::Discard {
                name: self.name.clone(),
                location: BLANK_LOCATION,
            },
            None => ArgNames::Named {
                name: self.name.clone(),
                location: BLANK_LOCATION,
            },
        };

        TypedArg {
            names,
            location: BLANK_LOCATION,
            annotation: None,
            type_: unknown_type(),
        }
    }
}

/// A simplified version of `Type`, which only cares about prelude types. Code
/// generation needs this type information, as some prelude types are handled
/// specially in certain cases. Custom type don't matter though, so they all get
/// reduced into a single value, which decreases cache size.
#[derive(Debug, Clone, PartialEq, Eq, serde::Serialize, serde::Deserialize)]
pub enum InlinableType {
    BitArray,
    Bool,
    Float,
    Int,
    List(Box<InlinableType>),
    Nil,
    Result {
        ok: Box<InlinableType>,
        error: Box<InlinableType>,
    },
    String,
    UtfCodepoint,

    Function {
        arguments: Vec<InlinableType>,
        return_: Box<InlinableType>,
    },

    Other,
}

fn unknown_type() -> Arc<Type> {
    type_::generic_var(0)
}

impl InlinableType {
    fn to_type(&self) -> Arc<Type> {
        match self {
            InlinableType::BitArray => type_::bit_array(),
            InlinableType::Bool => type_::bool(),
            InlinableType::Float => type_::float(),
            InlinableType::Int => type_::int(),
            InlinableType::List(element) => type_::list(element.to_type()),
            InlinableType::Nil => type_::nil(),
            InlinableType::Result { ok, error } => type_::result(ok.to_type(), error.to_type()),
            InlinableType::String => type_::string(),
            InlinableType::UtfCodepoint => type_::utf_codepoint(),

            InlinableType::Function { arguments, return_ } => type_::fn_(
                arguments.iter().map(Self::to_type).collect(),
                return_.to_type(),
            ),

            // Code generation doesn't care about custom types at all, only
            // prelude types are handled specially, so we treat custom types as
            // opaque generic type variables.
            InlinableType::Other => unknown_type(),
        }
    }
}
