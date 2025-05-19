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
//!   order functions have been inlined
//! - Remove calls to anonymous functions in pipelines
//! - Remove calls to function captures in pipelines
//!
//! See documentation of individual functions to explain better how the process
//! works.
//!

use std::{
    collections::HashMap,
    sync::{Arc, LazyLock},
};

use ecow::EcoString;
use itertools::Itertools;
use vec1::Vec1;

use crate::{
    STDLIB_PACKAGE_NAME,
    analyse::Inferred,
    ast::{
        ArgNames, Assert, Assignment, AssignmentKind, BitArrayOption, BitArraySegment, CallArg,
        Definition, FunctionLiteralKind, PipelineAssignmentKind, Publicity, SrcSpan, Statement,
        TypedArg, TypedAssert, TypedAssignment, TypedClause, TypedDefinition, TypedExpr,
        TypedExprBitArraySegment, TypedFunction, TypedModule, TypedPattern,
        TypedPipelineAssignment, TypedStatement, TypedUse,
    },
    exhaustiveness::CompiledCase,
    type_::{
        self, Deprecation, ModuleInterface, ModuleValueConstructor, PatternConstructor, Type,
        TypedCallArg, ValueConstructorVariant,
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

    module.definitions = module
        .definitions
        .into_iter()
        .map(|definition| inliner.definition(definition))
        .collect();
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
}

impl Inliner<'_> {
    fn new(modules: &im::HashMap<EcoString, ModuleInterface>) -> Inliner<'_> {
        Inliner {
            modules,
            inline_variables: HashMap::new(),
        }
    }

    /// Perform inlining over a single definition. This only does anything for
    /// function definitions as none of the other definitions can contain call
    /// expressions to be inlined.
    fn definition(&mut self, definition: TypedDefinition) -> TypedDefinition {
        match definition {
            Definition::Function(function_ast) => Definition::Function(self.function(function_ast)),
            Definition::TypeAlias(_)
            | Definition::CustomType(_)
            | Definition::Import(_)
            | Definition::ModuleConstant(_) => definition,
        }
    }

    fn function(&mut self, mut function: TypedFunction) -> TypedFunction {
        function.body = function.body.mapped(|statement| self.statement(statement));
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
            pattern,
            kind: self.assignment_kind(kind),
            annotation,
            compiled_case,
        }
    }

    fn assignment_kind(&mut self, kind: AssignmentKind<TypedExpr>) -> AssignmentKind<TypedExpr> {
        match kind {
            AssignmentKind::Let | AssignmentKind::Generated => kind,
            AssignmentKind::Assert { location, message } => AssignmentKind::Assert {
                location,
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
    fn expression(&mut self, expression: TypedExpr) -> TypedExpr {
        match expression {
            TypedExpr::Int { .. }
            | TypedExpr::Float { .. }
            | TypedExpr::String { .. }
            | TypedExpr::Fn { .. }
            | TypedExpr::ModuleSelect { .. }
            | TypedExpr::Invalid { .. } => expression,

            TypedExpr::Var {
                ref constructor,
                ref name,
                ..
            } => match &constructor.variant {
                // If this variable can be inlined, replace it with its value.
                // See the `inline_variables` documentation for an explanation.
                ValueConstructorVariant::LocalVariable { .. } => {
                    // We remove the variable as inlined variables can only be
                    // inlined once.
                    match self.inline_variables.remove(name) {
                        Some(expression) => expression,
                        None => expression,
                    }
                }
                ValueConstructorVariant::ModuleConstant { .. }
                | ValueConstructorVariant::LocalConstant { .. }
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
                args,
            } => self.call(location, type_, fun, args),

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
            } => TypedExpr::RecordAccess {
                location,
                field_start,
                type_,
                label,
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
            } => TypedExpr::Echo {
                location,
                expression: expression
                    .map(|boxed_expression| self.boxed_expression(boxed_expression)),
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
                record,
                constructor,
                args,
            } => TypedExpr::RecordUpdate {
                location,
                type_,
                record: Box::new(self.assignment(*record)),
                constructor: self.boxed_expression(constructor),
                args,
            },
        }
    }

    /// Where the magic happens. First, we check the left-hand side of the call
    /// so see if it's something we can inline. If not, we continue to walk the
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
    ) -> TypedExpr {
        let arguments = arguments
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
            .collect();

        // First, we traverse the left-hand side of this call. If this is called
        // inside another inlined function, this could potentially inline an
        // argument, allowing further inlining.
        let function = self.expression(*function);

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
                        let TypedExpr::Fn {
                            args: parameters,
                            body,
                            ..
                        } = function.to_anonymous_function()
                        else {
                            unreachable!("to_anonymous_function always returns TypedExpr::fn");
                        };
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
                | ValueConstructorVariant::LocalConstant { .. }
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
                        let TypedExpr::Fn {
                            args: parameters,
                            body,
                            ..
                        } = function.to_anonymous_function()
                        else {
                            unreachable!("to_anonymous_function always returns TypedExpr::fn");
                        };
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
                args: parameters,
                body,
                ..
            } => {
                return self.inline_anonymous_function_call(&parameters, arguments, body, &[]);
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
            location: location,
            type_: type_,
            fun: Box::new(function),
            args: arguments,
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

                // A function can be inlined if it is only used once (stored in
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

                // Otherwise, we make an assignment which assigns the value of
                // the argument to the correct parameter name.
                Some(Statement::Assignment(Box::new(Assignment {
                    location: BLANK_LOCATION,
                    value: argument.value,
                    pattern: TypedPattern::Variable {
                        location: BLANK_LOCATION,
                        name: name.clone(),
                        type_: NIL.clone(),
                        origin: VariableOrigin::Generated,
                    },
                    kind: AssignmentKind::Generated,
                    compiled_case: CompiledCase::simple_variable_assignment(name, NIL.clone()),
                    annotation: None,
                })))
            })
            .collect_vec();

        // If we are performing inlining within an already inlined function, there
        // might be inlinable variables in the outer scope. However, these cannot be
        // inlined inside a nested function, so they are saved and restored afterwards.
        let inline_variables = std::mem::replace(&mut self.inline_variables, inline);

        // Perform inlining on each of the statements in this function's body,
        // potentially inlining parameters and function calls inside this function.
        statements.extend(body.into_iter().map(|statement| self.statement(statement)));

        self.inline_variables = inline_variables;

        TypedExpr::Block {
            location: BLANK_LOCATION,
            statements: statements
                .try_into()
                .expect("Type checking ensures there is at least one statement"),
        }
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
            .map(
                |BitArraySegment {
                     location,
                     value,
                     options,
                     type_,
                 }| BitArraySegment {
                    location,
                    value: self.boxed_expression(value),
                    options: options
                        .into_iter()
                        .map(|bit_array_option| self.bit_array_option(bit_array_option))
                        .collect(),
                    type_,
                },
            )
            .collect();

        TypedExpr::BitArray {
            location,
            type_,
            segments,
        }
    }

    fn bit_array_option(&mut self, option: BitArrayOption<TypedExpr>) -> BitArrayOption<TypedExpr> {
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
                value: self.boxed_expression(value),
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
            .map(
                |TypedClause {
                     location,
                     pattern,
                     alternative_patterns,
                     guard,
                     then,
                 }| TypedClause {
                    location,
                    pattern,
                    alternative_patterns,
                    guard,
                    then: self.expression(then),
                },
            )
            .collect();

        TypedExpr::Case {
            location,
            type_,
            subjects,
            clauses,
            compiled_case,
        }
    }
}

pub fn inline_function(package: &str, module: &str, function: &TypedFunction) -> Option<Function> {
    // For now we only offer inlining of standard library functions
    if package != STDLIB_PACKAGE_NAME {
        return None;
    }

    let (_, name) = function.name.as_ref()?;

    match (module, name.as_str()) {
        // These are the functions which we currently inline
        ("gleam/bool", "guard") => {}
        ("gleam/bool", "lazy_guard") => {}
        ("gleam/result", "try") => {}
        ("gleam/result", "then") => {}
        ("gleam/result", "map") => {}
        _ => return None,
    }

    let parameters = function
        .arguments
        .iter()
        .map(|argument| match &argument.names {
            ArgNames::Discard { name, .. } | ArgNames::Named { name, .. } => Parameter {
                label: None,
                name: name.clone(),
            },
            ArgNames::LabelledDiscard { label, name, .. }
            | ArgNames::NamedLabelled { label, name, .. } => Parameter {
                label: Some(label.clone()),
                name: name.clone(),
            },
        })
        .collect();

    let mut inliner = FunctionInliner::new(&function.arguments);

    let body = function
        .body
        .iter()
        .map(|ast| inliner.ast_to_inline(ast))
        .collect::<Option<_>>()?;

    let inlinable_parameters = inliner
        .parameter_references
        .into_iter()
        .filter_map(|((name, _), used)| used.then_some(name))
        .collect();

    Some(Function {
        parameters,
        body,
        inlinable_parameters,
    })
}

struct FunctionInliner {
    parameter_references: HashMap<(EcoString, SrcSpan), bool>,
}

impl FunctionInliner {
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

    fn ast_to_inline(&mut self, statement: &TypedStatement) -> Option<Ast> {
        match statement {
            Statement::Expression(expression) => self.expression_to_inline(expression),
            Statement::Assignment(_) | Statement::Use(_) | Statement::Assert(_) => None,
        }
    }

    fn expression_to_inline(&mut self, expression: &TypedExpr) -> Option<Ast> {
        match expression {
            TypedExpr::Case {
                subjects,
                clauses,
                compiled_case,
                ..
            } => {
                let subjects = subjects
                    .iter()
                    .map(|expression| self.expression_to_inline(expression))
                    .collect::<Option<_>>()?;
                let clauses = clauses
                    .iter()
                    .map(|clause| self.clause_to_inline(clause))
                    .collect::<Option<_>>()?;

                Some(Ast::Case {
                    subjects,
                    clauses,
                    compiled_case: compiled_case.clone(),
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
                    | ValueConstructorVariant::LocalConstant { .. }
                    | ValueConstructorVariant::ModuleFn { .. }
                    | ValueConstructorVariant::Record { .. } => {}
                }

                Some(Ast::Variable {
                    name: name.clone(),
                    constructor: self.value_constructor(constructor)?,
                })
            }
            TypedExpr::Call { fun, args, .. } => {
                let function = self.expression_to_inline(fun)?;
                let arguments = args
                    .iter()
                    .map(|argument| {
                        Some(Argument {
                            label: argument.label.clone(),
                            value: self.expression_to_inline(&argument.value)?,
                        })
                    })
                    .collect::<Option<_>>()?;

                Some(Ast::Call {
                    function: Box::new(function),
                    arguments,
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

    fn value_constructor(
        &mut self,
        constructor: &type_::ValueConstructor,
    ) -> Option<ValueConstructor> {
        match &constructor.variant {
            ValueConstructorVariant::LocalVariable { .. } => Some(ValueConstructor::LocalVariable),
            ValueConstructorVariant::ModuleConstant { .. }
            | ValueConstructorVariant::LocalConstant { .. } => None,
            ValueConstructorVariant::ModuleFn { name, module, .. } => {
                Some(ValueConstructor::Function {
                    name: name.clone(),
                    module: module.clone(),
                })
            }
            ValueConstructorVariant::Record { name, module, .. } => {
                Some(ValueConstructor::Record {
                    name: name.clone(),
                    module: module.clone(),
                })
            }
        }
    }

    fn clause_to_inline(&mut self, clause: &TypedClause) -> Option<Clause> {
        let pattern = clause
            .pattern
            .iter()
            .map(|pattern| self.pattern_to_inline(pattern))
            .collect::<Option<_>>()?;
        let body = self.expression_to_inline(&clause.then)?;
        Some(Clause { pattern, body })
    }

    fn pattern_to_inline(&mut self, pattern: &TypedPattern) -> Option<Pattern> {
        match pattern {
            TypedPattern::Variable { name, .. } => Some(Pattern::Variable { name: name.clone() }),
            TypedPattern::Constructor {
                name,
                arguments,
                constructor: Inferred::Known(inferred),
                ..
            } => {
                let arguments = arguments
                    .iter()
                    .map(|argument| {
                        Some(Argument {
                            label: argument.label.clone(),
                            value: self.pattern_to_inline(&argument.value)?,
                        })
                    })
                    .collect::<Option<_>>()?;

                Some(Pattern::Constructor {
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
            | TypedPattern::VarUsage { .. }
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

#[derive(Debug, Clone, PartialEq, Eq, serde::Serialize, serde::Deserialize)]
pub struct Function {
    pub parameters: Vec<Parameter>,
    pub body: Vec<Ast>,
    pub inlinable_parameters: Vec<EcoString>,
}

const BLANK_LOCATION: SrcSpan = SrcSpan { start: 0, end: 0 };
const NIL: LazyLock<Arc<Type>> = LazyLock::new(type_::nil);

impl Function {
    fn to_anonymous_function(&self) -> TypedExpr {
        let parameters = self
            .parameters
            .iter()
            .map(|parameter| {
                let is_discard = parameter.name.starts_with('_');
                let names = match &parameter.label {
                    Some(label) if is_discard => ArgNames::LabelledDiscard {
                        label: label.clone(),
                        label_location: BLANK_LOCATION,
                        name: parameter.name.clone(),
                        name_location: BLANK_LOCATION,
                    },
                    Some(label) => ArgNames::NamedLabelled {
                        label: label.clone(),
                        label_location: BLANK_LOCATION,
                        name: parameter.name.clone(),
                        name_location: BLANK_LOCATION,
                    },
                    None if is_discard => ArgNames::Discard {
                        name: parameter.name.clone(),
                        location: BLANK_LOCATION,
                    },
                    None => ArgNames::Named {
                        name: parameter.name.clone(),
                        location: BLANK_LOCATION,
                    },
                };

                TypedArg {
                    names,
                    location: BLANK_LOCATION,
                    annotation: None,
                    type_: NIL.clone(),
                }
            })
            .collect();

        let body = self
            .body
            .iter()
            .map(|ast| Statement::Expression(ast.to_expression()))
            .collect_vec();

        TypedExpr::Fn {
            location: BLANK_LOCATION,
            type_: NIL.clone(),
            kind: FunctionLiteralKind::Anonymous {
                head: BLANK_LOCATION,
            },
            args: parameters,
            body: body
                .try_into()
                .expect("Type-checking ensured that the body has at least 1 statement"),
            return_annotation: None,
            purity: Purity::Unknown,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, serde::Serialize, serde::Deserialize)]
pub enum Ast {
    Case {
        subjects: Vec<Ast>,
        clauses: Vec<Clause>,
        compiled_case: CompiledCase,
    },

    Variable {
        name: EcoString,
        constructor: ValueConstructor,
    },

    Call {
        function: Box<Ast>,
        arguments: Vec<Argument<Ast>>,
    },
}

impl Ast {
    fn to_expression(&self) -> TypedExpr {
        match self {
            Ast::Case {
                subjects,
                clauses,
                compiled_case,
            } => TypedExpr::Case {
                location: BLANK_LOCATION,
                type_: NIL.clone(),
                subjects: subjects
                    .iter()
                    .map(|subject| subject.to_expression())
                    .collect(),
                clauses: clauses
                    .iter()
                    .map(|clause| clause.to_typed_clause())
                    .collect(),
                compiled_case: compiled_case.clone(),
            },
            Ast::Variable { name, constructor } => TypedExpr::Var {
                location: BLANK_LOCATION,
                constructor: constructor.to_value_constructor(),
                name: name.clone(),
            },
            Ast::Call {
                function,
                arguments,
            } => TypedExpr::Call {
                location: BLANK_LOCATION,
                type_: NIL.clone(),
                fun: Box::new(function.to_expression()),
                args: arguments
                    .iter()
                    .map(|argument| argument.to_call_arg(Self::to_expression))
                    .collect(),
            },
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, serde::Serialize, serde::Deserialize)]
pub struct Clause {
    pub pattern: Vec<Pattern>,
    pub body: Ast,
}

impl Clause {
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
pub enum Pattern {
    Constructor {
        name: EcoString,
        module: EcoString,
        arguments: Vec<Argument<Pattern>>,
    },

    Variable {
        name: EcoString,
    },
}

impl Pattern {
    fn to_typed_pattern(&self) -> TypedPattern {
        match self {
            Pattern::Constructor {
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
                type_: NIL.clone(),
            },
            Pattern::Variable { name } => TypedPattern::Variable {
                location: BLANK_LOCATION,
                name: name.clone(),
                type_: NIL.clone(),
                origin: VariableOrigin::Generated,
            },
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, serde::Serialize, serde::Deserialize)]
pub enum ValueConstructor {
    LocalVariable,
    Function { name: EcoString, module: EcoString },
    Record { name: EcoString, module: EcoString },
}

impl ValueConstructor {
    fn to_value_constructor(&self) -> type_::ValueConstructor {
        let variant = match self {
            ValueConstructor::LocalVariable => ValueConstructorVariant::LocalVariable {
                location: BLANK_LOCATION,
                origin: VariableOrigin::Generated,
            },
            ValueConstructor::Function { name, module } => ValueConstructorVariant::ModuleFn {
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
            },
            ValueConstructor::Record { name, module } => ValueConstructorVariant::Record {
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
        type_::ValueConstructor {
            publicity: Publicity::Private,
            deprecation: Deprecation::NotDeprecated,
            variant,
            type_: NIL.clone(),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, serde::Serialize, serde::Deserialize)]
pub struct Argument<T> {
    pub label: Option<EcoString>,
    pub value: T,
}

impl<T> Argument<T> {
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
pub struct Parameter {
    pub label: Option<EcoString>,
    pub name: EcoString,
}
