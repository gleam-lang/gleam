use super::{pipe::PipeTyper, *};
use crate::{
    STDLIB_PACKAGE_NAME,
    analyse::{Inferred, infer_bit_array_option, name::check_argument_names},
    ast::{
        Arg, Assert, Assignment, AssignmentKind, BinOp, BitArrayOption, BitArraySegment,
        CAPTURE_VARIABLE, CallArg, Clause, ClauseGuard, Constant, FunctionLiteralKind, HasLocation,
        ImplicitCallArgOrigin, InvalidExpression, Layer, RECORD_UPDATE_VARIABLE,
        RecordBeingUpdated, SrcSpan, Statement, TodoKind, TypeAst, TypedArg, TypedAssert,
        TypedAssignment, TypedClause, TypedClauseGuard, TypedConstant, TypedExpr,
        TypedMultiPattern, TypedStatement, USE_ASSIGNMENT_VARIABLE, UntypedArg, UntypedAssert,
        UntypedAssignment, UntypedClause, UntypedClauseGuard, UntypedConstant,
        UntypedConstantBitArraySegment, UntypedExpr, UntypedExprBitArraySegment,
        UntypedMultiPattern, UntypedStatement, UntypedUse, UntypedUseAssignment, Use,
        UseAssignment,
    },
    build::Target,
    exhaustiveness::{self, CompileCaseResult, CompiledCase, Reachability},
    parse::{LiteralFloatValue, PatternPosition},
    reference::ReferenceKind,
};
use ecow::eco_format;
use hexpm::version::{LowestVersion, Version};
use im::hashmap;
use itertools::Itertools;
use num_bigint::BigInt;
use vec1::Vec1;

#[derive(Clone, Copy, Debug, Eq, PartialOrd, Ord, PartialEq, Serialize)]
pub struct Implementations {
    /// Whether the function has a pure-gleam implementation.
    ///
    /// It's important to notice that, even if all individual targets are
    /// supported, it would not be the same as being pure Gleam.
    /// Imagine this scenario:
    ///
    /// ```gleam
    /// @external(javascript, "wibble", "wobble")
    /// @external(erlang, "wibble", "wobble")
    /// pub fn func() -> Int
    /// ```
    ///
    /// `func` supports all _current_ Gleam targets; however, if a new target
    /// is added - say a WASM target - `func` wouldn't support it! On the other
    /// hand, a pure Gleam function will support all future targets.
    pub gleam: bool,
    pub can_run_on_erlang: bool,
    pub can_run_on_javascript: bool,
    /// Whether the function has an implementation that uses external erlang
    /// code.
    pub uses_erlang_externals: bool,
    /// Whether the function has an implementation that uses external javascript
    /// code.
    pub uses_javascript_externals: bool,
}

impl Implementations {
    pub fn supporting_all() -> Self {
        Self {
            gleam: true,
            can_run_on_erlang: true,
            can_run_on_javascript: true,
            uses_javascript_externals: false,
            uses_erlang_externals: false,
        }
    }
}

/// The purity of a function.
///
/// This is not actually proper purity tracking, rather an approximation, which
/// is good enough for the purpose it is currently used for: warning for unused
/// pure functions. The current system contains some false negatives, i.e. some
/// cases where it will fail to emit a warning when it probably should.
///
/// If we wanted to properly track function side effects - say to perform
/// optimisations on pure Gleam code - we would probably need to lift that
/// tracking into the type system, the same way that variant inference currently
/// works. This would require quite a lot of work and doesn't seem a worthwhile
/// amount of effort for a single warning message, where a much simpler solution
/// is generally going to be good enough.
///
/// In the future we may want to implement a full side effect tracking system;
/// this current implementation will not be sufficient for anything beyond a
/// warning message to help people out in certain cases.
///
#[derive(Debug, Clone, Copy, PartialEq, Eq, serde::Serialize, serde::Deserialize)]
pub enum Purity {
    /// The function is in pure Gleam, and does not reference any language
    /// feature that can cause side effects, such as `panic`, `assert` or `echo`.
    /// It also does not call any impure functions.
    Pure,
    /// This function is part of the standard library, or an otherwise trusted
    /// source, and though it might use FFI, we can trust that the FFI function
    /// will not cause any side effects.
    TrustedPure,
    /// This function is impure because it either uses FFI, panics, uses `echo`,
    /// or calls another impure function.
    Impure,
    /// We don't know the purity of this function. This highlights the main issue
    /// with the current purity tracking system. In the following code for example:
    ///
    /// ```gleam
    /// let f = function.identity
    ///
    /// f(10)
    /// ```
    ///
    /// Since purity is not currently part of the type system, when analysing the
    /// call of the local `f` function, we now have no information about the
    /// purity of it, and therefore cannot infer the consequences of calling it.
    ///
    /// If there was a `purity` or `side_effects` field in the `Type::Fn` variant,
    /// we would be able to properly infer it.
    ///
    Unknown,
}

impl Purity {
    pub fn is_pure(&self) -> bool {
        match self {
            Purity::Pure | Purity::TrustedPure => true,
            Purity::Impure | Purity::Unknown => false,
        }
    }

    #[must_use]
    pub fn merge(self, other: Purity) -> Purity {
        match (self, other) {
            // If we call a trusted pure function, the current function remains pure
            (Purity::Pure, Purity::TrustedPure) => Purity::Pure,
            (Purity::Pure, other) => other,

            // If we call a pure function, the current function remains trusted pure
            (Purity::TrustedPure, Purity::Pure) => Purity::TrustedPure,
            (Purity::TrustedPure, other) => other,

            // Nothing can make an already impure function pure again
            (Purity::Impure, _) => Purity::Impure,

            // If we call an impure function from a function we don't know the
            // purity of, we are now certain that it is impure.
            (Purity::Unknown, Purity::Impure) => Purity::Impure,
            (Purity::Unknown, _) => Purity::Impure,
        }
    }
}

/// Tracking whether the function being currently type checked has externals
/// implementations or not.
/// This is used to determine whether an error should be raised in the case when
/// a value is used that does not have an implementation for the current target.
#[derive(Clone, Copy, Debug)]
pub struct FunctionDefinition {
    /// The function has { ... } after the function head
    pub has_body: bool,
    /// The function has @external(erlang, "...", "...")
    pub has_erlang_external: bool,
    /// The function has @external(JavaScript, "...", "...")
    pub has_javascript_external: bool,
}

impl FunctionDefinition {
    pub fn has_external_for_target(&self, target: Target) -> bool {
        match target {
            Target::Erlang => self.has_erlang_external,
            Target::JavaScript => self.has_javascript_external,
        }
    }
}

impl Implementations {
    /// Given the implementations of a function update those with taking into
    /// account the `implementations` of another function (or constant) used
    /// inside its body.
    pub fn update_from_use(
        &mut self,
        implementations: &Implementations,
        current_function_definition: &FunctionDefinition,
    ) {
        // With this pattern matching we won't forget to deal with new targets
        // when those are added :)
        let Implementations {
            gleam,
            uses_erlang_externals: other_uses_erlang_externals,
            uses_javascript_externals: other_uses_javascript_externals,
            can_run_on_erlang: other_can_run_on_erlang,
            can_run_on_javascript: other_can_run_on_javascript,
        } = implementations;
        let FunctionDefinition {
            has_body: _,
            has_erlang_external,
            has_javascript_external,
        } = current_function_definition;

        // If a pure-Gleam function uses a function that doesn't have a pure
        // Gleam implementation, then it's no longer pure-Gleam.
        self.gleam = self.gleam && *gleam;

        // A function can run on a target if the code that it uses can run on on
        // the same target,
        self.can_run_on_erlang = *has_erlang_external
            || (self.can_run_on_erlang && (*gleam || *other_can_run_on_erlang));
        self.can_run_on_javascript = *has_javascript_external
            || (self.can_run_on_javascript && (*gleam || *other_can_run_on_javascript));

        // If a function uses a function that relies on external code (be it
        // javascript or erlang) then it's considered as using external code as
        // well.
        //
        // For example:
        // ```gleam
        // @external(erlang, "wibble", "wobble")
        // pub fn erlang_only_with_pure_gleam_default() -> Int {
        //   1 + 1
        // }
        //
        // pub fn main() { erlang_only_with_pure_gleam_default() }
        // ```
        // Both functions will end up using external erlang code and have the
        // following implementations:
        // `Implementations { gleam: true, uses_erlang_externals: true, uses_javascript_externals: false}`.
        // They have a pure gleam implementation and an erlang specific external
        // implementation.
        self.uses_erlang_externals = self.uses_erlang_externals || *other_uses_erlang_externals;
        self.uses_javascript_externals =
            self.uses_javascript_externals || *other_uses_javascript_externals;
    }

    /// Returns true if the current target is supported by the given
    /// implementations.
    /// If something has a pure gleam implementation then it supports all
    /// targets automatically.
    pub fn supports(&self, target: Target) -> bool {
        self.gleam
            || match target {
                Target::Erlang => self.can_run_on_erlang,
                Target::JavaScript => self.can_run_on_javascript,
            }
    }
}

/// This is used to tell apart regular function calls and `use` expressions:
/// a `use` is still typed as if it were a normal function call but we want to
/// be able to tell the difference in order to provide better error message.
///
#[derive(Eq, PartialEq, Debug, Copy, Clone)]
pub enum CallKind {
    Function,
    Use {
        call_location: SrcSpan,
        assignments_location: SrcSpan,
        last_statement_location: SrcSpan,
    },
}

/// This is used to tell apart regular call arguments and the callback that is
/// implicitly passed to a `use` function call.
/// Both are going to be typed as usual but we want to tell them apart in order
/// to report better error messages for `use` expressions.
///
#[derive(Eq, PartialEq, Debug, Copy, Clone)]
pub enum ArgumentKind {
    Regular,
    UseCallback {
        function_location: SrcSpan,
        assignments_location: SrcSpan,
        last_statement_location: SrcSpan,
    },
}

#[derive(Debug)]
pub(crate) struct ExprTyper<'a, 'b> {
    pub(crate) environment: &'a mut Environment<'b>,

    /// The minimum Gleam version required to compile the typed expression.
    pub minimum_required_version: Version,

    // This is set to true if the previous expression that has been typed is
    // determined to always panic.
    // For example when typing a literal `panic`, this flag will be set to true.
    // The same goes, for example, if the branches of a case expression all
    // panic.
    pub(crate) previous_panics: bool,

    // This is used to track if we've already warned for unreachable code.
    // After emitting the first unreachable code warning we never emit another
    // one to avoid flooding with repetitive warnings.
    pub(crate) already_warned_for_unreachable_code: bool,

    pub(crate) implementations: Implementations,
    pub(crate) purity: Purity,
    pub(crate) current_function_definition: FunctionDefinition,

    // Type hydrator for creating types from annotations
    pub(crate) hydrator: Hydrator,

    // Accumulated errors and warnings found while typing the expression
    pub(crate) problems: &'a mut Problems,
}

impl<'a, 'b> ExprTyper<'a, 'b> {
    pub fn new(
        environment: &'a mut Environment<'b>,
        definition: FunctionDefinition,
        problems: &'a mut Problems,
    ) -> Self {
        let mut hydrator = Hydrator::new();

        let implementations = Implementations {
            // We start assuming the function is pure Gleam and narrow it down
            // if we run into functions/constants that have only external
            // implementations for some of the targets.
            gleam: definition.has_body,
            can_run_on_erlang: definition.has_body || definition.has_erlang_external,
            can_run_on_javascript: definition.has_body || definition.has_javascript_external,
            uses_erlang_externals: definition.has_erlang_external,
            uses_javascript_externals: definition.has_javascript_external,
        };

        let uses_externals = match environment.target {
            Target::Erlang => implementations.uses_erlang_externals,
            Target::JavaScript => implementations.uses_javascript_externals,
        };

        let purity = if is_trusted_pure_module(environment) {
            // The standard library uses a lot of FFI, but as we are the
            // maintainers we know that it can be trusted to be pure.
            Purity::TrustedPure
        } else if uses_externals {
            Purity::Impure
        } else {
            Purity::Pure
        };

        hydrator.permit_holes(true);
        Self {
            hydrator,
            previous_panics: false,
            already_warned_for_unreachable_code: false,
            environment,
            implementations,
            purity,
            current_function_definition: definition,
            minimum_required_version: Version::new(0, 1, 0),
            problems,
        }
    }

    fn in_new_scope<T, E>(
        &mut self,
        process_scope: impl FnOnce(&mut Self) -> Result<T, E>,
    ) -> Result<T, E> {
        self.scoped(|this| {
            let result = process_scope(this);
            let was_successful = result.is_ok();
            (result, was_successful)
        })
    }

    fn value_in_new_scope<A>(&mut self, process_scope: impl FnOnce(&mut Self) -> A) -> A {
        self.scoped(|this| (process_scope(this), true))
    }

    fn expr_in_new_scope(
        &mut self,
        process_scope: impl FnOnce(&mut Self) -> TypedExpr,
    ) -> TypedExpr {
        self.scoped(|this| {
            let expr = process_scope(this);
            let was_successful = !expr.is_invalid();
            (expr, was_successful)
        })
    }

    fn scoped<A>(&mut self, process_scope: impl FnOnce(&mut Self) -> (A, bool)) -> A {
        // Create new scope
        let environment_reset_data = self.environment.open_new_scope();
        let hydrator_reset_data = self.hydrator.open_new_scope();

        // Process the scope
        let (result, was_successful) = process_scope(self);

        // Close scope, discarding any scope local state
        self.environment
            .close_scope(environment_reset_data, was_successful, self.problems);
        self.hydrator.close_scope(hydrator_reset_data);
        result
    }

    pub fn type_from_ast(&mut self, ast: &TypeAst) -> Result<Arc<Type>, Error> {
        self.hydrator
            .type_from_ast(ast, self.environment, self.problems)
    }

    fn instantiate(&mut self, t: Arc<Type>, ids: &mut im::HashMap<u64, Arc<Type>>) -> Arc<Type> {
        self.environment.instantiate(t, ids, &self.hydrator)
    }

    pub fn new_unbound_var(&mut self) -> Arc<Type> {
        self.environment.new_unbound_var()
    }

    pub fn infer_or_error(&mut self, expr: UntypedExpr) -> Result<TypedExpr, Error> {
        if self.previous_panics {
            self.warn_for_unreachable_code(expr.location(), PanicPosition::PreviousExpression);
        }

        match expr {
            UntypedExpr::Todo {
                location,
                message: label,
                kind,
                ..
            } => Ok(self.infer_todo(location, kind, label)),

            UntypedExpr::Panic {
                location, message, ..
            } => Ok(self.infer_panic(location, message)),

            UntypedExpr::Echo {
                location,
                keyword_end,
                expression,
                message,
            } => Ok(self.infer_echo(location, keyword_end, expression, message)),

            UntypedExpr::Var { location, name, .. } => {
                self.infer_var(name, location, ReferenceRegistration::Register)
            }

            UntypedExpr::Int {
                location,
                value,
                int_value,
                ..
            } => {
                if self.environment.target == Target::JavaScript
                    && !self.current_function_definition.has_javascript_external
                {
                    check_javascript_int_safety(&int_value, location, self.problems);
                }

                Ok(self.infer_int(value, int_value, location))
            }

            UntypedExpr::Block {
                statements,
                location,
            } => Ok(self.infer_block(statements, location)),

            UntypedExpr::Tuple {
                location, elements, ..
            } => Ok(self.infer_tuple(elements, location)),

            UntypedExpr::Float {
                location,
                value,
                float_value,
            } => {
                check_float_safety(float_value, location, self.problems);
                Ok(self.infer_float(value, float_value, location))
            }

            UntypedExpr::String {
                location, value, ..
            } => Ok(self.infer_string(value, location)),

            UntypedExpr::PipeLine { expressions } => Ok(self.infer_pipeline(expressions)),

            UntypedExpr::Fn {
                location,
                kind,
                arguments,
                body,
                return_annotation,
                ..
            } => Ok(self.infer_fn(arguments, &[], body, kind, return_annotation, location)),

            UntypedExpr::Case {
                location,
                subjects,
                clauses,
                ..
            } => Ok(self.infer_case(subjects, clauses, location)),

            UntypedExpr::List {
                location,
                elements,
                tail,
            } => Ok(self.infer_list(elements, tail, location)),

            UntypedExpr::Call {
                location,
                fun,
                arguments,
                ..
            } => Ok(self.infer_call(*fun, arguments, location, CallKind::Function)),

            UntypedExpr::BinOp {
                location,
                name,
                name_location,
                left,
                right,
            } => Ok(self.infer_binop(name, name_location, *left, *right, location)),

            UntypedExpr::FieldAccess {
                label_location,
                label,
                container,
                location,
            } => Ok(self.infer_field_access(
                *container,
                location,
                label,
                label_location,
                FieldAccessUsage::Other,
            )),

            UntypedExpr::TupleIndex {
                location,
                index,
                tuple,
                ..
            } => self.infer_tuple_index(*tuple, index, location),

            UntypedExpr::BitArray { location, segments } => {
                self.infer_bit_array(segments, location)
            }

            UntypedExpr::RecordUpdate {
                location,
                constructor,
                record,
                arguments,
            } => self.infer_record_update(*constructor, record, arguments, location),

            UntypedExpr::NegateBool { location, value } => {
                Ok(self.infer_negate_bool(location, *value))
            }

            UntypedExpr::NegateInt { location, value } => {
                Ok(self.infer_negate_int(location, *value))
            }
        }
    }

    fn infer_pipeline(&mut self, expressions: Vec1<UntypedExpr>) -> TypedExpr {
        PipeTyper::infer(self, expressions)
    }

    fn infer_todo(
        &mut self,
        location: SrcSpan,
        kind: TodoKind,
        message: Option<Box<UntypedExpr>>,
    ) -> TypedExpr {
        // Type the todo as whatever it would need to be to type check.
        let type_ = self.new_unbound_var();

        // Emit a warning that there is a todo in the code.
        let warning_location = match kind {
            TodoKind::Keyword | TodoKind::IncompleteUse | TodoKind::EmptyBlock => location,
            TodoKind::EmptyFunction { function_location } => function_location,
        };
        self.problems.warning(Warning::Todo {
            kind,
            location: warning_location,
            type_: type_.clone(),
        });

        self.purity = Purity::Impure;

        let message = message.map(|message| Box::new(self.infer_and_unify(*message, string())));
        TypedExpr::Todo {
            location,
            type_,
            message,
            kind,
        }
    }

    fn infer_panic(&mut self, location: SrcSpan, message: Option<Box<UntypedExpr>>) -> TypedExpr {
        let type_ = self.new_unbound_var();
        self.purity = Purity::Impure;

        let message = message.map(|message| Box::new(self.infer_and_unify(*message, string())));
        self.previous_panics = true;
        TypedExpr::Panic {
            location,
            type_,
            message,
        }
    }

    fn infer_echo(
        &mut self,
        location: SrcSpan,
        keyword_end: u32,
        expression: Option<Box<UntypedExpr>>,
        message: Option<Box<UntypedExpr>>,
    ) -> TypedExpr {
        self.environment.echo_found = true;
        self.purity = Purity::Impure;

        let expression = if let Some(expression) = expression {
            let expression = self.infer(*expression);
            if self.previous_panics {
                self.warn_for_unreachable_code(location, PanicPosition::EchoExpression);
            }
            expression
        } else {
            let location = SrcSpan {
                start: location.start,
                end: keyword_end,
            };
            self.problems
                .error(Error::EchoWithNoFollowingExpression { location });
            self.error_expr(location)
        };

        TypedExpr::Echo {
            location,
            type_: expression.type_(),
            expression: Some(Box::new(expression)),
            message: message.map(|message| Box::new(self.infer_and_unify(*message, string()))),
        }
    }

    pub(crate) fn warn_for_unreachable_code(
        &mut self,
        location: SrcSpan,
        panic_position: PanicPosition,
    ) {
        // We don't want to warn twice for unreachable code inside the same
        // block, so we have to keep track if we've already emitted a warning of
        // this kind.
        if !self.already_warned_for_unreachable_code {
            self.already_warned_for_unreachable_code = true;
            self.problems.warning(Warning::UnreachableCodeAfterPanic {
                location,
                panic_position,
            })
        }
    }

    fn infer_string(&mut self, value: EcoString, location: SrcSpan) -> TypedExpr {
        TypedExpr::String {
            location,
            value,
            type_: string(),
        }
    }

    fn infer_int(&mut self, value: EcoString, int_value: BigInt, location: SrcSpan) -> TypedExpr {
        TypedExpr::Int {
            location,
            value,
            int_value,
            type_: int(),
        }
    }

    fn infer_float(
        &mut self,
        value: EcoString,
        float_value: LiteralFloatValue,
        location: SrcSpan,
    ) -> TypedExpr {
        TypedExpr::Float {
            location,
            value,
            float_value,
            type_: float(),
        }
    }

    /// Emit a warning if the given expressions should not be discarded.
    /// e.g. because it's a literal (why was it made in the first place?)
    /// e.g. because it's of the `Result` type (errors should be handled)
    fn expression_discarded(&mut self, discarded: &TypedExpr) {
        if discarded.is_literal() {
            self.problems.warning(Warning::UnusedLiteral {
                location: discarded.location(),
            });
        } else if discarded.type_().is_result() {
            self.problems.warning(Warning::ImplicitlyDiscardedResult {
                location: discarded.location(),
            });
        } else if discarded.is_pure_value_constructor() {
            self.problems.warning(Warning::UnusedValue {
                location: discarded.location(),
            })
        }
    }

    pub(crate) fn infer_statements(
        &mut self,
        untyped: Vec1<UntypedStatement>,
    ) -> Vec1<TypedStatement> {
        let count = untyped.len();
        let location = SrcSpan::new(
            untyped.first().location().start,
            untyped.last().location().end,
        );
        self.infer_iter_statements(location, count, untyped.into_iter())
    }

    // Helper to create a new error expr.
    fn error_expr(&mut self, location: SrcSpan) -> TypedExpr {
        TypedExpr::Invalid {
            location,
            type_: self.new_unbound_var(),
            extra_information: None,
        }
    }

    fn error_expr_with_information(
        &mut self,
        location: SrcSpan,
        extra_information: Option<InvalidExpression>,
    ) -> TypedExpr {
        TypedExpr::Invalid {
            location,
            type_: self.new_unbound_var(),
            extra_information,
        }
    }

    fn infer_iter_statements<StatementsIter: Iterator<Item = UntypedStatement>>(
        &mut self,
        location: SrcSpan,
        count: usize,
        mut untyped: StatementsIter,
    ) -> Vec1<TypedStatement> {
        let mut i = 0;
        let mut statements: Vec<TypedStatement> = Vec::with_capacity(count);

        while let Some(statement) = untyped.next() {
            i += 1;

            match statement {
                Statement::Use(use_) => {
                    let statement = self.infer_use(use_, location, untyped.collect());
                    statements.push(statement);
                    break; // Inferring the use has consumed the rest of the exprs
                }
                Statement::Expression(expression) => {
                    let location = expression.location();
                    let expression = match self.infer_or_error(expression) {
                        Ok(expression) => expression,
                        Err(error) => {
                            self.problems.error(error);
                            self.error_expr(location)
                        }
                    };

                    // This isn't the final expression in the sequence, so call the
                    // `expression_discarded` function to see if anything is being
                    // discarded that we think shouldn't be.
                    if i < count {
                        self.expression_discarded(&expression);
                    }
                    statements.push(Statement::Expression(expression));
                }
                Statement::Assignment(assignment) => {
                    let assignment = self.infer_assignment(*assignment);
                    statements.push(Statement::Assignment(Box::new(assignment)));
                }
                Statement::Assert(assert) => {
                    let assert = self.infer_assert(assert);
                    statements.push(Statement::Assert(assert));
                }
            }
        }

        Vec1::try_from_vec(statements).expect("empty sequence")
    }

    fn infer_use(
        &mut self,
        use_: UntypedUse,
        sequence_location: SrcSpan,
        mut following_expressions: Vec<UntypedStatement>,
    ) -> TypedStatement {
        let use_call_location = use_.call.location();
        let mut call = get_use_expression_call(*use_.call);
        let assignments = UseAssignments::from_use_expression(use_.assignments);

        let assignments_count = assignments.body_assignments.len();
        let mut statements = assignments.body_assignments;

        if following_expressions.is_empty() {
            let todo = Statement::Expression(UntypedExpr::Todo {
                location: use_.location,
                message: None,
                kind: TodoKind::IncompleteUse,
            });
            statements.push(todo);
        } else {
            statements.append(&mut following_expressions);
        }

        let statements = Vec1::try_from_vec(statements).expect("safe: todo added above");

        // We need this to report good error messages in case there's a type error
        // in use. We consider `use` to be the last statement of a block since
        // it consumes everything that comes below it and returns a single value.
        let last_statement_location = statements
            .iter()
            .find_or_last(|statement| statement.is_use())
            .expect("safe: iter from non empty vec")
            .location();

        let first = statements.first().location();

        // Collect the following expressions into a function to be passed as a
        // callback to the use's call function.
        let callback = UntypedExpr::Fn {
            arguments: assignments.function_arguments,
            location: SrcSpan::new(first.start, sequence_location.end),
            end_of_head_byte_index: sequence_location.end,
            return_annotation: None,
            kind: FunctionLiteralKind::Use {
                location: use_.location,
            },
            body: statements,
        };

        // Add this new callback function to the arguments to function call
        call.arguments.push(CallArg {
            label: None,
            location: SrcSpan::new(first.start, sequence_location.end),
            value: callback,
            // This argument is implicitly given by Gleam's use syntax so we
            // mark it as such.
            implicit: Some(ImplicitCallArgOrigin::Use),
        });

        let call_location = SrcSpan {
            start: use_.location.start,
            end: sequence_location.end,
        };

        // We use `stacker` to prevent overflowing the stack when many `use`
        // expressions are chained. See https://github.com/gleam-lang/gleam/issues/4287
        let infer_call = || {
            self.infer_call(
                *call.function,
                call.arguments,
                call_location,
                CallKind::Use {
                    call_location: use_call_location,
                    assignments_location: use_.assignments_location,
                    last_statement_location,
                },
            )
        };
        let call = stacker::maybe_grow(128 * 1024, 2 * 1024 * 1024, infer_call);

        // After typing the call we know that the last argument must be an
        // anonymous function and the first assignments in its body are the
        // typed assignments on the left hand side of a `use`.
        let assignments = extract_typed_use_call_assignments(&call, assignments_count);

        Statement::Use(Use {
            call: Box::new(call),
            location: use_.location,
            right_hand_side_location: use_.right_hand_side_location,
            assignments_location: use_.assignments_location,
            assignments,
        })
    }

    fn infer_negate_bool(&mut self, location: SrcSpan, value: UntypedExpr) -> TypedExpr {
        self.infer_multiple_negate_bool(location, 1, location, value)
    }

    fn infer_multiple_negate_bool(
        &mut self,
        starting_location: SrcSpan,
        negations: usize,
        location: SrcSpan,
        value: UntypedExpr,
    ) -> TypedExpr {
        // If we're typing a double negation we just keep going increasing the
        // number of consecutive negations, inferring the wrapped value.
        if let UntypedExpr::NegateBool {
            location: inner_location,
            value,
        } = value
        {
            return TypedExpr::NegateBool {
                location,
                value: Box::new(self.infer_multiple_negate_bool(
                    starting_location,
                    negations + 1,
                    inner_location,
                    *value,
                )),
            };
        }

        // We know the last value can't be a bool negation if we're here, so
        // we're ready to produce a typed value!
        let value = self.infer(value);
        if let Err(error) = unify(bool(), value.type_()) {
            self.problems
                .error(convert_unify_error(error, value.location()));
        }

        // If there's more than a single negation we can raise a warning
        // highlighting the unneded ones. How many negations are highlighted
        // depends if they're an even or odd number:
        //
        // ```gleam
        // !!True   // all negations are superfluous.
        // !!!True  // we can remove all but one negation.
        // ```
        if negations > 1 {
            let location = if negations.is_multiple_of(2) {
                SrcSpan {
                    start: starting_location.start,
                    end: location.start + 1,
                }
            } else {
                SrcSpan {
                    start: starting_location.start,
                    end: location.start,
                }
            };

            self.problems
                .warning(Warning::UnnecessaryDoubleBoolNegation { location });
        }

        TypedExpr::NegateBool {
            location,
            value: Box::new(value),
        }
    }

    fn infer_negate_int(&mut self, location: SrcSpan, value: UntypedExpr) -> TypedExpr {
        self.infer_multiple_negate_int(location, 1, location, value)
    }

    fn infer_multiple_negate_int(
        &mut self,
        starting_location: SrcSpan,
        mut negations: usize,
        location: SrcSpan,
        value: UntypedExpr,
    ) -> TypedExpr {
        // If we're typing a double negation we just keep going increasing the
        // number of consecutive negations, inferring the wrapped value.
        if let UntypedExpr::NegateInt {
            location: inner_location,
            value,
        } = value
        {
            return TypedExpr::NegateInt {
                location,
                value: Box::new(self.infer_multiple_negate_int(
                    starting_location,
                    negations + 1,
                    inner_location,
                    *value,
                )),
            };
        }

        // We know the last value can't be an int negation, so we're ready to
        // produce a typed value!
        let value = self.infer(value);
        if let Err(error) = unify(int(), value.type_()) {
            self.problems
                .error(convert_unify_error(error, value.location()));
        }

        // This is used to emit a warning in case there's multiple negations.
        let mut end = location.start;

        // There's one special case where the final integer being typed might be
        // negated as well, in that case we need to update the number of
        // consecutive negations.
        if let TypedExpr::Int {
            value: ref v,
            ref location,
            ..
        } = value
            && v.starts_with('-')
        {
            negations += 1;
            end = location.start;
        }

        // If there's more than a single negation we can raise a warning
        // highlighting the unneded ones. How many negations are highlighted
        // depends if they're an even or odd number:
        //
        // ```gleam
        // --1   // all negations are superfluous.
        // ---1  // we can remove all but one negation.
        // ```
        if negations > 1 {
            let location = if negations.is_multiple_of(2) {
                SrcSpan {
                    start: starting_location.start,
                    end: end + 1,
                }
            } else {
                SrcSpan {
                    start: starting_location.start,
                    end,
                }
            };

            self.problems
                .warning(Warning::UnnecessaryDoubleIntNegation { location });
        }

        TypedExpr::NegateInt {
            location,
            value: Box::new(value),
        }
    }

    fn infer_fn(
        &mut self,
        arguments: Vec<UntypedArg>,
        expected_arguments: &[Arc<Type>],
        body: Vec1<UntypedStatement>,
        kind: FunctionLiteralKind,
        return_annotation: Option<TypeAst>,
        location: SrcSpan,
    ) -> TypedExpr {
        for Arg { names, .. } in arguments.iter() {
            check_argument_names(names, self.problems);
        }

        let already_warned_for_unreachable_code = self.already_warned_for_unreachable_code;
        self.already_warned_for_unreachable_code = false;
        self.previous_panics = false;

        let outer_purity = self.purity;

        // If an anonymous function can panic, that doesn't mean that the outer
        // function can too, so we track the purity separately. For example, in
        // this code:
        //
        // ```gleam
        // pub fn divide_partial(dividend: Int) {
        //   fn(divisor) {
        //     case divisor {
        //       0 -> panic as "Cannot divide by 0"
        //       _ -> dividend / divisor
        //     }
        //   }
        // }
        // ```
        //
        // Although the `divide_partial` function uses the `panic` keyword, it is
        // actually pure. Only the anonymous function that it constructs is impure;
        // constructing and returning it does not have any side effects, so there is
        // no way for a call to `divide_partial` to produce any side effects.
        self.purity = Purity::Pure;

        let (arguments, body) = match self.do_infer_fn(
            None,
            arguments,
            expected_arguments,
            body,
            &return_annotation,
        ) {
            Ok(result) => result,
            Err(error) => {
                self.problems.error(error);
                return self.error_expr(location);
            }
        };
        let arguments_types = arguments.iter().map(|a| a.type_.clone()).collect();
        let type_ = fn_(arguments_types, body.last().type_());

        // Defining an anonymous function never panics.
        self.already_warned_for_unreachable_code = already_warned_for_unreachable_code;
        self.previous_panics = false;

        let function_purity = self.purity;
        self.purity = outer_purity;

        TypedExpr::Fn {
            location,
            type_,
            kind,
            arguments,
            body,
            return_annotation,
            purity: function_purity,
        }
    }

    fn infer_arg(
        &mut self,
        arg: UntypedArg,
        expected: Option<Arc<Type>>,
    ) -> Result<TypedArg, Error> {
        let Arg {
            names,
            annotation,
            location,
            ..
        } = arg;
        let type_ = annotation
            .clone()
            .map(|type_| self.type_from_ast(&type_))
            .unwrap_or_else(|| Ok(self.new_unbound_var()))?;

        match &names {
            ArgNames::Named { .. } | ArgNames::NamedLabelled { .. } => (),
            ArgNames::Discard { name, .. } | ArgNames::LabelledDiscard { name, .. } => {
                let _ = self
                    .environment
                    .discarded_names
                    .insert(name.clone(), location);
            }
        }

        // If we know the expected type of the argument from its contextual
        // usage then unify the newly constructed type with the expected type.
        // We do this here because then there is more type information for the
        // function being type checked, resulting in better type errors and the
        // record field access syntax working.
        if let Some(expected) = expected {
            unify(expected, type_.clone()).map_err(|e| convert_unify_error(e, location))?;
        }

        Ok(Arg {
            names,
            location,
            annotation,
            type_,
        })
    }

    fn infer_call(
        &mut self,
        fun: UntypedExpr,
        arguments: Vec<CallArg<UntypedExpr>>,
        location: SrcSpan,
        kind: CallKind,
    ) -> TypedExpr {
        let (fun, arguments, type_) = self.do_infer_call(fun, arguments, location, kind);

        // One common mistake is to think that the syntax for adding a message
        // to a `todo` or a `panic` exception is to `todo("...")`, but really
        // this does nothing as the `todo` or `panic` throws the exception
        // before it gets to the function call `("...")`.
        // If we find code doing this then emit a warning.
        let todopanic = match fun {
            TypedExpr::Todo { .. } => Some((location, TodoOrPanic::Todo)),
            TypedExpr::Panic { .. } => Some((location, TodoOrPanic::Panic)),
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
            | TypedExpr::Echo { .. }
            | TypedExpr::BitArray { .. }
            | TypedExpr::RecordUpdate { .. }
            | TypedExpr::NegateBool { .. }
            | TypedExpr::NegateInt { .. }
            | TypedExpr::Invalid { .. } => None,
        };
        if let Some((location, kind)) = todopanic {
            let arguments_location = match (arguments.first(), arguments.last()) {
                (Some(first), Some(last)) => Some(SrcSpan {
                    start: first.location().start,
                    end: last.location().end,
                }),
                _ => None,
            };
            self.problems.warning(Warning::TodoOrPanicUsedAsFunction {
                kind,
                location,
                arguments_location,
                arguments: arguments.len(),
            });
        }

        self.purity = self.purity.merge(fun.called_function_purity());

        TypedExpr::Call {
            location,
            type_,
            arguments,
            fun: Box::new(fun),
        }
    }

    fn infer_list(
        &mut self,
        elements: Vec<UntypedExpr>,
        tail: Option<Box<UntypedExpr>>,
        location: SrcSpan,
    ) -> TypedExpr {
        let type_ = self.new_unbound_var();
        // Type check each elements
        let mut inferred_elements = Vec::with_capacity(elements.len());
        for element in elements {
            let element = self.infer(element);
            if let Err(error) = unify(type_.clone(), element.type_()) {
                self.problems.error(convert_unify_error(
                    error.list_element_mismatch(),
                    element.location(),
                ))
            };
            inferred_elements.push(element);
        }

        // Type check the ..tail, if there is one
        let type_ = list(type_);
        let tail = match tail {
            Some(tail) => {
                let tail = self.infer(*tail);
                // Ensure the tail has the same type as the preceding elements
                if let Err(error) = unify(type_.clone(), tail.type_()) {
                    self.problems.error(convert_unify_error(
                        error.list_tail_mismatch(),
                        tail.location(),
                    ))
                }
                Some(Box::new(tail))
            }
            None => None,
        };
        TypedExpr::List {
            location,
            type_,
            elements: inferred_elements,
            tail,
        }
    }

    fn infer_tuple(&mut self, elements: Vec<UntypedExpr>, location: SrcSpan) -> TypedExpr {
        let elements = elements
            .into_iter()
            .map(|element| self.infer(element))
            .collect_vec();
        let type_ = tuple(elements.iter().map(HasType::type_).collect_vec());
        TypedExpr::Tuple {
            location,
            elements,
            type_,
        }
    }

    fn infer_var(
        &mut self,
        name: EcoString,
        location: SrcSpan,
        register_reference: ReferenceRegistration,
    ) -> Result<TypedExpr, Error> {
        let constructor =
            self.do_infer_value_constructor(&None, &name, &location, register_reference)?;
        self.narrow_implementations(location, &constructor.variant)?;
        Ok(TypedExpr::Var {
            constructor,
            location,
            name,
        })
    }

    fn narrow_implementations(
        &mut self,
        location: SrcSpan,
        variant: &ValueConstructorVariant,
    ) -> Result<(), Error> {
        let variant_implementations = match variant {
            ValueConstructorVariant::ModuleConstant {
                implementations, ..
            } => implementations,
            ValueConstructorVariant::ModuleFn {
                implementations, ..
            } => implementations,
            ValueConstructorVariant::Record { .. }
            | ValueConstructorVariant::LocalVariable { .. } => return Ok(()),
        };

        self.implementations
            .update_from_use(variant_implementations, &self.current_function_definition);

        if self.environment.target_support.is_enforced()
            // If the value used doesn't have an implementation that can be used
            // for the current target...
            && !variant_implementations.supports(self.environment.target)
            // ... and there is not an external implementation for it
            && !self
                    .current_function_definition
                    .has_external_for_target(self.environment.target)
        {
            Err(Error::UnsupportedExpressionTarget {
                target: self.environment.target,
                location,
            })
        } else {
            Ok(())
        }
    }

    /// Attempts to infer a record access. If the attempt fails, then will fallback to attempting to infer a module access.
    /// If both fail, then the error from the record access will be used.
    fn infer_field_access(
        &mut self,
        container: UntypedExpr,

        // The SrcSpan of the entire field access:
        // ```gleam
        //    wibble.wobble
        // // ^^^^^^^^^^^^^ This
        // ```
        //
        location: SrcSpan,
        label: EcoString,

        // The SrcSpan of the selection label:
        // ```gleam
        //    wibble.wobble
        // // ^^^^^^ This
        // ```
        //
        label_location: SrcSpan,
        usage: FieldAccessUsage,
    ) -> TypedExpr {
        let container_location = container.location();

        // Computes a potential module access. This will be used if a record access can't be used.
        // Computes both the inferred access and if it shadows a variable.
        let module_access = if let UntypedExpr::Var { name, .. } = &container {
            let module_access =
                self.infer_module_access(name, label.clone(), &container_location, label_location);
            // Returns the result and if it shadows an existing variable in scope
            Some((module_access, self.environment.scope.contains_key(name)))
        } else {
            None
        };
        let record = if let UntypedExpr::Var { location, name } = container {
            // If the left-hand-side of the record access is a variable, this might actually be
            // module access. In that case, we only want to register a reference to the variable
            // if we actually referencing it in the record access.
            self.infer_var(name, location, ReferenceRegistration::DoNotRegister)
        } else {
            self.infer_or_error(container)
        };
        // TODO: is this clone avoidable? we need to box the record for inference in both
        // the success case and in the valid record but invalid label case
        let record_access = match record.clone() {
            Ok(record) => self.infer_known_record_expression_access(
                record,
                label.clone(),
                location,
                label_location,
                container_location.start,
                usage,
            ),
            Err(e) => Err(e),
        };
        match (record_access, module_access) {
            // Record access is valid
            (Ok(record_access), _) => {
                // If this is actually record access and not module access, and we didn't register
                // the reference earlier, we register it now.
                if let TypedExpr::RecordAccess { record, .. } = &record_access
                    && let TypedExpr::Var {
                        location,
                        constructor,
                        name,
                    } = record.as_ref()
                {
                    self.register_value_constructor_reference(
                        name,
                        &constructor.variant,
                        *location,
                        ReferenceKind::Unqualified,
                    )
                }
                record_access
            }
            // Record access is invalid but module access is valid
            (
                _,
                Some((
                    Ok(TypedExpr::ModuleSelect {
                        location,
                        field_start,
                        type_,
                        label,
                        module_name,
                        module_alias,
                        constructor,
                    }),
                    _,
                )),
            ) => {
                // We only register the reference here, if we know that this is a module access.
                // Otherwise we would register module access even if we are actually accessing
                // the field on a record
                self.environment.references.register_value_reference(
                    module_name.clone(),
                    label.clone(),
                    &label,
                    SrcSpan::new(field_start, location.end),
                    ReferenceKind::Qualified,
                );
                TypedExpr::ModuleSelect {
                    location,
                    field_start,
                    type_,
                    label,
                    module_name,
                    module_alias,
                    constructor,
                }
            }
            // If module access failed because the module exists but that module does not export
            // the referenced value, we return extra information about the invalid module select,
            // so that we have information about the attempted module select and can use it, for
            // example, in the "Generate function" code action to support other modules.
            (
                _,
                Some((
                    Err(Error::UnknownModuleValue {
                        name,
                        module_name,
                        location,
                        value_constructors,
                        type_with_same_name,
                        context,
                    }),
                    false,
                )),
            ) => {
                self.problems.error(Error::UnknownModuleValue {
                    name: name.clone(),
                    module_name: module_name.clone(),
                    location,
                    value_constructors,
                    type_with_same_name,
                    context,
                });
                TypedExpr::Invalid {
                    location,
                    type_: self.new_unbound_var(),
                    extra_information: Some(InvalidExpression::ModuleSelect { module_name, label }),
                }
            }
            // If module access failed for some other reason, and no local variable shadows the
            // module, we just return an invalid expression.
            (_, Some((Err(module_access_err), false))) => {
                self.problems.error(module_access_err);
                TypedExpr::Invalid {
                    location,
                    type_: self.new_unbound_var(),
                    extra_information: None,
                }
            }
            // In any other case use the record access for the error
            (Err(record_access_err), _) => {
                self.problems.error(record_access_err);
                match record {
                    // If the record is valid then use a placeholder access
                    // This allows autocomplete to know a record access is being attempted
                    // Even if the access is not valid
                    Ok(record) => TypedExpr::RecordAccess {
                        location,
                        field_start: container_location.start,
                        type_: self.new_unbound_var(),
                        label: "".into(),
                        index: u64::MAX,
                        record: Box::new(record),
                        documentation: None,
                    },
                    Err(_) => TypedExpr::Invalid {
                        location,
                        type_: self.new_unbound_var(),
                        extra_information: None,
                    },
                }
            }
        }
    }

    fn infer_tuple_index(
        &mut self,
        tuple: UntypedExpr,
        index: u64,
        location: SrcSpan,
    ) -> Result<TypedExpr, Error> {
        if let UntypedExpr::TupleIndex { .. } = tuple {
            self.track_feature_usage(FeatureKind::NestedTupleAccess, location);
        }

        let tuple = self.infer_or_error(tuple)?;
        match collapse_links(tuple.type_()).as_ref() {
            Type::Tuple { elements } => {
                let type_ = elements
                    .get(index as usize)
                    .ok_or_else(|| Error::OutOfBoundsTupleIndex {
                        location: SrcSpan {
                            start: tuple.location().end,
                            end: location.end,
                        },
                        index,
                        size: elements.len(),
                    })?
                    .clone();
                Ok(TypedExpr::TupleIndex {
                    location,
                    index,
                    tuple: Box::new(tuple),
                    type_,
                })
            }

            type_ if type_.is_unbound() => Err(Error::NotATupleUnbound {
                location: tuple.location(),
            }),

            Type::Named { .. } | Type::Fn { .. } | Type::Var { .. } => Err(Error::NotATuple {
                location: tuple.location(),
                given: tuple.type_(),
            }),
        }
    }

    fn infer_bit_array(
        &mut self,
        segments: Vec<UntypedExprBitArraySegment>,
        location: SrcSpan,
    ) -> Result<TypedExpr, Error> {
        let segments = segments
            .into_iter()
            .map(|mut segment| {
                // If the segment doesn't have an explicit type option we add a default
                // one ourselves if the pattern is unambiguous: literal strings are
                // implicitly considered utf-8 encoded strings, while floats are
                // implicitly given the float type option.
                if !segment.has_type_option() {
                    match segment.value.as_ref() {
                        UntypedExpr::String { location, .. } => {
                            self.track_feature_usage(
                                FeatureKind::UnannotatedUtf8StringSegment,
                                *location,
                            );
                            segment.options.push(BitArrayOption::Utf8 {
                                location: SrcSpan::default(),
                            });
                        }

                        UntypedExpr::Float { location, .. } => {
                            self.track_feature_usage(
                                FeatureKind::UnannotatedFloatSegment,
                                *location,
                            );
                            segment.options.push(BitArrayOption::Float {
                                location: SrcSpan::default(),
                            })
                        }

                        UntypedExpr::Int { .. }
                        | UntypedExpr::Block { .. }
                        | UntypedExpr::Var { .. }
                        | UntypedExpr::Fn { .. }
                        | UntypedExpr::List { .. }
                        | UntypedExpr::Call { .. }
                        | UntypedExpr::BinOp { .. }
                        | UntypedExpr::PipeLine { .. }
                        | UntypedExpr::Case { .. }
                        | UntypedExpr::FieldAccess { .. }
                        | UntypedExpr::Tuple { .. }
                        | UntypedExpr::TupleIndex { .. }
                        | UntypedExpr::Todo { .. }
                        | UntypedExpr::Panic { .. }
                        | UntypedExpr::Echo { .. }
                        | UntypedExpr::BitArray { .. }
                        | UntypedExpr::RecordUpdate { .. }
                        | UntypedExpr::NegateBool { .. }
                        | UntypedExpr::NegateInt { .. } => (),
                    }
                }

                let segment = self.infer_bit_segment(
                    *segment.value,
                    segment.options,
                    segment.location,
                    |env, expr| env.infer_or_error(expr),
                );

                if let Ok(segment) = &segment {
                    // If we could successfully infer the segment we need to
                    // check if it's `size` option uses any feature that has to
                    // be tracked!
                    self.check_segment_size_expression(&segment.options);
                };

                segment
            })
            .try_collect()?;

        Ok(TypedExpr::BitArray {
            location,
            segments,
            type_: bit_array(),
        })
    }

    fn infer_constant_bit_array(
        &mut self,
        segments: Vec<UntypedConstantBitArraySegment>,
        location: SrcSpan,
    ) -> Result<TypedConstant, Error> {
        let segments = segments
            .into_iter()
            .map(|mut segment| {
                // If the segment doesn't have an explicit type option we add a default
                // one ourselves if the pattern is unambiguous: literal strings are
                // implicitly considered utf-8 encoded strings, while floats are
                // implicitly given the float type option.
                if !segment.has_type_option() {
                    match segment.value.as_ref() {
                        Constant::String { location, .. } => {
                            self.track_feature_usage(
                                FeatureKind::UnannotatedUtf8StringSegment,
                                *location,
                            );
                            segment.options.push(BitArrayOption::Utf8 {
                                location: SrcSpan::default(),
                            });
                        }

                        Constant::Float { location, .. } => {
                            self.track_feature_usage(
                                FeatureKind::UnannotatedFloatSegment,
                                *location,
                            );
                            segment.options.push(BitArrayOption::Float {
                                location: SrcSpan::default(),
                            })
                        }

                        Constant::Int { .. }
                        | Constant::Tuple { .. }
                        | Constant::List { .. }
                        | Constant::Record { .. }
                        | Constant::RecordUpdate { .. }
                        | Constant::BitArray { .. }
                        | Constant::Var { .. }
                        | Constant::StringConcatenation { .. }
                        | Constant::Invalid { .. } => (),
                    }
                }

                let segment = self.infer_bit_segment(
                    *segment.value,
                    segment.options,
                    segment.location,
                    |env, expr| Ok(env.infer_const(&None, expr)),
                );

                if let Ok(segment) = &segment {
                    // If we could successfully infer the segment we need to
                    // check if it's `size` option uses any feature that has to
                    // be tracked!
                    self.check_constant_segment_size_expression(&segment.options);
                }

                segment
            })
            .try_collect()?;

        Ok(Constant::BitArray { location, segments })
    }

    fn infer_bit_segment<UntypedValue, TypedValue, InferFn>(
        &mut self,
        value: UntypedValue,
        options: Vec<BitArrayOption<UntypedValue>>,
        location: SrcSpan,
        mut infer: InferFn,
    ) -> Result<BitArraySegment<TypedValue, Arc<Type>>, Error>
    where
        InferFn: FnMut(&mut Self, UntypedValue) -> Result<TypedValue, Error>,
        TypedValue: HasType + HasLocation + Clone + bit_array::GetLiteralValue,
    {
        let value = infer(self, value)?;

        let infer_option = |segment_option: BitArrayOption<UntypedValue>| {
            infer_bit_array_option(segment_option, |value, type_| {
                let typed_value = infer(self, value)?;
                unify(type_, typed_value.type_())
                    .map_err(|e| convert_unify_error(e, typed_value.location()))?;
                Ok(typed_value)
            })
        };

        let options: Vec<_> = options.into_iter().map(infer_option).try_collect()?;

        let type_ = bit_array::type_options_for_value(&options, self.environment.target).map_err(
            |error| Error::BitArraySegmentError {
                error: error.error,
                location: error.location,
            },
        )?;

        // Track usage of the unaligned bit arrays feature on JavaScript so that
        // warnings can be emitted if the Gleam version constraint is too low
        if self.environment.target == Target::JavaScript
            && !self.current_function_definition.has_javascript_external
        {
            for option in options.iter() {
                if let BitArrayOption::<TypedValue>::Size {
                    value, location, ..
                } = option
                {
                    let mut using_unaligned_bit_array = false;

                    if type_.is_int() {
                        match &(**value).as_int_literal() {
                            Some(size) if size % 8 != BigInt::ZERO => {
                                using_unaligned_bit_array = true;
                            }
                            _ => (),
                        }
                    } else if type_.is_bit_array() {
                        using_unaligned_bit_array = true;
                    }

                    if using_unaligned_bit_array {
                        self.track_feature_usage(
                            FeatureKind::JavaScriptUnalignedBitArray,
                            *location,
                        );
                        break;
                    }
                }
            }
        }

        unify(type_.clone(), value.type_())
            .map_err(|e| convert_unify_error(e, value.location()))?;

        let segment = BitArraySegment {
            location,
            type_,
            value: Box::new(value),
            options,
        };

        if let Some(truncation) = segment.check_for_truncated_value() {
            self.problems
                .warning(Warning::BitArraySegmentTruncatedValue {
                    location,
                    truncation,
                });
        }

        Ok(segment)
    }

    /// Same as `self.infer_or_error` but instead of returning a `Result` with an error,
    /// records the error and returns an invalid expression.
    ///
    pub fn infer(&mut self, expression: UntypedExpr) -> TypedExpr {
        let location = expression.location();
        match self.infer_or_error(expression) {
            Ok(result) => result,
            Err(error) => {
                let information = if let Error::UnknownVariable { name, .. } = &error {
                    Some(InvalidExpression::UnknownVariable { name: name.clone() })
                } else {
                    None
                };

                self.problems.error(error);
                self.error_expr_with_information(location, information)
            }
        }
    }

    /// Infers the type of the given function and tries to unify it with the
    /// given type, recording any unification error that might take place.
    /// The typed expression is returned in any case.
    ///
    pub fn infer_and_unify(&mut self, expression: UntypedExpr, type_: Arc<Type>) -> TypedExpr {
        let expression = self.infer(expression);
        if let Err(error) = unify(type_, expression.type_()) {
            self.problems
                .error(convert_unify_error(error, expression.location()))
        }
        expression
    }

    fn infer_binop(
        &mut self,
        name: BinOp,
        name_location: SrcSpan,
        left: UntypedExpr,
        right: UntypedExpr,
        location: SrcSpan,
    ) -> TypedExpr {
        let (input_type, output_type) = match &name {
            BinOp::Eq | BinOp::NotEq => {
                let left = self.infer(left);
                let right = self.infer(right);
                if let Err(error) = unify(left.type_(), right.type_()) {
                    self.problems
                        .error(convert_unify_error(error, right.location()));
                } else {
                    // We only want to warn for redundant comparisons if it
                    // makes sense to compare the two values.
                    // That is, their types should match!
                    self.check_for_redundant_comparison(name, &left, &right, location);
                }

                self.check_for_inefficient_empty_list_check(name, &left, &right, location);

                return TypedExpr::BinOp {
                    location,
                    name,
                    name_location,
                    type_: bool(),
                    left: Box::new(left),
                    right: Box::new(right),
                };
            }
            BinOp::And => (bool(), bool()),
            BinOp::Or => (bool(), bool()),
            BinOp::LtInt => (int(), bool()),
            BinOp::LtEqInt => (int(), bool()),
            BinOp::LtFloat => (float(), bool()),
            BinOp::LtEqFloat => (float(), bool()),
            BinOp::GtEqInt => (int(), bool()),
            BinOp::GtInt => (int(), bool()),
            BinOp::GtEqFloat => (float(), bool()),
            BinOp::GtFloat => (float(), bool()),
            BinOp::AddInt => (int(), int()),
            BinOp::AddFloat => (float(), float()),
            BinOp::SubInt => (int(), int()),
            BinOp::SubFloat => (float(), float()),
            BinOp::MultInt => (int(), int()),
            BinOp::MultFloat => (float(), float()),
            BinOp::DivInt => (int(), int()),
            BinOp::DivFloat => (float(), float()),
            BinOp::RemainderInt => (int(), int()),
            BinOp::Concatenate => (string(), string()),
        };

        let left = self.infer(left);
        let right = self.infer(right);
        let unify_left = unify(input_type.clone(), left.type_());
        let unify_right = unify(input_type.clone(), right.type_());

        if unify_left.is_ok() && unify_right.is_ok() {
            // We only want to warn for redundant comparisons if it makes sense
            // to compare the two values. That is, their types should match!
            self.check_for_redundant_comparison(name, &left, &right, location);
        }

        // There's some common cases in which we can provide nicer error messages:
        // - if we're using a float operator on int values
        // - if we're using an int operator on float values
        // - if we're using `+` on strings
        if name.is_float_operator() && left.type_().is_int() && right.type_().is_int() {
            self.problems.error(Error::FloatOperatorOnInts {
                operator: name,
                location: name_location,
            })
        } else if name.is_int_operator() && left.type_().is_float() && right.type_().is_float() {
            self.problems.error(Error::IntOperatorOnFloats {
                operator: name,
                location: name_location,
            })
        } else if name == BinOp::AddInt && left.type_().is_string() && right.type_().is_string() {
            self.problems.error(Error::StringConcatenationWithAddInt {
                location: name_location,
            })
        } else {
            // In all other cases we just report an error for each of the operands.
            if let Err(error) = unify_left {
                self.problems.error(
                    error
                        .operator_situation(name)
                        .into_error(left.type_defining_location()),
                );
            }
            if let Err(error) = unify_right {
                self.problems.error(
                    error
                        .operator_situation(name)
                        .into_error(right.type_defining_location()),
                );
            }
        }

        self.check_for_inefficient_empty_list_check(name, &left, &right, location);

        TypedExpr::BinOp {
            location,
            name,
            name_location,
            type_: output_type,
            left: Box::new(left),
            right: Box::new(right),
        }
    }

    /// Checks for inefficient usage of `list.length` for checking for the empty list.
    ///
    /// If we find one of these usages, emit a warning to use comparison with empty list instead.
    fn check_for_inefficient_empty_list_check(
        &mut self,
        binop: BinOp,
        left: &TypedExpr,
        right: &TypedExpr,
        location: SrcSpan,
    ) {
        // Look for a call expression as either of the binary operands.
        let fun = match (&left, &right) {
            (TypedExpr::Call { fun, .. }, _) | (_, TypedExpr::Call { fun, .. }) => fun,
            _ => return,
        };

        // Extract the module information from the call expression.
        let (module_name, module_alias, label) = if let TypedExpr::ModuleSelect {
            module_name,
            module_alias,
            label,
            ..
        } = fun.as_ref()
        {
            (module_name, module_alias, label)
        } else {
            return;
        };

        // Check if we have a `list.length` call from `gleam/list`.
        if module_name != "gleam/list" || label != "length" {
            return;
        }

        // Resolve the module against the imported modules we have available.
        let list_module = match self.environment.imported_modules.get(module_alias) {
            Some((_, list_module)) => list_module,
            None => return,
        };

        // Check that we're actually using `list.length` from the standard library.
        if list_module.package != STDLIB_PACKAGE_NAME {
            return;
        }

        // Check the kind of the empty list check so we know whether to recommend
        // `== []` or `!= []` syntax as a replacement.
        let kind = match get_empty_list_check_kind(binop, left, right) {
            Some(kind) => kind,
            None => return,
        };

        // If we've gotten this far, go ahead and emit the warning.
        self.problems
            .warning(Warning::InefficientEmptyListCheck { location, kind });
    }

    fn check_for_redundant_comparison(
        &mut self,
        binop: BinOp,
        left: &TypedExpr,
        right: &TypedExpr,
        location: SrcSpan,
    ) {
        let outcome = match (left, binop, right) {
            (left, BinOp::Eq, right) => match static_compare(left, right) {
                StaticComparison::CertainlyEqual => ComparisonOutcome::AlwaysSucceeds,
                StaticComparison::CertainlyDifferent => ComparisonOutcome::AlwaysFails,
                StaticComparison::CantTell => return,
            },

            (left, BinOp::NotEq, right) => match static_compare(left, right) {
                StaticComparison::CertainlyEqual => ComparisonOutcome::AlwaysFails,
                StaticComparison::CertainlyDifferent => ComparisonOutcome::AlwaysSucceeds,
                StaticComparison::CantTell => return,
            },

            // We special handle int literals as there's other comparisons we
            // might want to perform
            (TypedExpr::Int { int_value: n, .. }, op, TypedExpr::Int { int_value: m, .. }) => {
                match op {
                    BinOp::LtInt if n < m => ComparisonOutcome::AlwaysSucceeds,
                    BinOp::LtInt => ComparisonOutcome::AlwaysFails,
                    BinOp::LtEqInt if n <= m => ComparisonOutcome::AlwaysSucceeds,
                    BinOp::LtEqInt => ComparisonOutcome::AlwaysFails,
                    BinOp::GtInt if n > m => ComparisonOutcome::AlwaysSucceeds,
                    BinOp::GtInt => ComparisonOutcome::AlwaysFails,
                    BinOp::GtEqInt if n >= m => ComparisonOutcome::AlwaysSucceeds,
                    BinOp::GtEqInt => ComparisonOutcome::AlwaysFails,
                    BinOp::And
                    | BinOp::Or
                    | BinOp::Eq
                    | BinOp::NotEq
                    | BinOp::LtFloat
                    | BinOp::LtEqFloat
                    | BinOp::GtEqFloat
                    | BinOp::GtFloat
                    | BinOp::AddInt
                    | BinOp::AddFloat
                    | BinOp::SubInt
                    | BinOp::SubFloat
                    | BinOp::MultInt
                    | BinOp::MultFloat
                    | BinOp::DivInt
                    | BinOp::DivFloat
                    | BinOp::RemainderInt
                    | BinOp::Concatenate => return,
                }
            }

            (
                TypedExpr::Float { float_value: n, .. },
                op,
                TypedExpr::Float { float_value: m, .. },
            ) => match op {
                BinOp::LtFloat if n < m => ComparisonOutcome::AlwaysSucceeds,
                BinOp::LtFloat => ComparisonOutcome::AlwaysFails,
                BinOp::LtEqFloat if n <= m => ComparisonOutcome::AlwaysSucceeds,
                BinOp::LtEqFloat => ComparisonOutcome::AlwaysFails,
                BinOp::GtFloat if n > m => ComparisonOutcome::AlwaysSucceeds,
                BinOp::GtFloat => ComparisonOutcome::AlwaysFails,
                BinOp::GtEqFloat if n >= m => ComparisonOutcome::AlwaysSucceeds,
                BinOp::GtEqFloat => ComparisonOutcome::AlwaysFails,
                BinOp::And
                | BinOp::Or
                | BinOp::Eq
                | BinOp::NotEq
                | BinOp::LtInt
                | BinOp::LtEqInt
                | BinOp::GtEqInt
                | BinOp::GtInt
                | BinOp::AddInt
                | BinOp::AddFloat
                | BinOp::SubInt
                | BinOp::SubFloat
                | BinOp::MultInt
                | BinOp::MultFloat
                | BinOp::DivInt
                | BinOp::DivFloat
                | BinOp::RemainderInt
                | BinOp::Concatenate => return,
            },

            _ => return,
        };

        self.problems
            .warning(Warning::RedundantComparison { location, outcome });
    }

    fn infer_assignment(&mut self, assignment: UntypedAssignment) -> TypedAssignment {
        let Assignment {
            pattern,
            value,
            kind,
            annotation,
            location,
            compiled_case: _,
        } = assignment;
        let value = self.expr_in_new_scope(|this| this.infer(value));
        let type_ = value.type_();
        let kind = self.infer_assignment_kind(kind.clone());

        // Ensure the pattern matches the type of the value
        let mut pattern_typer = pattern::PatternTyper::new(
            self.environment,
            &self.current_function_definition,
            &self.hydrator,
            self.problems,
            PatternPosition::LetAssignment,
        );

        let pattern = pattern_typer.infer_single_pattern(pattern, &value);

        let minimum_required_version = pattern_typer.minimum_required_version;
        if minimum_required_version > self.minimum_required_version {
            self.minimum_required_version = minimum_required_version;
        }

        let pattern_typechecked_successfully = !pattern_typer.error_encountered;

        // Check that any type annotation is accurate.
        if let Some(annotation) = &annotation {
            match self
                .type_from_ast(annotation)
                .map(|type_| self.instantiate(type_, &mut hashmap![]))
            {
                Ok(annotated_type) => {
                    if let Err(error) = unify(annotated_type, type_.clone())
                        .map_err(|e| convert_unify_error(e, value.type_defining_location()))
                    {
                        self.problems.error(error);
                    }
                }
                Err(error) => {
                    self.problems.error(error);
                }
            }
        }

        // The exhaustiveness checker expects patterns to be valid and to type check;
        // if they are invalid, it will crash. Therefore, if any errors were found
        // when type checking the pattern, we don't perform the exhaustiveness check.
        if !pattern_typechecked_successfully {
            return Assignment {
                location,
                annotation,
                kind,
                compiled_case: CompiledCase::failure(),
                pattern,
                value,
            };
        }

        let (output, not_exhaustive_error) =
            self.check_let_exhaustiveness(location, value.type_(), &pattern);

        match (&kind, not_exhaustive_error) {
            // The pattern is exhaustive in a let assignment, there's no problem here.
            (AssignmentKind::Let | AssignmentKind::Generated, Ok(_)) => (),

            // If the pattern is not exhaustive and we're not asserting we want to
            // report the error!
            (AssignmentKind::Let | AssignmentKind::Generated, Err(e)) => {
                self.problems.error(e);
            }

            // If we're asserting but the pattern already covers all cases then the
            // `assert` is redundant and can be safely removed.
            (
                AssignmentKind::Assert {
                    location,
                    assert_keyword_start,
                    ..
                },
                Ok(_),
            ) => self.problems.warning(Warning::RedundantAssertAssignment {
                location: SrcSpan::new(*assert_keyword_start, location.end),
            }),

            // Otherwise, if the pattern is never reachable (through variant inference),
            // we can warn the user about this.
            (AssignmentKind::Assert { .. }, Err(_)) => {
                // There is only one pattern to match, so it is index 0
                match output.is_reachable(0, 0) {
                    Reachability::Unreachable(UnreachablePatternReason::ImpossibleVariant) => self
                        .problems
                        .warning(Warning::AssertAssignmentOnImpossiblePattern {
                            location: pattern.location(),
                            reason: AssertImpossiblePattern::InferredVariant,
                        }),

                    Reachability::Unreachable(UnreachablePatternReason::ImpossibleSegments(
                        segments,
                    )) => self
                        .problems
                        .warning(Warning::AssertAssignmentOnImpossiblePattern {
                            location: pattern.location(),
                            reason: AssertImpossiblePattern::ImpossibleSegments { segments },
                        }),
                    // A duplicate pattern warning should not happen, since there is only one pattern.
                    Reachability::Reachable
                    | Reachability::Unreachable(UnreachablePatternReason::DuplicatePattern) => {}
                }
            }
        };

        Assignment {
            location,
            annotation,
            kind,
            compiled_case: output.compiled_case,
            pattern,
            value,
        }
    }

    fn infer_assignment_kind(
        &mut self,
        kind: AssignmentKind<UntypedExpr>,
    ) -> AssignmentKind<TypedExpr> {
        match kind {
            AssignmentKind::Let => AssignmentKind::Let,
            AssignmentKind::Generated => AssignmentKind::Generated,
            AssignmentKind::Assert {
                location,
                message,
                assert_keyword_start,
            } => {
                self.purity = Purity::Impure;
                let message = match message {
                    Some(message) => {
                        self.track_feature_usage(
                            FeatureKind::LetAssertWithMessage,
                            message.location(),
                        );
                        Some(self.infer_and_unify(message, string()))
                    }
                    None => None,
                };
                AssignmentKind::Assert {
                    location,
                    message,
                    assert_keyword_start,
                }
            }
        }
    }

    fn infer_assert(&mut self, assert: UntypedAssert) -> TypedAssert {
        let Assert {
            value,
            location,
            message,
        } = assert;
        let value_location = value.location();

        let value = self.infer(value);

        self.purity = Purity::Impure;

        if value.is_known_bool() {
            self.problems.warning(Warning::AssertLiteralBool {
                location: value_location,
            });
        }

        match unify(bool(), value.type_()) {
            Ok(()) => {}
            Err(error) => self
                .problems
                .error(convert_unify_error(error, value_location)),
        }

        let message = message.map(|message| self.infer_and_unify(message, string()));
        self.track_feature_usage(FeatureKind::BoolAssert, location);

        Assert {
            location,
            value,
            message,
        }
    }

    fn infer_case(
        &mut self,
        subjects: Vec<UntypedExpr>,
        clauses: Option<Vec<UntypedClause>>,
        location: SrcSpan,
    ) -> TypedExpr {
        let subjects_count = subjects.len();
        let mut typed_subjects = Vec::with_capacity(subjects_count);
        let mut subject_types = Vec::with_capacity(subjects_count);

        let return_type = self.new_unbound_var();

        self.previous_panics = false;
        let mut any_subject_panics = false;
        for subject in subjects {
            let subject = self.expr_in_new_scope(|this| this.infer(subject));
            any_subject_panics = any_subject_panics || self.previous_panics;
            subject_types.push(subject.type_());
            typed_subjects.push(subject);
        }

        let clauses = match clauses {
            Some(clauses) => clauses,
            None => {
                self.problems.error(Error::MissingCaseBody { location });
                return TypedExpr::Case {
                    location,
                    type_: return_type,
                    compiled_case: CompiledCase::failure(),
                    subjects: typed_subjects,
                    clauses: Vec::new(),
                };
            }
        };

        let mut typed_clauses = Vec::with_capacity(clauses.len());
        let mut has_a_guard = false;
        let mut all_patterns_are_discards = true;
        // NOTE: if there are 0 clauses then there are 0 panics
        let mut all_clauses_panic = !clauses.is_empty();
        let mut patterns_typechecked_successfully = true;

        for clause in clauses {
            has_a_guard = has_a_guard || clause.guard.is_some();
            all_patterns_are_discards =
                all_patterns_are_discards && clause.pattern.iter().all(|p| p.is_discard());

            self.previous_panics = false;
            let (typed_clause, error_typing_patterns) = self.infer_clause(clause, &typed_subjects);
            if error_typing_patterns {
                patterns_typechecked_successfully = false
            }
            all_clauses_panic = all_clauses_panic && self.previous_panics;

            if let Err(error) = unify(return_type.clone(), typed_clause.then.type_()) {
                self.problems.error(
                    error
                        .case_clause_mismatch(typed_clause.location)
                        .into_error(typed_clause.then.type_defining_location()),
                );
            }
            typed_clauses.push(typed_clause);
        }

        self.previous_panics = all_clauses_panic || any_subject_panics;

        // The exhaustiveness checker expects patterns to be valid and to type check;
        // if they are invalid, it will crash. Therefore, if any errors were found
        // when type checking the pattern, we don't perform the exhaustiveness check.
        let compiled_case = if patterns_typechecked_successfully {
            self.check_case_exhaustiveness(location, &subject_types, &typed_clauses)
        } else {
            CompiledCase::failure()
        };

        // We track if the case expression is used like an if: that is all its
        // patterns are discarded and there's at least a guard. For example:
        // ```gleam
        // case True {
        //   _ if condition -> todo
        //   _ if other_condition -> todo
        //   _ -> todo
        // }
        // ```
        // This is the only case were it actually makes sense to match on a
        // constant value so when checking, we won't emit warnings for matching
        // on a literal value in this case.
        let case_used_like_if = all_patterns_are_discards && has_a_guard;
        typed_subjects
            .iter()
            .filter_map(|subject| check_subject_for_redundant_match(subject, case_used_like_if))
            .for_each(|warning| self.problems.warning(warning));

        TypedExpr::Case {
            location,
            compiled_case,
            type_: return_type,
            subjects: typed_subjects,
            clauses: typed_clauses,
        }
    }

    /// Returns a tuple with the typed clause and a bool that is true if an error
    /// was encountered while typing the clause patterns.
    ///
    fn infer_clause(
        &mut self,
        clause: UntypedClause,
        subjects: &[TypedExpr],
    ) -> (TypedClause, bool) {
        let Clause {
            pattern,
            alternative_patterns,
            guard,
            then,
            location,
        } = clause;
        self.value_in_new_scope(|this| {
            let (typed_pattern, typed_alternatives, error_encountered) =
                this.infer_clause_pattern(pattern, alternative_patterns, subjects, &location);

            let guard = match this.infer_optional_clause_guard(guard) {
                Ok(guard) => guard,
                // If an error occurs inferring guard then assume no guard
                Err(error) => {
                    this.problems.error(error);
                    None
                }
            };
            let then = this.infer(then);
            let clause = Clause {
                location,
                pattern: typed_pattern,
                alternative_patterns: typed_alternatives,
                guard,
                then,
            };
            (clause, error_encountered)
        })
    }

    fn infer_clause_pattern(
        &mut self,
        pattern: UntypedMultiPattern,
        alternatives: Vec<UntypedMultiPattern>,
        subjects: &[TypedExpr],
        location: &SrcSpan,
    ) -> (TypedMultiPattern, Vec<TypedMultiPattern>, bool) {
        let mut pattern_typer = pattern::PatternTyper::new(
            self.environment,
            &self.current_function_definition,
            &self.hydrator,
            self.problems,
            PatternPosition::CaseClause,
        );

        let typed_pattern = pattern_typer.infer_multi_pattern(pattern, subjects);

        // Each case clause has one or more patterns that may match the
        // subject in order for the clause to be selected, so we must type
        // check every pattern.
        let mut typed_alternatives = Vec::with_capacity(alternatives.len());
        for m in alternatives {
            typed_alternatives
                .push(pattern_typer.infer_alternative_multi_pattern(m, subjects, location));
        }

        let minimum_required_version = pattern_typer.minimum_required_version;
        if minimum_required_version > self.minimum_required_version {
            self.minimum_required_version = minimum_required_version;
        }

        (
            typed_pattern,
            typed_alternatives,
            pattern_typer.error_encountered,
        )
    }

    fn infer_optional_clause_guard(
        &mut self,
        guard: Option<UntypedClauseGuard>,
    ) -> Result<Option<TypedClauseGuard>, Error> {
        match guard {
            // If there is no guard we do nothing
            None => Ok(None),

            // If there is a guard we assert that it is of type Bool
            Some(guard) => {
                let guard = self.infer_clause_guard(guard)?;
                unify(bool(), guard.type_())
                    .map_err(|e| convert_unify_error(e, guard.location()))?;
                Ok(Some(guard))
            }
        }
    }

    fn infer_clause_guard(&mut self, guard: UntypedClauseGuard) -> Result<TypedClauseGuard, Error> {
        match guard {
            ClauseGuard::Var { location, name, .. } => {
                let constructor = self.infer_value_constructor(&None, &name, &location)?;

                // We cannot support all values in guard expressions as the BEAM does not
                let definition_location = match &constructor.variant {
                    ValueConstructorVariant::LocalVariable { location, .. } => *location,
                    ValueConstructorVariant::ModuleFn { .. }
                    | ValueConstructorVariant::Record { .. } => {
                        return Err(Error::NonLocalClauseGuardVariable { location, name });
                    }

                    ValueConstructorVariant::ModuleConstant { literal, .. } => {
                        return Ok(ClauseGuard::Constant(literal.clone()));
                    }
                };

                Ok(ClauseGuard::Var {
                    location,
                    name,
                    type_: constructor.type_,
                    definition_location,
                })
            }

            ClauseGuard::TupleIndex {
                location,
                tuple,
                index,
                ..
            } => {
                let tuple = self.infer_clause_guard(*tuple)?;
                match tuple.type_().as_ref() {
                    Type::Tuple { elements } => {
                        let type_ = elements
                            .get(index as usize)
                            .ok_or(Error::OutOfBoundsTupleIndex {
                                location,
                                index,
                                size: elements.len(),
                            })?
                            .clone();
                        Ok(ClauseGuard::TupleIndex {
                            location,
                            index,
                            type_,
                            tuple: Box::new(tuple),
                        })
                    }

                    type_ if type_.is_unbound() => Err(Error::NotATupleUnbound {
                        location: tuple.location(),
                    }),

                    Type::Named { .. } | Type::Fn { .. } | Type::Var { .. } => {
                        Err(Error::NotATuple {
                            location: tuple.location(),
                            given: tuple.type_(),
                        })
                    }
                }
            }

            ClauseGuard::FieldAccess {
                label_location,
                label,
                container,
                index: _,
                type_: (),
            } => match self.infer_clause_guard(*container.clone()) {
                Ok(container) => self.infer_guard_record_access(container, label, label_location),

                Err(err) => {
                    if let ClauseGuard::Var { name, location, .. } = *container {
                        self.infer_guard_module_access(name, label, location, label_location, err)
                    } else {
                        Err(Error::RecordAccessUnknownType {
                            location: label_location,
                        })
                    }
                }
            },

            ClauseGuard::ModuleSelect { location, .. } => {
                Err(Error::RecordAccessUnknownType { location })
            }

            ClauseGuard::Not {
                location,
                expression,
            } => {
                let expression = self.infer_clause_guard(*expression)?;
                unify(bool(), expression.type_())
                    .map_err(|e| convert_unify_error(e, expression.location()))?;
                Ok(ClauseGuard::Not {
                    location,
                    expression: Box::new(expression),
                })
            }

            ClauseGuard::Constant(constant) => {
                Ok(ClauseGuard::Constant(self.infer_const(&None, constant)))
            }

            ClauseGuard::Block { value, location } => {
                let value = self.infer_clause_guard(*value)?;
                Ok(ClauseGuard::Block {
                    location,
                    value: Box::new(value),
                })
            }

            ClauseGuard::BinaryOperator {
                location,
                operator,
                left,
                right,
            } => {
                let left = self.infer_clause_guard(*left)?;
                let right = self.infer_clause_guard(*right)?;

                match operator {
                    BinOp::And | BinOp::Or => {
                        unify(bool(), left.type_())
                            .map_err(|e| convert_unify_error(e, left.location()))?;
                        unify(bool(), right.type_())
                            .map_err(|e| convert_unify_error(e, right.location()))?;
                    }

                    BinOp::Eq | BinOp::NotEq => {
                        unify(left.type_(), right.type_())
                            .map_err(|e| convert_unify_error(e, location))?;
                    }

                    BinOp::GtInt
                    | BinOp::GtEqInt
                    | BinOp::LtInt
                    | BinOp::LtEqInt
                    | BinOp::AddInt
                    | BinOp::SubInt
                    | BinOp::DivInt
                    | BinOp::MultInt
                    | BinOp::RemainderInt => {
                        self.track_feature_usage(FeatureKind::ArithmeticInGuards, location);

                        if left.type_().is_float() && right.type_().is_float() {
                            return Err(Error::IntOperatorOnFloats { operator, location });
                        }

                        unify(int(), left.type_())
                            .map_err(|e| convert_unify_error(e, left.location()))?;
                        unify(int(), right.type_())
                            .map_err(|e| convert_unify_error(e, right.location()))?;
                    }

                    BinOp::GtFloat
                    | BinOp::GtEqFloat
                    | BinOp::LtFloat
                    | BinOp::LtEqFloat
                    | BinOp::AddFloat
                    | BinOp::SubFloat
                    | BinOp::DivFloat
                    | BinOp::MultFloat => {
                        self.track_feature_usage(FeatureKind::ArithmeticInGuards, location);

                        if left.type_().is_int() && right.type_().is_int() {
                            return Err(Error::FloatOperatorOnInts { operator, location });
                        }

                        unify(float(), left.type_())
                            .map_err(|e| convert_unify_error(e, left.location()))?;
                        unify(float(), right.type_())
                            .map_err(|e| convert_unify_error(e, right.location()))?;
                    }

                    BinOp::Concatenate => {
                        self.track_feature_usage(FeatureKind::ConcatenateInGuards, location);

                        unify(string(), left.type_())
                            .map_err(|e| convert_unify_error(e, left.location()))?;
                        unify(string(), right.type_())
                            .map_err(|e| convert_unify_error(e, right.location()))?;
                    }
                }

                Ok(ClauseGuard::BinaryOperator {
                    location,
                    operator,
                    left: Box::new(left),
                    right: Box::new(right),
                })
            }
        }
    }

    fn infer_guard_record_access(
        &mut self,
        container: TypedClauseGuard,
        label: EcoString,
        location: SrcSpan,
    ) -> Result<TypedClauseGuard, Error> {
        let container = Box::new(container);
        let container_type = container.type_();
        let RecordAccessor {
            index,
            label,
            type_,
            documentation: _,
        } = self.infer_known_record_access(
            container_type,
            container.location(),
            FieldAccessUsage::Other,
            location,
            label,
        )?;
        Ok(ClauseGuard::FieldAccess {
            container,
            label,
            index: Some(index),
            label_location: location,
            type_,
        })
    }

    fn infer_guard_module_access(
        &mut self,
        name: EcoString,
        label: EcoString,
        module_location: SrcSpan,
        label_location: SrcSpan,
        record_access_error: Error,
    ) -> Result<TypedClauseGuard, Error> {
        let module_access = self
            .infer_module_access(&name, label, &module_location, label_location)
            .and_then(|ma| {
                if let TypedExpr::ModuleSelect {
                    location,
                    field_start: _,
                    type_,
                    label,
                    module_name,
                    module_alias,
                    constructor,
                } = ma
                {
                    match constructor {
                        ModuleValueConstructor::Constant { literal, .. } => {
                            self.environment.references.register_value_reference(
                                module_name.clone(),
                                label.clone(),
                                &label,
                                label_location,
                                ReferenceKind::Qualified,
                            );

                            Ok(ClauseGuard::ModuleSelect {
                                location,
                                type_,
                                label,
                                module_name,
                                module_alias,
                                literal,
                            })
                        }

                        ModuleValueConstructor::Record { .. }
                        | ModuleValueConstructor::Fn { .. } => {
                            Err(Error::RecordAccessUnknownType { location })
                        }
                    }
                } else {
                    Err(Error::RecordAccessUnknownType {
                        location: module_location,
                    })
                }
            });

        // If the name is in the environment, use the original error from
        // inferring the record access, so that we can suggest possible
        // misspellings of field names
        if self.environment.scope.contains_key(&name) {
            module_access.map_err(|_| record_access_error)
        } else {
            module_access
        }
    }

    fn infer_module_access(
        &mut self,
        // This is the name of the module coming before the `.`: for example
        // in `result.try` it's `result`.
        module_alias: &EcoString,
        label: EcoString,
        module_location: &SrcSpan,
        select_location: SrcSpan,
    ) -> Result<TypedExpr, Error> {
        let location = module_location.merge(&select_location);

        let (module_name, constructor) = {
            let (_, module) = self
                .environment
                .imported_modules
                .get(module_alias)
                .ok_or_else(|| Error::UnknownModule {
                    name: module_alias.clone(),
                    location: *module_location,
                    suggestions: self
                        .environment
                        .suggest_modules(module_alias, Imported::Value(label.clone())),
                })?;

            let constructor =
                module
                    .get_public_value(&label)
                    .ok_or_else(|| Error::UnknownModuleValue {
                        name: label.clone(),
                        location: select_location,
                        module_name: module.name.clone(),
                        value_constructors: module.public_value_names(),
                        type_with_same_name: module.get_public_type(&label).is_some(),
                        context: ModuleValueUsageContext::ModuleAccess,
                    })?;

            // Emit a warning if the value being used is deprecated.
            if let Deprecation::Deprecated { message } = &constructor.deprecation {
                self.problems.warning(Warning::DeprecatedItem {
                    location: select_location,
                    message: message.clone(),
                    layer: Layer::Value,
                })
            }

            self.environment
                .references
                .register_module_reference(module_alias.clone());

            (module.name.clone(), constructor.clone())
        };

        let type_ = self.instantiate(constructor.type_, &mut hashmap![]);

        self.narrow_implementations(select_location, &constructor.variant)?;

        let constructor = match &constructor.variant {
            variant @ ValueConstructorVariant::ModuleFn { name, module, .. } => {
                variant.to_module_value_constructor(Arc::clone(&type_), module, name)
            }

            variant @ (ValueConstructorVariant::LocalVariable { .. }
            | ValueConstructorVariant::ModuleConstant { .. }
            | ValueConstructorVariant::Record { .. }) => {
                variant.to_module_value_constructor(Arc::clone(&type_), &module_name, &label)
            }
        };

        Ok(TypedExpr::ModuleSelect {
            location,
            field_start: select_location.start,
            label,
            type_: Arc::clone(&type_),
            module_name,
            module_alias: module_alias.clone(),
            constructor,
        })
    }

    fn infer_known_record_expression_access(
        &mut self,
        record: TypedExpr,
        label: EcoString,
        location: SrcSpan,
        label_location: SrcSpan,
        field_start: u32,
        usage: FieldAccessUsage,
    ) -> Result<TypedExpr, Error> {
        let record = Box::new(record);
        let record_type = record.type_();
        let RecordAccessor {
            index,
            label,
            type_,
            documentation,
        } = self.infer_known_record_access(
            record_type,
            record.location(),
            usage,
            label_location,
            label,
        )?;
        Ok(TypedExpr::RecordAccess {
            record,
            label,
            field_start,
            index,
            location,
            type_,
            documentation,
        })
    }

    fn infer_known_record_access(
        &mut self,
        record_type: Arc<Type>,
        record_location: SrcSpan,
        usage: FieldAccessUsage,
        location: SrcSpan,
        label: EcoString,
    ) -> Result<RecordAccessor, Error> {
        if record_type.is_unbound() {
            return Err(Error::RecordAccessUnknownType {
                location: record_location,
            });
        }

        let unknown_field = |fields| {
            self.unknown_field_error(fields, record_type.clone(), location, label.clone(), usage)
        };
        let (accessors_map, variant_accessors) = match collapse_links(record_type.clone()).as_ref()
        {
            // A type in the current module which may have fields
            Type::Named {
                module,
                name,
                inferred_variant,
                ..
            } if module == &self.environment.current_module => {
                self.environment.accessors.get(name).map(|accessors_map| {
                    (
                        accessors_map,
                        accessors_map.accessors_for_variant(*inferred_variant),
                    )
                })
            }

            // A type in another module which may have fields
            Type::Named {
                module,
                name,
                inferred_variant,
                ..
            } => self
                .environment
                .importable_modules
                .get(module)
                .and_then(|module| module.accessors.get(name))
                .filter(|a| {
                    a.publicity.is_importable() || module == &self.environment.current_module
                })
                .map(|accessors_map| {
                    (
                        accessors_map,
                        accessors_map.accessors_for_variant(*inferred_variant),
                    )
                }),

            // Non-named types do not have fields
            Type::Fn { .. } | Type::Var { .. } | Type::Tuple { .. } => {
                return Err(unknown_field(vec![]));
            }
        }
        .ok_or_else(|| unknown_field(vec![]))?;

        let RecordAccessor {
            index,
            label,
            type_,
            documentation,
        } = variant_accessors
            .get(&label)
            .ok_or_else(|| unknown_field(variant_accessors.keys().cloned().collect()))?
            .clone();

        let accessor_record_type = accessors_map.type_.clone();

        // If the accessor isn't shared across variants, this requires variant inference
        if !accessors_map.shared_accessors.contains_key(&label) {
            match usage {
                FieldAccessUsage::MethodCall | FieldAccessUsage::Other => {
                    self.track_feature_usage(FeatureKind::RecordAccessVariantInference, location);
                }
                // This feature for record updates should be tracked in
                // `infer_record_update`, so we don't track it here as it would lead
                // to a duplicate warning with a confusing message.
                FieldAccessUsage::RecordUpdate => {}
            }
        }

        let mut type_vars = hashmap![];
        let accessor_record_type = self.instantiate(accessor_record_type, &mut type_vars);
        let type_ = self.instantiate(type_, &mut type_vars);
        unify(accessor_record_type, record_type)
            .map_err(|e| convert_unify_error(e, record_location))?;
        Ok(RecordAccessor {
            index,
            label,
            type_,
            documentation,
        })
    }

    fn infer_record_update(
        &mut self,
        constructor: UntypedExpr,
        record: RecordBeingUpdated<UntypedExpr>,
        arguments: Vec<UntypedRecordUpdateArg>,
        location: SrcSpan,
    ) -> Result<TypedExpr, Error> {
        // infer the constructor being used
        let typed_constructor = self.infer_or_error(constructor.clone())?;
        let (module, name) = match &typed_constructor {
            TypedExpr::ModuleSelect {
                module_alias,
                label,
                location,
                ..
            } => (Some((module_alias, location)), label),

            TypedExpr::Var { name, .. } => (None, name),

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
            | TypedExpr::Invalid { .. } => {
                return Err(Error::RecordUpdateInvalidConstructor {
                    location: typed_constructor.location(),
                });
            }
        };

        let value_constructor = self
            .environment
            .get_value_constructor(module.map(|(module, _)| module), name)
            .map_err(|e| {
                convert_get_value_constructor_error(
                    e,
                    location,
                    module.map(|(_, location)| *location),
                )
            })?
            .clone();

        // infer the record being updated
        let record = self.infer_or_error(*record.base)?;
        let record_location = record.location();
        let record_type = record.type_();

        let (record_var, record_assignment) = if record.is_var() {
            (record, None)
        } else {
            // We create an Assignment for the old record expression and will use a Var expression
            // to refer back to it while constructing the arguments.
            let record_assignment = Assignment {
                location: record_location,
                pattern: Pattern::Variable {
                    location: record_location,
                    name: RECORD_UPDATE_VARIABLE.into(),
                    type_: record_type.clone(),
                    origin: VariableOrigin::generated(),
                },
                annotation: None,
                compiled_case: CompiledCase::failure(),
                kind: AssignmentKind::Generated,
                value: record,
            };

            let record_var = TypedExpr::Var {
                location: record_location,
                constructor: ValueConstructor {
                    publicity: Publicity::Private,
                    deprecation: Deprecation::NotDeprecated,
                    type_: record_type,
                    variant: ValueConstructorVariant::LocalVariable {
                        location: record_location,
                        origin: VariableOrigin::generated(),
                    },
                },
                name: RECORD_UPDATE_VARIABLE.into(),
            };
            (record_var, Some(Box::new(record_assignment)))
        };

        // infer the fields of the variant we want to update
        let variant =
            self.infer_record_update_variant(&typed_constructor, &value_constructor, &record_var)?;

        let arguments =
            self.infer_record_update_arguments(&variant, &record_var, arguments, location)?;

        Ok(TypedExpr::RecordUpdate {
            location,
            type_: variant.return_type,
            record_assignment,
            constructor: Box::new(typed_constructor),
            arguments,
        })
    }

    fn infer_record_update_arguments(
        &mut self,
        variant: &RecordUpdateVariant<'_>,
        record: &TypedExpr,
        arguments: Vec<UntypedRecordUpdateArg>,
        location: SrcSpan,
    ) -> Result<Vec<TypedCallArg>, Error> {
        let record_location = record.location();
        let record_type = record.type_();
        let return_type = variant.return_type.clone();

        // We clone the fields to remove all explicitly mentioned fields in the record update.
        let mut fields = variant.field_map.fields.clone();

        // collect explicit arguments given in the record update
        let explicit_arguments = arguments
            .iter()
            .map(
                |arg @ UntypedRecordUpdateArg {
                     label,
                     value,
                     location,
                 }| {
                    let value = self.infer_or_error(value.clone())?;

                    if arg.uses_label_shorthand() {
                        self.track_feature_usage(FeatureKind::LabelShorthandSyntax, *location);
                    }

                    match fields.remove(label) {
                        Some(index) => {
                            unify(variant.arg_type(index), value.type_())
                                .map_err(|e| convert_unify_error(e, *location))?;

                            Ok((
                                index,
                                CallArg {
                                    label: Some(label.clone()),
                                    location: *location,
                                    value,
                                    implicit: None,
                                },
                            ))
                        }
                        _ => {
                            if variant.has_field(label) {
                                Err(Error::DuplicateArgument {
                                    location: *location,
                                    label: label.clone(),
                                })
                            } else {
                                Err(self.unknown_field_error(
                                    variant.field_names(),
                                    record_type.clone(),
                                    *location,
                                    label.clone(),
                                    FieldAccessUsage::RecordUpdate,
                                ))
                            }
                        }
                    }
                },
            )
            .collect::<Result<Vec<_>, _>>()?;

        // Generate the remaining copied arguments, making sure they unify with our return type.
        let convert_incompatible_fields_error = |e: UnifyError, field: RecordField| match e {
            UnifyError::CouldNotUnify {
                expected, given, ..
            } => Error::UnsafeRecordUpdate {
                location: record_location,
                reason: UnsafeRecordUpdateReason::IncompatibleFieldTypes {
                    constructed_variant: return_type.clone(),
                    record_variant: record_type.clone(),
                    expected_field_type: expected,
                    record_field_type: given,
                    field,
                },
            },
            UnifyError::ExtraVarInAlternativePattern { .. }
            | UnifyError::MissingVarInAlternativePattern { .. }
            | UnifyError::DuplicateVarInPattern { .. }
            | UnifyError::RecursiveType => convert_unify_error(e, record_location),
        };

        let indices_to_labels = variant.field_map.indices_to_labels();
        let mut implicit_arguments = Vec::new();

        for index in 0..variant.field_map.arity {
            if let Some(&label) = indices_to_labels.get(&index) {
                if !fields.contains_key(label) {
                    continue;
                }

                let record_access = self.infer_known_record_expression_access(
                    record.clone(),
                    label.clone(),
                    record_location,
                    record_location,
                    record_location.start,
                    FieldAccessUsage::RecordUpdate,
                )?;

                unify(variant.arg_type(index), record_access.type_()).map_err(|e| {
                    convert_incompatible_fields_error(e, RecordField::Labelled(label.clone()))
                })?;

                implicit_arguments.push((
                    index,
                    CallArg {
                        location: record_location,
                        label: Some(label.clone()),
                        value: record_access,
                        implicit: Some(ImplicitCallArgOrigin::RecordUpdate),
                    },
                ))
            } else {
                let (accessor_type, positional_fields) =
                    match collapse_links(record.type_()).as_ref() {
                        // A type in the current module
                        Type::Named {
                            module,
                            name,
                            inferred_variant,
                            ..
                        } if module == &self.environment.current_module => {
                            self.environment
                                .accessors
                                .get(name)
                                .and_then(|accessors_map| {
                                    Some((
                                        accessors_map.type_.clone(),
                                        // For record updates we must know the variant of the record, so if the
                                        // variant has not been inferred, that means there must only be one.
                                        accessors_map
                                            .positional_accessors(inferred_variant.unwrap_or(0))?,
                                    ))
                                })
                        }

                        // A type in another module
                        Type::Named {
                            module,
                            name,
                            inferred_variant,
                            ..
                        } => self
                            .environment
                            .importable_modules
                            .get(module)
                            .and_then(|module| module.accessors.get(name))
                            .filter(|a| {
                                a.publicity.is_importable()
                                    || module == &self.environment.current_module
                            })
                            .and_then(|accessors_map| {
                                Some((
                                    accessors_map.type_.clone(),
                                    accessors_map
                                        .positional_accessors(inferred_variant.unwrap_or(0))?,
                                ))
                            }),

                        Type::Fn { .. } | Type::Var { .. } | Type::Tuple { .. } => {
                            panic!("Type has already checked to be valid")
                        }
                    }
                    .expect("Variant has already checked to be valid");

                let type_ = positional_fields
                    .get(index as usize)
                    .expect("Field exists")
                    .clone();
                let mut type_vars = im::HashMap::new();
                let accessor_type = self.instantiate(accessor_type, &mut type_vars);
                let type_ = self.instantiate(type_, &mut type_vars);
                unify(accessor_type, record_type.clone()).map_err(|e| {
                    convert_incompatible_fields_error(e, RecordField::Unlabelled(index))
                })?;

                let record_access = TypedExpr::PositionalAccess {
                    record: Box::new(record.clone()),
                    index: index as u64,
                    location: record_location,
                    type_: type_.clone(),
                };

                unify(variant.arg_type(index), type_.clone()).map_err(|e| {
                    convert_incompatible_fields_error(e, RecordField::Unlabelled(index))
                })?;

                implicit_arguments.push((
                    index,
                    CallArg {
                        location: record_location,
                        label: None,
                        value: record_access,
                        implicit: Some(ImplicitCallArgOrigin::RecordUpdate),
                    },
                ))
            }
        }

        if explicit_arguments.is_empty() {
            self.problems
                .warning(Warning::NoFieldsRecordUpdate { location });
        }

        if implicit_arguments.is_empty() {
            self.problems
                .warning(Warning::AllFieldsRecordUpdate { location });
        }

        let arguments = explicit_arguments
            .into_iter()
            .chain(implicit_arguments)
            .sorted_by_key(|(index, _)| *index)
            .map(|(_, value)| value)
            .collect();

        Ok(arguments)
    }

    fn infer_record_update_variant<'c>(
        &mut self,
        constructor: &TypedExpr,
        value_constructor: &'c ValueConstructor,
        record: &TypedExpr,
    ) -> Result<RecordUpdateVariant<'c>, Error> {
        let record_type = record.type_();
        // The record constructor needs to be a function.
        let (arguments_types, return_type) = match constructor.type_().as_ref() {
            Type::Fn { arguments, return_ } => (arguments.clone(), return_.clone()),
            Type::Named { .. } | Type::Var { .. } | Type::Tuple { .. } => {
                return Err(Error::RecordUpdateInvalidConstructor {
                    location: constructor.location(),
                });
            }
        };

        // It must be a record with a field map for us to be able to update it
        let (field_map, variants_count, variant_index, name) = match &value_constructor.variant {
            ValueConstructorVariant::Record {
                field_map: Some(field_map),
                variants_count,
                variant_index,
                name,
                ..
            } => (field_map, *variants_count, *variant_index, name.clone()),
            ValueConstructorVariant::Record {
                field_map: None, ..
            } => {
                return Err(Error::RecordUpdateInvalidConstructor {
                    location: constructor.location(),
                });
            }
            ValueConstructorVariant::LocalVariable { .. }
            | ValueConstructorVariant::ModuleConstant { .. }
            | ValueConstructorVariant::ModuleFn { .. } => {
                return Err(Error::RecordUpdateInvalidConstructor {
                    location: constructor.location(),
                });
            }
        };

        // Check that the record type unifies with the return type of the constructor, and is
        // not some unrelated other type. This should not affect our returned type, so we
        // instantiate a new copy of the generic return type for our value constructor.
        let return_type_copy = match value_constructor.type_.as_ref() {
            Type::Fn { return_, .. } => self.instantiate(return_.clone(), &mut hashmap![]),
            Type::Named { .. } | Type::Var { .. } | Type::Tuple { .. } => {
                return Err(Error::RecordUpdateInvalidConstructor {
                    location: constructor.location(),
                });
            }
        };

        unify(return_type_copy, record_type.clone())
            .map_err(|e| convert_unify_error(e, record.location()))?;

        let record_index = record_type.custom_type_inferred_variant();
        // Updating a record with only one variant is always safe
        if variants_count == 1 {
            return Ok(RecordUpdateVariant {
                arguments: arguments_types,
                return_type,
                field_map,
            });
        }

        // if we know the record that is being spread, and it does match the one being constructed,
        // we can safely perform this record update due to variant inference.
        if record_index.is_some_and(|index| index == variant_index) {
            self.track_feature_usage(FeatureKind::RecordUpdateVariantInference, record.location());
            return Ok(RecordUpdateVariant {
                arguments: arguments_types,
                return_type,
                field_map,
            });
        }

        // We definitely know that we can't do this record update safely.
        //
        // If we know the variant of the value being spread, and it doesn't match the
        // one being constructed, we can tell the user that it's always wrong
        if record_index.is_some() {
            let Type::Named {
                module: record_module,
                name: record_name,
                inferred_variant: Some(record_index),
                ..
            } = record_type.deref()
            else {
                panic!("Spread type must be named and with an index")
            };

            return Err(Error::UnsafeRecordUpdate {
                location: record.location(),
                reason: UnsafeRecordUpdateReason::WrongVariant {
                    constructed_variant: name,
                    spread_variant: self
                        .environment
                        .type_variant_name(record_module, record_name, *record_index)
                        .expect("Spread type must exist and variant must be valid")
                        .clone(),
                },
            });
        }

        // If we don't have information about the variant being spread, we tell the user
        // that it's not safe to update it as it could be any variant
        Err(Error::UnsafeRecordUpdate {
            location: record.location(),
            reason: UnsafeRecordUpdateReason::UnknownVariant {
                constructed_variant: name,
            },
        })
    }

    fn unknown_field_error(
        &self,
        fields: Vec<EcoString>,
        record_type: Arc<Type>,
        location: SrcSpan,
        label: EcoString,
        usage: FieldAccessUsage,
    ) -> Error {
        let error = |unknown_field| Error::UnknownRecordField {
            usage,
            type_: record_type.clone(),
            location,
            label: label.clone(),
            fields,
            unknown_field,
        };

        let Type::Named {
            module,
            name,
            inferred_variant,
            ..
        } = record_type.deref()
        else {
            return error(UnknownField::NoFields);
        };

        let all_fields = self.environment.get_type_variants_fields(module, name);

        if all_fields.is_empty() {
            return error(UnknownField::NoFields);
        }

        if !all_fields.iter().contains(&&label) {
            return error(UnknownField::TrulyUnknown);
        }

        // If we know the variant, the field must exist on a different
        // variant from the one we have inferred.
        if inferred_variant.is_some() {
            error(UnknownField::AppearsInAnImpossibleVariant)
        } else {
            error(UnknownField::AppearsInAVariant)
        }
    }

    fn infer_value_constructor(
        &mut self,
        module: &Option<(EcoString, SrcSpan)>,
        name: &EcoString,
        location: &SrcSpan,
    ) -> Result<ValueConstructor, Error> {
        self.do_infer_value_constructor(module, name, location, ReferenceRegistration::Register)
    }

    fn do_infer_value_constructor(
        &mut self,
        module: &Option<(EcoString, SrcSpan)>,
        name: &EcoString,
        location: &SrcSpan,
        register_reference: ReferenceRegistration,
    ) -> Result<ValueConstructor, Error> {
        let constructor = match module {
            // Look in the current scope for a binding with this name
            None => self
                .environment
                .get_variable(name)
                .cloned()
                .ok_or_else(|| self.report_name_error(name, location))?,

            // Look in an imported module for a binding with this name
            Some((module_name, module_location)) => {
                let (_, module) = &self
                    .environment
                    .imported_modules
                    .get(module_name)
                    .ok_or_else(|| Error::UnknownModule {
                        location: *module_location,
                        name: module_name.clone(),
                        suggestions: self
                            .environment
                            .suggest_modules(module_name, Imported::Value(name.clone())),
                    })?;

                self.environment
                    .references
                    .register_module_reference(module_name.clone());

                module
                    .values
                    .get(name)
                    .cloned()
                    .ok_or_else(|| Error::UnknownModuleValue {
                        location: *location,
                        module_name: module_name.clone(),
                        name: name.clone(),
                        value_constructors: module.public_value_names(),
                        type_with_same_name: module.get_public_type(name).is_some(),
                        context: ModuleValueUsageContext::ModuleAccess,
                    })?
            }
        };

        let ValueConstructor {
            publicity,
            variant,
            type_,
            deprecation,
        } = constructor;

        self.check_recursive_argument_usage(name, &variant, &register_reference);

        // Emit a warning if the value being used is deprecated.
        if let Deprecation::Deprecated { message } = &deprecation {
            self.problems.warning(Warning::DeprecatedItem {
                location: *location,
                message: message.clone(),
                layer: Layer::Value,
            })
        }

        self.narrow_implementations(*location, &variant)?;

        match register_reference {
            ReferenceRegistration::DoNotRegister => (),

            ReferenceRegistration::Register | ReferenceRegistration::VariableArgument { .. } => {
                self.register_value_constructor_reference(
                    name,
                    &variant,
                    *location,
                    if module.is_some() {
                        ReferenceKind::Qualified
                    } else {
                        ReferenceKind::Unqualified
                    },
                );
            }
        }

        // Instantiate generic variables into unbound variables for this usage
        let type_ = self.instantiate(type_, &mut hashmap![]);
        Ok(ValueConstructor {
            publicity,
            deprecation,
            variant,
            type_,
        })
    }

    fn check_recursive_argument_usage(
        &mut self,
        name: &EcoString,
        variant: &ValueConstructorVariant,
        register_reference: &ReferenceRegistration,
    ) {
        // If we are registering references for a call argument
        let ReferenceRegistration::VariableArgument {
            called_function,
            argument_index,
        } = register_reference
        else {
            return;
        };

        // If the passed argument is a function's parameter.
        let ValueConstructorVariant::LocalVariable { origin, .. } = variant else {
            return;
        };

        let VariableDeclaration::FunctionParameter {
            function_name: declaration_function,
            index: declaration_index,
        } = &origin.declaration
        else {
            return;
        };

        // If the called function is the same where the argument is defined,
        // and the argument is passed unchanged.
        if declaration_function.as_ref() == Some(called_function)
            && declaration_index == argument_index
        {
            self.environment.increment_recursive_usage(name);
        }
    }

    fn register_value_constructor_reference(
        &mut self,
        referenced_name: &EcoString,
        variant: &ValueConstructorVariant,
        location: SrcSpan,
        kind: ReferenceKind,
    ) {
        match variant {
            ValueConstructorVariant::ModuleFn {
                module,
                name: value_name,
                ..
            }
            | ValueConstructorVariant::Record {
                module,
                name: value_name,
                ..
            }
            | ValueConstructorVariant::ModuleConstant {
                module,
                name: value_name,
                ..
            } if value_name != referenced_name => {
                self.environment.references.register_value_reference(
                    module.clone(),
                    value_name.clone(),
                    referenced_name,
                    location,
                    ReferenceKind::Alias,
                )
            }
            ValueConstructorVariant::ModuleFn { name, module, .. }
            | ValueConstructorVariant::Record { name, module, .. }
            | ValueConstructorVariant::ModuleConstant { name, module, .. } => {
                self.environment.references.register_value_reference(
                    module.clone(),
                    name.clone(),
                    referenced_name,
                    location,
                    kind,
                )
            }
            ValueConstructorVariant::LocalVariable { .. } => {
                self.environment.increment_usage(referenced_name)
            }
        }
    }

    fn report_name_error(&mut self, name: &EcoString, location: &SrcSpan) -> Error {
        // First try to see if this is a module alias:
        // `import gleam/io`
        // `io.debug(io)`
        // Show nice error message for this case.
        let module = self.environment.imported_modules.get(name);
        match module {
            Some(_) => Error::ModuleAliasUsedAsName {
                location: *location,
                name: name.clone(),
            },
            None => Error::UnknownVariable {
                location: *location,
                name: name.clone(),
                variables: self.environment.local_value_names(),
                discarded_location: self
                    .environment
                    .discarded_names
                    .get(&eco_format!("_{name}"))
                    .cloned(),
                type_with_name_in_scope: self
                    .environment
                    .module_types
                    .keys()
                    .any(|typ| typ == name),
            },
        }
    }

    // helper for infer_const to get the value of a constant ignoring annotations
    fn infer_const_value(&mut self, value: UntypedConstant) -> TypedConstant {
        match value {
            Constant::Int {
                location,
                value,
                int_value,
            } => {
                if self.environment.target == Target::JavaScript {
                    check_javascript_int_safety(&int_value, location, self.problems);
                }

                Constant::Int {
                    location,
                    value,
                    int_value,
                }
            }

            Constant::Float {
                location,
                value,
                float_value,
            } => {
                check_float_safety(float_value, location, self.problems);
                Constant::Float {
                    location,
                    value,
                    float_value,
                }
            }

            Constant::String {
                location, value, ..
            } => Constant::String { location, value },

            Constant::Tuple {
                elements, location, ..
            } => self.infer_const_tuple(elements, location),

            Constant::List {
                elements, location, ..
            } => self.infer_const_list(elements, location),

            Constant::BitArray { location, segments } => {
                match self.infer_constant_bit_array(segments, location) {
                    Ok(inferred) => inferred,
                    Err(error) => {
                        self.problems.error(error);
                        Constant::Invalid {
                            location,
                            type_: bit_array(),
                            extra_information: None,
                        }
                    }
                }
            }

            Constant::RecordUpdate {
                constructor_location,
                module,
                location,
                name,
                record,
                arguments,
                ..
            } => {
                self.track_feature_usage(FeatureKind::ConstantRecordUpdate, location);
                let constructor = match self.infer_value_constructor(&module, &name, &location) {
                    Ok(constructor) => constructor,
                    Err(error) => {
                        self.problems.error(error);
                        return self.new_invalid_constant(location);
                    }
                };

                let (tag, field_map) = match &constructor.variant {
                    ValueConstructorVariant::Record {
                        name,
                        field_map: Some(field_map),
                        ..
                    } => (name.clone(), field_map.clone()),

                    ValueConstructorVariant::Record {
                        field_map: None, ..
                    } => {
                        self.problems.error(Error::RecordUpdateInvalidConstructor {
                            location: constructor_location,
                        });
                        return self.new_invalid_constant(location);
                    }

                    ValueConstructorVariant::ModuleFn { .. }
                    | ValueConstructorVariant::LocalVariable { .. } => {
                        self.problems
                            .error(Error::NonLocalClauseGuardVariable { location, name });
                        return self.new_invalid_constant(location);
                    }

                    ValueConstructorVariant::ModuleConstant { literal, .. } => {
                        return literal.clone();
                    }
                };

                // Type-check the record being updated
                let typed_record = self.infer_const(&None, *record.base.clone());
                let typed_record_type = typed_record.type_();

                // Instantiate the constructor type to enable generic re-specialization.
                let instantiated_constructor_type =
                    self.instantiate(constructor.type_.clone(), &mut hashmap![]);

                // Extract field types and return type from the instantiated constructor
                let (field_types, expected_type) = match instantiated_constructor_type.as_ref() {
                    Type::Fn { arguments, return_ } => (arguments.clone(), return_.clone()),
                    Type::Named { .. } | Type::Var { .. } | Type::Tuple { .. } => {
                        self.problems.error(Error::RecordUpdateInvalidConstructor {
                            location: constructor_location,
                        });
                        return self.new_invalid_constant(location);
                    }
                };

                // If the record being updated is a reference to a constant variable, resolve
                // it to get the actual record value
                let resolved_record = match &typed_record {
                    Constant::Var {
                        constructor: Some(value_constructor),
                        ..
                    } => match &value_constructor.variant {
                        ValueConstructorVariant::ModuleConstant { literal, .. } => literal.clone(),
                        ValueConstructorVariant::LocalVariable { .. }
                        | ValueConstructorVariant::ModuleFn { .. }
                        | ValueConstructorVariant::Record { .. } => typed_record,
                    },
                    Constant::Int { .. }
                    | Constant::Float { .. }
                    | Constant::String { .. }
                    | Constant::Tuple { .. }
                    | Constant::List { .. }
                    | Constant::Record { .. }
                    | Constant::RecordUpdate { .. }
                    | Constant::BitArray { .. }
                    | Constant::Var { .. }
                    | Constant::StringConcatenation { .. }
                    | Constant::Invalid { .. } => typed_record,
                };

                // Get the field arguments from the record that we'll use as the base.
                let (base_arguments, base_tag) =
                    if let Constant::Record { arguments, tag, .. } = resolved_record {
                        (arguments, tag)
                    } else {
                        self.problems.error(convert_unify_error(
                            UnifyError::CouldNotUnify {
                                expected: expected_type.clone(),
                                given: typed_record_type,
                                situation: None,
                            },
                            record.location,
                        ));
                        return self.new_invalid_constant(location);
                    };

                // Check that the variant being spread matches the constructor variant
                // For multi-variant custom types, you can't spread Dog to create Cat
                if tag != base_tag {
                    self.problems.error(Error::UnsafeRecordUpdate {
                        location: record.location,
                        reason: UnsafeRecordUpdateReason::WrongVariant {
                            constructed_variant: tag,
                            spread_variant: base_tag,
                        },
                    });
                    return self.new_invalid_constant(location);
                }

                // Emit warning if no fields are being overridden
                if arguments.is_empty() {
                    self.problems
                        .warning(Warning::NoFieldsRecordUpdate { location });
                }

                let mut implicit_labelled_arguments = field_map.fields.clone();
                let mut update_argument_indices = HashSet::new();

                let mut final_arguments = base_arguments;
                for argument in arguments {
                    if argument.uses_label_shorthand() {
                        self.track_feature_usage(
                            FeatureKind::LabelShorthandSyntax,
                            argument.location,
                        );
                    }

                    let label = &argument.label;
                    let typed_value = self.infer_const(&None, argument.value);

                    let Some(index) = implicit_labelled_arguments.remove(label) else {
                        if field_map.fields.contains_key(label) {
                            self.problems.error(Error::DuplicateArgument {
                                location: argument.location,
                                label: label.clone(),
                            });
                        } else {
                            self.problems.error(self.unknown_field_error(
                                field_map.fields.keys().cloned().collect(),
                                expected_type.clone(),
                                argument.location,
                                label.clone(),
                                FieldAccessUsage::Other,
                            ));
                        }

                        return self.new_invalid_constant(location);
                    };

                    // Record update argument value must match the field type
                    if let Some(expected_type) = field_types.get(index as usize)
                        && let Err(error) = unify(expected_type.clone(), typed_value.type_())
                    {
                        self.problems
                            .error(convert_unify_error(error, typed_value.location()));
                        return self.new_invalid_constant(location);
                    }

                    let _ = update_argument_indices.insert(index as usize);

                    *final_arguments
                        .get_mut(index as usize)
                        .expect("Index out of bounds") = CallArg {
                        label: Some(label.clone()),
                        value: typed_value,
                        location: argument.location,
                        implicit: None,
                    };
                }

                // Emit warning if all fields are being overriden
                if implicit_labelled_arguments.is_empty() {
                    self.problems
                        .warning(Warning::AllFieldsRecordUpdate { location });
                }

                // Check that fields implicitly overridden (including unlabelled ones) have compatible types.
                for (index, field_arg) in final_arguments.iter().enumerate() {
                    // Skip fields that were record update arguments, as they've already been type-checked above
                    if update_argument_indices.contains(&index) {
                        continue;
                    }

                    if let Some(expected_field_type) = field_types.get(index)
                        && let Err(unify_error) =
                            unify(expected_field_type.clone(), field_arg.value.type_())
                    {
                        let field = field_map
                            .fields
                            .iter()
                            .find(|(_, i)| **i == index as u32)
                            .map(|(name, _)| RecordField::Labelled(name.clone()))
                            .unwrap_or_else(|| RecordField::Unlabelled(index as u32));

                        self.problems.error(
                            if let UnifyError::CouldNotUnify {
                                expected, given, ..
                            } = unify_error
                            {
                                Error::UnsafeRecordUpdate {
                                    location: record.location,
                                    reason: UnsafeRecordUpdateReason::IncompatibleFieldTypes {
                                        constructed_variant: expected_type.clone(),
                                        record_variant: typed_record_type.clone(),
                                        expected_field_type: expected,
                                        record_field_type: given,
                                        field,
                                    },
                                }
                            } else {
                                convert_unify_error(unify_error, location)
                            },
                        );
                        return self.new_invalid_constant(location);
                    }
                }

                Constant::Record {
                    module,
                    location,
                    name,
                    arguments: final_arguments,
                    type_: expected_type,
                    tag,
                    field_map: Inferred::Known(field_map),
                    record_constructor: Some(Box::new(constructor)),
                }
            }

            Constant::Record {
                module,
                location,
                name,
                arguments,
                ..
            } if arguments.is_empty() => {
                // Type check the record constructor
                let constructor = match self.infer_value_constructor(&module, &name, &location) {
                    Ok(constructor) => constructor,
                    Err(error) => {
                        self.problems.error(error);
                        return self.new_invalid_constant(location);
                    }
                };

                let (tag, field_map) = match &constructor.variant {
                    ValueConstructorVariant::Record {
                        name, field_map, ..
                    } => (
                        name.clone(),
                        match field_map {
                            Some(fm) => Inferred::Known(fm.clone()),
                            None => Inferred::Unknown,
                        },
                    ),

                    ValueConstructorVariant::ModuleFn { .. }
                    | ValueConstructorVariant::LocalVariable { .. } => {
                        self.problems
                            .error(Error::NonLocalClauseGuardVariable { location, name });
                        return self.new_invalid_constant(location);
                    }

                    // TODO: remove this clone. Could use an rc instead
                    ValueConstructorVariant::ModuleConstant { literal, .. } => {
                        return literal.clone();
                    }
                };

                Constant::Record {
                    module,
                    location,
                    name,
                    arguments: vec![],
                    type_: constructor.type_.clone(),
                    tag,
                    field_map,
                    record_constructor: Some(Box::new(constructor)),
                }
            }

            Constant::Record {
                module,
                location,
                name,
                mut arguments,
                ..
            } => {
                let constructor = match self.infer_value_constructor(&module, &name, &location) {
                    Ok(constructor) => constructor,
                    Err(error) => {
                        self.problems.error(error);
                        return self.new_invalid_constant(location);
                    }
                };

                let (tag, field_map, variant_index) = match &constructor.variant {
                    ValueConstructorVariant::Record {
                        name,
                        field_map,
                        variant_index,
                        ..
                    } => (
                        name.clone(),
                        match field_map {
                            Some(fm) => Inferred::Known(fm.clone()),
                            None => Inferred::Unknown,
                        },
                        *variant_index,
                    ),

                    ValueConstructorVariant::ModuleFn { .. }
                    | ValueConstructorVariant::LocalVariable { .. } => {
                        self.problems
                            .error(Error::NonLocalClauseGuardVariable { location, name });
                        return self.new_invalid_constant(location);
                    }

                    // TODO: remove this clone. Could be an rc instead
                    ValueConstructorVariant::ModuleConstant { literal, .. } => {
                        return literal.clone();
                    }
                };

                // Pretty much all the other infer functions operate on UntypedExpr
                // or TypedExpr rather than ClauseGuard. To make things easier we
                // build the TypedExpr equivalent of the constructor and use that
                // TODO: resvisit this. It is rather awkward at present how we
                // have to convert to this other data structure.
                let fun = match &module {
                    Some((module_alias, module_location)) => {
                        let type_ = Arc::clone(&constructor.type_);
                        let module_name = self
                            .environment
                            .imported_modules
                            // TODO: remove
                            .get(module_alias)
                            .expect("Failed to find previously located module import")
                            .1
                            .name
                            .clone();
                        let module_value_constructor = ModuleValueConstructor::Record {
                            name: name.clone(),
                            variant_index,
                            field_map: match &field_map {
                                Inferred::Known(fm) => Some(fm.clone()),
                                Inferred::Unknown => None,
                            },
                            arity: arguments.len() as u16,
                            type_: Arc::clone(&type_),
                            location: constructor.variant.definition_location(),
                            documentation: None,
                        };

                        TypedExpr::ModuleSelect {
                            location: module_location.merge(&location),
                            field_start: location.start,
                            label: name.clone(),
                            module_alias: module_alias.clone(),
                            module_name,
                            type_,
                            constructor: module_value_constructor,
                        }
                    }

                    None => TypedExpr::Var {
                        constructor: constructor.clone(),
                        location,
                        name: name.clone(),
                    },
                };

                // This is basically the same code as do_infer_call_with_known_fun()
                // except the args are typed with infer_clause_guard() here.
                // This duplication is a bit awkward but it works!
                // Potentially this could be improved later
                let result = match self.get_field_map(&fun) {
                    // There's an error retrieving the field map, in that case we
                    // return an invalid constant.
                    Err(error) => {
                        self.problems
                            .error(convert_get_value_constructor_error(error, location, None));
                        return self.new_invalid_constant(location);
                    }
                    // The fun has a field map so labelled arguments may be present
                    // and need to be reordered.
                    Ok(Some(field_map)) => {
                        field_map.reorder(&mut arguments, location, IncorrectArityContext::Function)
                    }
                    // The fun or constructor has no field map and so we error
                    // if arguments have been labelled.
                    Ok(None) if fun.is_record_constructor_function() => {
                        assert_no_labelled_arguments(
                            &arguments,
                            UnexpectedLabelledArgKind::RecordConstructorArgument,
                        )
                    }
                    Ok(None) => assert_no_labelled_arguments(
                        &arguments,
                        UnexpectedLabelledArgKind::FunctionParameter,
                    ),
                };

                // If there's an error reordering the fields, or there's labelled
                // arguments with no field map, then we return an invalid expression.
                if let Err(error) = result {
                    self.problems.error(error);
                    return self.new_invalid_constant(location);
                }

                let (mut arguments_types, return_type) =
                    match match_fun_type(fun.type_(), arguments.len(), self.environment) {
                        Ok((arguments_types, return_type)) => (arguments_types, return_type),
                        Err(error) => {
                            self.problems.error(convert_not_fun_error(
                                error,
                                fun.location(),
                                location,
                                CallKind::Function,
                            ));
                            return self.new_invalid_constant(location);
                        }
                    };

                let arguments = arguments_types
                    .iter_mut()
                    .zip(arguments)
                    .map(|(type_, argument): (&mut Arc<Type>, _)| {
                        if argument.uses_label_shorthand() {
                            self.track_feature_usage(
                                FeatureKind::LabelShorthandSyntax,
                                argument.location,
                            );
                        }
                        let CallArg {
                            label,
                            value,
                            location,
                            implicit,
                        } = argument;
                        let value = self.infer_const(&None, value);
                        if let Err(error) = unify(type_.clone(), value.type_()) {
                            self.problems
                                .error(convert_unify_error(error, value.location()))
                        }
                        CallArg {
                            label,
                            value,
                            implicit,
                            location,
                        }
                    })
                    .collect_vec();

                Constant::Record {
                    module,
                    location,
                    name,
                    arguments,
                    type_: return_type,
                    tag,
                    field_map,
                    record_constructor: Some(Box::new(constructor)),
                }
            }

            Constant::Var {
                location,
                module,
                name,
                ..
            } => {
                // Infer the type of this constant
                let constructor = match self.infer_value_constructor(&module, &name, &location) {
                    Ok(constructor) => constructor,
                    Err(error) => {
                        self.problems.error(error);
                        return Constant::Invalid {
                            location,
                            type_: self.new_unbound_var(),
                            extra_information: Some(match module {
                                Some((module_name, _)) => InvalidExpression::ModuleSelect {
                                    module_name,
                                    label: name,
                                },
                                None => InvalidExpression::UnknownVariable { name },
                            }),
                        };
                    }
                };

                match constructor.variant {
                    ValueConstructorVariant::ModuleConstant { .. }
                    | ValueConstructorVariant::ModuleFn { .. }
                    | ValueConstructorVariant::LocalVariable { .. } => Constant::Var {
                        location,
                        module,
                        name,
                        type_: Arc::clone(&constructor.type_),
                        constructor: Some(Box::from(constructor)),
                    },
                    // It cannot be a Record because then this constant would have been
                    // parsed as a Constant::Record. Therefore this code is unreachable.
                    ValueConstructorVariant::Record { .. } => unreachable!(),
                }
            }

            Constant::StringConcatenation {
                location,
                left,
                right,
            } => {
                self.track_feature_usage(FeatureKind::ConstantStringConcatenation, location);
                let left = self.infer_const(&None, *left);

                if let Err(error) = unify(string(), left.type_()) {
                    self.problems.error(
                        error
                            .operator_situation(BinOp::Concatenate)
                            .into_error(left.location()),
                    )
                };

                let right = self.infer_const(&None, *right);
                if let Err(error) = unify(string(), right.type_()) {
                    self.problems.error(
                        error
                            .operator_situation(BinOp::Concatenate)
                            .into_error(right.location()),
                    )
                };

                Constant::StringConcatenation {
                    location,
                    left: Box::new(left),
                    right: Box::new(right),
                }
            }

            Constant::Invalid { .. } => panic!("invalid constants can not be in an untyped ast"),
        }
    }

    /// Returns an invalid constant with an unbound type and no extra information
    /// attached.
    fn new_invalid_constant(&mut self, location: SrcSpan) -> TypedConstant {
        Constant::Invalid {
            location,
            type_: self.new_unbound_var(),
            extra_information: None,
        }
    }

    pub fn infer_const(
        &mut self,
        annotation: &Option<TypeAst>,
        value: UntypedConstant,
    ) -> TypedConstant {
        let inferred = self.infer_const_value(value);

        match annotation
            .as_ref()
            .map(|annotation| self.type_from_ast(annotation))
        {
            Some(Err(error)) => {
                self.problems.error(error);
                inferred
            }

            // If there's an annotation we try and unify it with the inferred
            // type.
            Some(Ok(annotated_type)) => {
                if let Err(error) = unify(annotated_type.clone(), inferred.type_()) {
                    self.problems
                        .error(convert_unify_error(error, inferred.location()));
                    invalid_with_annotated_type(inferred, annotated_type)
                } else {
                    inferred
                }
            }

            None => inferred,
        }
    }

    fn infer_const_tuple(
        &mut self,
        untyped_elements: Vec<UntypedConstant>,
        location: SrcSpan,
    ) -> TypedConstant {
        let mut elements = Vec::with_capacity(untyped_elements.len());

        for element in untyped_elements {
            let element = self.infer_const(&None, element);
            elements.push(element);
        }

        let type_ = tuple(elements.iter().map(HasType::type_).collect_vec());

        Constant::Tuple {
            elements,
            location,
            type_,
        }
    }

    fn infer_const_list(
        &mut self,
        untyped_elements: Vec<UntypedConstant>,
        location: SrcSpan,
    ) -> TypedConstant {
        let type_ = self.new_unbound_var();
        let mut elements = Vec::with_capacity(untyped_elements.len());

        for element in untyped_elements {
            let element = self.infer_const(&None, element);
            if let Err(error) = unify(type_.clone(), element.type_()) {
                self.problems
                    .error(convert_unify_error(error, element.location()));
            }

            elements.push(element);
        }

        Constant::List {
            elements,
            location,
            type_: list(type_),
        }
    }

    fn get_field_map(
        &mut self,
        constructor: &TypedExpr,
    ) -> Result<Option<&FieldMap>, UnknownValueConstructorError> {
        let (module, name) = match constructor {
            TypedExpr::ModuleSelect {
                module_alias,
                label,
                ..
            } => (Some(module_alias), label),

            TypedExpr::Var { name, .. } => (None, name),

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
            | TypedExpr::Invalid { .. } => return Ok(None),
        };

        Ok(self
            .environment
            .get_value_constructor(module, name)?
            .field_map())
    }

    pub fn do_infer_call(
        &mut self,
        fun: UntypedExpr,
        arguments: Vec<CallArg<UntypedExpr>>,
        location: SrcSpan,
        kind: CallKind,
    ) -> (TypedExpr, Vec<TypedCallArg>, Arc<Type>) {
        let fun = match fun {
            UntypedExpr::FieldAccess {
                label,
                container,
                label_location,
                location,
            } => self.infer_field_access(
                *container,
                location,
                label,
                label_location,
                FieldAccessUsage::MethodCall,
            ),

            UntypedExpr::Fn {
                location,
                kind,
                arguments: fn_arguments,
                body,
                return_annotation,
                ..
            } if fn_arguments.len() == arguments.len() => self.infer_fn_with_call_context(
                fn_arguments,
                &arguments,
                body,
                kind,
                return_annotation,
                location,
            ),

            UntypedExpr::Int { .. }
            | UntypedExpr::Float { .. }
            | UntypedExpr::String { .. }
            | UntypedExpr::Block { .. }
            | UntypedExpr::Var { .. }
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
            | UntypedExpr::NegateBool { .. }
            | UntypedExpr::NegateInt { .. } => self.infer(fun),
        };

        let (fun, arguments, type_) =
            self.do_infer_call_with_known_fun(fun, arguments, location, kind);
        (fun, arguments, type_)
    }

    fn infer_fn_with_call_context(
        &mut self,
        arguments: Vec<UntypedArg>,
        call_arguments: &[CallArg<UntypedExpr>],
        body: Vec1<UntypedStatement>,
        kind: FunctionLiteralKind,
        return_annotation: Option<TypeAst>,
        location: SrcSpan,
    ) -> TypedExpr {
        let typed_call_arguments: Vec<Arc<Type>> = call_arguments
            .iter()
            .map(|argument| {
                match self.infer_or_error(argument.value.clone()) {
                    Ok(argument) => argument,
                    Err(_e) => self.error_expr(location),
                }
                .type_()
            })
            .collect_vec();
        self.infer_fn(
            arguments,
            &typed_call_arguments,
            body,
            kind,
            return_annotation,
            location,
        )
    }

    pub fn do_infer_call_with_known_fun(
        &mut self,
        fun: TypedExpr,
        mut arguments: Vec<CallArg<UntypedExpr>>,
        location: SrcSpan,
        kind: CallKind,
    ) -> (TypedExpr, Vec<TypedCallArg>, Arc<Type>) {
        let mut labelled_arity_error = false;

        // Check to see if the function accepts labelled arguments
        let field_map = self
            .get_field_map(&fun)
            .map_err(|e| convert_get_value_constructor_error(e, location, None))
            .and_then(|field_map| {
                match field_map {
                    // The fun has a field map so labelled arguments may be
                    // present and need to be reordered.
                    Some(field_map) => {
                        field_map.reorder(&mut arguments, location, IncorrectArityContext::Function)
                    }

                    // The fun or constructor has no field map and so we error
                    // if arguments have been labelled.
                    // There's an exception to this rule: if the function itself
                    // doesn't exist (that is it's an `Invalid` expression), then
                    // we don't want to error on any labels that might have been
                    // used as it would be quite noisy. Once the function is
                    // known to be a valid function we can make sure that there's
                    // no labelled arguments if it doesn't actually have a field map.
                    None if fun.is_invalid() => Ok(()),
                    None if fun.is_record_constructor_function() => assert_no_labelled_arguments(
                        &arguments,
                        UnexpectedLabelledArgKind::RecordConstructorArgument,
                    ),
                    None => assert_no_labelled_arguments(
                        &arguments,
                        UnexpectedLabelledArgKind::FunctionParameter,
                    ),
                }
            });

        if let Err(e) = field_map {
            if let Error::IncorrectArity {
                expected,
                given,
                context,
                labels,
                location,
            } = e
            {
                labelled_arity_error = true;
                self.problems.error(Error::IncorrectArity {
                    expected,
                    given,
                    context,
                    labels,
                    location,
                });
            } else {
                self.problems.error(e);
            }
        }

        let mut missing_arguments = 0;
        let mut ignored_labelled_arguments = vec![];
        // Extract the type of the fun, ensuring it actually is a function
        let (mut arguments_types, return_type) =
            match match_fun_type(fun.type_(), arguments.len(), self.environment) {
                Ok(function) => function,
                Err(error) => {
                    let converted_error =
                        convert_not_fun_error(error.clone(), fun.location(), location, kind);
                    match error {
                        // If the function was valid but had the wrong number of arguments passed.
                        // Then we keep the error but still want to continue analysing the arguments that were passed.
                        MatchFunTypeError::IncorrectArity {
                            arguments: arg_types,
                            return_type,
                            expected,
                            given,
                            ..
                        } => {
                            missing_arguments = expected.saturating_sub(given);
                            // If the function has labels then arity issues will already
                            // be handled by the field map so we can ignore them here.
                            if !labelled_arity_error {
                                self.problems.error(converted_error);
                                (arg_types, return_type)
                            } else {
                                // Since arity errors with labels cause incorrect
                                // ordering, we can't type check the labelled arguments here.
                                let first_labelled_arg =
                                    arguments.iter().position(|arg| arg.label.is_some());
                                ignored_labelled_arguments = arguments
                                    .iter()
                                    .skip_while(|argument| argument.label.is_none())
                                    .map(|argument| {
                                        (
                                            argument.label.clone(),
                                            argument.location,
                                            argument.implicit,
                                        )
                                    })
                                    .collect_vec();
                                let arguments_to_keep =
                                    first_labelled_arg.unwrap_or(arguments.len());
                                (
                                    arg_types.iter().take(arguments_to_keep).cloned().collect(),
                                    return_type,
                                )
                            }
                        }
                        MatchFunTypeError::NotFn { .. } => {
                            self.problems.error(converted_error);
                            (vec![], self.new_unbound_var())
                        }
                    }
                }
            };

        // When typing the function's arguments we don't care if the previous
        // expression panics or not because we want to provide a specialised
        // error message for this particular case.
        // So we set `previous_panics` to false to avoid raising any
        // unnecessarily generic warning.
        self.previous_panics = false;

        // Now if we had a mismatched arity error and we're typing a use call,
        // we want to insert all the missing arguments before the callback
        // argument that is implicitly passed by the compiler.
        // This way we can provide better argument hints for incomplete use
        // expressions.
        if let CallKind::Use { .. } = kind
            && let Some(last) = arguments.pop()
        {
            for _ in 0..missing_arguments {
                arguments.push(CallArg {
                    label: None,
                    location,
                    value: UntypedExpr::Panic {
                        // We intentionally give this an empty span since it
                        // is an implicit argument being passed by the compiler
                        // that doesn't appear in the source code.
                        location: SrcSpan {
                            start: last.location().start,
                            end: last.location().start,
                        },
                        message: None,
                    },
                    implicit: Some(ImplicitCallArgOrigin::IncorrectArityUse),
                });
            }
            arguments.push(last);
        };

        // Ensure that the given args have the correct types
        let arguments_count = arguments_types.len();
        let mut typed_arguments: Vec<_> = arguments_types
            .iter_mut()
            .zip(arguments)
            .enumerate()
            .map(|(argument_index, (type_, arg))| {
                if arg.uses_label_shorthand() {
                    self.track_feature_usage(FeatureKind::LabelShorthandSyntax, arg.location);
                }

                let CallArg {
                    label,
                    value,
                    location,
                    implicit,
                } = arg;

                // If we're typing a `use` call then the last argument is the
                // use callback and we want to treat it differently to report
                // better errors.
                let argument_kind = match kind {
                    CallKind::Use {
                        call_location,
                        last_statement_location,
                        assignments_location,
                    } if argument_index == arguments_count - 1 => ArgumentKind::UseCallback {
                        function_location: call_location,
                        assignments_location,
                        last_statement_location,
                    },
                    CallKind::Use { .. } | CallKind::Function => ArgumentKind::Regular,
                };

                // We don't want to emit a warning for unreachable function call if the
                // function being called is itself `panic`, for that we emit a more
                // specialised warning.
                if self.previous_panics && !fun.is_panic() {
                    self.warn_for_unreachable_code(
                        value.location(),
                        PanicPosition::PreviousFunctionArgument,
                    )
                }

                let value = self.infer_call_argument(
                    &fun,
                    value,
                    argument_index,
                    type_.clone(),
                    argument_kind,
                );

                CallArg {
                    label,
                    value,
                    implicit,
                    location,
                }
            })
            .collect();

        // Now if we had supplied less arguments than required and some of those
        // were labelled, in the previous step we would have got rid of those
        // _before_ typing.
        // That is because we can only reliably type positional arguments in
        // case of mismatched arity, as labelled arguments cannot be reordered.
        //
        // So now what we want to do is add back those labelled arguments to
        // make sure the LS can still see that those were explicitly supplied.
        for (label, location, implicit) in ignored_labelled_arguments {
            typed_arguments.push(CallArg {
                label,
                value: TypedExpr::Invalid {
                    location,
                    type_: self.new_unbound_var(),
                    extra_information: None,
                },
                implicit,
                location,
            })
        }

        // We don't want to emit a warning for unreachable function call if the
        // function being called is itself `panic`, for that we emit a more
        // specialised warning.
        if self.previous_panics && !fun.is_panic() {
            self.warn_for_unreachable_code(fun.location(), PanicPosition::LastFunctionArgument);
        }

        (fun, typed_arguments, return_type)
    }

    fn infer_call_argument(
        &mut self,
        called_function: &TypedExpr,
        argument: UntypedExpr,
        argument_index: usize,
        type_: Arc<Type>,
        kind: ArgumentKind,
    ) -> TypedExpr {
        let type_ = collapse_links(type_);
        let value = match (&*type_, argument) {
            // If the argument is expected to be a function and we are passed a
            // function literal with the correct number of arguments then we
            // have special handling of this argument, passing in information
            // about what the expected arguments are. This extra information
            // when type checking the function body means that the
            // `record.field` access syntax can be used, and improves error
            // messages.
            (
                Type::Fn {
                    arguments: expected_arguments,
                    ..
                },
                UntypedExpr::Fn {
                    arguments,
                    body,
                    return_annotation,
                    location,
                    kind,
                    ..
                },
            ) if expected_arguments.len() == arguments.len() => self.infer_fn(
                arguments,
                expected_arguments,
                body,
                kind,
                return_annotation,
                location,
            ),

            // If the argument is a regular var then we want to add some extra
            // checks. The value will be inferred regularly, but we also want to
            // see if this is an argument that is being passed recursively to
            // the same function that defined it!
            (_, UntypedExpr::Var { location, name }) => {
                self.infer_variable_call_arg(called_function, name, location, argument_index)
            }

            // Otherwise just perform normal type inference.
            (_, argument) => self.infer(argument),
        };

        if let Err(error) = unify(type_.clone(), value.type_()) {
            self.problems
                .error(convert_unify_call_error(error, value.location(), kind));
        }

        value
    }

    fn infer_variable_call_arg(
        &mut self,
        called_function: &TypedExpr,
        argument_name: EcoString,
        argument_location: SrcSpan,
        argument_index: usize,
    ) -> TypedExpr {
        // If the called function is a function defined in this same module we
        // pass it along to the `infer_var` function so that we can check if the
        // argument is being passed recursively to the function that is defining
        // it.
        let references = if let TypedExpr::Var {
            constructor:
                ValueConstructor {
                    variant: ValueConstructorVariant::ModuleFn { name, module, .. },
                    ..
                },
            ..
        } = called_function
            && *module == self.environment.current_module
        {
            ReferenceRegistration::VariableArgument {
                called_function: name.clone(),
                argument_index,
            }
        } else {
            ReferenceRegistration::Register
        };

        match self.infer_var(argument_name.clone(), argument_location, references) {
            Ok(result) => result,
            Err(error) => {
                self.problems.error(error);
                self.error_expr_with_information(
                    argument_location,
                    Some(InvalidExpression::UnknownVariable {
                        name: argument_name,
                    }),
                )
            }
        }
    }

    pub fn do_infer_fn(
        &mut self,
        function_name: Option<EcoString>,
        arguments: Vec<UntypedArg>,
        expected_arguments: &[Arc<Type>],
        body: Vec1<UntypedStatement>,
        return_annotation: &Option<TypeAst>,
    ) -> Result<(Vec<TypedArg>, Vec1<TypedStatement>), Error> {
        // Construct an initial type for each argument of the function- either an unbound
        // type variable or a type provided by an annotation.
        let arguments: Vec<_> = arguments
            .into_iter()
            .enumerate()
            .map(|(i, argument)| self.infer_arg(argument, expected_arguments.get(i).cloned()))
            .try_collect()?;

        let return_type = match return_annotation {
            Some(ann) => Some(self.type_from_ast(ann)?),
            None => None,
        };

        let (arguments, body) =
            self.infer_fn_with_known_types(function_name, arguments, body.to_vec(), return_type)?;
        let body =
            Vec1::try_from_vec(body).expect("body guaranteed to have at least one statement");
        Ok((arguments, body))
    }

    pub fn infer_fn_with_known_types(
        &mut self,
        function_name: Option<EcoString>,
        arguments: Vec<TypedArg>,
        body: Vec<UntypedStatement>,
        return_type: Option<Arc<Type>>,
    ) -> Result<(Vec<TypedArg>, Vec<TypedStatement>), Error> {
        // If a function has an empty body then it doesn't have a pure gleam
        // implementation.
        if body.is_empty() {
            self.implementations.gleam = false;
        }

        self.in_new_scope(|body_typer| {
            // Used to track if any argument names are used more than once
            let mut argument_names = HashSet::with_capacity(arguments.len());

            for (argument_index, argument) in arguments.iter().enumerate() {
                match &argument.names {
                    ArgNames::Named { name, location }
                    | ArgNames::NamedLabelled {
                        name,
                        name_location: location,
                        ..
                    } => {
                        // Check that this name has not already been used for
                        // another argument
                        if !argument_names.insert(name) {
                            return Err(Error::ArgumentNameAlreadyUsed {
                                location: argument.location,
                                name: name.clone(),
                            });
                        }

                        let syntax = if name == CAPTURE_VARIABLE {
                            VariableSyntax::Generated
                        } else {
                            VariableSyntax::Variable(name.clone())
                        };

                        let origin = VariableOrigin {
                            syntax,
                            declaration: VariableDeclaration::FunctionParameter {
                                function_name: function_name.clone(),
                                index: argument_index,
                            },
                        };

                        // Insert a variable for the argument into the environment
                        body_typer.environment.insert_local_variable(
                            name.clone(),
                            *location,
                            origin.clone(),
                            argument.type_.clone(),
                        );

                        if !body.is_empty() {
                            // Register the variable in the usage tracker so that we
                            // can identify if it is unused
                            body_typer.environment.init_usage(
                                name.clone(),
                                origin,
                                *location,
                                body_typer.problems,
                            );
                        }
                    }
                    ArgNames::Discard { .. } | ArgNames::LabelledDiscard { .. } => (),
                };
            }

            if let Ok(body) = Vec1::try_from_vec(body) {
                let mut body = body_typer.infer_statements(body);

                // Check that any return type is compatible with the annotation.
                if let Some(return_type) = return_type {
                    let mut instantiated_ids = hashmap![];
                    let flexible_hydrator = Hydrator::new();
                    let instantiated_annotation = body_typer.environment.instantiate(
                        return_type.clone(),
                        &mut instantiated_ids,
                        &flexible_hydrator,
                    );

                    if let Err(error) = unify(instantiated_annotation, body.last().type_()) {
                        let error = error
                            .return_annotation_mismatch()
                            .into_error(body.last().type_defining_location());
                        body_typer.problems.error(error);

                        // If the return type doesn't match with the annotation we
                        // add a new expression to the end of the function to match
                        // the annotated type and allow type inference to keep
                        // going.
                        body.push(Statement::Expression(TypedExpr::Invalid {
                            // This is deliberately an empty span since this
                            // placeholder expression is implicitly inserted by the
                            // compiler and doesn't actually appear in the source
                            // code.
                            location: SrcSpan {
                                start: body.last().location().end,
                                end: body.last().location().end,
                            },
                            type_: body_typer.new_unbound_var(),
                            extra_information: None,
                        }))
                    }
                };

                Ok((arguments, body.to_vec()))
            } else {
                Ok((arguments, vec![]))
            }
        })
    }

    fn infer_block(&mut self, statements: Vec1<UntypedStatement>, location: SrcSpan) -> TypedExpr {
        self.expr_in_new_scope(|typer| {
            let statements = typer.infer_statements(statements);
            TypedExpr::Block {
                statements,
                location,
            }
        })
    }

    /// Returns `Ok(())` if the let is exhaustive, returns an `InexhaustiveLetAssignment`
    /// error if the given pattern doesn't cover all possible cases.
    ///
    fn check_let_exhaustiveness(
        &self,
        location: SrcSpan,
        subject: Arc<Type>,
        pattern: &TypedPattern,
    ) -> (CompileCaseResult, Result<(), Error>) {
        let mut case = exhaustiveness::CaseToCompile::new(&[subject]);
        case.add_pattern(pattern);
        let output = case.compile(self.environment);

        // Error for missing clauses that would cause a crash
        let result = if output.diagnostics.missing {
            Err(Error::InexhaustiveLetAssignment {
                location,
                missing: output.missing_patterns(self.environment),
            })
        } else {
            Ok(())
        };

        (output, result)
    }

    fn check_case_exhaustiveness(
        &mut self,
        location: SrcSpan,
        subject_types: &[Arc<Type>],
        clauses: &[TypedClause],
    ) -> CompiledCase {
        let mut case = exhaustiveness::CaseToCompile::new(subject_types);
        clauses.iter().for_each(|clause| case.add_clause(clause));
        let result = case.compile(self.environment);

        // Error for missing clauses that would cause a crash
        if result.diagnostics.missing {
            self.problems.error(Error::InexhaustiveCaseExpression {
                location,
                missing: result.missing_patterns(self.environment),
            });
        }

        // Emit warnings for unreachable patterns
        for (clause_index, clause) in clauses.iter().enumerate() {
            let patterns_iterator =
                std::iter::once(&clause.pattern).chain(clause.alternative_patterns.iter());

            for (pattern_index, multi_pattern) in patterns_iterator.enumerate() {
                match result.is_reachable(clause_index, pattern_index) {
                    Reachability::Reachable => {}
                    Reachability::Unreachable(reason) => {
                        let first = multi_pattern
                            .first()
                            .expect("All case expressions match at least one subject");
                        let last = multi_pattern
                            .last()
                            .expect("All case expressions match at least one subject");

                        let location = SrcSpan::new(first.location().start, last.location().end);

                        self.problems
                            .warning(Warning::UnreachableCasePattern { location, reason })
                    }
                }
            }
        }

        result.compiled_case
    }

    fn track_feature_usage(&mut self, feature_kind: FeatureKind, location: SrcSpan) {
        let minimum_required_version = feature_kind.required_version();

        // Then if the required version is not in the specified version for the
        // range we emit a warning highlighting the usage of the feature.
        if let Some(gleam_version) = &self.environment.gleam_version
            && let Some(lowest_allowed_version) = gleam_version.lowest_version()
        {
            // There is a version in the specified range that is lower than
            // the one required by this feature! This means that the
            // specified range is wrong and would allow someone to run a
            // compiler that is too old to know of this feature.
            if minimum_required_version > lowest_allowed_version {
                self.problems
                    .warning(Warning::FeatureRequiresHigherGleamVersion {
                        location,
                        feature_kind,
                        minimum_required_version: minimum_required_version.clone(),
                        wrongfully_allowed_version: lowest_allowed_version,
                    })
            }
        }

        if minimum_required_version > self.minimum_required_version {
            self.minimum_required_version = minimum_required_version;
        }
    }

    /// Checks if one of the options is a size option using an expression.
    /// This needs to be tracked as it was introduced in Gleam 1.12.0.
    fn check_segment_size_expression(&mut self, options: &[BitArrayOption<TypedExpr>]) {
        let Some(size_value) = options.iter().find_map(|option| match option {
            BitArrayOption::Size { value, .. } => Some(value),

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
            | BitArrayOption::Unit { .. } => None,
        }) else {
            return;
        };

        match size_value.as_ref() {
            // Ints and vars were always allowed from the start
            TypedExpr::Int { .. } | TypedExpr::Var { .. } => (),

            // Blocks and binops were added in Gleam 1.12.0!
            TypedExpr::Block { location, .. } | TypedExpr::BinOp { location, .. } => {
                self.track_feature_usage(FeatureKind::ExpressionInSegmentSize, *location)
            }

            // None of these are currently supported... for now!
            TypedExpr::Float { .. }
            | TypedExpr::String { .. }
            | TypedExpr::Pipeline { .. }
            | TypedExpr::Fn { .. }
            | TypedExpr::List { .. }
            | TypedExpr::Call { .. }
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
            | TypedExpr::Invalid { .. } => (),
        }
    }

    /// Checks if one of the options is a size option using an expression.
    /// This needs to be tracked as it was introduced in Gleam 1.12.0.
    ///
    /// This is basically the same as the function above working on expressions!
    fn check_constant_segment_size_expression(&self, options: &[BitArrayOption<TypedConstant>]) {
        let Some(size_value) = options.iter().find_map(|option| match option {
            BitArrayOption::Size { value, .. } => Some(value),

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
            | BitArrayOption::Unit { .. } => None,
        }) else {
            return;
        };

        // Expressions are not allowed in constants so for now nothing needs
        // tracking. Though this is handy to have already in place if we were to
        // lift this restriction for constant bit arrays as well!
        match size_value.as_ref() {
            // Ints and vars were always allowed from the start
            TypedConstant::Int { .. } | TypedConstant::Var { .. } => (),

            // None of these are currently supported... for now!
            Constant::Float { .. }
            | Constant::String { .. }
            | Constant::Tuple { .. }
            | Constant::List { .. }
            | Constant::Record { .. }
            | Constant::RecordUpdate { .. }
            | Constant::BitArray { .. }
            | Constant::StringConcatenation { .. }
            | Constant::Invalid { .. } => (),
        }
    }
}

/// Given a constants, this will change its type into the given one, turning
/// the constant into an `Invalid` one if necessary.
///
fn invalid_with_annotated_type(constant: TypedConstant, new_type: Arc<Type>) -> TypedConstant {
    // In case the types cannot be unified we change the inferred we
    // return a constant where the type matches the annotated one.
    // This can help minimise fals positive later on!
    match constant {
        // For simple variants that don't carry their own type we
        // replace them with an invalid constant with the same
        // location and the new annotated type.
        Constant::Int { location, .. }
        | Constant::Float { location, .. }
        | Constant::String { location, .. }
        | Constant::BitArray { location, .. }
        | Constant::StringConcatenation { location, .. } => TypedConstant::Invalid {
            location,
            type_: new_type,
            extra_information: None,
        },

        // In all other cases we don't want to lose information on
        // the actual structure of the invalid expression. So we just
        // replace the type with the annotated one.
        Constant::Invalid {
            location,
            type_: _,
            extra_information,
        } => Constant::Invalid {
            location,
            type_: new_type,
            extra_information,
        },

        Constant::Tuple {
            location,
            elements,
            type_: _,
        } => Constant::Tuple {
            location,
            elements,
            type_: new_type,
        },

        Constant::List {
            location,
            elements,
            type_: _,
        } => Constant::List {
            location,
            elements,
            type_: new_type,
        },

        Constant::Record {
            location,
            module,
            name,
            arguments,
            tag,
            type_: _,
            field_map,
            record_constructor,
        } => Constant::Record {
            location,
            module,
            name,
            arguments,
            tag,
            type_: new_type,
            field_map,
            record_constructor,
        },

        Constant::RecordUpdate {
            location,
            constructor_location,
            module,
            name,
            record,
            arguments,
            tag,
            type_: _,
            field_map,
        } => Constant::RecordUpdate {
            location,
            constructor_location,
            module,
            name,
            record,
            arguments,
            tag,
            type_: new_type,
            field_map,
        },

        Constant::Var {
            location,
            module,
            name,
            constructor,
            type_: _,
        } => Constant::Var {
            location,
            module,
            name,
            constructor,
            type_: new_type,
        },
    }
}

/// Returns `true` if the current module is one that the Gleam core team
/// maintains and we know it to be pure.
/// Used in purity tracking.
fn is_trusted_pure_module(environment: &Environment<'_>) -> bool {
    if environment.current_package != STDLIB_PACKAGE_NAME {
        return false;
    }

    // The gleam/io module has side effects
    if environment.current_module == "gleam/io" {
        return false;
    }

    // Test and dev modules may have side effects
    environment.origin == Origin::Src
}

#[derive(Debug, Clone)]
enum ReferenceRegistration {
    Register,
    DoNotRegister,

    /// A special case that happens if we're registering references for
    /// a variable call argument being passed to a function defined in the
    /// current module.
    VariableArgument {
        /// The name of the function being called, the function is defined in
        /// the current module.
        called_function: EcoString,
        /// The position where the variable is being passed as an argument.
        argument_index: usize,
    },
}

fn extract_typed_use_call_assignments(
    call: &TypedExpr,
    assignments_count: usize,
) -> Vec<UseAssignment<Arc<Type>>> {
    // A use call function has the use callback as its last argument, the
    // assignments will be the first statements in its body.
    let Some(use_callback_body) = call
        .call_arguments()
        .and_then(|call_arguments| call_arguments.last())
        .and_then(|last_call_argument| last_call_argument.value.fn_expression_body())
    else {
        return vec![];
    };

    // Once we get a hold of the callback function body we take out the first
    // `assignments_count` statements and turn those into typed
    // `UseAssignments`.
    //
    // Note how we can't just `.expect` them to be a `Statement::Assignment`
    // because in case of type errors those might be invalid expressions and we
    // don't want to crash the compiler in that case!
    use_callback_body
        .iter()
        .take(assignments_count)
        .map(|statement| match statement {
            Statement::Expression(_) | Statement::Use(_) | Statement::Assert(_) => None,
            Statement::Assignment(assignment) => Some(UseAssignment {
                location: assignment.location,
                pattern: assignment.pattern.clone(),
                annotation: assignment.annotation.clone(),
            }),
        })
        .collect::<Option<Vec<_>>>()
        .unwrap_or(vec![])
}

fn check_subject_for_redundant_match(
    subject: &TypedExpr,
    case_used_like_if: bool,
) -> Option<Warning> {
    match subject {
        TypedExpr::Tuple { elements, .. } if !elements.is_empty() => {
            Some(Warning::CaseMatchOnLiteralCollection {
                kind: LiteralCollectionKind::Tuple,
                location: subject.location(),
            })
        }

        TypedExpr::List { elements, tail, .. } if !elements.is_empty() || tail.is_some() => {
            Some(Warning::CaseMatchOnLiteralCollection {
                kind: LiteralCollectionKind::List,
                location: subject.location(),
            })
        }

        TypedExpr::BitArray { segments, .. } if !segments.is_empty() => {
            // We don't want a warning when matching on literal bit arrays
            // because it can make sense to do it; for example if one is
            // matching on segments that do not align with the segments used
            // for construction.
            None
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
        | TypedExpr::Invalid { .. } => match subject.record_constructor_arity() {
            // We make sure to not emit warnings if the case is being used like an
            // if expression:
            // ```gleam
            // case True {
            //   _ if condition -> todo
            //   _ if other_condition -> todo
            //   _ -> todo
            // }
            // ```
            Some(0) if !case_used_like_if => Some(Warning::CaseMatchOnLiteralValue {
                location: subject.location(),
            }),
            Some(0) => None,
            Some(_) => Some(Warning::CaseMatchOnLiteralCollection {
                kind: LiteralCollectionKind::Record,
                location: subject.location(),
            }),
            None if subject.is_literal() && !case_used_like_if => {
                Some(Warning::CaseMatchOnLiteralValue {
                    location: subject.location(),
                })
            }
            None => None,
        },
    }
}

/// Returns the kind of an empty list check.
///
/// Based on the binary operator being used and the position of the operands we
/// can categorize an empty list check in one of two ways:
///   - Checking for the empty list
///   - Checking for a non-empty list
fn get_empty_list_check_kind<'a>(
    binop: BinOp,
    left: &'a TypedExpr,
    right: &'a TypedExpr,
) -> Option<EmptyListCheckKind> {
    match (&left, &right) {
        // For `==` and `!=` we don't care which side each of the operands are on.
        (_, TypedExpr::Int { value, .. }) | (TypedExpr::Int { value, .. }, _)
            if binop == BinOp::Eq || binop == BinOp::NotEq =>
        {
            match (binop, value.as_str()) {
                (BinOp::Eq, "0" | "-0") => Some(EmptyListCheckKind::Empty),
                (BinOp::NotEq, "0" | "-0") => Some(EmptyListCheckKind::NonEmpty),
                _ => None,
            }
        }
        (_, TypedExpr::Int { value, .. }) => match (binop, value.as_str()) {
            (BinOp::LtEqInt, "0" | "-0") | (BinOp::LtInt, "1") => Some(EmptyListCheckKind::Empty),
            (BinOp::GtInt, "0" | "-0") => Some(EmptyListCheckKind::NonEmpty),
            _ => None,
        },
        (TypedExpr::Int { value, .. }, _) => match (binop, value.as_str()) {
            (BinOp::GtEqInt | BinOp::LtInt, "0" | "-0") | (BinOp::GtInt, "1") => {
                Some(EmptyListCheckKind::NonEmpty)
            }
            _ => None,
        },
        _ => None,
    }
}

struct UseCall {
    function: Box<UntypedExpr>,
    arguments: Vec<CallArg<UntypedExpr>>,
}

fn get_use_expression_call(call: UntypedExpr) -> UseCall {
    // Ensure that the use's call is of the right structure. i.e. it is a
    // call to a function.
    if let UntypedExpr::Call {
        fun: function,
        arguments,
        ..
    } = call
    {
        UseCall {
            arguments,
            function,
        }
    } else {
        UseCall {
            function: Box::new(call),
            arguments: vec![],
        }
    }
}

#[derive(Debug, Default)]
struct UseAssignments {
    /// With sugar
    /// ```gleam
    /// use Box(x) = ...
    /// ```
    /// Without sugar
    /// ```gleam
    /// fn(_use1) { let Box(x) = _use1 }
    /// // ^^^^^ The function arguments
    /// ```
    function_arguments: Vec<UntypedArg>,

    /// With sugar
    /// ```gleam
    /// use Box(x) = ...
    /// ```
    /// Without sugar
    /// ```gleam
    /// fn(_use1) { let Box(x) = _use1 }
    /// //          ^^^^^^^^^^^^^^^^^^ The body assignments
    /// ```
    body_assignments: Vec<UntypedStatement>,
}

impl UseAssignments {
    fn from_use_expression(sugar_assignments: Vec<UntypedUseAssignment>) -> UseAssignments {
        let mut assignments = UseAssignments::default();

        for (index, assignment) in sugar_assignments.into_iter().enumerate() {
            let UseAssignment {
                location,
                pattern,
                annotation,
            } = assignment;
            match pattern {
                // For discards we add a discard function arguments.
                Pattern::Discard { name, .. } => assignments.function_arguments.push(Arg {
                    location,
                    names: ArgNames::Discard { name, location },
                    annotation: None,
                    type_: (),
                }),

                // For simple patterns of a single variable we add a regular
                // function argument.
                Pattern::Variable { name, .. } => assignments.function_arguments.push(Arg {
                    location,
                    annotation,
                    names: ArgNames::Named { name, location },
                    type_: (),
                }),

                // For more complex patterns we add a function argument and also
                // an assignment in the function body to handle the pattern.
                pattern @ (Pattern::Int { .. }
                | Pattern::Float { .. }
                | Pattern::String { .. }
                | Pattern::BitArraySize { .. }
                | Pattern::Assign { .. }
                | Pattern::List { .. }
                | Pattern::Constructor { .. }
                | Pattern::Tuple { .. }
                | Pattern::BitArray { .. }
                | Pattern::StringPrefix { .. }
                | Pattern::Invalid { .. }) => {
                    let name: EcoString = format!("{USE_ASSIGNMENT_VARIABLE}{index}").into();
                    assignments.function_arguments.push(Arg {
                        location,
                        names: ArgNames::Named {
                            name: name.clone(),
                            location,
                        },
                        annotation: None,
                        type_: (),
                    });
                    let assignment = Assignment {
                        location,
                        pattern,
                        annotation,
                        compiled_case: CompiledCase::failure(),
                        kind: AssignmentKind::Generated,
                        value: UntypedExpr::Var { location, name },
                    };
                    assignments
                        .body_assignments
                        .push(Statement::Assignment(Box::new(assignment)))
                }
            }
        }

        assignments
    }
}

#[derive(Debug)]
struct RecordUpdateVariant<'a> {
    arguments: Vec<Arc<Type>>,
    return_type: Arc<Type>,
    field_map: &'a FieldMap,
}

impl RecordUpdateVariant<'_> {
    fn arg_type(&self, index: u32) -> Arc<Type> {
        self.arguments
            .get(index as usize)
            .expect("Failed to get record argument type after successfully inferring that field")
            .clone()
    }

    fn has_field(&self, str: &EcoString) -> bool {
        self.field_map.fields.contains_key(str)
    }

    fn field_names(&self) -> Vec<EcoString> {
        self.field_map.fields.keys().cloned().collect()
    }
}

#[derive(Debug, Eq, PartialEq, Clone, Copy, serde::Serialize, serde::Deserialize)]
pub enum ComparisonOutcome {
    AlwaysFails,
    AlwaysSucceeds,
}

enum StaticComparison {
    /// When we can statically tell that two values are going to be exactly the
    /// same.
    CertainlyEqual,
    /// When we can statically tell that two values are not going to be the
    /// same.
    CertainlyDifferent,
    /// When it's impossible to statically tell if two values are the same.
    CantTell,
}

fn static_compare(one: &TypedExpr, other: &TypedExpr) -> StaticComparison {
    if one.is_record_constructor_function() && other.is_record_constructor_function() {
        return StaticComparison::CantTell;
    }

    match (one, other) {
        (
            TypedExpr::Var {
                name: one,
                constructor: constructor_one,
                ..
            },
            TypedExpr::Var {
                name: other,
                constructor: constructor_other,
                ..
            },
        ) => match (&constructor_one.variant, &constructor_other.variant) {
            (
                ValueConstructorVariant::LocalVariable { .. },
                ValueConstructorVariant::LocalVariable { .. },
            )
            | (
                ValueConstructorVariant::ModuleConstant { .. },
                ValueConstructorVariant::ModuleConstant { .. },
            )
            | (ValueConstructorVariant::Record { .. }, ValueConstructorVariant::Record { .. })
                if one == other =>
            {
                StaticComparison::CertainlyEqual
            }

            (
                ValueConstructorVariant::Record {
                    variant_index: index_one,
                    ..
                },
                ValueConstructorVariant::Record {
                    variant_index: index_other,
                    ..
                },
            ) if index_one != index_other => StaticComparison::CertainlyDifferent,

            (
                ValueConstructorVariant::LocalVariable { .. }
                | ValueConstructorVariant::ModuleConstant { .. }
                | ValueConstructorVariant::ModuleFn { .. }
                | ValueConstructorVariant::Record { .. },
                _,
            ) => StaticComparison::CantTell,
        },

        (TypedExpr::Int { int_value: n, .. }, TypedExpr::Int { int_value: m, .. }) => {
            if n == m {
                StaticComparison::CertainlyEqual
            } else {
                StaticComparison::CertainlyDifferent
            }
        }

        (TypedExpr::Float { float_value: n, .. }, TypedExpr::Float { float_value: m, .. }) => {
            if n == m {
                StaticComparison::CertainlyEqual
            } else {
                StaticComparison::CertainlyDifferent
            }
        }

        (TypedExpr::String { value: one, .. }, TypedExpr::String { value: other, .. }) => {
            if one == other {
                StaticComparison::CertainlyEqual
            } else {
                StaticComparison::CertainlyDifferent
            }
        }

        (TypedExpr::NegateInt { value: one, .. }, TypedExpr::NegateInt { value: other, .. })
        | (TypedExpr::NegateBool { value: one, .. }, TypedExpr::NegateBool { value: other, .. }) => {
            static_compare(one, other)
        }

        (
            TypedExpr::List {
                elements: elements_one,
                tail: tail_one,
                ..
            },
            TypedExpr::List {
                elements: elements_other,
                tail: tail_other,
                ..
            },
        ) => {
            match (tail_one, tail_other) {
                (Some(one_tail), Some(other_tail)) => match static_compare(one_tail, other_tail) {
                    StaticComparison::CertainlyDifferent => {
                        return StaticComparison::CertainlyDifferent;
                    }
                    StaticComparison::CantTell => return StaticComparison::CantTell,
                    StaticComparison::CertainlyEqual => (),
                },
                (None, Some(_)) | (Some(_), None) => return StaticComparison::CantTell,
                (None, None) => (),
            };

            // If we can tell the two lists have a different number of items
            // then we know it's never going to match.
            if elements_one.len() != elements_other.len() {
                return StaticComparison::CertainlyDifferent;
            }

            let mut comparison = StaticComparison::CertainlyEqual;
            for (one, other) in elements_one.iter().zip(elements_other.iter()) {
                match static_compare(one, other) {
                    StaticComparison::CertainlyEqual => (),
                    StaticComparison::CertainlyDifferent => {
                        return StaticComparison::CertainlyDifferent;
                    }
                    StaticComparison::CantTell => comparison = StaticComparison::CantTell,
                }
            }
            comparison
        }

        (
            TypedExpr::Tuple {
                elements: elements_one,
                ..
            },
            TypedExpr::Tuple {
                elements: elements_other,
                ..
            },
        ) => {
            let mut comparison = StaticComparison::CertainlyEqual;
            for (one, other) in elements_one.iter().zip(elements_other.iter()) {
                match static_compare(one, other) {
                    StaticComparison::CertainlyEqual => (),
                    StaticComparison::CertainlyDifferent => {
                        return StaticComparison::CertainlyDifferent;
                    }
                    StaticComparison::CantTell => comparison = StaticComparison::CantTell,
                }
            }
            comparison
        }

        (
            TypedExpr::ModuleSelect {
                constructor: constructor_one,
                module_name: module_name_one,
                ..
            },
            TypedExpr::ModuleSelect {
                constructor: constructor_other,
                module_name: module_name_other,
                ..
            },
        ) => {
            if module_name_one == module_name_other && constructor_one == constructor_other {
                StaticComparison::CertainlyEqual
            } else {
                StaticComparison::CantTell
            }
        }

        (
            TypedExpr::Call {
                fun: fun_one,
                arguments: arguments_one,
                ..
            },
            TypedExpr::Call {
                fun: fun_other,
                arguments: arguments_other,
                ..
            },
        ) => match (fun_one.variant_index(), fun_other.variant_index()) {
            // Both have to be literal record builders, otherwise we can't really tell
            // anything at compile time!
            (None, _) | (_, None) => StaticComparison::CantTell,

            // If they're both literal record builders and are building different
            // variants, then we know they'll always be different.
            (Some(index_one), Some(index_other)) if index_one != index_other => {
                StaticComparison::CertainlyDifferent
            }

            // Otherwise we need to check their arguments pairwise:
            (Some(_), Some(_)) => {
                let mut comparison = StaticComparison::CertainlyEqual;
                for (one, other) in arguments_one.iter().zip(arguments_other.iter()) {
                    match static_compare(&one.value, &other.value) {
                        StaticComparison::CertainlyEqual => (),
                        // If we can tell any of the arguments are never going to
                        // be the same then we can short circuit and be sure
                        // that the two variants are not the same as well!
                        StaticComparison::CertainlyDifferent => {
                            return StaticComparison::CertainlyDifferent;
                        }
                        // If we can't compare two of the arguments then there's
                        // nothing we can tell at compile time. Notice how we
                        // don't short circuit here: we still want to go over all
                        // the other arguments because we might find two that are
                        // certainly going to be different!
                        StaticComparison::CantTell => comparison = StaticComparison::CantTell,
                    }
                }
                comparison
            }
        },

        // If we're building two variants with a different index then we can
        // tell for sure they're going to be different.
        (one, other)
            if one
                .variant_index()
                .is_some_and(|one| other.variant_index().is_some_and(|other| one != other)) =>
        {
            StaticComparison::CertainlyDifferent
        }

        (
            TypedExpr::RecordAccess {
                index: index_one,
                record: record_one,
                ..
            },
            TypedExpr::RecordAccess {
                index: index_other,
                record: record_other,
                ..
            },
        ) => match static_compare(record_one, record_other) {
            StaticComparison::CertainlyEqual if index_one == index_other => {
                StaticComparison::CertainlyEqual
            }
            StaticComparison::CertainlyEqual
            | StaticComparison::CertainlyDifferent
            | StaticComparison::CantTell => StaticComparison::CantTell,
        },

        // TODO: For complex expressions we just give up, maybe in future we
        // could be smarter and perform further comparisons but it sounds like
        // there's no huge value in this.
        //
        (_, _) => StaticComparison::CantTell,
    }
}
