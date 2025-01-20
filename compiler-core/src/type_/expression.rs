use super::{pipe::PipeTyper, *};
use crate::{
    analyse::{infer_bit_array_option, name::check_argument_names},
    ast::{
        Arg, Assignment, AssignmentKind, BinOp, BitArrayOption, BitArraySegment, CallArg, Clause,
        ClauseGuard, Constant, FunctionLiteralKind, HasLocation, ImplicitCallArgOrigin, Layer,
        RecordBeingUpdated, SrcSpan, Statement, TodoKind, TypeAst, TypedArg, TypedAssignment,
        TypedClause, TypedClauseGuard, TypedConstant, TypedExpr, TypedMultiPattern, TypedStatement,
        UntypedArg, UntypedAssignment, UntypedClause, UntypedClauseGuard, UntypedConstant,
        UntypedConstantBitArraySegment, UntypedExpr, UntypedExprBitArraySegment,
        UntypedMultiPattern, UntypedStatement, UntypedUse, UntypedUseAssignment, Use,
        UseAssignment, RECORD_UPDATE_VARIABLE, USE_ASSIGNMENT_VARIABLE,
    },
    build::Target,
    exhaustiveness::{self, Reachability},
};
use hexpm::version::Version;
use id_arena::Arena;
use im::hashmap;
use itertools::Itertools;
use num_bigint::BigInt;
use vec1::Vec1;

#[derive(Clone, Copy, Debug, Eq, PartialOrd, Ord, PartialEq, Serialize)]
pub struct Implementations {
    /// Wether the function has a pure-gleam implementation.
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
    /// Wether the function has an implementation that uses external erlang
    /// code.
    pub uses_erlang_externals: bool,
    /// Wether the function has an implementation that uses external javascript
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

        hydrator.permit_holes(true);
        Self {
            hydrator,
            previous_panics: false,
            already_warned_for_unreachable_code: false,
            environment,
            implementations,
            current_function_definition: definition,
            minimum_required_version: Version::new(0, 1, 0),
            problems,
        }
    }

    pub fn in_new_scope<T, E>(
        &mut self,
        process_scope: impl FnOnce(&mut Self) -> Result<T, E>,
    ) -> Result<T, E> {
        // Create new scope
        let environment_reset_data = self.environment.open_new_scope();
        let hydrator_reset_data = self.hydrator.open_new_scope();

        // Process the scope
        let result = process_scope(self);

        // Close scope, discarding any scope local state
        self.environment
            .close_scope(environment_reset_data, result.is_ok(), self.problems);
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

    pub fn infer(&mut self, expr: UntypedExpr) -> Result<TypedExpr, Error> {
        if self.previous_panics {
            self.warn_for_unreachable_code(expr.location(), PanicPosition::PreviousExpression);
        }

        match expr {
            UntypedExpr::Todo {
                location,
                message: label,
                kind,
                ..
            } => self.infer_todo(location, kind, label),

            // A placeholder is used when the author has not provided a function
            // body, instead only giving an external implementation for this
            // target. This placeholder implementation will never be used so we
            // treat it as a `panic` expression during analysis.
            UntypedExpr::Placeholder { location } => self.infer_panic(location, None),

            UntypedExpr::Panic {
                location, message, ..
            } => self.infer_panic(location, message),

            UntypedExpr::Var { location, name, .. } => self.infer_var(name, location),

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
            } => self.infer_block(statements, location),

            UntypedExpr::Tuple {
                location, elems, ..
            } => self.infer_tuple(elems, location),

            UntypedExpr::Float {
                location, value, ..
            } => {
                if self.environment.target == Target::Erlang
                    && !self.current_function_definition.has_erlang_external
                {
                    check_erlang_float_safety(&value, location, self.problems)
                }

                Ok(self.infer_float(value, location))
            }

            UntypedExpr::String {
                location, value, ..
            } => Ok(self.infer_string(value, location)),

            UntypedExpr::PipeLine { expressions } => Ok(self.infer_pipeline(expressions)),

            UntypedExpr::Fn {
                location,
                kind,
                arguments: args,
                body,
                return_annotation,
                ..
            } => self.infer_fn(args, &[], body, kind, return_annotation, location),

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
            } => self.infer_list(elements, tail, location),

            UntypedExpr::Call {
                location,
                fun,
                arguments: args,
                ..
            } => Ok(self.infer_call(*fun, args, location, CallKind::Function)),

            UntypedExpr::BinOp {
                location,
                name,
                left,
                right,
                ..
            } => self.infer_binop(name, *left, *right, location),

            UntypedExpr::FieldAccess {
                label_location,
                label,
                container,
                ..
            } => Ok(self.infer_field_access(
                *container,
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
                arguments: args,
            } => self.infer_record_update(*constructor, record, args, location),

            UntypedExpr::NegateBool { location, value } => self.infer_negate_bool(location, *value),

            UntypedExpr::NegateInt { location, value } => self.infer_negate_int(location, *value),
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
    ) -> Result<TypedExpr, Error> {
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

        let message = message
            .map(|message| {
                // If there is a message expression then it must be a string.
                let message = self.infer(*message)?;
                unify(string(), message.type_())
                    .map_err(|e| convert_unify_error(e, message.location()))?;
                Ok(Box::new(message))
            })
            .transpose()?;

        Ok(TypedExpr::Todo {
            location,
            type_,
            message,
            kind,
        })
    }

    fn infer_panic(
        &mut self,
        location: SrcSpan,
        message: Option<Box<UntypedExpr>>,
    ) -> Result<TypedExpr, Error> {
        let type_ = self.new_unbound_var();
        let message = match message {
            Some(message) => {
                let message = self.infer(*message)?;
                unify(string(), message.type_())
                    .map_err(|e| convert_unify_error(e, message.location()))?;
                Some(Box::new(message))
            }
            None => None,
        };
        self.previous_panics = true;
        Ok(TypedExpr::Panic {
            location,
            type_,
            message,
        })
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

    fn infer_float(&mut self, value: EcoString, location: SrcSpan) -> TypedExpr {
        TypedExpr::Float {
            location,
            value,
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
        }
    }

    // Helper to push a new error to the errors list and return an invalid pattern.
    fn error_pattern_with_rigid_names(
        &mut self,
        location: SrcSpan,
        error: Error,
        type_: Arc<Type>,
    ) -> TypedPattern {
        self.problems.error(error);
        Pattern::Invalid { location, type_ }
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
                    let expression = match self.infer(expression) {
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
                    let assignment = self.infer_assignment(assignment);
                    statements.push(Statement::Assignment(assignment));
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
            .find_or_last(|e| e.is_use())
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

        let call = self.infer_call(
            *call.function,
            call.arguments,
            call_location,
            CallKind::Use {
                call_location: use_call_location,
                assignments_location: use_.assignments_location,
                last_statement_location,
            },
        );

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

    fn infer_negate_bool(
        &mut self,
        location: SrcSpan,
        value: UntypedExpr,
    ) -> Result<TypedExpr, Error> {
        let value = self.infer(value)?;

        unify(bool(), value.type_()).map_err(|e| convert_unify_error(e, value.location()))?;

        if let TypedExpr::NegateBool { .. } = value {
            self.problems
                .warning(Warning::UnnecessaryDoubleBoolNegation { location });
        }

        Ok(TypedExpr::NegateBool {
            location,
            value: Box::new(value),
        })
    }

    fn infer_negate_int(
        &mut self,
        location: SrcSpan,
        value: UntypedExpr,
    ) -> Result<TypedExpr, Error> {
        let value = self.infer(value)?;

        unify(int(), value.type_()).map_err(|e| convert_unify_error(e, value.location()))?;

        if let TypedExpr::Int { value: ref v, .. } = value {
            if v.starts_with('-') {
                self.problems
                    .warning(Warning::UnnecessaryDoubleIntNegation { location });
            }
        }

        if let TypedExpr::NegateInt { .. } = value {
            self.problems
                .warning(Warning::UnnecessaryDoubleIntNegation { location });
        }

        Ok(TypedExpr::NegateInt {
            location,
            value: Box::new(value),
        })
    }

    fn infer_fn(
        &mut self,
        args: Vec<UntypedArg>,
        expected_args: &[Arc<Type>],
        body: Vec1<UntypedStatement>,
        kind: FunctionLiteralKind,
        return_annotation: Option<TypeAst>,
        location: SrcSpan,
    ) -> Result<TypedExpr, Error> {
        for Arg { names, .. } in args.iter() {
            check_argument_names(names, self.problems);
        }

        let already_warned_for_unreachable_code = self.already_warned_for_unreachable_code;
        self.already_warned_for_unreachable_code = false;
        self.previous_panics = false;

        let (args, body) = self.do_infer_fn(args, expected_args, body, &return_annotation)?;
        let args_types = args.iter().map(|a| a.type_.clone()).collect();
        let type_ = fn_(args_types, body.last().type_());

        // Defining an anonymous function never panics.
        self.already_warned_for_unreachable_code = already_warned_for_unreachable_code;
        self.previous_panics = false;

        Ok(TypedExpr::Fn {
            location,
            type_,
            kind,
            args,
            body,
            return_annotation,
        })
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
            .map(|t| self.type_from_ast(&t))
            .unwrap_or_else(|| Ok(self.new_unbound_var()))?;

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
        args: Vec<CallArg<UntypedExpr>>,
        location: SrcSpan,
        kind: CallKind,
    ) -> TypedExpr {
        let (fun, args, type_) = self.do_infer_call(fun, args, location, kind);

        // One common mistake is to think that the syntax for adding a message
        // to a `todo` or a `panic` exception is to `todo("...")`, but really
        // this does nothing as the `todo` or `panic` throws the exception
        // before it gets to the function call `("...")`.
        // If we find code doing this then emit a warning.
        let todopanic = match fun {
            TypedExpr::Todo { .. } => Some((location, TodoOrPanic::Todo)),
            TypedExpr::Panic { .. } => Some((location, TodoOrPanic::Panic)),
            _ => None,
        };
        if let Some((location, kind)) = todopanic {
            let args_location = match (args.first(), args.last()) {
                (Some(first), Some(last)) => Some(SrcSpan {
                    start: first.location().start,
                    end: last.location().end,
                }),
                _ => None,
            };
            self.problems.warning(Warning::TodoOrPanicUsedAsFunction {
                kind,
                location,
                args_location,
                args: args.len(),
            });
        }

        TypedExpr::Call {
            location,
            type_,
            args,
            fun: Box::new(fun),
        }
    }

    fn infer_list(
        &mut self,
        elements: Vec<UntypedExpr>,
        tail: Option<Box<UntypedExpr>>,
        location: SrcSpan,
    ) -> Result<TypedExpr, Error> {
        let type_ = self.new_unbound_var();
        // Type check each elements
        let elements = elements
            .into_iter()
            .map(|element| {
                let element = self.infer(element)?;
                // Ensure they all have the same type
                unify(type_.clone(), element.type_()).map_err(|e| {
                    convert_unify_error(e.list_element_mismatch(), element.location())
                })?;
                Ok(element)
            })
            .try_collect()?;
        // Type check the ..tail, if there is one
        let type_ = list(type_);
        let tail = match tail {
            Some(tail) => {
                let tail = self.infer(*tail)?;
                // Ensure the tail has the same type as the preceding elements
                unify(type_.clone(), tail.type_())
                    .map_err(|e| convert_unify_error(e.list_tail_mismatch(), tail.location()))?;
                Some(Box::new(tail))
            }
            None => None,
        };
        Ok(TypedExpr::List {
            location,
            type_,
            elements,
            tail,
        })
    }

    fn infer_tuple(
        &mut self,
        elems: Vec<UntypedExpr>,
        location: SrcSpan,
    ) -> Result<TypedExpr, Error> {
        let elems: Vec<_> = elems.into_iter().map(|e| self.infer(e)).try_collect()?;
        let type_ = tuple(elems.iter().map(HasType::type_).collect());
        Ok(TypedExpr::Tuple {
            location,
            elems,
            type_,
        })
    }

    fn infer_var(&mut self, name: EcoString, location: SrcSpan) -> Result<TypedExpr, Error> {
        let constructor = self.infer_value_constructor(&None, &name, &location)?;
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
            | ValueConstructorVariant::LocalVariable { .. }
            | ValueConstructorVariant::LocalConstant { .. } => return Ok(()),
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
        label: EcoString,
        label_location: SrcSpan,
        usage: FieldAccessUsage,
    ) -> TypedExpr {
        // Computes a potential module access. This will be used if a record access can't be used.
        // Computes both the inferred access and if it shadows a variable.
        let module_access = match &container {
            UntypedExpr::Var { location, name } => {
                let module_access =
                    self.infer_module_access(name, label.clone(), location, label_location);
                // Returns the result and if it shadows an existing variable in scope
                Some((module_access, self.environment.scope.contains_key(name)))
            }
            _ => None,
        };
        let record = self.infer(container);
        // TODO: is this clone avoidable? we need to box the record for inference in both
        // the success case and in the valid record but invalid label case
        let record_access = match record.clone() {
            Ok(record) => self.infer_known_record_expression_access(
                record,
                label.clone(),
                label_location,
                usage,
            ),
            Err(e) => Err(e),
        };
        match (record_access, module_access) {
            // Record access is valid
            (Ok(record_access), _) => record_access,
            // Record access is invalid but module access is valid
            (_, Some((Ok(module_access), _))) => module_access,
            // Module access was attempted but failed and it does not shadow an existing variable
            (_, Some((Err(module_access_err), false))) => {
                self.problems.error(module_access_err);
                TypedExpr::Invalid {
                    location: label_location,
                    type_: self.new_unbound_var(),
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
                        location: label_location,
                        type_: self.new_unbound_var(),
                        label: "".into(),
                        index: u64::MAX,
                        record: Box::new(record),
                    },
                    Err(_) => TypedExpr::Invalid {
                        location: label_location,
                        type_: self.new_unbound_var(),
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

        let tuple = self.infer(tuple)?;
        match collapse_links(tuple.type_()).as_ref() {
            Type::Tuple { elems } => {
                let type_ = elems
                    .get(index as usize)
                    .ok_or_else(|| Error::OutOfBoundsTupleIndex {
                        location: SrcSpan {
                            start: tuple.location().end,
                            end: location.end,
                        },
                        index,
                        size: elems.len(),
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

            _ => Err(Error::NotATuple {
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
            .map(|s| {
                let options = match s.value.as_ref() {
                    UntypedExpr::String { location, .. } if s.options.is_empty() => {
                        self.track_feature_usage(
                            FeatureKind::UnannotatedUtf8StringSegment,
                            *location,
                        );
                        vec![BitArrayOption::Utf8 {
                            location: SrcSpan::default(),
                        }]
                    }
                    _ => s.options,
                };
                self.infer_bit_segment(*s.value, options, s.location, |env, expr| env.infer(expr))
            })
            .try_collect()?;

        Ok(TypedExpr::BitArray {
            location,
            segments,
            type_: bits(),
        })
    }

    fn infer_constant_bit_array(
        &mut self,
        segments: Vec<UntypedConstantBitArraySegment>,
        location: SrcSpan,
    ) -> Result<TypedConstant, Error> {
        let segments = segments
            .into_iter()
            .map(|s| {
                let options = match s.value.as_ref() {
                    Constant::String { location, .. } if s.options.is_empty() => {
                        self.track_feature_usage(
                            FeatureKind::UnannotatedUtf8StringSegment,
                            *location,
                        );
                        vec![BitArrayOption::Utf8 {
                            location: SrcSpan::default(),
                        }]
                    }
                    _ => s.options,
                };
                self.infer_bit_segment(*s.value, options, s.location, |env, expr| {
                    Ok(env.infer_const(&None, expr))
                })
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

        let type_ = bit_array::type_options_for_value(&options).map_err(|error| {
            Error::BitArraySegmentError {
                error: error.error,
                location: error.location,
            }
        })?;

        unify(type_.clone(), value.type_())
            .map_err(|e| convert_unify_error(e, value.location()))?;

        Ok(BitArraySegment {
            location,
            type_,
            value: Box::new(value),
            options,
        })
    }

    fn infer_binop(
        &mut self,
        name: BinOp,
        left: UntypedExpr,
        right: UntypedExpr,
        location: SrcSpan,
    ) -> Result<TypedExpr, Error> {
        let (input_type, output_type) = match &name {
            BinOp::Eq | BinOp::NotEq => {
                let left = self.infer(left)?;
                let right = self.infer(right)?;
                unify(left.type_(), right.type_())
                    .map_err(|e| convert_unify_error(e, right.location()))?;

                self.check_for_inefficient_empty_list_check(name, &left, &right, location);

                return Ok(TypedExpr::BinOp {
                    location,
                    name,
                    type_: bool(),
                    left: Box::new(left),
                    right: Box::new(right),
                });
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

        let left = self.infer(left)?;
        unify(input_type.clone(), left.type_()).map_err(|e| {
            e.operator_situation(name)
                .into_error(left.type_defining_location())
        })?;
        let right = self.infer(right)?;
        unify(input_type, right.type_()).map_err(|e| {
            e.operator_situation(name)
                .into_error(right.type_defining_location())
        })?;

        self.check_for_inefficient_empty_list_check(name, &left, &right, location);

        Ok(TypedExpr::BinOp {
            location,
            name,
            type_: output_type,
            left: Box::new(left),
            right: Box::new(right),
        })
    }

    /// Checks for inefficient usage of `list.length` for checking for the empty list.
    ///
    /// If we find one of these usages, emit a warning to use `list.is_empty` instead.
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
        let (module_name, module_alias, label) = match fun.as_ref() {
            TypedExpr::ModuleSelect {
                module_name,
                module_alias,
                label,
                ..
            } => (module_name, module_alias, label),
            _ => return,
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
        if list_module.package != crate::STDLIB_PACKAGE_NAME {
            return;
        }

        // Check the kind of the empty list check so we know whether to recommend
        // `list.is_empty` or `!list.is_empty` as a replacement.
        let kind = match get_empty_list_check_kind(binop, left, right) {
            Some(kind) => kind,
            None => return,
        };

        // If we've gotten this far, go ahead and emit the warning.
        self.problems
            .warning(Warning::InefficientEmptyListCheck { location, kind });
    }

    fn infer_assignment(&mut self, assignment: UntypedAssignment) -> TypedAssignment {
        let Assignment {
            pattern,
            value,
            kind,
            annotation,
            location,
        } = assignment;
        let value_location = value.location();
        let value = match self.in_new_scope(|value_typer| value_typer.infer(*value)) {
            Ok(value) => value,
            Err(error) => {
                self.problems.error(error);
                self.error_expr(value_location)
            }
        };

        let value_typ = value.type_();

        // Ensure the pattern matches the type of the value
        let pattern_location = pattern.location();
        let mut pattern_typer = pattern::PatternTyper::new(
            self.environment,
            &self.implementations,
            &self.hydrator,
            self.problems,
        );
        let unify_result = pattern_typer.unify(pattern, value_typ.clone(), None);

        let minimum_required_version = pattern_typer.minimum_required_version;
        if minimum_required_version > self.minimum_required_version {
            self.minimum_required_version = minimum_required_version;
        }

        let pattern = match unify_result {
            Ok(pattern) => pattern,
            Err(error) => {
                self.error_pattern_with_rigid_names(pattern_location, error, value_typ.clone())
            }
        };

        // Check that any type annotation is accurate.
        if let Some(annotation) = &annotation {
            match self
                .type_from_ast(annotation)
                .map(|t| self.instantiate(t, &mut hashmap![]))
            {
                Ok(ann_typ) => {
                    if let Err(error) = unify(ann_typ, value_typ.clone())
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

        // Do not perform exhaustiveness checking if user explicitly used `let assert ... = ...`.
        let exhaustiveness_check = self.check_let_exhaustiveness(location, value.type_(), &pattern);
        match (&kind, exhaustiveness_check) {
            (AssignmentKind::Let | AssignmentKind::Generated, Ok(_)) => {}
            (AssignmentKind::Let | AssignmentKind::Generated, Err(e)) => {
                self.problems.error(e);
            }
            (AssignmentKind::Assert { location, .. }, Ok(_)) => {
                self.problems.warning(Warning::RedundantAssertAssignment {
                    location: *location,
                })
            }
            (AssignmentKind::Assert { .. }, _) => {}
        }

        let kind = match self.infer_assignment_kind(kind) {
            Ok(kind) => kind,
            Err(error) => {
                self.problems.error(error);
                AssignmentKind::Generated
            }
        };

        Assignment {
            location,
            annotation,
            kind,
            pattern,
            value: Box::new(value),
        }
    }

    fn infer_assignment_kind(
        &mut self,
        kind: AssignmentKind<UntypedExpr>,
    ) -> Result<AssignmentKind<TypedExpr>, Error> {
        match kind {
            AssignmentKind::Let => Ok(AssignmentKind::Let),
            AssignmentKind::Generated => Ok(AssignmentKind::Generated),
            AssignmentKind::Assert { location, message } => {
                let message = match message {
                    Some(message) => {
                        self.track_feature_usage(
                            FeatureKind::LetAssertWithMessage,
                            message.location(),
                        );
                        let message = self.infer(*message)?;

                        unify(string(), message.type_())
                            .map_err(|e| convert_unify_error(e, message.location()))?;
                        Some(Box::new(message))
                    }
                    None => None,
                };
                Ok(AssignmentKind::Assert { location, message })
            }
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
            let subject_location = subject.location();
            let subject = self.in_new_scope(|subject_typer| {
                let subject = subject_typer.infer(subject)?;
                Ok(subject)
            });
            let subject = match subject {
                Ok(subject) => subject,
                Err(error) => {
                    self.problems.error(error);
                    self.error_expr(subject_location)
                }
            };

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
        for clause in clauses {
            has_a_guard = has_a_guard || clause.guard.is_some();
            all_patterns_are_discards =
                all_patterns_are_discards && clause.pattern.iter().all(|p| p.is_discard());

            self.previous_panics = false;
            let typed_clause = self.infer_clause(clause, &typed_subjects);
            all_clauses_panic = all_clauses_panic && self.previous_panics;

            if let Err(e) = unify(return_type.clone(), typed_clause.then.type_()).map_err(|e| {
                e.case_clause_mismatch(typed_clause.location)
                    .into_error(typed_clause.then.type_defining_location())
            }) {
                self.problems.error(e);
            }
            typed_clauses.push(typed_clause);
        }

        self.previous_panics = all_clauses_panic || any_subject_panics;

        if let Err(e) = self.check_case_exhaustiveness(location, &subject_types, &typed_clauses) {
            self.problems.error(e);
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
            type_: return_type,
            subjects: typed_subjects,
            clauses: typed_clauses,
        }
    }

    fn infer_clause(&mut self, clause: UntypedClause, subjects: &[TypedExpr]) -> TypedClause {
        let Clause {
            pattern,
            alternative_patterns,
            guard,
            then,
            location,
        } = clause;
        let then_location = then.location();

        let scoped_clause_inference = self.in_new_scope(|clause_typer| {
            // Check the types
            let (typed_pattern, typed_alternatives) = match clause_typer.infer_clause_pattern(
                pattern,
                alternative_patterns,
                subjects,
                &location,
            ) {
                Ok(res) => res,
                // If an error occurs inferring patterns then assume no patterns
                Err(error) => {
                    clause_typer.problems.error(error);
                    (vec![], vec![])
                }
            };
            let guard = match clause_typer.infer_optional_clause_guard(guard) {
                Ok(guard) => guard,
                // If an error occurs inferring guard then assume no guard
                Err(error) => {
                    clause_typer.problems.error(error);
                    None
                }
            };
            let then = match clause_typer.infer(then) {
                Ok(then) => then,
                Err(error) => {
                    clause_typer.problems.error(error);
                    clause_typer.error_expr(then_location)
                }
            };

            Ok((guard, then, typed_pattern, typed_alternatives))
        });
        let (guard, then, typed_pattern, typed_alternatives) = match scoped_clause_inference {
            Ok(res) => res,
            Err(error) => {
                // NOTE: theoretically it should be impossible to get here
                // since the individual parts have been made fault tolerant
                // but in_new_scope requires that the return type be a result
                self.problems.error(error);
                (
                    None,
                    TypedExpr::Invalid {
                        location: then_location,
                        type_: self.new_unbound_var(),
                    },
                    vec![],
                    vec![],
                )
            }
        };

        Clause {
            location,
            pattern: typed_pattern,
            alternative_patterns: typed_alternatives,
            guard,
            then,
        }
    }

    fn infer_clause_pattern(
        &mut self,
        pattern: UntypedMultiPattern,
        alternatives: Vec<UntypedMultiPattern>,
        subjects: &[TypedExpr],
        location: &SrcSpan,
    ) -> Result<(TypedMultiPattern, Vec<TypedMultiPattern>), Error> {
        let mut pattern_typer = pattern::PatternTyper::new(
            self.environment,
            &self.implementations,
            &self.hydrator,
            self.problems,
        );
        let typed_pattern = pattern_typer.infer_multi_pattern(pattern, subjects, location)?;

        // Each case clause has one or more patterns that may match the
        // subject in order for the clause to be selected, so we must type
        // check every pattern.
        let mut typed_alternatives = Vec::with_capacity(alternatives.len());
        for m in alternatives {
            typed_alternatives
                .push(pattern_typer.infer_alternative_multi_pattern(m, subjects, location)?);
        }

        let minimum_required_version = pattern_typer.minimum_required_version;
        if minimum_required_version > self.minimum_required_version {
            self.minimum_required_version = minimum_required_version;
        }

        Ok((typed_pattern, typed_alternatives))
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

                    ValueConstructorVariant::ModuleConstant { literal, .. }
                    | ValueConstructorVariant::LocalConstant { literal } => {
                        return Ok(ClauseGuard::Constant(literal.clone()))
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
                    Type::Tuple { elems } => {
                        let type_ = elems
                            .get(index as usize)
                            .ok_or(Error::OutOfBoundsTupleIndex {
                                location,
                                index,
                                size: elems.len(),
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

                    _ => Err(Error::NotATuple {
                        location: tuple.location(),
                        given: tuple.type_(),
                    }),
                }
            }

            ClauseGuard::FieldAccess {
                location,
                label,
                container,
                index: _,
                type_: (),
            } => match self.infer_clause_guard(*container.clone()) {
                Ok(container) => self.infer_guard_record_access(container, label, location),

                Err(err) => match *container {
                    ClauseGuard::Var { name, location, .. } => {
                        self.infer_guard_module_access(name, label, location, err)
                    }

                    _ => Err(Error::RecordAccessUnknownType { location }),
                },
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

            ClauseGuard::And {
                location,
                left,
                right,
                ..
            } => {
                let left = self.infer_clause_guard(*left)?;
                unify(bool(), left.type_()).map_err(|e| convert_unify_error(e, left.location()))?;
                let right = self.infer_clause_guard(*right)?;
                unify(bool(), right.type_())
                    .map_err(|e| convert_unify_error(e, right.location()))?;
                Ok(ClauseGuard::And {
                    location,
                    left: Box::new(left),
                    right: Box::new(right),
                })
            }

            ClauseGuard::Or {
                location,
                left,
                right,
                ..
            } => {
                let left = self.infer_clause_guard(*left)?;
                unify(bool(), left.type_()).map_err(|e| convert_unify_error(e, left.location()))?;
                let right = self.infer_clause_guard(*right)?;
                unify(bool(), right.type_())
                    .map_err(|e| convert_unify_error(e, right.location()))?;
                Ok(ClauseGuard::Or {
                    location,
                    left: Box::new(left),
                    right: Box::new(right),
                })
            }

            ClauseGuard::Equals {
                location,
                left,
                right,
                ..
            } => {
                let left = self.infer_clause_guard(*left)?;
                let right = self.infer_clause_guard(*right)?;
                unify(left.type_(), right.type_()).map_err(|e| convert_unify_error(e, location))?;
                Ok(ClauseGuard::Equals {
                    location,
                    left: Box::new(left),
                    right: Box::new(right),
                })
            }

            ClauseGuard::NotEquals {
                location,
                left,
                right,
                ..
            } => {
                let left = self.infer_clause_guard(*left)?;
                let right = self.infer_clause_guard(*right)?;
                unify(left.type_(), right.type_()).map_err(|e| convert_unify_error(e, location))?;
                Ok(ClauseGuard::NotEquals {
                    location,
                    left: Box::new(left),
                    right: Box::new(right),
                })
            }

            ClauseGuard::GtInt {
                location,
                left,
                right,
                ..
            } => {
                let left = self.infer_clause_guard(*left)?;
                unify(int(), left.type_()).map_err(|e| convert_unify_error(e, left.location()))?;
                let right = self.infer_clause_guard(*right)?;
                unify(int(), right.type_())
                    .map_err(|e| convert_unify_error(e, right.location()))?;
                Ok(ClauseGuard::GtInt {
                    location,
                    left: Box::new(left),
                    right: Box::new(right),
                })
            }

            ClauseGuard::GtEqInt {
                location,
                left,
                right,
                ..
            } => {
                let left = self.infer_clause_guard(*left)?;
                unify(int(), left.type_()).map_err(|e| convert_unify_error(e, left.location()))?;
                let right = self.infer_clause_guard(*right)?;
                unify(int(), right.type_())
                    .map_err(|e| convert_unify_error(e, right.location()))?;
                Ok(ClauseGuard::GtEqInt {
                    location,
                    left: Box::new(left),
                    right: Box::new(right),
                })
            }

            ClauseGuard::LtInt {
                location,
                left,
                right,
                ..
            } => {
                let left = self.infer_clause_guard(*left)?;
                unify(int(), left.type_()).map_err(|e| convert_unify_error(e, left.location()))?;
                let right = self.infer_clause_guard(*right)?;
                unify(int(), right.type_())
                    .map_err(|e| convert_unify_error(e, right.location()))?;
                Ok(ClauseGuard::LtInt {
                    location,
                    left: Box::new(left),
                    right: Box::new(right),
                })
            }

            ClauseGuard::LtEqInt {
                location,
                left,
                right,
                ..
            } => {
                let left = self.infer_clause_guard(*left)?;
                unify(int(), left.type_()).map_err(|e| convert_unify_error(e, left.location()))?;
                let right = self.infer_clause_guard(*right)?;
                unify(int(), right.type_())
                    .map_err(|e| convert_unify_error(e, right.location()))?;
                Ok(ClauseGuard::LtEqInt {
                    location,
                    left: Box::new(left),
                    right: Box::new(right),
                })
            }

            ClauseGuard::GtFloat {
                location,
                left,
                right,
                ..
            } => {
                let left = self.infer_clause_guard(*left)?;
                unify(float(), left.type_())
                    .map_err(|e| convert_unify_error(e, left.location()))?;
                let right = self.infer_clause_guard(*right)?;
                unify(float(), right.type_())
                    .map_err(|e| convert_unify_error(e, right.location()))?;
                Ok(ClauseGuard::GtFloat {
                    location,
                    left: Box::new(left),
                    right: Box::new(right),
                })
            }

            ClauseGuard::GtEqFloat {
                location,
                left,
                right,
                ..
            } => {
                let left = self.infer_clause_guard(*left)?;
                unify(float(), left.type_())
                    .map_err(|e| convert_unify_error(e, left.location()))?;
                let right = self.infer_clause_guard(*right)?;
                unify(float(), right.type_())
                    .map_err(|e| convert_unify_error(e, right.location()))?;
                Ok(ClauseGuard::GtEqFloat {
                    location,
                    left: Box::new(left),
                    right: Box::new(right),
                })
            }

            ClauseGuard::LtFloat {
                location,
                left,
                right,
                ..
            } => {
                let left = self.infer_clause_guard(*left)?;
                unify(float(), left.type_())
                    .map_err(|e| convert_unify_error(e, left.location()))?;
                let right = self.infer_clause_guard(*right)?;
                unify(float(), right.type_())
                    .map_err(|e| convert_unify_error(e, right.location()))?;
                Ok(ClauseGuard::LtFloat {
                    location,
                    left: Box::new(left),
                    right: Box::new(right),
                })
            }

            ClauseGuard::LtEqFloat {
                location,
                left,
                right,
                ..
            } => {
                let left = self.infer_clause_guard(*left)?;
                unify(float(), left.type_())
                    .map_err(|e| convert_unify_error(e, left.location()))?;
                let right = self.infer_clause_guard(*right)?;
                unify(float(), right.type_())
                    .map_err(|e| convert_unify_error(e, right.location()))?;
                Ok(ClauseGuard::LtEqFloat {
                    location,
                    left: Box::new(left),
                    right: Box::new(right),
                })
            }

            ClauseGuard::AddInt {
                location,
                left,
                right,
                ..
            } => {
                self.track_feature_usage(FeatureKind::ArithmeticInGuards, location);
                let left = self.infer_clause_guard(*left)?;
                unify(int(), left.type_()).map_err(|e| convert_unify_error(e, left.location()))?;
                let right = self.infer_clause_guard(*right)?;
                unify(int(), right.type_())
                    .map_err(|e| convert_unify_error(e, right.location()))?;
                Ok(ClauseGuard::AddInt {
                    location,
                    left: Box::new(left),
                    right: Box::new(right),
                })
            }

            ClauseGuard::AddFloat {
                location,
                left,
                right,
                ..
            } => {
                self.track_feature_usage(FeatureKind::ArithmeticInGuards, location);
                let left = self.infer_clause_guard(*left)?;
                unify(float(), left.type_())
                    .map_err(|e| convert_unify_error(e, left.location()))?;
                let right = self.infer_clause_guard(*right)?;
                unify(float(), right.type_())
                    .map_err(|e| convert_unify_error(e, right.location()))?;
                Ok(ClauseGuard::AddFloat {
                    location,
                    left: Box::new(left),
                    right: Box::new(right),
                })
            }

            ClauseGuard::SubInt {
                location,
                left,
                right,
                ..
            } => {
                self.track_feature_usage(FeatureKind::ArithmeticInGuards, location);
                let left = self.infer_clause_guard(*left)?;
                unify(int(), left.type_()).map_err(|e| convert_unify_error(e, left.location()))?;
                let right = self.infer_clause_guard(*right)?;
                unify(int(), right.type_())
                    .map_err(|e| convert_unify_error(e, right.location()))?;
                Ok(ClauseGuard::SubInt {
                    location,
                    left: Box::new(left),
                    right: Box::new(right),
                })
            }

            ClauseGuard::SubFloat {
                location,
                left,
                right,
                ..
            } => {
                self.track_feature_usage(FeatureKind::ArithmeticInGuards, location);
                let left = self.infer_clause_guard(*left)?;
                unify(float(), left.type_())
                    .map_err(|e| convert_unify_error(e, left.location()))?;
                let right = self.infer_clause_guard(*right)?;
                unify(float(), right.type_())
                    .map_err(|e| convert_unify_error(e, right.location()))?;
                Ok(ClauseGuard::SubFloat {
                    location,
                    left: Box::new(left),
                    right: Box::new(right),
                })
            }

            ClauseGuard::MultInt {
                location,
                left,
                right,
                ..
            } => {
                self.track_feature_usage(FeatureKind::ArithmeticInGuards, location);
                let left = self.infer_clause_guard(*left)?;
                unify(int(), left.type_()).map_err(|e| convert_unify_error(e, left.location()))?;
                let right = self.infer_clause_guard(*right)?;
                unify(int(), right.type_())
                    .map_err(|e| convert_unify_error(e, right.location()))?;
                Ok(ClauseGuard::MultInt {
                    location,
                    left: Box::new(left),
                    right: Box::new(right),
                })
            }

            ClauseGuard::MultFloat {
                location,
                left,
                right,
                ..
            } => {
                self.track_feature_usage(FeatureKind::ArithmeticInGuards, location);
                let left = self.infer_clause_guard(*left)?;
                unify(float(), left.type_())
                    .map_err(|e| convert_unify_error(e, left.location()))?;
                let right = self.infer_clause_guard(*right)?;
                unify(float(), right.type_())
                    .map_err(|e| convert_unify_error(e, right.location()))?;
                Ok(ClauseGuard::MultFloat {
                    location,
                    left: Box::new(left),
                    right: Box::new(right),
                })
            }

            ClauseGuard::DivInt {
                location,
                left,
                right,
                ..
            } => {
                self.track_feature_usage(FeatureKind::ArithmeticInGuards, location);
                let left = self.infer_clause_guard(*left)?;
                unify(int(), left.type_()).map_err(|e| convert_unify_error(e, left.location()))?;
                let right = self.infer_clause_guard(*right)?;
                unify(int(), right.type_())
                    .map_err(|e| convert_unify_error(e, right.location()))?;
                Ok(ClauseGuard::DivInt {
                    location,
                    left: Box::new(left),
                    right: Box::new(right),
                })
            }

            ClauseGuard::DivFloat {
                location,
                left,
                right,
                ..
            } => {
                self.track_feature_usage(FeatureKind::ArithmeticInGuards, location);
                let left = self.infer_clause_guard(*left)?;
                unify(float(), left.type_())
                    .map_err(|e| convert_unify_error(e, left.location()))?;
                let right = self.infer_clause_guard(*right)?;
                unify(float(), right.type_())
                    .map_err(|e| convert_unify_error(e, right.location()))?;
                Ok(ClauseGuard::DivFloat {
                    location,
                    left: Box::new(left),
                    right: Box::new(right),
                })
            }

            ClauseGuard::RemainderInt {
                location,
                left,
                right,
                ..
            } => {
                self.track_feature_usage(FeatureKind::ArithmeticInGuards, location);
                let left = self.infer_clause_guard(*left)?;
                unify(int(), left.type_()).map_err(|e| convert_unify_error(e, left.location()))?;
                let right = self.infer_clause_guard(*right)?;
                unify(int(), right.type_())
                    .map_err(|e| convert_unify_error(e, right.location()))?;
                Ok(ClauseGuard::RemainderInt {
                    location,
                    left: Box::new(left),
                    right: Box::new(right),
                })
            }

            ClauseGuard::Constant(constant) => {
                Ok(ClauseGuard::Constant(self.infer_const(&None, constant)))
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
        let (index, label, type_) = self.infer_known_record_access(
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
            location,
            type_,
        })
    }

    fn infer_guard_module_access(
        &mut self,
        name: EcoString,
        label: EcoString,
        location: SrcSpan,
        record_access_error: Error,
    ) -> Result<TypedClauseGuard, Error> {
        let module_access = self
            .infer_module_access(&name, label, &location, location)
            .and_then(|ma| match ma {
                TypedExpr::ModuleSelect {
                    location,
                    type_,
                    label,
                    module_name,
                    module_alias,
                    constructor,
                } => match constructor {
                    ModuleValueConstructor::Constant { literal, .. } => {
                        Ok(ClauseGuard::ModuleSelect {
                            location,
                            type_,
                            label,
                            module_name,
                            module_alias,
                            literal,
                        })
                    }

                    _ => Err(Error::RecordAccessUnknownType { location }),
                },

                _ => Err(Error::RecordAccessUnknownType { location }),
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
        module_alias: &EcoString,
        label: EcoString,
        module_location: &SrcSpan,
        select_location: SrcSpan,
    ) -> Result<TypedExpr, Error> {
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
                        location: SrcSpan {
                            start: module_location.end,
                            end: select_location.end,
                        },
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

            // Register this imported module as having been used, to inform
            // warnings of unused imports later
            let _ = self.environment.unused_modules.remove(module_alias);
            let _ = self.environment.unused_module_aliases.remove(module_alias);

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
            | ValueConstructorVariant::LocalConstant { .. }
            | ValueConstructorVariant::Record { .. }) => {
                variant.to_module_value_constructor(Arc::clone(&type_), &module_name, &label)
            }
        };

        Ok(TypedExpr::ModuleSelect {
            label,
            type_: Arc::clone(&type_),
            location: select_location,
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
        usage: FieldAccessUsage,
    ) -> Result<TypedExpr, Error> {
        let record = Box::new(record);
        let record_type = record.type_();
        let (index, label, type_) =
            self.infer_known_record_access(record_type, record.location(), usage, location, label)?;
        Ok(TypedExpr::RecordAccess {
            record,
            label,
            index,
            location,
            type_,
        })
    }

    fn infer_known_record_access(
        &mut self,
        record_type: Arc<Type>,
        record_location: SrcSpan,
        usage: FieldAccessUsage,
        location: SrcSpan,
        label: EcoString,
    ) -> Result<(u64, EcoString, Arc<Type>), Error> {
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

            _something_without_fields => return Err(unknown_field(vec![])),
        }
        .ok_or_else(|| unknown_field(vec![]))?;

        let RecordAccessor {
            index,
            label,
            type_,
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
        Ok((index, label, type_))
    }

    fn infer_record_update(
        &mut self,
        constructor: UntypedExpr,
        record: RecordBeingUpdated,
        args: Vec<UntypedRecordUpdateArg>,
        location: SrcSpan,
    ) -> Result<TypedExpr, Error> {
        // infer the constructor being used
        let typed_constructor = self.infer(constructor.clone())?;
        let (module, name) = match &typed_constructor {
            TypedExpr::ModuleSelect {
                module_alias,
                label,
                location,
                ..
            } => (Some((module_alias, location)), label),

            TypedExpr::Var { name, .. } => (None, name),

            constructor => {
                return Err(Error::RecordUpdateInvalidConstructor {
                    location: constructor.location(),
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
        let record = self.infer(*record.base)?;
        let record_location = record.location();
        let record_type = record.type_();

        // We create an Assignment for the old record expression and will use a Var expression
        // to refer back to it while constructing the arguments.
        let record_assignment = Assignment {
            location: record_location,
            pattern: Pattern::Variable {
                location: record_location,
                name: RECORD_UPDATE_VARIABLE.into(),
                type_: record_type.clone(),
                origin: VariableOrigin::Generated,
            },
            annotation: None,
            kind: AssignmentKind::Generated,
            value: Box::new(record),
        };

        let record_var = TypedExpr::Var {
            location: record_location,
            constructor: ValueConstructor {
                publicity: Publicity::Private,
                deprecation: Deprecation::NotDeprecated,
                type_: record_type,
                variant: ValueConstructorVariant::LocalVariable {
                    location: record_location,
                    origin: VariableOrigin::Generated,
                },
            },
            name: RECORD_UPDATE_VARIABLE.into(),
        };

        // infer the fields of the variant we want to update
        let variant =
            self.infer_record_update_variant(&typed_constructor, &value_constructor, &record_var)?;

        let args = self.infer_record_update_args(&variant, &record_var, args, location)?;

        Ok(TypedExpr::RecordUpdate {
            location,
            type_: variant.retn,
            record: record_assignment,
            constructor: Box::new(typed_constructor),
            args,
        })
    }

    fn infer_record_update_args(
        &mut self,
        variant: &RecordUpdateVariant<'_>,
        record: &TypedExpr,
        args: Vec<UntypedRecordUpdateArg>,
        location: SrcSpan,
    ) -> Result<Vec<TypedCallArg>, Error> {
        let record_location = record.location();
        let record_type = record.type_();
        let return_type = variant.retn.clone();

        // We clone the fields to remove all explicitly mentioned fields in the record update.
        let mut fields = variant.fields.clone();

        // collect explicit arguments given in the record update
        let explicit_args = args
            .iter()
            .map(
                |arg @ UntypedRecordUpdateArg {
                     label,
                     value,
                     location,
                 }| {
                    let value = self.infer(value.clone())?;

                    if arg.uses_label_shorthand() {
                        self.track_feature_usage(FeatureKind::LabelShorthandSyntax, *location);
                    }

                    if let Some(index) = fields.remove(label) {
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
                    } else if variant.has_field(label) {
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
                },
            )
            .collect::<Result<Vec<_>, _>>()?;

        // Generate the remaining copied arguments, making sure they unify with our return type.
        let convert_incompatible_fields_error = |e: UnifyError, label: EcoString| match e {
            UnifyError::CouldNotUnify {
                expected, given, ..
            } => Error::UnsafeRecordUpdate {
                location: record_location,
                reason: UnsafeRecordUpdateReason::IncompatibleFieldTypes {
                    constructed_variant: return_type.clone(),
                    record_variant: record_type.clone(),
                    expected_field_type: expected,
                    record_field_type: given,
                    field_name: label,
                },
            },
            _ => convert_unify_error(e, record_location),
        };

        let implicit_args = fields
            .into_iter()
            .map(|(label, index)| {
                let record_access = self.infer_known_record_expression_access(
                    record.clone(),
                    label.clone(),
                    record_location,
                    FieldAccessUsage::RecordUpdate,
                )?;

                unify(variant.arg_type(index), record_access.type_())
                    .map_err(|e| convert_incompatible_fields_error(e, label.clone()))?;

                Ok((
                    index,
                    CallArg {
                        location: record_location,
                        label: Some(label),
                        value: record_access,
                        implicit: Some(ImplicitCallArgOrigin::RecordUpdate),
                    },
                ))
            })
            .collect::<Result<Vec<_>, _>>()?;

        if explicit_args.is_empty() {
            self.problems
                .warning(Warning::NoFieldsRecordUpdate { location });
        }

        if implicit_args.is_empty() {
            self.problems
                .warning(Warning::AllFieldsRecordUpdate { location });
        }

        let args = explicit_args
            .into_iter()
            .chain(implicit_args)
            .sorted_by_key(|(index, _)| *index)
            .map(|(_, value)| value)
            .collect();

        Ok(args)
    }

    fn infer_record_update_variant<'c>(
        &mut self,
        constructor: &TypedExpr,
        value_constructor: &'c ValueConstructor,
        record: &TypedExpr,
    ) -> Result<RecordUpdateVariant<'c>, Error> {
        let record_type = record.type_();
        // The record constructor needs to be a function.
        let (args_types, return_type) = match constructor.type_().as_ref() {
            Type::Fn { args, retrn } => (args.clone(), retrn.clone()),
            _ => {
                return Err(Error::RecordUpdateInvalidConstructor {
                    location: constructor.location(),
                })
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
            _ => {
                return Err(Error::RecordUpdateInvalidConstructor {
                    location: constructor.location(),
                });
            }
        };

        // Check that the record type unifies with the return type of the constructor, and is
        // not some unrelated other type. This should not affect our returned type, so we
        // instantiate a new copy of the generic return type for our value constructor.
        let return_type_copy = match value_constructor.type_.as_ref() {
            Type::Fn { retrn, .. } => self.instantiate(retrn.clone(), &mut hashmap![]),
            _ => {
                return Err(Error::RecordUpdateInvalidConstructor {
                    location: constructor.location(),
                })
            }
        };

        unify(return_type_copy, record_type.clone())
            .map_err(|e| convert_unify_error(e, record.location()))?;

        let record_index = record_type.custom_type_inferred_variant();
        // Updating a record with only one variant is always safe
        if variants_count == 1 {
            return Ok(RecordUpdateVariant {
                args: args_types,
                retn: return_type,
                fields: &field_map.fields,
            });
        }

        // if we know the record that is being spread, and it does match the one being constructed,
        // we can safely perform this record update due to variant inference.
        if record_index.is_some_and(|index| index == variant_index) {
            self.track_feature_usage(FeatureKind::RecordUpdateVariantInference, record.location());
            return Ok(RecordUpdateVariant {
                args: args_types,
                retn: return_type,
                fields: &field_map.fields,
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
        let constructor = match module {
            // Look in the current scope for a binding with this name
            None => {
                let constructor = self
                    .environment
                    .get_variable(name)
                    .cloned()
                    .ok_or_else(|| self.report_name_error(name, location))?;

                // Register the value as seen for detection of unused values
                self.environment.increment_usage(name);

                constructor
            }

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

        // Emit a warning if the value being used is deprecated.
        if let Deprecation::Deprecated { message } = &deprecation {
            self.problems.warning(Warning::DeprecatedItem {
                location: *location,
                message: message.clone(),
                layer: Layer::Value,
            })
        }

        self.narrow_implementations(*location, &variant)?;

        // Instantiate generic variables into unbound variables for this usage
        let type_ = self.instantiate(type_, &mut hashmap![]);
        Ok(ValueConstructor {
            publicity,
            deprecation,
            variant,
            type_,
        })
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
                type_with_name_in_scope: self
                    .environment
                    .module_types
                    .keys()
                    .any(|typ| typ == name),
            },
        }
    }

    // helper for infer_const to get the value of a constant ignoring annotations
    fn infer_const_value(&mut self, value: UntypedConstant) -> Result<TypedConstant, Error> {
        match value {
            Constant::Int {
                location,
                value,
                int_value,
            } => {
                if self.environment.target == Target::JavaScript {
                    check_javascript_int_safety(&int_value, location, self.problems);
                }

                Ok(Constant::Int {
                    location,
                    value,
                    int_value,
                })
            }

            Constant::Float {
                location, value, ..
            } => {
                if self.environment.target == Target::Erlang {
                    check_erlang_float_safety(&value, location, self.problems)
                }

                Ok(Constant::Float { location, value })
            }

            Constant::String {
                location, value, ..
            } => Ok(Constant::String { location, value }),

            Constant::Tuple {
                elements, location, ..
            } => self.infer_const_tuple(elements, location),

            Constant::List {
                elements, location, ..
            } => self.infer_const_list(elements, location),

            Constant::BitArray { location, segments } => {
                self.infer_constant_bit_array(segments, location)
            }

            Constant::Record {
                module,
                location,
                name,
                args,
                // field_map, is always None here because untyped not yet unified
                ..
            } if args.is_empty() => {
                // Register the module as having been used if it was imported
                if let Some((module, _)) = &module {
                    _ = self.environment.unused_modules.remove(module);
                    _ = self.environment.unused_module_aliases.remove(module);
                }

                // Type check the record constructor
                let constructor = self.infer_value_constructor(&module, &name, &location)?;

                let (tag, field_map) = match &constructor.variant {
                    ValueConstructorVariant::Record {
                        name, field_map, ..
                    } => (name.clone(), field_map.clone()),

                    ValueConstructorVariant::ModuleFn { .. }
                    | ValueConstructorVariant::LocalVariable { .. } => {
                        return Err(Error::NonLocalClauseGuardVariable { location, name })
                    }

                    // TODO: remove this clone. Could use an rc instead
                    ValueConstructorVariant::ModuleConstant { literal, .. }
                    | ValueConstructorVariant::LocalConstant { literal } => {
                        return Ok(literal.clone())
                    }
                };

                Ok(Constant::Record {
                    module,
                    location,
                    name,
                    args: vec![],
                    type_: constructor.type_,
                    tag,
                    field_map,
                })
            }

            Constant::Record {
                module,
                location,
                name,
                mut args,
                // field_map, is always None here because untyped not yet unified
                ..
            } => {
                // Register the module as having been used if it was imported
                if let Some((module, _)) = &module {
                    _ = self.environment.unused_modules.remove(module);
                    _ = self.environment.unused_module_aliases.remove(module);
                }

                let constructor = self.infer_value_constructor(&module, &name, &location)?;

                let (tag, field_map) = match &constructor.variant {
                    ValueConstructorVariant::Record {
                        name, field_map, ..
                    } => (name.clone(), field_map.clone()),

                    ValueConstructorVariant::ModuleFn { .. }
                    | ValueConstructorVariant::LocalVariable { .. } => {
                        return Err(Error::NonLocalClauseGuardVariable { location, name })
                    }

                    // TODO: remove this clone. Could be an rc instead
                    ValueConstructorVariant::ModuleConstant { literal, .. }
                    | ValueConstructorVariant::LocalConstant { literal } => {
                        return Ok(literal.clone())
                    }
                };

                // Pretty much all the other infer functions operate on UntypedExpr
                // or TypedExpr rather than ClauseGuard. To make things easier we
                // build the TypedExpr equivalent of the constructor and use that
                // TODO: resvisit this. It is rather awkward at present how we
                // have to convert to this other data structure.
                let fun = match &module {
                    Some((module_alias, _)) => {
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
                            field_map: field_map.clone(),
                            arity: args.len() as u16,
                            type_: Arc::clone(&type_),
                            location: constructor.variant.definition_location(),
                            documentation: None,
                        };

                        TypedExpr::ModuleSelect {
                            label: name.clone(),
                            module_alias: module_alias.clone(),
                            module_name,
                            type_,
                            constructor: module_value_constructor,
                            location,
                        }
                    }

                    None => TypedExpr::Var {
                        constructor,
                        location,
                        name: name.clone(),
                    },
                };

                // This is basically the same code as do_infer_call_with_known_fun()
                // except the args are typed with infer_clause_guard() here.
                // This duplication is a bit awkward but it works!
                // Potentially this could be improved later
                match self
                    .get_field_map(&fun)
                    .map_err(|e| convert_get_value_constructor_error(e, location, None))?
                {
                    // The fun has a field map so labelled arguments may be present and need to be reordered.
                    Some(field_map) => field_map.reorder(&mut args, location)?,

                    // The fun has no field map and so we error if arguments have been labelled
                    None => assert_no_labelled_arguments(&args)?,
                }

                let (mut args_types, return_type) =
                    match_fun_type(fun.type_(), args.len(), self.environment).map_err(|e| {
                        convert_not_fun_error(e, fun.location(), location, CallKind::Function)
                    })?;

                let args = args_types
                    .iter_mut()
                    .zip(args)
                    .map(|(type_, arg): (&mut Arc<Type>, _)| {
                        if arg.uses_label_shorthand() {
                            self.track_feature_usage(
                                FeatureKind::LabelShorthandSyntax,
                                arg.location,
                            );
                        }
                        let CallArg {
                            label,
                            value,
                            location,
                            implicit,
                        } = arg;
                        let value = self.infer_const(&None, value);
                        unify(type_.clone(), value.type_())
                            .map_err(|e| convert_unify_error(e, value.location()))?;
                        Ok(CallArg {
                            label,
                            value,
                            implicit,
                            location,
                        })
                    })
                    .try_collect()?;

                Ok(Constant::Record {
                    module,
                    location,
                    name,
                    args,
                    type_: return_type,
                    tag,
                    field_map,
                })
            }

            Constant::Var {
                location,
                module,
                name,
                ..
            } => {
                // Register the module as having been used if it was imported
                if let Some((module, _)) = &module {
                    _ = self.environment.unused_modules.remove(module);
                    _ = self.environment.unused_module_aliases.remove(module);
                }

                // Infer the type of this constant
                let constructor = self.infer_value_constructor(&module, &name, &location)?;
                match constructor.variant {
                    ValueConstructorVariant::ModuleConstant { .. }
                    | ValueConstructorVariant::LocalConstant { .. }
                    | ValueConstructorVariant::ModuleFn { .. }
                    | ValueConstructorVariant::LocalVariable { .. } => Ok(Constant::Var {
                        location,
                        module,
                        name,
                        type_: Arc::clone(&constructor.type_),
                        constructor: Some(Box::from(constructor)),
                    }),
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
                unify(string(), left.type_()).map_err(|e| {
                    e.operator_situation(BinOp::Concatenate)
                        .into_error(left.location())
                })?;
                let right = self.infer_const(&None, *right);
                unify(string(), right.type_()).map_err(|e| {
                    e.operator_situation(BinOp::Concatenate)
                        .into_error(right.location())
                })?;

                Ok(Constant::StringConcatenation {
                    location,
                    left: Box::new(left),
                    right: Box::new(right),
                })
            }

            Constant::Invalid { .. } => panic!("invalid constants can not be in an untyped ast"),
        }
    }

    pub fn infer_const(
        &mut self,
        annotation: &Option<TypeAst>,
        value: UntypedConstant,
    ) -> TypedConstant {
        let loc = value.location();
        let inferred = self.infer_const_value(value);

        // Get the type of the annotation if it exists and validate it against the inferred value.
        let annotation = annotation.as_ref().map(|a| self.type_from_ast(a));
        match (annotation, inferred) {
            // No annotation and valid inferred value.
            (None, Ok(inferred)) => inferred,
            // No annotation and invalid inferred value. Use an unbound variable hole.
            (None, Err(e)) => {
                self.problems.error(e);
                Constant::Invalid {
                    location: loc,
                    type_: self.new_unbound_var(),
                }
            }
            // Type annotation and inferred value are valid. Ensure they are unifiable.
            // NOTE: if the types are not unifiable we use the annotated type.
            (Some(Ok(const_ann)), Ok(inferred)) => {
                if let Err(e) = unify(const_ann.clone(), inferred.type_())
                    .map_err(|e| convert_unify_error(e, inferred.location()))
                {
                    self.problems.error(e);
                    Constant::Invalid {
                        location: loc,
                        type_: const_ann,
                    }
                } else {
                    inferred
                }
            }
            // Type annotation is valid but not the inferred value. Place a placeholder constant with the annotation type.
            // This should limit the errors to only the definition.
            (Some(Ok(const_ann)), Err(value_err)) => {
                self.problems.error(value_err);
                Constant::Invalid {
                    location: loc,
                    type_: const_ann,
                }
            }
            // Type annotation is invalid but the inferred value is ok. Use the inferred type.
            (Some(Err(annotation_err)), Ok(inferred)) => {
                self.problems.error(annotation_err);
                inferred
            }
            // Type annotation and inferred value are invalid. Place a placeholder constant with an unbound type.
            // This should limit the errors to only the definition assuming the constant is used consistently.
            (Some(Err(annotation_err)), Err(value_err)) => {
                self.problems.error(annotation_err);
                self.problems.error(value_err);
                Constant::Invalid {
                    location: loc,
                    type_: self.new_unbound_var(),
                }
            }
        }
    }

    fn infer_const_tuple(
        &mut self,
        untyped_elements: Vec<UntypedConstant>,
        location: SrcSpan,
    ) -> Result<TypedConstant, Error> {
        let mut elements = Vec::with_capacity(untyped_elements.len());

        for element in untyped_elements {
            let element = self.infer_const(&None, element);
            elements.push(element);
        }

        Ok(Constant::Tuple { elements, location })
    }

    fn infer_const_list(
        &mut self,
        untyped_elements: Vec<UntypedConstant>,
        location: SrcSpan,
    ) -> Result<TypedConstant, Error> {
        let type_ = self.new_unbound_var();
        let mut elements = Vec::with_capacity(untyped_elements.len());

        for element in untyped_elements {
            let element = self.infer_const(&None, element);
            unify(type_.clone(), element.type_())
                .map_err(|e| convert_unify_error(e, element.location()))?;
            elements.push(element);
        }

        Ok(Constant::List {
            elements,
            location,
            type_: list(type_),
        })
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

            _ => return Ok(None),
        };

        Ok(self
            .environment
            .get_value_constructor(module, name)?
            .field_map())
    }

    pub fn do_infer_call(
        &mut self,
        fun: UntypedExpr,
        args: Vec<CallArg<UntypedExpr>>,
        location: SrcSpan,
        kind: CallKind,
    ) -> (TypedExpr, Vec<TypedCallArg>, Arc<Type>) {
        let function_location = fun.location();
        let typed_fun = match fun {
            UntypedExpr::FieldAccess {
                label,
                container,
                label_location,
                ..
            } => Ok(self.infer_field_access(
                *container,
                label,
                label_location,
                FieldAccessUsage::MethodCall,
            )),

            UntypedExpr::Fn {
                location,
                kind,
                arguments,
                body,
                return_annotation,
                ..
            } if arguments.len() == args.len() => self.infer_fn_with_call_context(
                arguments,
                &args,
                body,
                kind,
                return_annotation,
                location,
            ),

            fun => self.infer(fun),
        };

        let fun = match typed_fun {
            Ok(fun) => fun,
            Err(function_inference_error) => {
                self.problems.error(function_inference_error);
                self.error_expr(function_location)
            }
        };

        let (fun, args, type_) = self.do_infer_call_with_known_fun(fun, args, location, kind);
        (fun, args, type_)
    }

    fn infer_fn_with_call_context(
        &mut self,
        args: Vec<UntypedArg>,
        call_args: &[CallArg<UntypedExpr>],
        body: Vec1<UntypedStatement>,
        kind: FunctionLiteralKind,
        return_annotation: Option<TypeAst>,
        location: SrcSpan,
    ) -> Result<TypedExpr, Error> {
        let typed_call_args: Vec<Arc<Type>> = call_args
            .iter()
            .map(|a| {
                match self.infer(a.value.clone()) {
                    Ok(arg) => arg,
                    Err(_e) => self.error_expr(location),
                }
                .type_()
            })
            .collect_vec();
        self.infer_fn(
            args,
            &typed_call_args,
            body,
            kind,
            return_annotation,
            location,
        )
    }

    pub fn do_infer_call_with_known_fun(
        &mut self,
        fun: TypedExpr,
        mut args: Vec<CallArg<UntypedExpr>>,
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
                    // The fun has a field map so labelled arguments may be present and need to be reordered.
                    Some(field_map) => field_map.reorder(&mut args, location),

                    // The fun has no field map and so we error if arguments have been labelled
                    None => assert_no_labelled_arguments(&args),
                }
            });
        if let Err(e) = field_map {
            if let Error::IncorrectArity {
                expected,
                given,
                labels,
                location,
            } = e
            {
                labelled_arity_error = true;
                self.problems.error(Error::IncorrectArity {
                    expected,
                    given,
                    labels,
                    location,
                });
            } else {
                self.problems.error(e);
            }
        }

        let mut missing_args = 0;
        let mut ignored_labelled_args = vec![];
        // Extract the type of the fun, ensuring it actually is a function
        let (mut args_types, return_type) =
            match match_fun_type(fun.type_(), args.len(), self.environment) {
                Ok(fun) => fun,
                Err(e) => {
                    let converted_error =
                        convert_not_fun_error(e.clone(), fun.location(), location, kind);
                    match e {
                        // If the function was valid but had the wrong number of arguments passed.
                        // Then we keep the error but still want to continue analysing the arguments that were passed.
                        MatchFunTypeError::IncorrectArity {
                            args: arg_types,
                            return_type,
                            expected,
                            given,
                            ..
                        } => {
                            missing_args = expected.saturating_sub(given);
                            // If the function has labels then arity issues will already
                            // be handled by the field map so we can ignore them here.
                            if !labelled_arity_error {
                                self.problems.error(converted_error);
                                (arg_types, return_type)
                            } else {
                                // Since arity errors with labels cause incorrect
                                // ordering, we can't type check the labelled arguments here.
                                let first_labelled_arg =
                                    args.iter().position(|arg| arg.label.is_some());
                                ignored_labelled_args = args
                                    .iter()
                                    .skip_while(|arg| arg.label.is_none())
                                    .map(|arg| (arg.label.clone(), arg.location, arg.implicit))
                                    .collect_vec();
                                let args_to_keep = first_labelled_arg.unwrap_or(args.len());
                                (
                                    arg_types.iter().take(args_to_keep).cloned().collect(),
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
        if let CallKind::Use { .. } = kind {
            if let Some(last) = args.pop() {
                for _ in 0..missing_args {
                    args.push(CallArg {
                        label: None,
                        location,
                        value: UntypedExpr::Placeholder {
                            // We intentionally give this an empty span since it
                            // is an implicit argument being passed by the compiler
                            // that doesn't appear in the source code.
                            location: SrcSpan {
                                start: last.location().start,
                                end: last.location().start,
                            },
                        },
                        implicit: Some(ImplicitCallArgOrigin::IncorrectArityUse),
                    });
                }
                args.push(last);
            }
        };

        // Ensure that the given args have the correct types
        let args_count = args_types.len();
        let mut typed_args: Vec<_> = args_types
            .iter_mut()
            .zip(args)
            .enumerate()
            .map(|(i, (type_, arg))| {
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
                    } if i == args_count - 1 => ArgumentKind::UseCallback {
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

                let value = self.infer_call_argument(value, type_.clone(), argument_kind);
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
        for (label, location, implicit) in ignored_labelled_args {
            typed_args.push(CallArg {
                label,
                value: TypedExpr::Invalid {
                    location,
                    type_: self.new_unbound_var(),
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

        (fun, typed_args, return_type)
    }

    fn infer_call_argument(
        &mut self,
        value: UntypedExpr,
        type_: Arc<Type>,
        kind: ArgumentKind,
    ) -> TypedExpr {
        let type_ = collapse_links(type_);

        let value_location = value.location();
        let result = match (&*type_, value) {
            // If the argument is expected to be a function and we are passed a
            // function literal with the correct number of arguments then we
            // have special handling of this argument, passing in information
            // about what the expected arguments are. This extra information
            // when type checking the function body means that the
            // `record.field` access syntax can be used, and improves error
            // messages.
            (
                Type::Fn {
                    args: expected_arguments,
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

            // Otherwise just perform normal type inference.
            (_, value) => self.infer(value),
        };

        match result {
            Err(error) => {
                // If we couldn't infer the value, we record the error and
                // return an invalid expression with the type we were expecting
                // to see.
                self.problems.error(error);
                TypedExpr::Invalid {
                    location: value_location,
                    type_,
                }
            }
            Ok(value) => match unify(type_.clone(), value.type_()) {
                Ok(_) => value,
                Err(error) => {
                    // If we couldn't unify it, we record the error and return
                    // an invalid expression with the type we infered for the
                    // value.
                    let location = value.location();
                    let error = convert_unify_call_error(error, location, kind);
                    self.problems.error(error);
                    TypedExpr::Invalid {
                        location,
                        type_: value.type_(),
                    }
                }
            },
        }
    }

    pub fn do_infer_fn(
        &mut self,
        args: Vec<UntypedArg>,
        expected_args: &[Arc<Type>],
        body: Vec1<UntypedStatement>,
        return_annotation: &Option<TypeAst>,
    ) -> Result<(Vec<TypedArg>, Vec1<TypedStatement>), Error> {
        // Construct an initial type for each argument of the function- either an unbound
        // type variable or a type provided by an annotation.
        let args: Vec<_> = args
            .into_iter()
            .enumerate()
            .map(|(i, arg)| self.infer_arg(arg, expected_args.get(i).cloned()))
            .try_collect()?;

        let return_type = match return_annotation {
            Some(ann) => Some(self.type_from_ast(ann)?),
            None => None,
        };

        self.infer_fn_with_known_types(args, body, return_type)
    }

    pub fn infer_fn_with_known_types(
        &mut self,
        args: Vec<TypedArg>,
        body: Vec1<UntypedStatement>,
        return_type: Option<Arc<Type>>,
    ) -> Result<(Vec<TypedArg>, Vec1<TypedStatement>), Error> {
        // If a function has an empty body then it doesn't have a pure gleam
        // implementation.
        if body.first().is_placeholder() {
            self.implementations.gleam = false;
        }
        self.in_new_scope(|body_typer| {
            // Used to track if any argument names are used more than once
            let mut argument_names = HashSet::with_capacity(args.len());

            for (arg, t) in args.iter().zip(args.iter().map(|arg| arg.type_.clone())) {
                match &arg.names {
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
                                location: arg.location,
                                name: name.clone(),
                            });
                        }

                        // Insert a variable for the argument into the environment
                        body_typer.environment.insert_local_variable(
                            name.clone(),
                            *location,
                            VariableOrigin::Variable(name.clone()),
                            t,
                        );

                        if !body.first().is_placeholder() {
                            // Register the variable in the usage tracker so that we
                            // can identify if it is unused
                            body_typer.environment.init_usage(
                                name.clone(),
                                EntityKind::Variable {
                                    origin: VariableOrigin::Variable(name.clone()),
                                },
                                arg.location,
                                body_typer.problems,
                            );
                        }
                    }
                    ArgNames::Discard { .. } | ArgNames::LabelledDiscard { .. } => (),
                };
            }

            let mut body = body_typer.infer_statements(body);

            // Check that any return type is accurate.
            if let Some(return_type) = return_type {
                if let Err(error) = unify(return_type, body.last().type_()) {
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
                    }))
                };
            }

            Ok((args, body))
        })
    }

    fn infer_block(
        &mut self,
        statements: Vec1<UntypedStatement>,
        location: SrcSpan,
    ) -> Result<TypedExpr, Error> {
        self.in_new_scope(|typer| {
            let statements = typer.infer_statements(statements);
            Ok(TypedExpr::Block {
                statements,
                location,
            })
        })
    }

    fn check_let_exhaustiveness(
        &self,
        location: SrcSpan,
        subject: Arc<Type>,
        pattern: &TypedPattern,
    ) -> Result<(), Error> {
        use exhaustiveness::{Body, Column, Compiler, PatternArena, Row};

        let mut compiler = Compiler::new(self.environment, Arena::new());
        let mut arena = PatternArena::new();

        let subject_variable = compiler.subject_variable(subject.clone());

        let mut rows = Vec::with_capacity(1);

        let pattern = arena.register(pattern);
        let column = Column::new(subject_variable.clone(), pattern);
        let guard = None;
        let body = Body::new(0);
        let row = Row::new(vec![column], guard, body);
        rows.push(row);

        // Perform exhaustiveness checking, building a decision tree
        compiler.set_pattern_arena(arena.into_inner());
        let output = compiler.compile(rows);

        // Error for missing clauses that would cause a crash
        if output.diagnostics.missing {
            return Err(Error::InexhaustiveLetAssignment {
                location,
                missing: output.missing_patterns(self.environment),
            });
        }

        Ok(())
    }

    fn check_case_exhaustiveness(
        &mut self,
        location: SrcSpan,
        subject_types: &[Arc<Type>],
        clauses: &[TypedClause],
    ) -> Result<(), Error> {
        use exhaustiveness::{Body, Column, Compiler, PatternArena, Row};

        let mut compiler = Compiler::new(self.environment, Arena::new());
        let mut arena = PatternArena::new();

        let subject_variables = subject_types
            .iter()
            .map(|t| compiler.subject_variable(t.clone()))
            .collect_vec();

        let mut rows = Vec::with_capacity(clauses.iter().map(Clause::pattern_count).sum::<usize>());

        for (clause_index, clause) in clauses.iter().enumerate() {
            let mut add = |multi_pattern: &[TypedPattern]| {
                let mut columns = Vec::with_capacity(multi_pattern.len());
                for (subject_index, pattern) in multi_pattern.iter().enumerate() {
                    let pattern = arena.register(pattern);
                    let var = subject_variables
                        .get(subject_index)
                        .expect("Subject variable")
                        .clone();
                    columns.push(Column::new(var, pattern));
                }
                let guard = clause.guard.as_ref().map(|_| clause_index);
                let body = Body::new(clause_index as u16);
                rows.push(Row::new(columns, guard, body));
            };

            add(&clause.pattern);
            for multi_pattern in &clause.alternative_patterns {
                add(multi_pattern);
            }
        }

        // Perform exhaustiveness checking, building a decision tree
        compiler.set_pattern_arena(arena.into_inner());
        let output = compiler.compile(rows);

        // Error for missing clauses that would cause a crash
        if output.diagnostics.missing {
            return Err(Error::InexhaustiveCaseExpression {
                location,
                missing: output.missing_patterns(self.environment),
            });
        }

        // Emit warnings for unreachable clauses
        for (clause_index, clause) in clauses.iter().enumerate() {
            match output.is_reachable(clause_index) {
                Reachability::Reachable => {}
                Reachability::Unreachable(reason) => {
                    self.problems.warning(Warning::UnreachableCaseClause {
                        location: clause.location,
                        reason,
                    })
                }
            }
        }

        Ok(())
    }

    fn track_feature_usage(&mut self, feature_kind: FeatureKind, location: SrcSpan) {
        let minimum_required_version = feature_kind.required_version();

        // Then if the required version is not in the specified version for the
        // range we emit a warning highlighting the usage of the feature.
        if let Some(gleam_version) = &self.environment.gleam_version {
            if let Some(lowest_allowed_version) = gleam_version.lowest_version() {
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
        }

        if minimum_required_version > self.minimum_required_version {
            self.minimum_required_version = minimum_required_version;
        }
    }
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
            Statement::Expression(_) | Statement::Use(_) => None,
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
        TypedExpr::Tuple { elems, .. } if !elems.is_empty() => {
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

        // We make sure to not emit warnings if the case is being used like an
        // if expression:
        // ```gleam
        // case True {
        //   _ if condition -> todo
        //   _ if other_condition -> todo
        //   _ -> todo
        // }
        // ```
        _ => match subject.record_constructor_arity() {
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
            _ => None,
        },
        (TypedExpr::Int { value, .. }, _) => match (binop, value.as_str()) {
            (BinOp::GtEqInt, "0" | "-0") | (BinOp::GtInt, "1") => {
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
    match call {
        UntypedExpr::Call {
            fun: function,
            arguments,
            ..
        } => UseCall {
            arguments,
            function,
        },

        other => UseCall {
            function: Box::new(other),
            arguments: vec![],
        },
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
                | Pattern::VarUsage { .. }
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
                        kind: AssignmentKind::Generated,
                        value: Box::new(UntypedExpr::Var { location, name }),
                    };
                    assignments
                        .body_assignments
                        .push(Statement::Assignment(assignment))
                }
            }
        }

        assignments
    }
}

/// Used during `infer_record_update` to return information about the updated variant.
#[derive(Debug)]
struct RecordUpdateVariant<'a> {
    args: Vec<Arc<Type>>,
    retn: Arc<Type>,
    fields: &'a HashMap<EcoString, u32>,
}

impl RecordUpdateVariant<'_> {
    fn arg_type(&self, index: u32) -> Arc<Type> {
        self.args
            .get(index as usize)
            .expect("Failed to get record argument type after successfully inferring that field")
            .clone()
    }

    fn has_field(&self, str: &EcoString) -> bool {
        self.fields.contains_key(str)
    }

    fn field_names(&self) -> Vec<EcoString> {
        self.fields.keys().cloned().collect()
    }
}
