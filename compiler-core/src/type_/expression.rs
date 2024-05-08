use super::{pipe::PipeTyper, *};
use crate::{
    analyse::infer_bit_array_option,
    ast::{
        Arg, Assignment, AssignmentKind, BinOp, BitArrayOption, BitArraySegment, CallArg, Clause,
        ClauseGuard, Constant, HasLocation, Layer, RecordUpdateSpread, SrcSpan, Statement,
        TodoKind, TypeAst, TypedArg, TypedAssignment, TypedClause, TypedClauseGuard, TypedConstant,
        TypedExpr, TypedMultiPattern, TypedStatement, UntypedArg, UntypedAssignment, UntypedClause,
        UntypedClauseGuard, UntypedConstant, UntypedConstantBitArraySegment, UntypedExpr,
        UntypedExprBitArraySegment, UntypedMultiPattern, UntypedStatement, Use, UseAssignment,
        USE_ASSIGNMENT_VARIABLE,
    },
    build::Target,
    exhaustiveness,
};
use id_arena::Arena;
use im::hashmap;
use itertools::Itertools;
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
    /// @external(javascript, "foo", "bar")
    /// @external(erlang, "foo", "bar")
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
        // @external(erlang, "foo", "bar")
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

    pub(crate) implementations: Implementations,
    pub(crate) current_function_definition: FunctionDefinition,

    // Type hydrator for creating types from annotations
    pub(crate) hydrator: Hydrator,

    // Accumulated errors found while typing the expression
    pub(crate) errors: &'a mut Vec<Error>,
}

impl<'a, 'b> ExprTyper<'a, 'b> {
    pub fn new(
        environment: &'a mut Environment<'b>,
        definition: FunctionDefinition,
        errors: &'a mut Vec<Error>,
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
            environment,
            implementations,
            current_function_definition: definition,
            errors,
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
            .close_scope(environment_reset_data, result.is_ok());
        self.hydrator.close_scope(hydrator_reset_data);
        result
    }

    pub fn type_from_ast(&mut self, ast: &TypeAst) -> Result<Arc<Type>, Error> {
        self.hydrator.type_from_ast(ast, self.environment)
    }

    fn instantiate(&mut self, t: Arc<Type>, ids: &mut im::HashMap<u64, Arc<Type>>) -> Arc<Type> {
        self.environment.instantiate(t, ids, &self.hydrator)
    }

    pub fn new_unbound_var(&mut self) -> Arc<Type> {
        self.environment.new_unbound_var()
    }

    pub fn infer(&mut self, expr: UntypedExpr) -> Result<TypedExpr, Error> {
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
                location, value, ..
            } => Ok(self.infer_int(value, location)),

            UntypedExpr::Block {
                statements,
                location,
            } => self.infer_block(statements, location),

            UntypedExpr::Tuple {
                location, elems, ..
            } => self.infer_tuple(elems, location),

            UntypedExpr::Float {
                location, value, ..
            } => Ok(self.infer_float(value, location)),

            UntypedExpr::String {
                location, value, ..
            } => Ok(self.infer_string(value, location)),

            UntypedExpr::PipeLine { expressions } => self.infer_pipeline(expressions),

            UntypedExpr::Fn {
                location,
                is_capture,
                arguments: args,
                body,
                return_annotation,
                ..
            } => self.infer_fn(args, &[], body, is_capture, return_annotation, location),

            UntypedExpr::Case {
                location,
                subjects,
                clauses,
                ..
            } => self.infer_case(subjects, clauses, location),

            UntypedExpr::List {
                location,
                elements,
                tail,
                ..
            } => self.infer_list(elements, tail, location),

            UntypedExpr::Call {
                location,
                fun,
                arguments: args,
                ..
            } => self.infer_call(*fun, args, location, CallKind::Function),

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
            } => {
                self.infer_field_access(*container, label, label_location, FieldAccessUsage::Other)
            }

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
                spread,
                arguments: args,
            } => self.infer_record_update(*constructor, spread, args, location),

            UntypedExpr::NegateBool { location, value } => self.infer_negate_bool(location, *value),

            UntypedExpr::NegateInt { location, value } => self.infer_negate_int(location, *value),
        }
    }

    fn infer_pipeline(&mut self, expressions: Vec1<UntypedExpr>) -> Result<TypedExpr, Error> {
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
        self.environment.warnings.emit(Warning::Todo {
            kind,
            location,
            typ: type_.clone(),
        });

        // We've seen a todo, so register that fact. This can be used by higher
        // level tooling such as the build tool when publishing a package.
        self.environment.todo_encountered = true;

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
        Ok(TypedExpr::Panic {
            location,
            type_,
            message,
        })
    }

    fn infer_string(&mut self, value: EcoString, location: SrcSpan) -> TypedExpr {
        TypedExpr::String {
            location,
            value,
            typ: string(),
        }
    }

    fn infer_int(&mut self, value: EcoString, location: SrcSpan) -> TypedExpr {
        TypedExpr::Int {
            location,
            value,
            typ: int(),
        }
    }

    fn infer_float(&mut self, value: EcoString, location: SrcSpan) -> TypedExpr {
        TypedExpr::Float {
            location,
            value,
            typ: float(),
        }
    }

    /// Emit a warning if the given expressions should not be discarded.
    /// e.g. because it's a literal (why was it made in the first place?)
    /// e.g. because it's of the `Result` type (errors should be handled)
    fn expression_discarded(&mut self, discarded: &TypedExpr) {
        if discarded.is_literal() {
            self.environment.warnings.emit(Warning::UnusedLiteral {
                location: discarded.location(),
            });
        } else if discarded.type_().is_result() {
            self.environment
                .warnings
                .emit(Warning::ImplicitlyDiscardedResult {
                    location: discarded.location(),
                });
        } else if discarded.is_pure_value_constructor() {
            self.environment.warnings.emit(Warning::UnusedValue {
                location: discarded.location(),
            })
        }
    }

    pub(crate) fn infer_statements(
        &mut self,
        untyped: Vec1<UntypedStatement>,
    ) -> Result<Vec1<TypedStatement>, Error> {
        let count = untyped.len();
        let location = SrcSpan::new(
            untyped.first().location().start,
            untyped.last().location().end,
        );
        self.infer_iter_statements(location, count, untyped.into_iter())
    }

    fn infer_iter_statements<StatementsIter: Iterator<Item = UntypedStatement>>(
        &mut self,
        location: SrcSpan,
        count: usize,
        mut untyped: StatementsIter,
    ) -> Result<Vec1<TypedStatement>, Error> {
        let mut i = 0;
        let mut statements: Vec<TypedStatement> = Vec::with_capacity(count);

        while let Some(statement) = untyped.next() {
            i += 1;

            match statement {
                Statement::Use(use_) => {
                    let statement = self.infer_use(use_, location, untyped.collect())?;
                    statements.push(statement);
                    break; // Inferring the use has consumed the rest of the exprs
                }

                Statement::Expression(expression) => {
                    let expression = self.infer(expression)?;

                    // This isn't the final expression in the sequence, so call the
                    // `expression_discarded` function to see if anything is being
                    // discarded that we think shouldn't be.
                    if i < count {
                        self.expression_discarded(&expression);
                    }

                    statements.push(Statement::Expression(expression));
                }

                Statement::Assignment(assignment) => {
                    let assignment = self.infer_assignment(assignment)?;
                    statements.push(Statement::Assignment(assignment));
                }
            }
        }

        Ok(Vec1::try_from_vec(statements).expect("empty sequence"))
    }

    fn infer_use(
        &mut self,
        use_: Use,
        sequence_location: SrcSpan,
        mut following_expressions: Vec<UntypedStatement>,
    ) -> Result<TypedStatement, Error> {
        let use_call_location = use_.call.location();
        let mut call = get_use_expression_call(*use_.call)?;
        let assignments = UseAssignments::from_use_expression(use_.assignments);

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
            return_annotation: None,
            is_capture: false,
            body: statements,
        };

        // Add this new callback function to the arguments to function call
        call.arguments.push(CallArg {
            label: None,
            location: SrcSpan::new(first.start, sequence_location.end),
            value: callback,
            // This argument is implicitly given by Gleam's use syntax so we
            // mark it as such.
            implicit: true,
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
        )?;

        Ok(Statement::Expression(call))
    }

    fn infer_negate_bool(
        &mut self,
        location: SrcSpan,
        value: UntypedExpr,
    ) -> Result<TypedExpr, Error> {
        let value = self.infer(value)?;

        unify(bool(), value.type_()).map_err(|e| convert_unify_error(e, value.location()))?;

        if let TypedExpr::NegateBool { .. } = value {
            self.environment
                .warnings
                .emit(Warning::UnnecessaryDoubleBoolNegation { location });
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
                self.environment
                    .warnings
                    .emit(Warning::UnnecessaryDoubleIntNegation { location });
            }
        }

        if let TypedExpr::NegateInt { .. } = value {
            self.environment
                .warnings
                .emit(Warning::UnnecessaryDoubleIntNegation { location });
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
        is_capture: bool,
        return_annotation: Option<TypeAst>,
        location: SrcSpan,
    ) -> Result<TypedExpr, Error> {
        let (args, body) = self.do_infer_fn(args, expected_args, body, &return_annotation)?;
        let args_types = args.iter().map(|a| a.type_.clone()).collect();
        let typ = fn_(args_types, body.last().type_());
        Ok(TypedExpr::Fn {
            location,
            typ,
            is_capture,
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
        let typ = annotation
            .clone()
            .map(|t| self.type_from_ast(&t))
            .unwrap_or_else(|| Ok(self.new_unbound_var()))?;

        // If we know the expected type of the argument from its contextual
        // usage then unify the newly constructed type with the expected type.
        // We do this here because then there is more type information for the
        // function being type checked, resulting in better type errors and the
        // record field access syntax working.
        if let Some(expected) = expected {
            unify(expected, typ.clone()).map_err(|e| convert_unify_error(e, location))?;
        }

        Ok(Arg {
            names,
            location,
            annotation,
            type_: typ,
        })
    }

    fn infer_call(
        &mut self,
        fun: UntypedExpr,
        args: Vec<CallArg<UntypedExpr>>,
        location: SrcSpan,
        kind: CallKind,
    ) -> Result<TypedExpr, Error> {
        let (fun, args, typ) = self.do_infer_call(fun, args, location, kind)?;

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
            self.environment
                .warnings
                .emit(Warning::TodoOrPanicUsedAsFunction {
                    kind,
                    location,
                    args_location,
                    args: args.len(),
                });
        }

        Ok(TypedExpr::Call {
            location,
            typ,
            args,
            fun: Box::new(fun),
        })
    }

    fn infer_list(
        &mut self,
        elements: Vec<UntypedExpr>,
        tail: Option<Box<UntypedExpr>>,
        location: SrcSpan,
    ) -> Result<TypedExpr, Error> {
        let typ = self.new_unbound_var();
        // Type check each elements
        let elements = elements
            .into_iter()
            .map(|element| {
                let element = self.infer(element)?;
                // Ensure they all have the same type
                unify(typ.clone(), element.type_()).map_err(|e| {
                    convert_unify_error(e.list_element_mismatch(), element.location())
                })?;
                Ok(element)
            })
            .try_collect()?;
        // Type check the ..tail, if there is one
        let typ = list(typ);
        let tail = match tail {
            Some(tail) => {
                let tail = self.infer(*tail)?;
                // Ensure the tail has the same type as the preceding elements
                unify(typ.clone(), tail.type_())
                    .map_err(|e| convert_unify_error(e.list_tail_mismatch(), tail.location()))?;
                Some(Box::new(tail))
            }
            None => None,
        };
        Ok(TypedExpr::List {
            location,
            typ,
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
        let typ = tuple(elems.iter().map(HasType::type_).collect());
        Ok(TypedExpr::Tuple {
            location,
            elems,
            typ,
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

    fn infer_field_access(
        &mut self,
        container: UntypedExpr,
        label: EcoString,
        label_location: SrcSpan,
        usage: FieldAccessUsage,
    ) -> Result<TypedExpr, Error> {
        // Attempt to infer the container as a record access. If that fails, we may be shadowing the name
        // of an imported module, so attempt to infer the container as a module access.
        // TODO: Remove this cloning
        match self.infer_record_expression_access(
            container.clone(),
            label.clone(),
            label_location,
            usage,
        ) {
            Ok(record_access) => Ok(record_access),
            Err(err) => match container {
                UntypedExpr::Var { name, location, .. } => {
                    let module_access =
                        self.infer_module_access(&name, label, &location, label_location);

                    // If the name is in the environment, use the original error from
                    // inferring the record access, so that we can suggest possible
                    // misspellings of field names
                    if self.environment.scope.contains_key(&name) {
                        module_access.map_err(|_| err)
                    } else {
                        module_access
                    }
                }
                _ => Err(err),
            },
        }
    }

    fn infer_tuple_index(
        &mut self,
        tuple: UntypedExpr,
        index: u64,
        location: SrcSpan,
    ) -> Result<TypedExpr, Error> {
        let tuple = self.infer(tuple)?;
        match collapse_links(tuple.type_()).as_ref() {
            Type::Tuple { elems } => {
                let typ = elems
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
                    typ,
                })
            }

            typ if typ.is_unbound() => Err(Error::NotATupleUnbound {
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
                self.infer_bit_segment(*s.value, s.options, s.location, |env, expr| env.infer(expr))
            })
            .try_collect()?;

        Ok(TypedExpr::BitArray {
            location,
            segments,
            typ: bits(),
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
                self.infer_bit_segment(*s.value, s.options, s.location, |env, expr| {
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
            infer_bit_array_option(segment_option, |value, typ| {
                let typed_value = infer(self, value)?;
                unify(typ, typed_value.type_())
                    .map_err(|e| convert_unify_error(e, typed_value.location()))?;
                Ok(typed_value)
            })
        };

        let options: Vec<_> = options.into_iter().map(infer_option).try_collect()?;

        let typ = bit_array::type_options_for_value(&options).map_err(|error| {
            Error::BitArraySegmentError {
                error: error.error,
                location: error.location,
            }
        })?;

        unify(typ.clone(), value.type_()).map_err(|e| convert_unify_error(e, value.location()))?;

        Ok(BitArraySegment {
            location,
            type_: typ,
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
                    typ: bool(),
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
            typ: output_type,
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
        self.environment
            .warnings
            .emit(Warning::InefficientEmptyListCheck { location, kind });
    }

    fn infer_assignment(
        &mut self,
        assignment: UntypedAssignment,
    ) -> Result<TypedAssignment, Error> {
        let Assignment {
            pattern,
            value,
            kind,
            annotation,
            location,
        } = assignment;
        let value = self.in_new_scope(|value_typer| value_typer.infer(*value))?;
        let value_typ = value.type_();

        // Ensure the pattern matches the type of the value
        let pattern = pattern::PatternTyper::new(self.environment, &self.hydrator)
            .unify(pattern, value_typ.clone())?;

        // Check that any type annotation is accurate.
        if let Some(annotation) = &annotation {
            let ann_typ = self
                .type_from_ast(annotation)
                .map(|t| self.instantiate(t, &mut hashmap![]))?;
            unify(ann_typ, value_typ.clone())
                .map_err(|e| convert_unify_error(e, value.type_defining_location()))?;
        }

        // Do not perform exhaustiveness checking if user explicitly used `let assert ... = ...`.
        let exhaustiveness_check = self.check_let_exhaustiveness(location, value.type_(), &pattern);
        match kind {
            AssignmentKind::Let => exhaustiveness_check?,
            AssignmentKind::Assert { location } if exhaustiveness_check.is_ok() => self
                .environment
                .warnings
                .emit(Warning::RedundantAssertAssignment { location }),
            AssignmentKind::Assert { .. } => {}
        }

        Ok(Assignment {
            location,
            annotation,
            kind,
            pattern,
            value: Box::new(value),
        })
    }

    fn infer_case(
        &mut self,
        subjects: Vec<UntypedExpr>,
        clauses: Vec<UntypedClause>,
        location: SrcSpan,
    ) -> Result<TypedExpr, Error> {
        let subjects_count = subjects.len();
        let mut typed_subjects = Vec::with_capacity(subjects_count);
        let mut subject_types = Vec::with_capacity(subjects_count);
        let mut typed_clauses = Vec::with_capacity(clauses.len());

        let return_type = self.new_unbound_var();

        for subject in subjects {
            let subject = self.in_new_scope(|subject_typer| {
                let subject = subject_typer.infer(subject)?;

                Ok(subject)
            })?;

            subject_types.push(subject.type_());
            typed_subjects.push(subject);
        }

        for clause in clauses {
            let typed_clause = self.infer_clause(clause, &subject_types)?;
            unify(return_type.clone(), typed_clause.then.type_())
                .map_err(|e| e.case_clause_mismatch().into_error(typed_clause.location()))?;
            typed_clauses.push(typed_clause);
        }

        self.check_case_exhaustiveness(location, &subject_types, &typed_clauses)?;
        typed_subjects
            .iter()
            .filter_map(check_subject_for_redundant_match)
            .for_each(|warning| self.environment.warnings.emit(warning));

        Ok(TypedExpr::Case {
            location,
            typ: return_type,
            subjects: typed_subjects,
            clauses: typed_clauses,
        })
    }

    fn infer_clause(
        &mut self,
        clause: UntypedClause,
        subjects: &[Arc<Type>],
    ) -> Result<TypedClause, Error> {
        let Clause {
            pattern,
            alternative_patterns,
            guard,
            then,
            location,
        } = clause;

        let (guard, then, typed_pattern, typed_alternatives) =
            self.in_new_scope(|clause_typer| {
                // Check the types
                let (typed_pattern, typed_alternatives) = clause_typer.infer_clause_pattern(
                    pattern,
                    alternative_patterns,
                    subjects,
                    &location,
                )?;
                let guard = clause_typer.infer_optional_clause_guard(guard)?;
                let then = clause_typer.infer(then)?;

                Ok((guard, then, typed_pattern, typed_alternatives))
            })?;

        Ok(Clause {
            location,
            pattern: typed_pattern,
            alternative_patterns: typed_alternatives,
            guard,
            then,
        })
    }

    fn infer_clause_pattern(
        &mut self,
        pattern: UntypedMultiPattern,
        alternatives: Vec<UntypedMultiPattern>,
        subjects: &[Arc<Type>],
        location: &SrcSpan,
    ) -> Result<(TypedMultiPattern, Vec<TypedMultiPattern>), Error> {
        let mut pattern_typer = pattern::PatternTyper::new(self.environment, &self.hydrator);
        let typed_pattern = pattern_typer.infer_multi_pattern(pattern, subjects, location)?;

        // Each case clause has one or more patterns that may match the
        // subject in order for the clause to be selected, so we must type
        // check every pattern.
        let mut typed_alternatives = Vec::with_capacity(alternatives.len());
        for m in alternatives {
            typed_alternatives
                .push(pattern_typer.infer_alternative_multi_pattern(m, subjects, location)?);
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
                match &constructor.variant {
                    ValueConstructorVariant::LocalVariable { .. } => (),
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

                    typ if typ.is_unbound() => Err(Error::NotATupleUnbound {
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

            ClauseGuard::Constant(constant) => {
                Ok(ClauseGuard::Constant(self.infer_const(&None, constant)))
            }
        }
    }

    fn infer_guard_record_access(
        &mut self,
        container: ClauseGuard<Arc<Type>, EcoString>,
        label: EcoString,
        location: SrcSpan,
    ) -> Result<ClauseGuard<Arc<Type>, EcoString>, Error> {
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
        record_access_erorr: Error,
    ) -> Result<ClauseGuard<Arc<Type>, EcoString>, Error> {
        let module_access = self
            .infer_module_access(&name, label, &location, location)
            .and_then(|ma| match ma {
                TypedExpr::ModuleSelect {
                    location,
                    typ,
                    label,
                    module_name,
                    module_alias,
                    constructor,
                } => match constructor {
                    ModuleValueConstructor::Constant { literal, .. } => {
                        Ok(ClauseGuard::ModuleSelect {
                            location,
                            type_: typ,
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
            module_access.map_err(|_| record_access_erorr)
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
                    imported_modules: self.environment.imported_modules.keys().cloned().collect(),
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
                        type_with_same_name: false,
                    })?;

            // Emit a warning if the value being used is deprecated.
            if let Deprecation::Deprecated { message } = &constructor.deprecation {
                self.environment.warnings.emit(Warning::DeprecatedItem {
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
            typ: Arc::clone(&type_),
            location: select_location,
            module_name,
            module_alias: module_alias.clone(),
            constructor,
        })
    }

    fn infer_record_expression_access(
        &mut self,
        record: UntypedExpr,
        label: EcoString,
        location: SrcSpan,
        usage: FieldAccessUsage,
    ) -> Result<TypedExpr, Error> {
        // Infer the type of the (presumed) record
        let record = self.infer(record)?;
        self.infer_known_record_expression_access(record, label, location, usage)
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
        let (index, label, typ) =
            self.infer_known_record_access(record_type, record.location(), usage, location, label)?;
        Ok(TypedExpr::RecordAccess {
            record,
            label,
            index,
            location,
            typ,
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

        let has_variants = match &*record_type {
            Type::Named { name, .. } => self
                .environment
                .module_types_constructors
                .get(name)
                .map_or(RecordVariants::NoVariants, |type_variant_constructors| {
                    if type_variant_constructors.variants.len() > 1 {
                        RecordVariants::HasVariants
                    } else {
                        RecordVariants::NoVariants
                    }
                }),
            _ => RecordVariants::NoVariants,
        };

        let unknown_field = |fields| Error::UnknownRecordField {
            usage,
            typ: record_type.clone(),
            location,
            label: label.clone(),
            fields,
            variants: has_variants,
        };
        let accessors = match collapse_links(record_type.clone()).as_ref() {
            // A type in the current module which may have fields
            Type::Named { module, name, .. } if module == &self.environment.current_module => {
                self.environment.accessors.get(name)
            }

            // A type in another module which may have fields
            Type::Named { module, name, .. } => self
                .environment
                .importable_modules
                .get(module)
                .and_then(|module| module.accessors.get(name)),

            _something_without_fields => return Err(unknown_field(vec![])),
        }
        .ok_or_else(|| unknown_field(vec![]))?;
        let RecordAccessor {
            index,
            label,
            type_: typ,
        } = accessors
            .accessors
            .get(&label)
            .ok_or_else(|| unknown_field(accessors.accessors.keys().cloned().collect()))?
            .clone();
        let accessor_record_type = accessors.type_.clone();
        let mut type_vars = hashmap![];
        let accessor_record_type = self.instantiate(accessor_record_type, &mut type_vars);
        let typ = self.instantiate(typ, &mut type_vars);
        unify(accessor_record_type, record_type)
            .map_err(|e| convert_unify_error(e, record_location))?;
        Ok((index, label, typ))
    }

    fn infer_record_update(
        &mut self,
        constructor: UntypedExpr,
        spread: RecordUpdateSpread,
        args: Vec<UntypedRecordUpdateArg>,
        location: SrcSpan,
    ) -> Result<TypedExpr, Error> {
        let (module, name) = match self.infer(constructor.clone())? {
            TypedExpr::ModuleSelect {
                module_alias,
                label,
                ..
            } => (Some(module_alias), label),

            TypedExpr::Var { name, .. } => (None, name),

            constructor => {
                return Err(Error::RecordUpdateInvalidConstructor {
                    location: constructor.location(),
                });
            }
        };

        let value_constructor = self
            .environment
            .get_value_constructor(module.as_ref(), &name)
            .map_err(|e| convert_get_value_constructor_error(e, location))?
            .clone();

        // It must be a record with a field map for us to be able to update it
        let (field_map, constructors_count) = match &value_constructor.variant {
            ValueConstructorVariant::Record {
                field_map: Some(field_map),
                constructors_count,
                ..
            } => (field_map, *constructors_count),
            _ => {
                return Err(Error::RecordUpdateInvalidConstructor {
                    location: constructor.location(),
                });
            }
        };

        // We can only update a record if it is the only variant of its type.
        // If a record has multiple variants it cannot be safely updated as it
        // could be one of the other variants.
        if constructors_count != 1 {
            return Err(Error::UpdateMultiConstructorType {
                location: constructor.location(),
            });
        }

        // The type must be a function for it to be a record constructor
        let retrn = match value_constructor.type_.as_ref() {
            Type::Fn { retrn, .. } => retrn,
            _ => {
                return Err(Error::RecordUpdateInvalidConstructor {
                    location: constructor.location(),
                })
            }
        };

        let spread = self.infer(*spread.base)?;
        let return_type = self.instantiate(retrn.clone(), &mut hashmap![]);

        // Check that the spread variable unifies with the return type of the constructor
        unify(return_type, spread.type_())
            .map_err(|e| convert_unify_error(e, spread.location()))?;

        let args: Vec<TypedRecordUpdateArg> = args
            .iter()
            .map(
                |UntypedRecordUpdateArg {
                     label,
                     value,
                     location,
                 }| {
                    let value = self.infer(value.clone())?;
                    let spread_field = self.infer_known_record_expression_access(
                        spread.clone(),
                        label.clone(),
                        *location,
                        FieldAccessUsage::Other,
                    )?;

                    // Check that the update argument unifies with the corresponding
                    // field in the record contained within the spread variable. We
                    // need to check the spread, and not the constructor, in order
                    // to handle polymorphic types.
                    unify(spread_field.type_(), value.type_())
                        .map_err(|e| convert_unify_error(e, value.location()))?;

                    match field_map.fields.get(label) {
                        None => panic!(
                            "Failed to lookup record field after successfully inferring that field",
                        ),
                        Some(p) => Ok(TypedRecordUpdateArg {
                            location: *location,
                            label: label.clone(),
                            value,
                            index: *p,
                        }),
                    }
                },
            )
            .try_collect()?;

        if args.is_empty() {
            self.environment
                .warnings
                .emit(Warning::NoFieldsRecordUpdate { location });
        }

        if args.len() == field_map.arity as usize {
            self.environment
                .warnings
                .emit(Warning::AllFieldsRecordUpdate { location });
        }

        Ok(TypedExpr::RecordUpdate {
            location,
            typ: spread.type_(),
            spread: Box::new(spread),
            args,
        })
    }

    fn infer_value_constructor(
        &mut self,
        module: &Option<EcoString>,
        name: &EcoString,
        location: &SrcSpan,
    ) -> Result<ValueConstructor, Error> {
        let constructor = match module {
            // Look in the current scope for a binding with this name
            None => {
                let constructor =
                    self.environment
                        .get_variable(name)
                        .cloned()
                        .ok_or_else(|| Error::UnknownVariable {
                            location: *location,
                            name: name.clone(),
                            variables: self.environment.local_value_names(),
                            type_with_name_in_scope: self
                                .environment
                                .module_types
                                .keys()
                                .any(|typ| typ == name),
                        })?;

                // Register the value as seen for detection of unused values
                self.environment.increment_usage(name);

                constructor
            }

            // Look in an imported module for a binding with this name
            Some(module_name) => {
                let (_, module) = &self
                    .environment
                    .imported_modules
                    .get(module_name)
                    .ok_or_else(|| Error::UnknownModule {
                        location: *location,
                        name: module_name.clone(),
                        imported_modules: self
                            .environment
                            .imported_modules
                            .keys()
                            .cloned()
                            .collect(),
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
                        type_with_same_name: false,
                    })?
            }
        };

        let ValueConstructor {
            publicity,
            variant,
            type_: typ,
            deprecation,
        } = constructor;

        // Emit a warning if the value being used is deprecated.
        if let Deprecation::Deprecated { message } = &deprecation {
            self.environment.warnings.emit(Warning::DeprecatedItem {
                location: *location,
                message: message.clone(),
                layer: Layer::Value,
            })
        }

        self.narrow_implementations(*location, &variant)?;

        // Instantiate generic variables into unbound variables for this usage
        let typ = self.instantiate(typ, &mut hashmap![]);
        Ok(ValueConstructor {
            publicity,
            deprecation,
            variant,
            type_: typ,
        })
    }

    // helper for infer_const to get the value of a constant ignoring annotations
    fn infer_const_value(&mut self, value: UntypedConstant) -> Result<TypedConstant, Error> {
        match value {
            Constant::Int {
                location, value, ..
            } => Ok(Constant::Int { location, value }),

            Constant::Float {
                location, value, ..
            } => Ok(Constant::Float { location, value }),

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
                if let Some(ref module) = &module {
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
                    typ: constructor.type_,
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
                if let Some(ref module) = &module {
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
                    Some(module_alias) => {
                        let typ = Arc::clone(&constructor.type_);
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
                            type_: Arc::clone(&typ),
                            location: constructor.variant.definition_location(),
                            documentation: None,
                        };

                        TypedExpr::ModuleSelect {
                            label: name.clone(),
                            module_alias: module_alias.clone(),
                            module_name,
                            typ,
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
                    .map_err(|e| convert_get_value_constructor_error(e, location))?
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
                    .map(|(typ, arg): (&mut Arc<Type>, _)| {
                        let CallArg {
                            label,
                            value,
                            location,
                            implicit,
                        } = arg;
                        let value = self.infer_const(&None, value);
                        unify(typ.clone(), value.type_())
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
                    typ: return_type,
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
                if let Some(ref module) = &module {
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
                        typ: Arc::clone(&constructor.type_),
                        constructor: Some(Box::from(constructor)),
                    }),
                    // It cannot be a Record because then this constant would have been
                    // parsed as a Constant::Record. Therefore this code is unreachable.
                    ValueConstructorVariant::Record { .. } => unreachable!(),
                }
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
                self.errors.push(e);
                Constant::Invalid {
                    location: loc,
                    typ: self.new_unbound_var(),
                }
            }
            // Type annotation and inferred value are valid. Ensure they are unifiable.
            // NOTE: if the types are not unifiable we use the annotated type.
            (Some(Ok(const_ann)), Ok(inferred)) => {
                if let Err(e) = unify(const_ann.clone(), inferred.type_())
                    .map_err(|e| convert_unify_error(e, inferred.location()))
                {
                    self.errors.push(e);
                    Constant::Invalid {
                        location: loc,
                        typ: const_ann,
                    }
                } else {
                    inferred
                }
            }
            // Type annotation is valid but not the inferred value. Place a placeholder constant with the annotation type.
            // This should limit the errors to only the definition.
            (Some(Ok(const_ann)), Err(value_err)) => {
                self.errors.push(value_err);
                Constant::Invalid {
                    location: loc,
                    typ: const_ann,
                }
            }
            // Type annotation is invalid but the inferred value is ok. Use the inferred type.
            (Some(Err(annotation_err)), Ok(inferred)) => {
                self.errors.push(annotation_err);
                inferred
            }
            // Type annotation and inferred value are invalid. Place a placeholder constant with an unbound type.
            // This should limit the errors to only the definition assuming the constant is used consistently.
            (Some(Err(annotation_err)), Err(value_err)) => {
                self.errors.push(annotation_err);
                self.errors.push(value_err);
                Constant::Invalid {
                    location: loc,
                    typ: self.new_unbound_var(),
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
        let typ = self.new_unbound_var();
        let mut elements = Vec::with_capacity(untyped_elements.len());

        for element in untyped_elements {
            let element = self.infer_const(&None, element);
            unify(typ.clone(), element.type_())
                .map_err(|e| convert_unify_error(e, element.location()))?;
            elements.push(element);
        }

        Ok(Constant::List {
            elements,
            location,
            typ: list(typ),
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
            } => (Some(EcoString::from(module_alias.as_str())), label),

            TypedExpr::Var { name, .. } => (None, name),

            _ => return Ok(None),
        };

        Ok(self
            .environment
            .get_value_constructor(module.as_ref(), name)?
            .field_map())
    }

    pub fn do_infer_call(
        &mut self,
        fun: UntypedExpr,
        args: Vec<CallArg<UntypedExpr>>,
        location: SrcSpan,
        kind: CallKind,
    ) -> Result<(TypedExpr, Vec<TypedCallArg>, Arc<Type>), Error> {
        let fun = match fun {
            UntypedExpr::FieldAccess {
                label,
                container,
                label_location,
                ..
            } => self.infer_field_access(
                *container,
                label,
                label_location,
                FieldAccessUsage::MethodCall,
            ),

            fun => self.infer(fun),
        }?;

        let (fun, args, typ) = self.do_infer_call_with_known_fun(fun, args, location, kind)?;
        Ok((fun, args, typ))
    }

    pub fn do_infer_call_with_known_fun(
        &mut self,
        fun: TypedExpr,
        mut args: Vec<CallArg<UntypedExpr>>,
        location: SrcSpan,
        kind: CallKind,
    ) -> Result<(TypedExpr, Vec<TypedCallArg>, Arc<Type>), Error> {
        // Check to see if the function accepts labelled arguments
        match self
            .get_field_map(&fun)
            .map_err(|e| convert_get_value_constructor_error(e, location))?
        {
            // The fun has a field map so labelled arguments may be present and need to be reordered.
            Some(field_map) => field_map.reorder(&mut args, location)?,

            // The fun has no field map and so we error if arguments have been labelled
            None => assert_no_labelled_arguments(&args)?,
        }

        // Extract the type of the fun, ensuring it actually is a function
        let (mut args_types, return_type) =
            match_fun_type(fun.type_(), args.len(), self.environment)
                .map_err(|e| convert_not_fun_error(e, fun.location(), location, kind))?;

        // Ensure that the given args have the correct types
        let args_count = args_types.len();
        let args = args_types
            .iter_mut()
            .zip(args)
            .enumerate()
            .map(|(i, (typ, arg))| {
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
                let value = self.infer_call_argument(value, typ.clone(), argument_kind)?;

                Ok(CallArg {
                    label,
                    value,
                    implicit,
                    location,
                })
            })
            .try_collect()?;
        Ok((fun, args, return_type))
    }

    fn infer_call_argument(
        &mut self,
        value: UntypedExpr,
        typ: Arc<Type>,
        kind: ArgumentKind,
    ) -> Result<TypedExpr, Error> {
        let typ = collapse_links(typ);

        let value = match (&*typ, value) {
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
                    is_capture: false,
                    ..
                },
            ) if expected_arguments.len() == arguments.len() => self.infer_fn(
                arguments,
                expected_arguments,
                body,
                false,
                return_annotation,
                location,
            ),

            // Otherwise just perform normal type inference.
            (_, value) => self.infer(value),
        }?;

        unify(typ, value.type_())
            .map_err(|e| convert_unify_call_error(e, value.location(), kind))?;
        Ok(value)
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
                    ArgNames::Named { name } | ArgNames::NamedLabelled { name, .. } => {
                        // Check that this name has not already been used for
                        // another argument
                        if !argument_names.insert(name) {
                            return Err(Error::ArgumentNameAlreadyUsed {
                                location: arg.location,
                                name: name.clone(),
                            });
                        }

                        // Insert a variable for the argument into the environment
                        body_typer
                            .environment
                            .insert_local_variable(name.clone(), arg.location, t);

                        if !body.first().is_placeholder() {
                            // Register the variable in the usage tracker so that we
                            // can identify if it is unused
                            body_typer.environment.init_usage(
                                name.clone(),
                                EntityKind::Variable,
                                arg.location,
                            );
                        }
                    }
                    ArgNames::Discard { .. } | ArgNames::LabelledDiscard { .. } => (),
                };
            }

            let body = body_typer.infer_statements(body);
            let body_rigid_names = body_typer.hydrator.rigid_names();
            let body = body.map_err(|e| e.with_unify_error_rigid_names(&body_rigid_names))?;

            // Check that any return type is accurate.
            if let Some(return_type) = return_type {
                unify(return_type, body.last().type_()).map_err(|e| {
                    e.return_annotation_mismatch()
                        .into_error(body.last().type_defining_location())
                        .with_unify_error_rigid_names(&body_rigid_names)
                })?;
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
            let statements = typer.infer_statements(statements)?;
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

        let subject_variable = compiler.new_variable(subject.clone());

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
        &self,
        location: SrcSpan,
        subject_types: &[Arc<Type>],
        clauses: &[Clause<TypedExpr, Arc<Type>, EcoString>],
    ) -> Result<(), Error> {
        use exhaustiveness::{Body, Column, Compiler, PatternArena, Row};

        let mut compiler = Compiler::new(self.environment, Arena::new());
        let mut arena = PatternArena::new();

        let subject_variables = subject_types
            .iter()
            .map(|t| compiler.new_variable(t.clone()))
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
            if !output.is_reachable(clause_index) {
                self.environment
                    .warnings
                    .emit(Warning::UnreachableCaseClause {
                        location: clause.location,
                    })
            }
        }

        Ok(())
    }
}

fn check_subject_for_redundant_match(subject: &TypedExpr) -> Option<Warning> {
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

        _ => match subject.record_constructor_arity() {
            Some(0) => Some(Warning::CaseMatchOnLiteralValue {
                location: subject.location(),
            }),
            Some(_) => Some(Warning::CaseMatchOnLiteralCollection {
                kind: LiteralCollectionKind::Record,
                location: subject.location(),
            }),
            None if subject.is_literal() => Some(Warning::CaseMatchOnLiteralValue {
                location: subject.location(),
            }),
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

fn get_use_expression_call(call: UntypedExpr) -> Result<UseCall, Error> {
    // Ensure that the use's call is of the right structure. i.e. it is a
    // call to a function.
    match call {
        UntypedExpr::Call {
            fun: function,
            arguments,
            ..
        } => Ok(UseCall {
            arguments,
            function,
        }),

        other => Ok(UseCall {
            function: Box::new(other),
            arguments: vec![],
        }),
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
    function_arguments: Vec<Arg<()>>,

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
    fn from_use_expression(sugar_assignments: Vec<UseAssignment>) -> UseAssignments {
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
                    names: ArgNames::Discard { name },
                    annotation: None,
                    type_: (),
                }),

                // For simple patterns of a single variable we add a regular
                // function argument.
                Pattern::Variable { name, .. } => assignments.function_arguments.push(Arg {
                    location,
                    annotation,
                    names: ArgNames::Named { name },
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
                | Pattern::StringPrefix { .. }) => {
                    let name: EcoString = format!("{USE_ASSIGNMENT_VARIABLE}{index}").into();
                    assignments.function_arguments.push(Arg {
                        location,
                        names: ArgNames::Named { name: name.clone() },
                        annotation: None,
                        type_: (),
                    });
                    let assignment = Assignment {
                        location,
                        pattern,
                        annotation,
                        kind: AssignmentKind::Let,
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
