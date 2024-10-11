use self::expression::CallKind;

use super::*;
use crate::ast::{
    ImplicitCallArgOrigin, Statement, TypedPipelineAssignment, UntypedExpr, PIPE_VARIABLE,
};
use vec1::Vec1;

#[derive(Debug)]
pub(crate) struct PipeTyper<'a, 'b, 'c> {
    size: usize,
    argument_type: Arc<Type>,
    argument_location: SrcSpan,
    location: SrcSpan,
    assignments: Vec<TypedPipelineAssignment>,
    expr_typer: &'a mut ExprTyper<'b, 'c>,
}

impl<'a, 'b, 'c> PipeTyper<'a, 'b, 'c> {
    pub fn infer(
        expr_typer: &'a mut ExprTyper<'b, 'c>,
        expressions: Vec1<UntypedExpr>,
    ) -> Result<TypedExpr, Error> {
        // The scope is reset as pipelines are rewritten into a series of
        // assignments, and we don't want these variables to leak out of the
        // pipeline.
        let scope = expr_typer.environment.scope.clone();
        let result = PipeTyper::run(expr_typer, expressions);
        expr_typer.environment.scope = scope;
        result
    }

    fn run(
        expr_typer: &'a mut ExprTyper<'b, 'c>,
        expressions: Vec1<UntypedExpr>,
    ) -> Result<TypedExpr, Error> {
        let size = expressions.len();
        let end = expressions.last().location().end;
        let mut expressions = expressions.into_iter();
        let first = expr_typer.infer(expressions.next().expect("Empty pipeline in typer"))?;
        let mut typer = Self {
            size,
            expr_typer,
            argument_type: first.type_(),
            argument_location: first.location(),
            location: SrcSpan {
                start: first.location().start,
                end,
            },
            assignments: Vec::with_capacity(size),
        };

        // No need to update self.argument_* as we set it above
        typer.push_assignment_no_update(first);

        // Perform the type checking
        typer.infer_expressions(expressions)
    }

    fn infer_expressions(
        &mut self,
        expressions: impl IntoIterator<Item = UntypedExpr>,
    ) -> Result<TypedExpr, Error> {
        let finally = self.infer_each_expression(expressions);

        // Return any errors after clean-up
        let finally = finally?;
        let assignments = std::mem::take(&mut self.assignments);

        Ok(TypedExpr::Pipeline {
            assignments,
            location: self.location,
            finally: Box::new(finally),
        })
    }

    fn infer_each_expression(
        &mut self,
        expressions: impl IntoIterator<Item = UntypedExpr>,
    ) -> Result<TypedExpr, Error> {
        let mut finally = None;

        for (i, call) in expressions.into_iter().enumerate() {
            if self.expr_typer.previous_panics {
                self.expr_typer
                    .warn_for_unreachable_code(call.location(), PanicPosition::PreviousExpression);
            }

            self.warn_if_call_first_argument_is_hole(&call);

            let call = match call {
                func @ UntypedExpr::Fn { location, .. } => {
                    let (func, args, return_type) = self.expr_typer.do_infer_call(
                        func.clone(),
                        vec![self.untyped_left_hand_value_variable_call_argument()],
                        location,
                        CallKind::Function,
                    );
                    TypedExpr::Call {
                        location,
                        args,
                        type_: return_type,
                        fun: Box::new(func),
                    }
                }

                // left |> right(..args)
                UntypedExpr::Call {
                    fun,
                    arguments,
                    location,
                    ..
                } => {
                    let fun = self.expr_typer.infer(*fun)?;
                    match fun.type_().fn_types() {
                        // Rewrite as right(..args)(left)
                        Some((args, _)) if args.len() == arguments.len() => {
                            self.infer_apply_to_call_pipe(fun, arguments, location)
                        }
                        // Rewrite as right(left, ..args)
                        _ => self.infer_insert_pipe(fun, arguments, location),
                    }
                }

                UntypedExpr::Echo {
                    location,
                    expression: None,
                } => {
                    self.expr_typer.environment.echo_found = true;
                    // An echo that is not followed by an expression that is
                    // used as a pipeline's step is just like the identity
                    // function.
                    // So it gets the type of the value coming from the previous
                    // step of the pipeline.
                    TypedExpr::Echo {
                        location,
                        expression: None,
                        type_: self.argument_type.clone(),
                    }
                }

                // right(left)
                call => self.infer_apply_pipe(call)?,
            };

            if i + 2 == self.size {
                finally = Some(call);
            } else {
                self.push_assignment(call);
            }
        }

        Ok(finally.expect("Empty pipeline in typer"))
    }

    /// Create a call argument that can be used to refer to the value on the
    /// left hand side of the pipe
    fn typed_left_hand_value_variable_call_argument(&self) -> CallArg<TypedExpr> {
        CallArg {
            label: None,
            location: self.argument_location,
            value: self.typed_left_hand_value_variable(),
            // This argument is given implicitly by the pipe, not explicitly by
            // the programmer.
            implicit: Some(ImplicitCallArgOrigin::Pipe),
        }
    }

    /// Create a call argument that can be used to refer to the value on the
    /// left hand side of the pipe
    fn untyped_left_hand_value_variable_call_argument(&self) -> CallArg<UntypedExpr> {
        CallArg {
            label: None,
            location: self.argument_location,
            value: self.untyped_left_hand_value_variable(),
            // This argument is given implicitly by the pipe, not explicitly by
            // the programmer.
            implicit: Some(ImplicitCallArgOrigin::Pipe),
        }
    }

    /// Create a variable that can be used to refer to the value on the left
    /// hand side of the pipe
    fn typed_left_hand_value_variable(&self) -> TypedExpr {
        TypedExpr::Var {
            location: self.argument_location,
            name: PIPE_VARIABLE.into(),
            constructor: ValueConstructor {
                publicity: Publicity::Public,
                deprecation: Deprecation::NotDeprecated,
                type_: self.argument_type.clone(),
                variant: ValueConstructorVariant::LocalVariable {
                    location: self.argument_location,
                },
            },
        }
    }

    /// Create a variable that can be used to refer to the value on the left
    /// hand side of the pipe
    fn untyped_left_hand_value_variable(&self) -> UntypedExpr {
        UntypedExpr::Var {
            location: self.argument_location,
            name: PIPE_VARIABLE.into(),
        }
    }

    /// Push an assignment for the value on the left hand side of the pipe
    fn push_assignment(&mut self, expression: TypedExpr) {
        self.argument_type = expression.type_();
        self.argument_location = expression.location();
        self.push_assignment_no_update(expression)
    }

    fn push_assignment_no_update(&mut self, expression: TypedExpr) {
        let location = expression.location();
        // Insert the variable for use in type checking the rest of the pipeline
        self.expr_typer.environment.insert_local_variable(
            PIPE_VARIABLE.into(),
            location,
            expression.type_(),
        );
        // Add the assignment to the AST
        let assignment = TypedPipelineAssignment {
            location,
            name: PIPE_VARIABLE.into(),
            value: Box::new(expression),
        };
        self.assignments.push(assignment);
    }

    /// Attempt to infer a |> b(..c) as b(..c)(a)
    fn infer_apply_to_call_pipe(
        &mut self,
        function: TypedExpr,
        args: Vec<CallArg<UntypedExpr>>,
        location: SrcSpan,
    ) -> TypedExpr {
        let (function, args, type_) = self.expr_typer.do_infer_call_with_known_fun(
            function,
            args,
            location,
            CallKind::Function,
        );
        let function = TypedExpr::Call {
            location,
            type_,
            args,
            fun: Box::new(function),
        };
        let args = vec![self.untyped_left_hand_value_variable_call_argument()];
        // TODO: use `.with_unify_error_situation(UnifyErrorSituation::PipeTypeMismatch)`
        // This will require the typing of the arguments to be lifted up out of
        // the function below. If it is not we don't know if the error comes
        // from incorrect usage of the pipe or if it originates from the
        // argument expressions.
        let (function, args, type_) = self.expr_typer.do_infer_call_with_known_fun(
            function,
            args,
            location,
            CallKind::Function,
        );
        TypedExpr::Call {
            location,
            type_,
            args,
            fun: Box::new(function),
        }
    }

    /// Attempt to infer a |> b(c) as b(a, c)
    fn infer_insert_pipe(
        &mut self,
        function: TypedExpr,
        mut arguments: Vec<CallArg<UntypedExpr>>,
        location: SrcSpan,
    ) -> TypedExpr {
        arguments.insert(0, self.untyped_left_hand_value_variable_call_argument());
        // TODO: use `.with_unify_error_situation(UnifyErrorSituation::PipeTypeMismatch)`
        // This will require the typing of the arguments to be lifted up out of
        // the function below. If it is not we don't know if the error comes
        // from incorrect usage of the pipe or if it originates from the
        // argument expressions.
        let (fun, args, type_) = self.expr_typer.do_infer_call_with_known_fun(
            function,
            arguments,
            location,
            CallKind::Function,
        );
        TypedExpr::Call {
            location,
            type_,
            args,
            fun: Box::new(fun),
        }
    }

    /// Attempt to infer a |> b as b(a)
    fn infer_apply_pipe(&mut self, function: UntypedExpr) -> Result<TypedExpr, Error> {
        let function = Box::new(self.expr_typer.infer(function)?);
        let return_type = self.expr_typer.new_unbound_var();
        // Ensure that the function accepts one argument of the correct type
        unify(
            function.type_(),
            fn_(vec![self.argument_type.clone()], return_type.clone()),
        )
        .map_err(|e| {
            if self.check_if_pipe_type_mismatch(&e) {
                return convert_unify_error(e, function.location())
                    .with_unify_error_situation(UnifyErrorSituation::PipeTypeMismatch);
            }
            convert_unify_error(flip_unify_error(e), function.location())
        })?;

        Ok(TypedExpr::Call {
            location: function.location(),
            type_: return_type,
            fun: function,
            args: vec![self.typed_left_hand_value_variable_call_argument()],
        })
    }

    fn check_if_pipe_type_mismatch(&mut self, error: &UnifyError) -> bool {
        let types = match error {
            UnifyError::CouldNotUnify {
                expected, given, ..
            } => (expected.as_ref(), given.as_ref()),
            _ => return false,
        };

        match types {
            (Type::Fn { args: a, .. }, Type::Fn { args: b, .. }) if a.len() == b.len() => {
                match (a.first(), b.first()) {
                    (Some(a), Some(b)) => unify(a.clone(), b.clone()).is_err(),
                    _ => false,
                }
            }
            _ => false,
        }
    }

    fn warn_if_call_first_argument_is_hole(&mut self, call: &UntypedExpr) {
        if let UntypedExpr::Fn {
            is_capture: true,
            body,
            ..
        } = &call
        {
            if let Statement::Expression(UntypedExpr::Call { arguments, .. }) = body.first() {
                match arguments.as_slice() {
                    // If the first argument is labelled, we don't warn the user
                    // as they might be intentionally adding it to provide more
                    // information about exactly which argument is being piped into.
                    [first] | [first, ..] if first.is_capture_hole() && first.label.is_none() => {
                        self.expr_typer
                            .problems
                            .warning(Warning::RedundantPipeFunctionCapture {
                                location: first.location,
                            })
                    }
                    _ => (),
                }
            }
        }
    }
}
