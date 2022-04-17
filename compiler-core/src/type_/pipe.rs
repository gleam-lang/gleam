use super::*;
use crate::ast::{AssignmentKind, UntypedExpr, PIPE_VARIABLE};
use vec1::Vec1;

#[derive(Debug)]
pub(crate) struct PipeTyper<'a, 'b, 'c> {
    size: usize,
    argument_type: Arc<Type>,
    argument_location: SrcSpan,
    location: SrcSpan,
    expressions: Vec<TypedExpr>,
    expr_typer: &'a mut ExprTyper<'b, 'c>,
}

impl<'a, 'b, 'c> PipeTyper<'a, 'b, 'c> {
    pub fn infer(
        expr_typer: &'a mut ExprTyper<'b, 'c>,
        expressions: Vec1<UntypedExpr>,
    ) -> Result<TypedExpr, Error> {
        let size = expressions.len();
        let end = &expressions[..]
            .last()
            // The vec is non-empty, this indexing can never fail
            .expect("Empty pipeline in typer")
            .location()
            .end;
        let mut expressions = expressions.into_iter();
        let first = expr_typer.infer(expressions.next().expect("Empty pipeline in typer"))?;
        let mut typer = Self {
            size,
            expr_typer,
            argument_type: first.type_(),
            argument_location: first.location(),
            location: SrcSpan {
                start: first.location().start,
                end: *end,
            },
            expressions: Vec::with_capacity(size),
        };
        // No need to update self.argument_* as we set it above
        typer.push_assignment_no_update(first);
        // Perform the type checking
        typer.infer_expressions(expressions)
    }

    fn infer_expressions(
        mut self,
        expressions: impl IntoIterator<Item = UntypedExpr>,
    ) -> Result<TypedExpr, Error> {
        let result = self.infer_each_expression(expressions);

        // Clean-up the pipe variables inserted so they cannot be used outside this pipeline
        let _ = self
            .expr_typer
            .environment
            .local_values
            .remove(PIPE_VARIABLE);

        // Return any errors after clean-up
        result?;

        Ok(TypedExpr::Pipeline {
            expressions: self.expressions,
            location: self.location,
        })
    }

    fn infer_each_expression(
        &mut self,
        expressions: impl IntoIterator<Item = UntypedExpr>,
    ) -> Result<(), Error> {
        for (i, call) in expressions.into_iter().enumerate() {
            let call = match call {
                // left |> right(..args)
                UntypedExpr::Call {
                    fun,
                    arguments,
                    location,
                    ..
                } => {
                    let fun = self.expr_typer.infer(*fun)?;
                    match fun.type_().fn_arity() {
                        // Rewrite as right(left, ..args)
                        Some(arity) if arity == arguments.len() + 1 => {
                            self.infer_insert_pipe(fun, arguments, location)?
                        }

                        // Rewrite as right(..args)(left)
                        _ => self.infer_apply_to_call_pipe(fun, arguments, location)?,
                    }
                }

                // right(left)
                call => self.infer_apply_pipe(call)?,
            };
            if i + 2 == self.size {
                self.expressions.push(call);
            } else {
                self.push_assignment(call);
            }
        }
        Ok(())
    }

    /// Create a call argument that can be used to refer to the value on the
    /// left hand side of the pipe
    fn typed_left_hand_value_variable_call_argument(&self) -> CallArg<TypedExpr> {
        CallArg {
            label: None,
            location: self.argument_location,
            value: self.typed_left_hand_value_variable(),
        }
    }

    /// Create a call argument that can be used to refer to the value on the
    /// left hand side of the pipe
    fn untyped_left_hand_value_variable_call_argument(&self) -> CallArg<UntypedExpr> {
        CallArg {
            label: None,
            location: self.argument_location,
            value: self.untyped_left_hand_value_variable(),
        }
    }

    /// Create a variable that can be used to refer to the value on the left
    /// hand side of the pipe
    fn typed_left_hand_value_variable(&self) -> TypedExpr {
        TypedExpr::Var {
            location: self.argument_location,
            name: PIPE_VARIABLE.to_string(),
            constructor: ValueConstructor {
                public: true,
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
            name: PIPE_VARIABLE.to_string(),
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
        self.expr_typer.environment.insert_variable(
            PIPE_VARIABLE.to_string(),
            ValueConstructorVariant::LocalVariable { location },
            expression.type_(),
        );
        // Add the assignment to the AST
        let assignment = TypedExpr::Assignment {
            location,
            typ: expression.type_(),
            kind: AssignmentKind::Let,
            value: Box::new(expression),
            pattern: Pattern::Var {
                location,
                name: PIPE_VARIABLE.to_string(),
            },
        };
        self.expressions.push(assignment);
    }

    /// Attempt to infer a |> b(..c) as b(..c)(a)
    fn infer_apply_to_call_pipe(
        &mut self,
        function: TypedExpr,
        args: Vec<CallArg<UntypedExpr>>,
        location: SrcSpan,
    ) -> Result<TypedExpr, Error> {
        let (function, args, typ) = self
            .expr_typer
            .do_infer_call_with_known_fun(function, args, location)?;
        let function = TypedExpr::Call {
            location,
            typ,
            args,
            fun: Box::new(function),
        };
        let args = vec![self.untyped_left_hand_value_variable_call_argument()];
        // TODO: use `.with_unify_error_situation(UnifyErrorSituation::PipeTypeMismatch)`
        // This will require the typing of the arguments to be lifted up out of
        // the function below. If it is not we don't know if the error comes
        // from incorrect usage of the pipe or if it originates from the
        // argument expressions.
        let (function, args, typ) = self
            .expr_typer
            .do_infer_call_with_known_fun(function, args, location)?;
        Ok(TypedExpr::Call {
            location,
            typ,
            args,
            fun: Box::new(function),
        })
    }

    /// Attempt to infer a |> b(c) as b(a, c)
    fn infer_insert_pipe(
        &mut self,
        function: TypedExpr,
        mut arguments: Vec<CallArg<UntypedExpr>>,
        location: SrcSpan,
    ) -> Result<TypedExpr, Error> {
        arguments.insert(0, self.untyped_left_hand_value_variable_call_argument());
        // TODO: use `.with_unify_error_situation(UnifyErrorSituation::PipeTypeMismatch)`
        // This will require the typing of the arguments to be lifted up out of
        // the function below. If it is not we don't know if the error comes
        // from incorrect usage of the pipe or if it originates from the
        // argument expressions.
        let (fun, args, typ) = self
            .expr_typer
            .do_infer_call_with_known_fun(function, arguments, location)?;
        Ok(TypedExpr::Call {
            location,
            typ,
            args,
            fun: Box::new(fun),
        })
    }

    /// Attempt to infer a |> b as b(a)
    fn infer_apply_pipe(&mut self, function: UntypedExpr) -> Result<TypedExpr, Error> {
        let function = Box::new(self.expr_typer.infer(function)?);
        let return_type = self.expr_typer.new_unbound_var();
        // Ensure that the function accepts one argument of the correct type
        self.expr_typer
            .environment
            .unify(
                function.type_(),
                fn_(vec![self.argument_type.clone()], return_type.clone()),
            )
            .map_err(|e| {
                let is_pipe_mismatch = self.check_if_pipe_type_mismatch(&e);
                let error = convert_unify_error(e, function.location());
                if is_pipe_mismatch {
                    error.with_unify_error_situation(UnifyErrorSituation::PipeTypeMismatch)
                } else {
                    error
                }
            })?;

        Ok(TypedExpr::Call {
            location: function.location(),
            typ: return_type,
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
                match (a.get(0), b.get(0)) {
                    (Some(a), Some(b)) => self
                        .expr_typer
                        .environment
                        .unify(a.clone(), b.clone())
                        .is_err(),
                    _ => false,
                }
            }
            _ => false,
        }
    }
}
