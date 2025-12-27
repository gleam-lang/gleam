use self::expression::CallKind;

use super::*;
use crate::ast::{
    FunctionLiteralKind, ImplicitCallArgOrigin, PIPE_VARIABLE, PipelineAssignmentKind, Statement,
    TypedPipelineAssignment, UntypedExpr,
};
use vec1::Vec1;

#[derive(Debug)]
pub(crate) struct PipeTyper<'a, 'b, 'c> {
    size: usize,
    argument_type: Arc<Type>,
    argument_location: SrcSpan,
    location: SrcSpan,
    first_value: TypedPipelineAssignment,
    assignments: Vec<(TypedPipelineAssignment, PipelineAssignmentKind)>,
    expr_typer: &'a mut ExprTyper<'b, 'c>,
}

impl<'a, 'b, 'c> PipeTyper<'a, 'b, 'c> {
    fn new(expr_typer: &'a mut ExprTyper<'b, 'c>, size: usize, first: TypedExpr, end: u32) -> Self {
        let first_type = first.type_();
        let first_location = first.location();
        let first_value = new_pipeline_assignment(expr_typer, first);
        Self {
            size,
            expr_typer,
            argument_type: first_type,
            argument_location: first_location,
            location: SrcSpan {
                start: first_location.start,
                end,
            },
            assignments: Vec::with_capacity(size),
            first_value,
        }
    }

    pub fn infer(
        expr_typer: &'a mut ExprTyper<'b, 'c>,
        expressions: Vec1<UntypedExpr>,
    ) -> TypedExpr {
        // The scope is reset as pipelines are rewritten into a series of
        // assignments, and we don't want these variables to leak out of the
        // pipeline.
        let scope = expr_typer.environment.scope.clone();
        let result = PipeTyper::run(expr_typer, expressions);
        expr_typer.environment.scope = scope;
        result
    }

    fn run(expr_typer: &'a mut ExprTyper<'b, 'c>, expressions: Vec1<UntypedExpr>) -> TypedExpr {
        let size = expressions.len();
        let end = expressions.last().location().end;
        let mut expressions = expressions.into_iter();
        let first = expressions.next().expect("Empty pipeline in typer");
        let first = expr_typer.infer(first);

        Self::new(expr_typer, size, first, end).infer_expressions(expressions)
    }

    fn infer_expressions(
        mut self,
        expressions: impl IntoIterator<Item = UntypedExpr>,
    ) -> TypedExpr {
        let (finally, finally_kind) = self.infer_each_expression(expressions);
        let assignments = std::mem::take(&mut self.assignments);
        TypedExpr::Pipeline {
            location: self.location,
            first_value: self.first_value,
            assignments,
            finally: Box::new(finally),
            finally_kind,
        }
    }

    fn infer_each_expression(
        &mut self,
        expressions: impl IntoIterator<Item = UntypedExpr>,
    ) -> (TypedExpr, PipelineAssignmentKind) {
        let mut finally = None;

        for (i, call) in expressions.into_iter().enumerate() {
            if self.expr_typer.previous_panics {
                self.expr_typer
                    .warn_for_unreachable_code(call.location(), PanicPosition::PreviousExpression);
            }

            self.warn_if_call_first_argument_is_hole(&call);

            let (kind, call) = match call {
                func @ UntypedExpr::Fn { location, kind, .. } => {
                    let (func, arguments, return_type) = self.expr_typer.do_infer_call(
                        func,
                        vec![self.untyped_left_hand_value_variable_call_argument()],
                        location,
                        CallKind::Function,
                    );

                    self.expr_typer.purity =
                        self.expr_typer.purity.merge(func.called_function_purity());

                    let kind = match kind {
                        FunctionLiteralKind::Capture { hole } => {
                            PipelineAssignmentKind::Hole { hole }
                        }
                        FunctionLiteralKind::Anonymous { .. } | FunctionLiteralKind::Use { .. } => {
                            PipelineAssignmentKind::FunctionCall
                        }
                    };

                    (
                        kind,
                        TypedExpr::Call {
                            location,
                            arguments,
                            type_: return_type,
                            fun: Box::new(func),
                            arguments_start: None,
                        },
                    )
                }

                // left |> right(..args)
                //         ^^^^^ This is `fun`
                UntypedExpr::Call {
                    fun,
                    arguments,
                    location,
                    ..
                } => {
                    let fun = self.expr_typer.infer(*fun);

                    match fun.type_().fn_types() {
                        // Rewrite as right(..args)(left)
                        Some((fn_arguments, _)) if fn_arguments.len() == arguments.len() => {
                            // We are calling the return value of another function.
                            // Without lifting purity tracking into the type system,
                            // we have no idea whether it's pure or not!
                            self.expr_typer.purity = self.expr_typer.purity.merge(Purity::Unknown);
                            (
                                PipelineAssignmentKind::FunctionCall,
                                self.infer_apply_to_call_pipe(fun, arguments, location),
                            )
                        }

                        // Rewrite as right(left, ..args)
                        _ => {
                            self.expr_typer.purity =
                                self.expr_typer.purity.merge(fun.called_function_purity());
                            (
                                PipelineAssignmentKind::FirstArgument {
                                    second_argument: arguments.first().map(|arg| arg.location),
                                },
                                self.infer_insert_pipe(fun, arguments, location),
                            )
                        }
                    }
                }

                UntypedExpr::Echo {
                    location,
                    keyword_end: _,
                    expression: None,
                    message,
                } => {
                    self.expr_typer.environment.echo_found = true;
                    self.expr_typer.purity = Purity::Impure;
                    // An echo that is not followed by an expression that is
                    // used as a pipeline's step is just like the identity
                    // function.
                    // So it gets the type of the value coming from the previous
                    // step of the pipeline.
                    (
                        PipelineAssignmentKind::Echo,
                        TypedExpr::Echo {
                            location,
                            expression: None,
                            type_: self.argument_type.clone(),
                            message: message.map(|message| {
                                Box::new(self.expr_typer.infer_and_unify(*message, string()))
                            }),
                        },
                    )
                }

                // right(left)
                UntypedExpr::Int { .. }
                | UntypedExpr::Float { .. }
                | UntypedExpr::String { .. }
                | UntypedExpr::Block { .. }
                | UntypedExpr::Var { .. }
                | UntypedExpr::List { .. }
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
                | UntypedExpr::NegateInt { .. } => (
                    PipelineAssignmentKind::FunctionCall,
                    self.infer_apply_pipe(call),
                ),
            };

            if i + 2 == self.size {
                finally = Some((call, kind));
            } else {
                self.push_assignment(call, kind);
            }
        }

        finally.expect("Empty pipeline in typer")
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
            constructor: ValueConstructor::local_variable(
                self.argument_location,
                VariableOrigin::generated(),
                self.argument_type.clone(),
            ),
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
    fn push_assignment(&mut self, expression: TypedExpr, kind: PipelineAssignmentKind) {
        self.argument_type = expression.type_();
        self.argument_location = expression.location();
        let assignment = new_pipeline_assignment(self.expr_typer, expression);
        self.assignments.push((assignment, kind));
    }

    /// Attempt to infer a |> b(..c) as b(..c)(a)
    fn infer_apply_to_call_pipe(
        &mut self,
        function: TypedExpr,
        arguments: Vec<CallArg<UntypedExpr>>,
        location: SrcSpan,
    ) -> TypedExpr {
        let (function, arguments, type_) = self.expr_typer.do_infer_call_with_known_fun(
            function,
            arguments,
            location,
            CallKind::Function,
        );
        let function = TypedExpr::Call {
            location,
            type_,
            arguments,
            fun: Box::new(function),
            arguments_start: None,
        };
        let arguments = vec![self.untyped_left_hand_value_variable_call_argument()];
        // TODO: use `.with_unify_error_situation(UnifyErrorSituation::PipeTypeMismatch)`
        // This will require the typing of the arguments to be lifted up out of
        // the function below. If it is not we don't know if the error comes
        // from incorrect usage of the pipe or if it originates from the
        // argument expressions.
        let (function, arguments, type_) = self.expr_typer.do_infer_call_with_known_fun(
            function,
            arguments,
            location,
            CallKind::Function,
        );
        TypedExpr::Call {
            location,
            type_,
            arguments,
            fun: Box::new(function),
            arguments_start: None,
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
        let (fun, arguments, type_) = self.expr_typer.do_infer_call_with_known_fun(
            function,
            arguments,
            location,
            CallKind::Function,
        );
        TypedExpr::Call {
            location,
            type_,
            arguments,
            fun: Box::new(fun),
            arguments_start: None,
        }
    }

    /// Attempt to infer a |> b as b(a)
    /// b is the `function` argument.
    fn infer_apply_pipe(&mut self, function: UntypedExpr) -> TypedExpr {
        let function_location = function.location();
        let function = Box::new(self.expr_typer.infer(function));

        self.expr_typer.purity = self
            .expr_typer
            .purity
            .merge(function.called_function_purity());

        let return_type = self.expr_typer.new_unbound_var();
        // Ensure that the function accepts one argument of the correct type
        let unification_result = unify(
            function.type_(),
            fn_(vec![self.argument_type.clone()], return_type.clone()),
        );
        match unification_result {
            Ok(_) => (),
            Err(error) => {
                let error = if self.check_if_pipe_type_mismatch(&error) {
                    convert_unify_error(error, function.location())
                        .with_unify_error_situation(UnifyErrorSituation::PipeTypeMismatch)
                } else {
                    convert_unify_error(flip_unify_error(error), function.location())
                };
                self.expr_typer.problems.error(error);
            }
        };

        TypedExpr::Call {
            location: function_location,
            type_: return_type,
            fun: function,
            arguments: vec![self.typed_left_hand_value_variable_call_argument()],
            arguments_start: None,
        }
    }

    fn check_if_pipe_type_mismatch(&mut self, error: &UnifyError) -> bool {
        let types = match error {
            UnifyError::CouldNotUnify {
                expected, given, ..
            } => (expected.as_ref(), given.as_ref()),
            UnifyError::ExtraVarInAlternativePattern { .. }
            | UnifyError::MissingVarInAlternativePattern { .. }
            | UnifyError::DuplicateVarInPattern { .. }
            | UnifyError::RecursiveType => return false,
        };

        match types {
            (Type::Fn { arguments: a, .. }, Type::Fn { arguments: b, .. })
                if a.len() == b.len() =>
            {
                match (a.first(), b.first()) {
                    (Some(a), Some(b)) => unify(a.clone(), b.clone()).is_err(),
                    _ => false,
                }
            }
            _ => false,
        }
    }

    fn warn_if_call_first_argument_is_hole(&mut self, call: &UntypedExpr) {
        if let UntypedExpr::Fn { kind, body, .. } = &call
            && kind.is_capture()
            && let Statement::Expression(UntypedExpr::Call { arguments, .. }) = body.first()
        {
            match arguments.as_slice() {
                // If the first argument is labelled, we don't warn the user
                // as they might be intentionally adding it to provide more
                // information about exactly which argument is being piped into.
                [first] | [first, ..] if first.is_capture_hole() && first.label.is_none() => self
                    .expr_typer
                    .problems
                    .warning(Warning::RedundantPipeFunctionCapture {
                        location: first.location,
                    }),
                _ => (),
            }
        }
    }
}

fn new_pipeline_assignment(
    expr_typer: &mut ExprTyper<'_, '_>,
    expression: TypedExpr,
) -> TypedPipelineAssignment {
    let location = expression.location();
    // Insert the variable for use in type checking the rest of the pipeline
    expr_typer.environment.insert_local_variable(
        PIPE_VARIABLE.into(),
        location,
        VariableOrigin::generated(),
        expression.type_(),
    );
    TypedPipelineAssignment {
        location,
        name: PIPE_VARIABLE.into(),
        value: Box::new(expression),
    }
}
