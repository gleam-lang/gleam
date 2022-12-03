use itertools::Itertools;
use vec1::Vec1;

use super::{pipe::PipeTyper, *};
use crate::{
    ast::{
        Arg, AssignName, AssignmentKind, BinOp, BitStringSegment, BitStringSegmentOption, CallArg,
        Clause, ClauseGuard, Constant, HasLocation, RecordUpdateSpread, SrcSpan, TodoKind, TypeAst,
        TypedArg, TypedClause, TypedClauseGuard, TypedConstant, TypedExpr, TypedMultiPattern,
        UntypedArg, UntypedClause, UntypedClauseGuard, UntypedConstant,
        UntypedConstantBitStringSegment, UntypedExpr, UntypedExprBitStringSegment,
        UntypedMultiPattern, UntypedPattern, Use,
    },
    filled_result::{FilledResult, FilledResultContext},
};

use im::hashmap;

#[derive(Debug)]
pub(crate) struct ExprTyper<'a, 'b> {
    pub(crate) environment: &'a mut Environment<'b>,

    // Type hydrator for creating types from annotations
    pub(crate) hydrator: Hydrator,

    // We keep track of whether any ungeneralised functions have been used
    // to determine whether it is safe to generalise this expression after
    // it has been inferred.
    pub(crate) ungeneralised_function_used: bool,
}

impl<'a, 'b> ExprTyper<'a, 'b> {
    pub fn new(environment: &'a mut Environment<'b>) -> Self {
        let mut hydrator = Hydrator::new();
        hydrator.permit_holes(true);
        Self {
            hydrator,
            environment,
            ungeneralised_function_used: false,
        }
    }

    pub fn in_new_scope<T>(&mut self, process_scope: impl FnOnce(&mut Self) -> T) -> T {
        // Create new scope
        let environment_reset_data = self.environment.open_new_scope();
        let hydrator_reset_data = self.hydrator.open_new_scope();

        // Process the scope
        let result = process_scope(self);

        // Close scope, discarding any scope local state
        self.environment.close_scope(environment_reset_data);
        self.hydrator.close_scope(hydrator_reset_data);
        result
    }

    pub fn type_from_ast(&mut self, ast: &TypeAst) -> FilledResult<Arc<Type>, Error> {
        self.hydrator.type_from_ast(ast, self.environment)
    }

    fn instantiate(&mut self, t: Arc<Type>, ids: &mut im::HashMap<u64, Arc<Type>>) -> Arc<Type> {
        self.environment.instantiate(t, ids, &self.hydrator)
    }

    pub fn new_unbound_var(&mut self) -> Arc<Type> {
        self.environment.new_unbound_var()
    }

    /// Crawl the AST, annotating each node with the inferred type or
    /// returning an error.
    ///
    pub fn infer(&mut self, expr: UntypedExpr) -> FilledResult<TypedExpr, Error> {
        match expr {
            UntypedExpr::Todo {
                location,
                label,
                kind,
                ..
            } => FilledResult::ok(self.infer_todo(location, kind, label)),

            UntypedExpr::Var { location, name, .. } => self.infer_var(name, location),

            UntypedExpr::Int {
                location, value, ..
            } => FilledResult::ok(self.infer_int(value, location)),

            UntypedExpr::Sequence {
                expressions,
                location,
            } => self.infer_seq(location, expressions),

            UntypedExpr::Tuple {
                location, elems, ..
            } => self.infer_tuple(elems, location),

            UntypedExpr::Float {
                location, value, ..
            } => FilledResult::ok(self.infer_float(value, location)),

            UntypedExpr::String {
                location, value, ..
            } => FilledResult::ok(self.infer_string(value, location)),

            UntypedExpr::PipeLine { expressions } => self.infer_pipeline(expressions),

            UntypedExpr::Fn {
                location,
                is_capture,
                arguments: args,
                body,
                return_annotation,
                ..
            } => self.infer_fn(args, &[], *body, is_capture, return_annotation, location),

            UntypedExpr::Assignment {
                location,
                pattern,
                value,
                kind,
                annotation,
                ..
            } => self.infer_assignment(pattern, *value, kind, &annotation, location),

            UntypedExpr::Try {
                location,
                pattern,
                value,
                then,
                annotation,
                ..
            } => self.infer_try(pattern, *value, *then, &annotation, location),

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
            } => self.infer_call(*fun, args, location),

            UntypedExpr::BinOp {
                location,
                name,
                left,
                right,
                ..
            } => self.infer_binop(name, *left, *right, location),

            UntypedExpr::FieldAccess {
                location,
                label,
                container,
                ..
            } => self.infer_field_access(*container, label, location, FieldAccessUsage::Other),

            UntypedExpr::TupleIndex {
                location,
                index,
                tuple,
                ..
            } => self.infer_tuple_index(*tuple, index, location),

            UntypedExpr::BitString { location, segments } => {
                self.infer_bit_string(segments, location)
            }

            UntypedExpr::RecordUpdate {
                location,
                constructor,
                spread,
                arguments: args,
            } => self.infer_record_update(*constructor, spread, args, location),

            UntypedExpr::Negate { location, value } => self.infer_negate(location, value),

            UntypedExpr::Use(use_) => {
                let location = use_.location;
                self.infer_use(use_, location, vec![])
            }
        }
    }

    fn infer_pipeline(&mut self, expressions: Vec1<UntypedExpr>) -> FilledResult<TypedExpr, Error> {
        PipeTyper::infer(self, expressions)
    }

    fn infer_todo(
        &mut self,
        location: SrcSpan,
        kind: TodoKind,
        label: Option<String>,
    ) -> TypedExpr {
        let typ = self.new_unbound_var();
        self.environment.warnings.push(Warning::Todo {
            kind,
            location,
            typ: typ.clone(),
        });

        TypedExpr::Todo {
            location,
            label,
            typ,
        }
    }

    fn infer_string(&mut self, value: String, location: SrcSpan) -> TypedExpr {
        TypedExpr::String {
            location,
            value,
            typ: string(),
        }
    }

    fn infer_int(&mut self, value: String, location: SrcSpan) -> TypedExpr {
        TypedExpr::Int {
            location,
            value,
            typ: int(),
        }
    }

    fn infer_float(&mut self, value: String, location: SrcSpan) -> TypedExpr {
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
            self.environment.warnings.push(Warning::UnusedLiteral {
                location: discarded.location(),
            });
        }
        if discarded.type_().is_result() && !discarded.is_assignment() {
            self.environment
                .warnings
                .push(Warning::ImplicitlyDiscardedResult {
                    location: discarded.location(),
                });
        }
    }

    fn infer_seq(
        &mut self,
        location: SrcSpan,
        untyped: Vec<UntypedExpr>,
    ) -> FilledResult<TypedExpr, Error> {
        let count = untyped.len();
        let untyped = untyped.into_iter();
        self.infer_iter_seq(location, count, untyped)
    }

    fn infer_iter_seq<Exprs: Iterator<Item = UntypedExpr>>(
        &mut self,
        location: SrcSpan,
        count: usize,
        mut untyped: Exprs,
    ) -> FilledResult<TypedExpr, Error> {
        let mut ctx = FilledResultContext::new();
        let mut i = 0;
        let mut expressions = Vec::with_capacity(count);

        while let Some(expression) = untyped.next() {
            i += 1;

            // Special case for `use` expressions, which need the remaining
            // expressions in the sequence to be passed to them during as an
            // implicit anonymous function.
            if let UntypedExpr::Use(use_) = expression {
                let expression =
                    ctx.slurp_filled(self.infer_use(use_, location, untyped.collect()));
                expressions.push(expression);
                break; // Inferring the use has consumed the rest of the exprs
            }

            let expression = ctx.slurp_filled(self.infer(expression));
            // This isn't the final expression in the sequence, so call the
            // `expression_discarded` function to see if anything is being
            // discarded that we think shouldn't be.
            if i < count {
                self.expression_discarded(&expression);
            }
            expressions.push(expression);
        }

        ctx.finish(TypedExpr::Sequence {
            location,
            expressions,
        })
    }

    fn infer_use(
        &mut self,
        use_: Use,
        sequence_location: SrcSpan,
        mut following_expressions: Vec<UntypedExpr>,
    ) -> FilledResult<TypedExpr, Error> {
        let mut call = get_use_expression_call(*use_.call);
        let callback_arguments = use_assignments_to_function_arguments(use_.assignments);

        // TODO: Upgrade this to an error when we have partial type checking.
        // If there are no following expressions then this expressions is
        // incomplete. In this case we insert a `todo` so that the user can type
        // check this code even if it would fail when run.
        if following_expressions.is_empty() {
            let todo = UntypedExpr::Todo {
                location: use_.location,
                label: None,
                kind: TodoKind::IncompleteUse,
            };
            following_expressions.push(todo);
        }

        // Collect the following expressions into a function to be passed as a
        // callback to the use's call function.
        let first = following_expressions
            .get(0)
            .expect("default todo set above");
        let callback = UntypedExpr::Fn {
            arguments: callback_arguments,
            location: SrcSpan::new(first.location().start, sequence_location.end),
            return_annotation: None,
            is_capture: false,
            body: Box::new(UntypedExpr::Sequence {
                location: sequence_location,
                expressions: following_expressions,
            }),
        };

        // Add this new callback function to the arguments to function call
        call.arguments.push(CallArg {
            label: None,
            location: callback.location(),
            value: callback,
            // This argument is implicitly given by Gleam's use syntax so we
            // mark it as such.
            implicit: true,
        });

        self.infer(UntypedExpr::Call {
            location: call.location,
            fun: call.function,
            arguments: call.arguments,
        })
    }

    fn infer_negate(
        &mut self,
        location: SrcSpan,
        value: Box<UntypedExpr>,
    ) -> FilledResult<TypedExpr, Error> {
        let mut ctx = FilledResultContext::new();
        let value = ctx.slurp_filled(self.infer(*value));

        ctx.just_slurp_result(
            unify(bool(), value.type_()).map_err(|e| convert_unify_error(e, value.location())),
        );

        ctx.finish(TypedExpr::Negate {
            location,
            value: Box::new(value),
        })
    }

    fn infer_fn(
        &mut self,
        args: Vec<UntypedArg>,
        expected_args: &[Arc<Type>],
        body: UntypedExpr,
        is_capture: bool,
        return_annotation: Option<TypeAst>,
        location: SrcSpan,
    ) -> FilledResult<TypedExpr, Error> {
        self.do_infer_fn(args, expected_args, body, &return_annotation)
            .map(|(args, body)| {
                let arg_types = args.iter().map(|a| a.type_.clone()).collect();
                let typ = fn_(arg_types, body.type_());
                TypedExpr::Fn {
                    location,
                    typ,
                    is_capture,
                    args,
                    body: Box::new(body),
                    return_annotation,
                }
            })
    }

    fn infer_arg(
        &mut self,
        arg: UntypedArg,
        expected: Option<Arc<Type>>,
    ) -> FilledResult<TypedArg, Error> {
        let mut ctx = FilledResultContext::new();
        let Arg {
            names,
            annotation,
            location,
            ..
        } = arg;
        let typ = annotation
            .as_ref()
            .map(|t| ctx.slurp_filled(self.type_from_ast(t)))
            .unwrap_or_else(|| self.environment.new_unbound_var());

        // If we know the expected type of the argument from its contextual
        // usage then unify the newly constructed type with the expected type.
        // We do this here because then there is more type information for the
        // function being type checked, resulting in better type errors and the
        // record field access syntax working.
        if let Some(expected) = expected {
            let _ = ctx.slurp_result(
                unify(expected, typ.clone()).map_err(|e| convert_unify_error(e, location)),
            );
        }

        ctx.finish(Arg {
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
    ) -> FilledResult<TypedExpr, Error> {
        self.do_infer_call(fun, args, location)
            .map(|(fun, args, typ)| TypedExpr::Call {
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
    ) -> FilledResult<TypedExpr, Error> {
        let mut ctx = FilledResultContext::new();
        let typ = self.new_unbound_var();
        // Type check each elements
        let elements = elements
            .into_iter()
            .map(|element| {
                let element = ctx.slurp_filled(self.infer(element));
                // Ensure they all have the same type
                let _ = ctx.slurp_result(
                    unify(typ.clone(), element.type_())
                        .map_err(|e| convert_unify_error(e, location)),
                );
                element
            })
            .collect();
        // Type check the ..tail, if there is one
        let typ = list(typ);
        let tail = tail.map(|tail| {
            let tail = ctx.slurp_filled(self.infer(*tail));
            // Ensure the tail has the same type as the preceeding elements
            let _ = ctx.slurp_result(
                unify(typ.clone(), tail.type_()).map_err(|e| convert_unify_error(e, location)),
            );
            Box::new(tail)
        });
        ctx.finish(TypedExpr::List {
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
    ) -> FilledResult<TypedExpr, Error> {
        let mut ctx = FilledResultContext::new();
        let elems: Vec<_> = elems
            .into_iter()
            .map(|e| ctx.slurp_filled(self.infer(e)))
            .collect();
        let typ = tuple(elems.iter().map(HasType::type_).collect());
        ctx.finish(TypedExpr::Tuple {
            location,
            elems,
            typ,
        })
    }
    fn infer_var(&mut self, name: String, location: SrcSpan) -> FilledResult<TypedExpr, Error> {
        let mut ctx = FilledResultContext::new();
        let constructor = ctx
            .slurp_result(self.infer_value_constructor(&None, &name, &location))
            .unwrap_or_else(|| ValueConstructor {
                public: true,
                variant: ValueConstructorVariant::LocalVariable { location },
                type_: self.environment.new_unbound_var(),
            });

        ctx.finish(TypedExpr::Var {
            constructor,
            location,
            name,
        })
    }

    fn infer_field_access(
        &mut self,
        container: UntypedExpr,
        label: String,
        access_location: SrcSpan,
        usage: FieldAccessUsage,
    ) -> FilledResult<TypedExpr, Error> {
        // Attempt to infer the container as a record access. If that fails, we may be shadowing the name
        // of an imported module, so attempt to infer the container as a module access.
        // TODO: Remove this cloning

        let (mut ctx, access) = self
            .infer_record_access(container.clone(), label.clone(), access_location, usage)
            .into_context();

        let final_access = 'try_module_access: {
            if ctx.has_errors() {
                if let UntypedExpr::Var { name, location, .. } = container {
                    match self.infer_module_access(&name, label, &location, access_location) {
                        Err(e) => {
                            // If the name is in the environment, use the original error from
                            // inferring the record access, so that we can suggest possible
                            // misspellings of field names
                            if !self.environment.scope.contains_key(&name) {
                                ctx.clear();
                                ctx.register_error(e);
                            }
                        }
                        Ok(v) => {
                            ctx.clear();
                            break 'try_module_access v;
                        }
                    }
                }
            }
            access
        };

        ctx.finish(final_access)
    }

    fn infer_tuple_index(
        &mut self,
        tuple: UntypedExpr,
        index: u64,
        location: SrcSpan,
    ) -> FilledResult<TypedExpr, Error> {
        let mut ctx = FilledResultContext::new();
        let tuple = ctx.slurp_filled(self.infer(tuple));
        match collapse_links(tuple.type_()).as_ref() {
            Type::Tuple { elems } => {
                let typ = ctx
                    .slurp_result(elems.get(index as usize).cloned().ok_or_else(|| {
                        Error::OutOfBoundsTupleIndex {
                            location: SrcSpan {
                                start: tuple.location().end,
                                end: location.end,
                            },
                            index,
                            size: elems.len(),
                        }
                    }))
                    .unwrap_or_else(|| self.environment.new_unbound_var());
                ctx.finish(TypedExpr::TupleIndex {
                    location,
                    index,
                    tuple: Box::new(tuple),
                    typ,
                })
            }

            typ if typ.is_unbound() => {
                ctx.register_error(Error::NotATupleUnbound {
                    location: tuple.location(),
                });
                ctx.finish(TypedExpr::TupleIndex {
                    location,
                    index,
                    tuple: Box::new(tuple),
                    typ: self.environment.new_unbound_var(),
                })
            }

            _ => {
                ctx.register_error(Error::NotATuple {
                    location: tuple.location(),
                    given: tuple.type_(),
                });
                ctx.finish(TypedExpr::TupleIndex {
                    location,
                    index,
                    tuple: Box::new(tuple),
                    typ: self.environment.new_unbound_var(),
                })
            }
        }
    }

    fn infer_bit_string(
        &mut self,
        segments: Vec<UntypedExprBitStringSegment>,
        location: SrcSpan,
    ) -> FilledResult<TypedExpr, Error> {
        let mut ctx = FilledResultContext::new();
        let segments = ctx.slurp_filled_collect(segments.into_iter().map(|s| {
            self.infer_bit_segment(*s.value, s.options, s.location, |env, expr| env.infer(expr))
        }));

        ctx.finish(TypedExpr::BitString {
            location,
            segments,
            typ: bit_string(),
        })
    }

    fn infer_constant_bit_string(
        &mut self,
        segments: Vec<UntypedConstantBitStringSegment>,
        location: SrcSpan,
    ) -> FilledResult<TypedConstant, Error> {
        let mut ctx = FilledResultContext::new();
        let segments = ctx.slurp_filled_collect(segments.into_iter().map(|s| {
            self.infer_bit_segment(*s.value, s.options, s.location, |env, expr| {
                env.infer_const(&None, expr)
            })
        }));

        ctx.finish(Constant::BitString { location, segments })
    }

    fn infer_bit_segment<UntypedValue, TypedValue, InferFn>(
        &mut self,
        value: UntypedValue,
        options: Vec<BitStringSegmentOption<UntypedValue>>,
        location: SrcSpan,
        mut infer: InferFn,
    ) -> FilledResult<BitStringSegment<TypedValue, Arc<Type>>, Error>
    where
        InferFn: FnMut(&mut Self, UntypedValue) -> FilledResult<TypedValue, Error>,
        TypedValue: HasType + HasLocation + Clone + bit_string::GetLitValue,
    {
        let mut ctx = FilledResultContext::new();
        let value = ctx.slurp_filled(infer(self, value));

        let infer_option = |segment_option: BitStringSegmentOption<UntypedValue>| {
            infer_bit_string_segment_option(segment_option, |value, typ| {
                let typed_value = ctx.slurp_filled(infer(self, value));
                ctx.just_slurp_result(
                    unify(typ, typed_value.type_())
                        .map_err(|e| convert_unify_error(e, typed_value.location())),
                );
                FilledResult::ok(typed_value)
            })
        };

        // SAFE: we can unwrap the FilledResult's because the closure inside
        // `infer_bit_string_segment_option` always returns `FilledResult::ok` and the called
        // function has `ok`s in the rest of the branches where it doesn't call our callback.
        let options: Vec<_> = options
            .into_iter()
            .map(infer_option)
            .map(FilledResult::no_errors)
            .collect();

        let typ =
            ctx.slurp_result(crate::bit_string::type_options_for_value(&options).map_err(
                |error| Error::BitStringSegmentError {
                    error: error.error,
                    location: error.location,
                },
            ))
            .unwrap_or_else(|| self.environment.new_unbound_var());

        ctx.just_slurp_result(
            unify(typ.clone(), value.type_()).map_err(|e| convert_unify_error(e, value.location())),
        );

        ctx.finish(BitStringSegment {
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
    ) -> FilledResult<TypedExpr, Error> {
        let mut ctx = FilledResultContext::new();
        let (input_type, output_type) = match &name {
            BinOp::Eq | BinOp::NotEq => {
                let left = ctx.slurp_filled(self.infer(left));
                let right = ctx.slurp_filled(self.infer(right));
                ctx.just_slurp_result(
                    unify(left.type_(), right.type_())
                        .map_err(|e| convert_unify_error(e, right.location())),
                );

                return ctx.finish(TypedExpr::BinOp {
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

        let left = ctx.slurp_filled(self.infer(left));
        ctx.just_slurp_result(unify(input_type.clone(), left.type_()).map_err(|e| {
            e.operator_situation(name)
                .into_error(left.type_defining_location())
        }));
        let right = ctx.slurp_filled(self.infer(right));
        ctx.just_slurp_result(unify(input_type, right.type_()).map_err(|e| {
            e.operator_situation(name)
                .into_error(right.type_defining_location())
        }));

        ctx.finish(TypedExpr::BinOp {
            location,
            name,
            typ: output_type,
            left: Box::new(left),
            right: Box::new(right),
        })
    }

    fn infer_assignment(
        &mut self,
        pattern: UntypedPattern,
        value: UntypedExpr,
        kind: AssignmentKind,
        annotation: &Option<TypeAst>,
        location: SrcSpan,
    ) -> FilledResult<TypedExpr, Error> {
        let (mut ctx, value) = self
            .in_new_scope(|value_typer| value_typer.infer(value))
            .into_context();
        let value_typ = value.type_();

        // Ensure the pattern matches the type of the value
        let pattern = ctx.slurp_filled(
            pattern::PatternTyper::new(self.environment, &self.hydrator)
                .unify(pattern, value_typ.clone()),
        );

        // Check that any type annotation is accurate.
        if let Some(ann) = annotation {
            let ann_typ = ctx.slurp_filled(
                self.type_from_ast(ann)
                    .map(|t| self.instantiate(t, &mut hashmap![])),
            );
            ctx.just_slurp_result(
                unify(ann_typ, value_typ.clone())
                    .map_err(|e| convert_unify_error(e, value.type_defining_location())),
            );
        }

        // We currently only do only limited exhaustiveness checking of custom types
        // at the top level of patterns.
        // Do not perform exhaustiveness checking if user explicitly used `assert`.
        if kind != AssignmentKind::Assert {
            if let Err(unmatched) = self
                .environment
                .check_exhaustiveness(vec![pattern.clone()], collapse_links(value_typ.clone()))
            {
                ctx.register_error(Error::NotExhaustivePatternMatch {
                    location,
                    unmatched,
                });
            }
        }

        ctx.finish(TypedExpr::Assignment {
            location,
            typ: value_typ,
            kind,
            pattern,
            value: Box::new(value),
        })
    }

    fn infer_try(
        &mut self,
        pattern: UntypedPattern,
        value: UntypedExpr,
        then: UntypedExpr,
        annotation: &Option<TypeAst>,
        location: SrcSpan,
    ) -> FilledResult<TypedExpr, Error> {
        let mut ctx = FilledResultContext::new();
        let value = ctx.slurp_filled(self.in_new_scope(|value_typer| value_typer.infer(value)));

        let value_type = self.new_unbound_var();
        let try_error_type = self.new_unbound_var();

        // Ensure that the value is a result
        {
            let v = value_type.clone();
            let e = try_error_type.clone();
            ctx.just_slurp_result(
                unify(result(v, e), value.type_())
                    .map_err(|e| convert_unify_error(e, value.type_defining_location())),
            );
        };

        // Ensure the pattern matches the type of the value
        let pattern = ctx.slurp_filled(
            pattern::PatternTyper::new(self.environment, &self.hydrator)
                .unify(pattern, value_type.clone()),
        );

        // Check the type of the following code
        let then = ctx.slurp_filled(self.infer(then));
        let typ = then.type_();

        // Ensure that a Result with the right error type is returned for `try`
        {
            let t = self.new_unbound_var();
            ctx.just_slurp_result(unify(result(t, try_error_type), typ.clone()).map_err(|e| {
                e.inconsistent_try(typ.is_result())
                    .into_error(then.type_defining_location())
            }));
        }

        // Check that any type annotation is accurate.
        if let Some(ann) = annotation {
            let ann_typ = ctx.slurp_filled(
                self.type_from_ast(ann)
                    .map(|t| self.instantiate(t, &mut hashmap![])),
            );
            ctx.just_slurp_result(
                unify(ann_typ, value_type)
                    .map_err(|e| convert_unify_error(e, value.type_defining_location())),
            );
        }

        ctx.finish(TypedExpr::Try {
            location,
            typ,
            pattern,
            value: Box::new(value),
            then: Box::new(then),
        })
    }

    fn infer_case(
        &mut self,
        subjects: Vec<UntypedExpr>,
        clauses: Vec<UntypedClause>,
        location: SrcSpan,
    ) -> FilledResult<TypedExpr, Error> {
        let mut ctx = FilledResultContext::new();
        let subjects_count = subjects.len();

        let mut typed_subjects = Vec::with_capacity(subjects_count);
        let mut subject_types = Vec::with_capacity(subjects_count);
        let mut typed_clauses = Vec::with_capacity(clauses.len());

        let return_type = self.new_unbound_var();

        for subject in subjects {
            let subject =
                self.in_new_scope(|subject_typer| ctx.slurp_filled(subject_typer.infer(subject)));

            subject_types.push(subject.type_());
            typed_subjects.push(subject);
        }

        for clause in clauses {
            let typed_clause = ctx.slurp_filled(self.infer_clause(clause, &subject_types));
            ctx.just_slurp_result(
                unify(return_type.clone(), typed_clause.then.type_())
                    .map_err(|e| e.case_clause_mismatch().into_error(typed_clause.location())),
            );
            typed_clauses.push(typed_clause);
        }

        if let Err(unmatched) =
            self.check_case_exhaustiveness(subjects_count, &subject_types, &typed_clauses)
        {
            ctx.register_error(Error::NotExhaustivePatternMatch {
                location,
                unmatched,
            });
        }

        ctx.finish(TypedExpr::Case {
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
    ) -> FilledResult<TypedClause, Error> {
        let mut ctx = FilledResultContext::new();
        let Clause {
            pattern,
            alternative_patterns,
            guard,
            then,
            location,
        } = clause;

        let (guard, then, typed_pattern, typed_alternatives) = self.in_new_scope(|clause_typer| {
            // Check the types
            let (typed_pattern, typed_alternatives) =
                ctx.slurp_filled(clause_typer.infer_clause_pattern(
                    pattern,
                    alternative_patterns,
                    subjects,
                    &location,
                ));
            let guard = ctx.slurp_filled(clause_typer.infer_optional_clause_guard(guard));
            let then = ctx.slurp_filled(clause_typer.infer(then));

            (guard, then, typed_pattern, typed_alternatives)
        });

        ctx.finish(Clause {
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
    ) -> FilledResult<(TypedMultiPattern, Vec<TypedMultiPattern>), Error> {
        let mut pattern_typer = pattern::PatternTyper::new(self.environment, &self.hydrator);
        let (mut ctx, typed_pattern) = pattern_typer
            .infer_multi_pattern(pattern, subjects, location)
            .into_context();

        // Each case clause has one or more patterns that may match the
        // subject in order for the clause to be selected, so we must type
        // check every pattern.
        let typed_alternatives = ctx.slurp_filled_collect(
            alternatives
                .into_iter()
                .map(|alt| pattern_typer.infer_alternative_multi_pattern(alt, subjects, location)),
        );

        ctx.finish((typed_pattern, typed_alternatives))
    }

    fn infer_optional_clause_guard(
        &mut self,
        guard: Option<UntypedClauseGuard>,
    ) -> FilledResult<Option<TypedClauseGuard>, Error> {
        let mut ctx = FilledResultContext::new();
        match guard {
            // If there is no guard we do nothing
            None => ctx.finish(None),

            // If there is a guard we assert that it is of type Bool
            Some(guard) => {
                let guard = ctx.slurp_filled(self.infer_clause_guard(guard));
                ctx.just_slurp_result(
                    unify(bool(), guard.type_())
                        .map_err(|e| convert_unify_error(e, guard.location())),
                );
                ctx.finish(Some(guard))
            }
        }
    }

    fn infer_clause_guard(
        &mut self,
        guard: UntypedClauseGuard,
    ) -> FilledResult<TypedClauseGuard, Error> {
        match guard {
            ClauseGuard::Var { location, name, .. } => {
                let mut ctx = FilledResultContext::new();

                let (typ, variant) = ctx
                    .slurp_result(
                        self.infer_value_constructor(&None, &name, &location)
                            .map(|x| (x.type_, x.variant)),
                    )
                    .unwrap_or_else(|| {
                        let typ = self.environment.new_unbound_var();
                        (
                            typ.clone(),
                            ValueConstructorVariant::ModuleConstant {
                                literal: Constant::Var {
                                    location,
                                    name: name.clone(),
                                    module: None,
                                    constructor: None,
                                    typ,
                                },
                                location,
                                module: "".to_string(),
                            },
                        )
                    });

                // We cannot support all values in guard expressions as the BEAM does not
                match variant {
                    ValueConstructorVariant::LocalVariable { .. } => (),
                    ValueConstructorVariant::ModuleFn { .. }
                    | ValueConstructorVariant::Record { .. } => {
                        ctx.register_error(Error::NonLocalClauseGuardVariable {
                            location,
                            name: name.clone(),
                        });
                    }

                    ValueConstructorVariant::ModuleConstant { literal, .. } => {
                        return ctx.finish(ClauseGuard::Constant(literal))
                    }
                };

                ctx.finish(ClauseGuard::Var {
                    location,
                    name,
                    type_: typ,
                })
            }

            ClauseGuard::TupleIndex {
                location,
                tuple,
                index,
                ..
            } => {
                let (mut ctx, tuple) = self.infer_clause_guard(*tuple).into_context();
                let res = match tuple.type_().as_ref() {
                    Type::Tuple { elems } => {
                        elems
                            .get(index as usize)
                            .cloned()
                            .ok_or(Error::OutOfBoundsTupleIndex {
                                location,
                                index,
                                size: elems.len(),
                            })
                    }
                    other => Err(if other.is_unbound() {
                        Error::NotATupleUnbound {
                            location: tuple.location(),
                        }
                    } else {
                        Error::NotATuple {
                            location: tuple.location(),
                            given: tuple.type_(),
                        }
                    }),
                };

                let type_ = ctx
                    .slurp_result(res)
                    .unwrap_or_else(|| self.environment.new_unbound_var());
                ctx.finish(ClauseGuard::TupleIndex {
                    location,
                    index,
                    type_,
                    tuple: Box::new(tuple),
                })
            }

            ClauseGuard::And {
                location,
                left,
                right,
                ..
            } => {
                let (mut ctx, left) = self.infer_clause_guard(*left).into_context();
                ctx.just_slurp_result(
                    unify(bool(), left.type_())
                        .map_err(|e| convert_unify_error(e, left.location())),
                );
                let right = ctx.slurp_filled(self.infer_clause_guard(*right));
                ctx.just_slurp_result(
                    unify(bool(), right.type_())
                        .map_err(|e| convert_unify_error(e, right.location())),
                );
                ctx.finish(ClauseGuard::And {
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
                let (mut ctx, left) = self.infer_clause_guard(*left).into_context();
                ctx.just_slurp_result(
                    unify(bool(), left.type_())
                        .map_err(|e| convert_unify_error(e, left.location())),
                );
                let right = ctx.slurp_filled(self.infer_clause_guard(*right));
                ctx.just_slurp_result(
                    unify(bool(), right.type_())
                        .map_err(|e| convert_unify_error(e, right.location())),
                );
                ctx.finish(ClauseGuard::Or {
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
                let (mut ctx, left) = self.infer_clause_guard(*left).into_context();
                let right = ctx.slurp_filled(self.infer_clause_guard(*right));
                ctx.just_slurp_result(
                    unify(left.type_(), right.type_())
                        .map_err(|e| convert_unify_error(e, location)),
                );
                ctx.finish(ClauseGuard::Equals {
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
                let (mut ctx, left) = self.infer_clause_guard(*left).into_context();
                let right = ctx.slurp_filled(self.infer_clause_guard(*right));
                ctx.just_slurp_result(
                    unify(left.type_(), right.type_())
                        .map_err(|e| convert_unify_error(e, location)),
                );
                ctx.finish(ClauseGuard::NotEquals {
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
                let (mut ctx, left) = self.infer_clause_guard(*left).into_context();
                ctx.just_slurp_result(
                    unify(int(), left.type_()).map_err(|e| convert_unify_error(e, left.location())),
                );
                let right = ctx.slurp_filled(self.infer_clause_guard(*right));
                ctx.just_slurp_result(
                    unify(int(), right.type_())
                        .map_err(|e| convert_unify_error(e, right.location())),
                );
                ctx.finish(ClauseGuard::GtInt {
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
                let (mut ctx, left) = self.infer_clause_guard(*left).into_context();
                ctx.just_slurp_result(
                    unify(int(), left.type_()).map_err(|e| convert_unify_error(e, left.location())),
                );
                let right = ctx.slurp_filled(self.infer_clause_guard(*right));
                ctx.just_slurp_result(
                    unify(int(), right.type_())
                        .map_err(|e| convert_unify_error(e, right.location())),
                );
                ctx.finish(ClauseGuard::GtEqInt {
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
                let (mut ctx, left) = self.infer_clause_guard(*left).into_context();
                ctx.just_slurp_result(
                    unify(int(), left.type_()).map_err(|e| convert_unify_error(e, left.location())),
                );
                let right = ctx.slurp_filled(self.infer_clause_guard(*right));
                ctx.just_slurp_result(
                    unify(int(), right.type_())
                        .map_err(|e| convert_unify_error(e, right.location())),
                );
                ctx.finish(ClauseGuard::LtInt {
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
                let (mut ctx, left) = self.infer_clause_guard(*left).into_context();
                ctx.just_slurp_result(
                    unify(int(), left.type_()).map_err(|e| convert_unify_error(e, left.location())),
                );
                let right = ctx.slurp_filled(self.infer_clause_guard(*right));
                ctx.just_slurp_result(
                    unify(int(), right.type_())
                        .map_err(|e| convert_unify_error(e, right.location())),
                );
                ctx.finish(ClauseGuard::LtEqInt {
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
                let (mut ctx, left) = self.infer_clause_guard(*left).into_context();
                ctx.just_slurp_result(
                    unify(float(), left.type_())
                        .map_err(|e| convert_unify_error(e, left.location())),
                );
                let right = ctx.slurp_filled(self.infer_clause_guard(*right));
                ctx.just_slurp_result(
                    unify(float(), right.type_())
                        .map_err(|e| convert_unify_error(e, right.location())),
                );
                ctx.finish(ClauseGuard::GtFloat {
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
                let (mut ctx, left) = self.infer_clause_guard(*left).into_context();
                ctx.just_slurp_result(
                    unify(float(), left.type_())
                        .map_err(|e| convert_unify_error(e, left.location())),
                );
                let right = ctx.slurp_filled(self.infer_clause_guard(*right));
                ctx.just_slurp_result(
                    unify(float(), right.type_())
                        .map_err(|e| convert_unify_error(e, right.location())),
                );
                ctx.finish(ClauseGuard::GtEqFloat {
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
                let (mut ctx, left) = self.infer_clause_guard(*left).into_context();
                ctx.just_slurp_result(
                    unify(float(), left.type_())
                        .map_err(|e| convert_unify_error(e, left.location())),
                );
                let right = ctx.slurp_filled(self.infer_clause_guard(*right));
                ctx.just_slurp_result(
                    unify(float(), right.type_())
                        .map_err(|e| convert_unify_error(e, right.location())),
                );
                ctx.finish(ClauseGuard::LtFloat {
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
                let (mut ctx, left) = self.infer_clause_guard(*left).into_context();
                ctx.just_slurp_result(
                    unify(float(), left.type_())
                        .map_err(|e| convert_unify_error(e, left.location())),
                );
                let right = ctx.slurp_filled(self.infer_clause_guard(*right));
                ctx.just_slurp_result(
                    unify(float(), right.type_())
                        .map_err(|e| convert_unify_error(e, right.location())),
                );
                ctx.finish(ClauseGuard::LtEqFloat {
                    location,
                    left: Box::new(left),
                    right: Box::new(right),
                })
            }

            ClauseGuard::Constant(constant) => {
                self.infer_const(&None, constant).map(ClauseGuard::Constant)
            }
        }
    }

    fn infer_module_access(
        &mut self,
        module_alias: &str,
        label: String,
        module_location: &SrcSpan,
        select_location: SrcSpan,
    ) -> Result<TypedExpr, Error> {
        let (module_name, constructor) = {
            let (_, module) = self
                .environment
                .imported_modules
                .get(module_alias)
                .ok_or_else(|| Error::UnknownModule {
                    name: module_alias.to_string(),
                    location: *module_location,
                    imported_modules: self
                        .environment
                        .imported_modules
                        .keys()
                        .map(|t| t.to_string())
                        .collect(),
                })?;

            let constructor =
                module
                    .values
                    .get(&label)
                    .ok_or_else(|| Error::UnknownModuleValue {
                        name: label.clone(),
                        location: SrcSpan {
                            start: module_location.end,
                            end: select_location.end,
                        },
                        module_name: module.name.clone(),
                        value_constructors: module.values.keys().map(|t| t.to_string()).collect(),
                    })?;

            // Register this imported module as having been used, to inform
            // warnings of unused imports later
            let _ = self.environment.unused_modules.remove(module_alias);

            (module.name.clone(), constructor.clone())
        };

        let type_ = self.instantiate(constructor.type_, &mut hashmap![]);

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
            label,
            typ: Arc::clone(&type_),
            location: select_location,
            module_name: module_name.join("/"),
            module_alias: module_alias.to_string(),
            constructor,
        })
    }

    fn infer_record_access(
        &mut self,
        record: UntypedExpr,
        label: String,
        location: SrcSpan,
        usage: FieldAccessUsage,
    ) -> FilledResult<TypedExpr, Error> {
        // Infer the type of the (presumed) record
        self.infer(record)
            .join_with(|record| self.infer_known_record_access(record, label, location, usage))
    }

    fn infer_known_record_access(
        &mut self,
        record: TypedExpr,
        label: String,
        location: SrcSpan,
        usage: FieldAccessUsage,
    ) -> FilledResult<TypedExpr, Error> {
        let mut ctx = FilledResultContext::new();
        let record = Box::new(record);

        // If we don't yet know the type of the record then we cannot use any accessors
        if record.type_().is_unbound() {
            ctx.register_error(Error::RecordAccessUnknownType {
                location: record.location(),
            });
        }

        // Error constructor helper function
        let unknown_field = |fields| Error::UnknownRecordField {
            usage,
            typ: record.type_(),
            location,
            label: label.clone(),
            fields,
        };

        // Check to see if it's a Type that can have accessible fields
        let accessors = match collapse_links(record.type_()).as_ref() {
            // A type in the current module which may have fields
            Type::App { module, name, .. } if module == self.environment.current_module => {
                self.environment.accessors.get(name).cloned()
            }

            // A type in another module which may have fields
            Type::App { module, name, .. } => self
                .environment
                .importable_modules
                .get(&module.join("/"))
                .and_then(|module| module.accessors.get(name))
                .cloned(),

            _something_without_fields => None,
        };

        if accessors.is_none() {
            ctx.register_error(unknown_field(vec![]));
        }

        // Find the accessor, if the type has one with the same label
        let RecordAccessor {
            index,
            label,
            type_: typ,
        } = ctx
            .slurp_result(
                accessors
                    .as_ref()
                    .ok_or_else(|| unknown_field(vec![]))
                    .and_then(|accessors| {
                        accessors.accessors.get(&label).cloned().ok_or_else(|| {
                            unknown_field(accessors.accessors.keys().cloned().collect())
                        })
                    }),
            )
            .unwrap_or_else(|| RecordAccessor {
                index: 0,
                label,
                type_: self.environment.new_unbound_var(),
            });

        // Unify the record type with the accessor's stored copy of the record type.
        // This ensure that the type parameters of the retrieved value have the correct
        // types for this instance of the record.
        let accessor_record_type = accessors
            .map(|acc| acc.type_)
            .unwrap_or_else(|| self.environment.new_unbound_var());
        let mut type_vars = hashmap![];
        let accessor_record_type = self.instantiate(accessor_record_type, &mut type_vars);
        let typ = self.instantiate(typ, &mut type_vars);
        ctx.just_slurp_result(
            unify(accessor_record_type, record.type_())
                .map_err(|e| convert_unify_error(e, record.location())),
        );

        ctx.finish(TypedExpr::RecordAccess {
            record,
            label,
            index,
            location,
            typ,
        })
    }

    fn infer_record_update(
        &mut self,
        constructor: UntypedExpr,
        spread: RecordUpdateSpread,
        args: Vec<UntypedRecordUpdateArg>,
        location: SrcSpan,
    ) -> FilledResult<TypedExpr, Error> {
        let mut ctx = FilledResultContext::new();

        // have an optional (module, name) tuple so that we don't error about an invalid name when
        // we had already caught an invalid constructor.
        let opt_module_name = match ctx.slurp_filled(self.infer(constructor.clone())) {
            TypedExpr::ModuleSelect {
                module_alias,
                label,
                ..
            } => Some((Some(module_alias), label)),
            TypedExpr::Var { name, .. } => Some((None, name)),
            constructor => {
                ctx.register_error(Error::RecordUpdateInvalidConstructor {
                    location: constructor.location(),
                });
                None
            }
        };

        let (ctor_type, opt_ctor_variant) = opt_module_name
            .and_then(|(module, name)| {
                ctx.slurp_result(
                    self.environment
                        .get_value_constructor(module.as_ref(), &name)
                        .map(|ctor| {
                            (
                                ctor.type_.clone(),
                                Some((ctor.variant.clone(), module, name)),
                            )
                        })
                        .map_err(|e| convert_get_value_constructor_error(e, location)),
                )
            })
            .unwrap_or_else(|| (self.environment.new_unbound_var(), None));

        // this whole part does need the 'break on first error' mechanism, so a labelled block
        // is used here to be able to `break` from it with Err, and then the error is passed to the
        // context.
        let result = 'check_ctor: {
            if let Some((ctor_variant, _module, _name)) = opt_ctor_variant.as_ref() {
                // It must be a record with a field map for us to be able to update it
                let (field_map, constructors_count) = match ctor_variant {
                    ValueConstructorVariant::Record {
                        field_map: Some(field_map),
                        constructors_count,
                        ..
                    } => (field_map, *constructors_count),
                    _ => {
                        break 'check_ctor Err(Error::RecordUpdateInvalidConstructor {
                            location: constructor.location(),
                        });
                    }
                };

                // We can only update a record if it is the only variant of its type.
                // If a record has multiple variants it cannot be safely updated as it
                // could be one of the other variants.
                if constructors_count != 1 {
                    ctx.register_error(Error::UpdateMultiConstructorType {
                        location: constructor.location(),
                    });
                }
                Ok(Some(field_map))
            } else {
                Ok(None)
            }
        };
        let opt_field_map = ctx.slurp_result(result).flatten();

        let arg_indices: Vec<_> = if let Some(field_map) = opt_field_map {
            if args.len() == field_map.arity as usize {
                self.environment
                    .warnings
                    .push(Warning::AllFieldsRecordUpdate { location });
            }
            args.iter()
                .map(|r| {
                    *field_map.fields.get(&r.label).expect(
                        "Failed to lookup record field after successfully inferring that field",
                    )
                })
                .collect()
        } else {
            (0..args.len() as u32).collect()
        };

        // The type must be a function for it to be a record constructor
        let retrn = match ctor_type.as_ref() {
            Type::Fn { retrn, .. } => retrn.clone(),
            _ => {
                ctx.register_error(Error::RecordUpdateInvalidConstructor {
                    location: constructor.location(),
                });
                self.environment.new_unbound_var()
            }
        };

        let spread = ctx.slurp_filled(self.infer(*spread.base));
        let return_type = self.instantiate(retrn.clone(), &mut hashmap![]);

        // Check that the spread variable unifies with the return type of the constructor
        ctx.just_slurp_result(
            unify(return_type, spread.type_())
                .map_err(|e| convert_unify_error(e, spread.location())),
        );

        if args.is_empty() {
            self.environment
                .warnings
                .push(Warning::NoFieldsRecordUpdate { location });
        }

        let args: Vec<TypedRecordUpdateArg> = args
            .into_iter()
            .zip(arg_indices)
            .map(
                |(
                    UntypedRecordUpdateArg {
                        label,
                        value,
                        location,
                    },
                    index,
                )| {
                    let value = ctx.slurp_filled(self.infer(value.clone()));
                    let spread_field = ctx.slurp_filled(self.infer_known_record_access(
                        spread.clone(),
                        label.to_string(),
                        location,
                        FieldAccessUsage::Other,
                    ));

                    // Check that the update argument unifies with the corresponding
                    // field in the record contained within the spread variable. We
                    // need to check the spread, and not the constructor, in order
                    // to handle polymorphic types.
                    ctx.just_slurp_result(
                        unify(spread_field.type_(), value.type_())
                            .map_err(|e| convert_unify_error(e, value.location())),
                    );

                    TypedRecordUpdateArg {
                        location,
                        label,
                        value,
                        index,
                    }
                },
            )
            .collect();

        ctx.finish(TypedExpr::RecordUpdate {
            location,
            typ: spread.type_(),
            spread: Box::new(spread),
            args,
        })
    }

    fn infer_value_constructor(
        &mut self,
        module: &Option<String>,
        name: &str,
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
                            name: name.to_string(),
                            variables: self.environment.local_value_names(),
                        })?;

                // Note whether we are using an ungeneralised function so that we can
                // tell if it is safe to generalise this function after inference has
                // completed.
                if matches!(
                    &constructor.variant,
                    ValueConstructorVariant::ModuleFn { .. }
                ) {
                    let is_ungeneralised = self.environment.ungeneralised_functions.contains(name);
                    self.ungeneralised_function_used =
                        self.ungeneralised_function_used || is_ungeneralised;
                }

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
                        name: module_name.to_string(),
                        imported_modules: self
                            .environment
                            .imported_modules
                            .keys()
                            .map(|t| t.to_string())
                            .collect(),
                    })?;
                module
                    .values
                    .get(name)
                    .cloned()
                    .ok_or_else(|| Error::UnknownModuleValue {
                        location: *location,
                        module_name: vec![module_name.to_string()],
                        name: name.to_string(),
                        value_constructors: module.values.keys().map(|t| t.to_string()).collect(),
                    })?
            }
        };

        let ValueConstructor {
            public,
            variant,
            type_: typ,
        } = constructor;

        // Instantiate generic variables into unbound variables for this usage
        let typ = self.instantiate(typ, &mut hashmap![]);
        Ok(ValueConstructor {
            public,
            variant,
            type_: typ,
        })
    }

    // TODO: extract the type annotation checking into a infer_module_const
    // function that uses this function internally
    pub fn infer_const(
        &mut self,
        annotation: &Option<TypeAst>,
        value: UntypedConstant,
    ) -> FilledResult<TypedConstant, Error> {
        let mut ctx = FilledResultContext::new();
        let inferred = match value {
            Constant::Int {
                location, value, ..
            } => Constant::Int { location, value },

            Constant::Float {
                location, value, ..
            } => Constant::Float { location, value },

            Constant::String {
                location, value, ..
            } => Constant::String { location, value },

            Constant::Tuple {
                elements, location, ..
            } => ctx.slurp_filled(self.infer_const_tuple(elements, location)),

            Constant::List {
                elements, location, ..
            } => ctx.slurp_filled(self.infer_const_list(elements, location)),

            Constant::BitString { location, segments } => {
                ctx.slurp_filled(self.infer_constant_bit_string(segments, location))
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
                }

                // Type check the record constructor
                let constructor = ctx
                    .slurp_result(self.infer_value_constructor(&module, &name, &location))
                    .unwrap_or_else(|| ValueConstructor {
                        public: true,
                        variant: ValueConstructorVariant::Record {
                            name: name.clone(),
                            arity: 0,
                            field_map: None,
                            location,
                            module: module.clone().unwrap_or_else(|| "".to_string()),
                            constructors_count: 1,
                        },
                        type_: self.environment.new_unbound_var(),
                    });

                match &constructor.variant {
                    ValueConstructorVariant::Record {
                        name, field_map, ..
                    } => Constant::Record {
                        module,
                        location,
                        name: name.clone(),
                        args: vec![],
                        typ: constructor.type_,
                        tag: name.clone(),
                        field_map: field_map.clone(),
                    },
                    ValueConstructorVariant::ModuleFn { .. }
                    | ValueConstructorVariant::LocalVariable { .. } => {
                        ctx.register_error(Error::NonLocalClauseGuardVariable {
                            location,
                            name: name.clone(),
                        });
                        Constant::Var {
                            location,
                            module,
                            typ: self.environment.new_unbound_var(),
                            name,
                            constructor: Some(Box::new(constructor)),
                        }
                    }
                    ValueConstructorVariant::ModuleConstant { literal, .. } => literal.clone(),
                }
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
                }

                let ctor =
                    ctx.slurp_result(self.infer_value_constructor(&module, &name, &location));

                if let Some(ValueConstructorVariant::ModuleConstant { literal, .. }) =
                    ctor.as_ref().map(|c| &c.variant)
                {
                    return ctx.finish(literal.clone());
                }

                let (field_map, tag) = ctor
                    .as_ref()
                    .and_then(|ctor| {
                        if let ValueConstructorVariant::Record {
                            field_map, name, ..
                        } = &ctor.variant
                        {
                            Some((field_map.clone(), name.clone()))
                        } else {
                            ctx.register_error(Error::NonLocalClauseGuardVariable {
                                location,
                                name: name.clone(),
                            });
                            None
                        }
                    })
                    .unwrap_or_else(|| {
                        // create a custom field map based on the passed arguments.
                        // This is made to ensure that further checks on arguments pass.
                        let mut field_map = FieldMap::new(args.len() as u32);

                        for (index, label, location) in
                            args.iter().enumerate().filter_map(|(i, arg)| {
                                Some((i as u32, arg.label.as_ref()?.clone(), arg.location))
                            })
                        {
                            ctx.just_slurp_result(
                                field_map
                                    .insert(label.clone(), index)
                                    .map_err(|_| Error::DuplicateField { location, label }),
                            );
                        }

                        (field_map.into_option(), name.clone())
                    });

                let typ = ctor
                    .as_ref()
                    .map(|ctor| ctor.type_.clone())
                    .unwrap_or_else(|| self.environment.new_unbound_var());

                // Pretty much all the other infer functions operate on UntypedExpr
                // or TypedExpr rather than ClauseGuard. To make things easier we
                // build the TypedExpr equivalent of the constructor and use that
                // TODO: resvisit this. It is rather awkward at present how we
                // have to convert to this other data structure.
                let fun = match &module {
                    Some(module_alias) => {
                        let module_name = self
                            .environment
                            .imported_modules
                            .get(module_alias)
                            .expect("Failed to find previously located module import")
                            .1
                            .name
                            .join("/");
                        let module_value_constructor = ModuleValueConstructor::Record {
                            name: name.clone(),
                            field_map: field_map.clone(),
                            arity: args.len() as u16,
                            type_: Arc::clone(&typ),
                            location: ctor
                                .as_ref()
                                .map(|ctor| ctor.variant.definition_location())
                                .unwrap_or(location),
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
                        constructor: ctor.unwrap_or_else(|| ValueConstructor {
                            public: true,
                            variant: ValueConstructorVariant::Record {
                                name: name.clone(),
                                arity: args.len() as u16,
                                field_map: field_map.clone(),
                                location,
                                module: module.clone().unwrap_or_else(|| "".to_string()),
                                constructors_count: 1,
                            },
                            type_: typ,
                        }),
                        location,
                        name: name.clone(),
                    },
                };

                // This is basically the same code as do_infer_call_with_known_fun()
                // except the args are typed with infer_clause_guard() here.
                // This duplication is a bit awkward but it works!
                // Potentially this could be improved later
                match ctx
                    .slurp_result(
                        self.get_field_map(&fun)
                            .map_err(|e| convert_get_value_constructor_error(e, location)),
                    )
                    .flatten()
                {
                    // The fun has a field map so labelled arguments may be present and need to be reordered.
                    Some(field_map) => {
                        ctx.just_slurp_result(field_map.reorder(&mut args, location))
                    }

                    // The fun has no field map and so we error if arguments have been labelled
                    None => ctx.just_slurp_result(assert_no_labelled_arguments(&args)),
                }

                let (mut args_types, return_type) = ctx.slurp_filled_with(
                    match_fun_type(fun.type_(), args.len(), self.environment),
                    |errs| errs.map(|e| convert_not_fun_error(e, fun.location(), location)),
                );
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
                        let value = ctx.slurp_filled(self.infer_const(&None, value));
                        ctx.just_slurp_result(
                            unify(typ.clone(), value.type_())
                                .map_err(|e| convert_unify_error(e, value.location())),
                        );
                        CallArg {
                            label,
                            value,
                            implicit,
                            location,
                        }
                    })
                    .collect();

                Constant::Record {
                    module,
                    location,
                    name,
                    args,
                    typ: return_type,
                    tag,
                    field_map,
                }
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
                }

                let ctor = ctx
                    .slurp_result(self.infer_value_constructor(&module, &name, &location))
                    .unwrap_or_else(|| {
                        let typ = self.environment.new_unbound_var();
                        ValueConstructor {
                            public: true,
                            variant: ValueConstructorVariant::ModuleConstant {
                                location,
                                module: module.clone().unwrap_or_else(|| "".to_string()),
                                literal: Constant::Var {
                                    location,
                                    module: module.clone(),
                                    name: name.clone(),
                                    typ: typ.clone(),
                                    constructor: None,
                                },
                            },
                            type_: typ,
                        }
                    });
                // Infer the type of this constant
                match ctor.variant {
                    ValueConstructorVariant::ModuleConstant { .. }
                    | ValueConstructorVariant::ModuleFn { .. } => Constant::Var {
                        location,
                        module,
                        name,
                        typ: ctor.type_.clone(),
                        constructor: Some(Box::from(ctor)),
                    },
                    // constructor.variant cannot be a LocalVariable because module constants can
                    // only be defined at module scope. It also cannot be a Record because then
                    // this constant would have been parsed as a Constant::Record. Therefore this
                    // code is unreachable.
                    _ => unreachable!(),
                }
            }
        };

        // Check type annotation is accurate.
        if let Some(ann) = annotation {
            let const_ann = ctx.slurp_filled(self.type_from_ast(ann));
            ctx.just_slurp_result(
                unify(const_ann, inferred.type_())
                    .map_err(|e| convert_unify_error(e, inferred.location())),
            );
        };

        ctx.finish(inferred)
    }

    fn infer_const_tuple(
        &mut self,
        untyped_elements: Vec<UntypedConstant>,
        location: SrcSpan,
    ) -> FilledResult<TypedConstant, Error> {
        let mut ctx = FilledResultContext::new();

        let elements = ctx.slurp_filled_collect(
            untyped_elements
                .into_iter()
                .map(|elem| self.infer_const(&None, elem)),
        );

        ctx.finish(Constant::Tuple { elements, location })
    }

    fn infer_const_list(
        &mut self,
        untyped_elements: Vec<UntypedConstant>,
        location: SrcSpan,
    ) -> FilledResult<TypedConstant, Error> {
        let mut ctx = FilledResultContext::new();
        let typ = self.new_unbound_var();

        let elements = untyped_elements
            .into_iter()
            .map(|element| {
                let element = ctx.slurp_filled(self.infer_const(&None, element));
                ctx.just_slurp_result(
                    unify(typ.clone(), element.type_())
                        .map_err(|e| convert_unify_error(e, element.location())),
                );
                element
            })
            .collect();

        ctx.finish(Constant::List {
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
    ) -> FilledResult<(TypedExpr, Vec<TypedCallArg>, Arc<Type>), Error> {
        let mut ctx = FilledResultContext::new();
        let fun = ctx.slurp_filled(match fun {
            UntypedExpr::FieldAccess {
                location,
                label,
                container,
            } => self.infer_field_access(*container, label, location, FieldAccessUsage::MethodCall),

            fun => self.infer(fun),
        });

        let (fun, args, typ) =
            ctx.slurp_filled(self.do_infer_call_with_known_fun(fun, args, location));
        ctx.finish((fun, args, typ))
    }

    pub fn do_infer_call_with_known_fun(
        &mut self,
        fun: TypedExpr,
        mut args: Vec<CallArg<UntypedExpr>>,
        location: SrcSpan,
    ) -> FilledResult<(TypedExpr, Vec<TypedCallArg>, Arc<Type>), Error> {
        let mut ctx = FilledResultContext::new();
        // Check to see if the function accepts labelled arguments
        match ctx
            .slurp_result(
                self.get_field_map(&fun)
                    .map_err(|e| convert_get_value_constructor_error(e, location)),
            )
            .flatten()
        {
            // The fun has a field map so labelled arguments may be present and need to be reordered.
            Some(field_map) => {
                let _ = ctx.slurp_result(field_map.reorder(&mut args, location));
            }

            // The fun has no field map and so we error if arguments have been labelled
            None => {
                let _ = ctx.slurp_result(assert_no_labelled_arguments(&args));
            }
        }

        // Extract the type of the fun, ensuring it actually is a function
        let (mut args_types, return_type) = ctx.slurp_filled_with(
            match_fun_type(fun.type_(), args.len(), self.environment),
            |i| i.map(|e| convert_not_fun_error(e, fun.location(), location)),
        );

        // Ensure that the given args have the correct types
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
                let value = ctx.slurp_filled(self.infer_call_argument(value, typ.clone()));
                CallArg {
                    label,
                    value,
                    implicit,
                    location,
                }
            })
            .collect();
        ctx.finish((fun, args, return_type))
    }

    fn infer_call_argument(
        &mut self,
        value: UntypedExpr,
        typ: Arc<Type>,
    ) -> FilledResult<TypedExpr, Error> {
        let typ = collapse_links(typ);

        let (mut ctx, value) = match (&*typ, value) {
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
                *body,
                false,
                return_annotation,
                location,
            ),

            // Otherwise just perform normal type inference.
            (_, value) => self.infer(value),
        }
        .into_context();

        ctx.just_slurp_result(
            unify(typ, value.type_()).map_err(|e| convert_unify_error(e, value.location())),
        );
        ctx.finish(value)
    }

    pub fn do_infer_fn(
        &mut self,
        args: Vec<UntypedArg>,
        expected_args: &[Arc<Type>],
        body: UntypedExpr,
        return_annotation: &Option<TypeAst>,
    ) -> FilledResult<(Vec<TypedArg>, TypedExpr), Error> {
        let mut ctx = FilledResultContext::new();
        // Construct an initial type for each argument of the function- either an unbound
        // type variable or a type provided by an annotation.
        let args: Vec<_> = ctx.slurp_filled_collect(
            args.into_iter()
                .enumerate()
                .map(|(i, arg)| self.infer_arg(arg, expected_args.get(i).cloned())),
        );

        let return_type = return_annotation
            .as_ref()
            .map(|ann| ctx.slurp_filled(self.type_from_ast(&ann)));

        ctx.finish(())
            .join_with(|_| self.infer_fn_with_known_types(args, body, return_type))
    }

    pub fn infer_fn_with_known_types(
        &mut self,
        args: Vec<TypedArg>,
        body: UntypedExpr,
        return_type: Option<Arc<Type>>,
    ) -> FilledResult<(Vec<TypedArg>, TypedExpr), Error> {
        let mut ctx = FilledResultContext::new();
        let (body_rigid_names, body_infer) = self.in_new_scope(|body_typer| {
            for (arg, t) in args.iter().zip(args.iter().map(|arg| arg.type_.clone())) {
                match &arg.names {
                    ArgNames::Named { name } | ArgNames::NamedLabelled { name, .. } => {
                        body_typer.environment.insert_local_variable(
                            name.to_string(),
                            arg.location,
                            t,
                        );
                        body_typer.environment.init_usage(
                            name.to_string(),
                            EntityKind::Variable,
                            arg.location,
                        );
                    }
                    ArgNames::Discard { .. } | ArgNames::LabelledDiscard { .. } => (),
                };
            }

            (body_typer.hydrator.rigid_names(), body_typer.infer(body))
        });

        let body = ctx.slurp_filled_with(body_infer, |errs| {
            errs.map(|e| e.with_unify_error_rigid_names(&body_rigid_names))
        });

        // Check that any return type is accurate.
        if let Some(return_type) = return_type {
            ctx.just_slurp_result(unify(return_type, body.type_()).map_err(|e| {
                e.return_annotation_mismatch()
                    .into_error(body.type_defining_location())
                    .with_unify_error_rigid_names(&body_rigid_names)
            }));
        }

        ctx.finish((args, body))
    }

    fn check_case_exhaustiveness(
        &mut self,
        subjects_count: usize,
        subjects: &[Arc<Type>],
        typed_clauses: &[Clause<TypedExpr, PatternConstructor, Arc<Type>, String>],
    ) -> Result<(), Vec<String>> {
        // Because exhaustiveness checking in presence of multiple subjects is similar
        // to full exhaustiveness checking of tuples or other nested record patterns,
        // and we currently only do only limited exhaustiveness checking of custom types
        // at the top level of patterns, only consider case expressions with one subject.
        if subjects_count != 1 {
            return Ok(());
        }
        let subject_type = subjects
            .get(0)
            .expect("Asserted there's one case subject but found none");
        let value_typ = collapse_links(subject_type.clone());

        // Currently guards in exhaustiveness checking are assumed that they can fail,
        // so we go through all clauses and pluck out only the patterns
        // for clauses that don't have guards.
        let mut patterns = Vec::new();
        for clause in typed_clauses {
            if let Clause { guard: None, .. } = clause {
                // clause.pattern is a list of patterns for all subjects
                if let Some(pattern) = clause.pattern.get(0) {
                    patterns.push(pattern.clone());
                }
                // A clause can be built with alternative patterns as well, e.g. `Audio(_) | Text(_) ->`.
                // We're interested in all patterns so we build a flattened list.
                for alternative_pattern in &clause.alternative_patterns {
                    // clause.alternative_pattern is a list of patterns for all subjects
                    if let Some(pattern) = alternative_pattern.get(0) {
                        patterns.push(pattern.clone());
                    }
                }
            }
        }
        self.environment.check_exhaustiveness(patterns, value_typ)
    }
}

struct UseCall {
    location: SrcSpan,
    function: Box<UntypedExpr>,
    arguments: Vec<CallArg<UntypedExpr>>,
}

fn get_use_expression_call(call: UntypedExpr) -> UseCall {
    // Ensure that the use's call is of the right structure. i.e. it is a
    // call to a function.
    match call {
        UntypedExpr::Call {
            location,
            fun: function,
            arguments,
        } => UseCall {
            location,
            arguments,
            function,
        },

        other => UseCall {
            location: other.location(),
            function: Box::new(other),
            arguments: vec![],
        },
    }
}

fn use_assignments_to_function_arguments(assignments: Vec<(AssignName, SrcSpan)>) -> Vec<Arg<()>> {
    assignments
        .into_iter()
        .map(|(name, location)| Arg {
            names: name.to_arg_names(),
            location,
            annotation: None,
            type_: (),
        })
        .collect()
}
