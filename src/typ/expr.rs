use super::{
    assert_no_labelled_arguments, bit_string, bool, collapse_links, convert_binary_error,
    convert_get_value_constructor_error, convert_not_fun_error, convert_unify_error, float, fn_,
    generalise, infer_bit_string_segment_option, int, list, match_fun_type, pattern, result,
    string, tuple, Arc, ArgNames, BinaryTypeSpecifier, Environment, Error, FieldMap,
    GetValueConstructorError, GleamExpect, HasType, Hydrator, RecordAccessor, Type, TypedCallArg,
    TypedRecordUpdateArg, Typer, UntypedRecordUpdateArg, ValueConstructor, ValueConstructorVariant,
    Warning,
};
use crate::ast::{
    Arg, BinOp, BindingKind, BitStringSegment, BitStringSegmentOption, CallArg, Clause,
    ClauseGuard, Constant, HasLocation, RecordUpdateSpread, SrcSpan, TypeAst, TypedArg,
    TypedClause, TypedClauseGuard, TypedConstant, TypedExpr, TypedMultiPattern, UntypedArg,
    UntypedClause, UntypedClauseGuard, UntypedConstant, UntypedConstantBitStringSegment,
    UntypedExpr, UntypedExprBitStringSegment, UntypedMultiPattern, UntypedPattern,
};
use crate::num_util::to_usize;

pub struct ExprTyper<'a, 'b, 'c> {
    environment: &'a mut Environment<'b, 'c>,

    // Type hydrator for creating types from annotations
    pub hydrator: Hydrator,

    // We keep track of whether any ungeneralised functions have been used
    // to determine whether it is safe to generalise this expression after
    // it has been inferred.
    pub ungeneralised_function_used: bool,
}

impl<'a, 'b, 'c> Typer for ExprTyper<'a, 'b, 'c> {
    fn with_environment<T>(&mut self, f: impl FnOnce(&mut Environment<'_, '_>) -> T) -> T {
        f(self.environment)
    }
}

impl<'a, 'b, 'c> ExprTyper<'a, 'b, 'c> {
    pub fn new(environment: &'a mut Environment<'b, 'c>) -> Self {
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

    pub fn type_from_ast(&mut self, ast: &TypeAst) -> Result<Arc<Type>, Error> {
        self.hydrator.type_from_ast(ast, self.environment)
    }

    fn instantiate(
        &mut self,
        t: &Arc<Type>,
        ctx_level: usize,
        ids: &mut im::HashMap<usize, Arc<Type>>,
    ) -> Arc<Type> {
        self.environment
            .instantiate(t, ctx_level, ids, &self.hydrator)
    }

    /// Crawl the AST, annotating each node with the inferred type or
    /// returning an error.
    ///
    pub fn infer(&mut self, expr: UntypedExpr) -> Result<TypedExpr, Error> {
        match expr {
            UntypedExpr::ListNil { location, .. } => Ok(self.make_nil(location)),

            UntypedExpr::Todo {
                location, label, ..
            } => Ok(self.make_todo(location, label)),

            UntypedExpr::Var { location, name, .. } => self.infer_var(name, location),

            UntypedExpr::Int {
                location, value, ..
            } => Ok(Self::make_int(value, location)),

            UntypedExpr::Seq { first, then, .. } => self.infer_seq(*first, *then),

            UntypedExpr::Tuple {
                location, elems, ..
            } => self.infer_tuple(elems, location),

            UntypedExpr::Float {
                location, value, ..
            } => Ok(Self::make_float(value, location)),

            UntypedExpr::String {
                location, value, ..
            } => Ok(Self::make_string(value, location)),

            UntypedExpr::Pipe {
                left,
                right,
                location,
            } => self.infer_pipe(*left, *right, location),

            UntypedExpr::Fn {
                location,
                is_capture,
                args,
                body,
                return_annotation,
                ..
            } => self.infer_fn(args, *body, is_capture, return_annotation, location),

            UntypedExpr::Let {
                location,
                pattern,
                value,
                then,
                kind,
                annotation,
                ..
            } => self.infer_let(pattern, *value, *then, kind, &annotation, location),

            UntypedExpr::Case {
                location,
                subjects,
                clauses,
                ..
            } => self.infer_case(&subjects, &clauses, location),

            UntypedExpr::ListCons {
                location,
                head,
                tail,
                ..
            } => self.infer_cons(*head, *tail, location),

            UntypedExpr::Call {
                location,
                fun,
                args,
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
            } => self.infer_field_access(*container, label, location),

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
                args,
            } => self.infer_record_update(constructor.as_ref(), spread, &args, location),
        }
    }

    fn infer_pipe(
        &mut self,
        left: UntypedExpr,
        right: UntypedExpr,
        location: SrcSpan,
    ) -> Result<TypedExpr, Error> {
        match right {
            // left |> right(..args)
            UntypedExpr::Call {
                fun,
                args,
                location,
                ..
            } => {
                let fun = self.infer(*fun)?;
                match fun.typ().fn_arity() {
                    // Rewrite as right(left, ..args)
                    Some(arity) if arity == args.len() + 1 => {
                        self.infer_insert_pipe(fun, args, left, location)
                    }

                    // Rewrite as right(..args)(left)
                    _ => self.infer_apply_to_call_pipe(fun, args, left, location),
                }
            }

            // right(left)
            right => self.infer_apply_pipe(left, right, location),
        }
    }

    /// Attempt to infer a |> b(..c) as b(..c)(a)
    fn infer_apply_to_call_pipe(
        &mut self,
        fun: TypedExpr,
        args: Vec<CallArg<UntypedExpr>>,
        left: UntypedExpr,
        location: SrcSpan,
    ) -> Result<TypedExpr, Error> {
        let (fun, args, typ) = self.do_infer_call_with_known_fun(fun, args, location)?;
        let fun = TypedExpr::Call {
            location,
            typ,
            args,
            fun: Box::new(fun),
        };
        let args = vec![CallArg {
            label: None,
            location: left.location(),
            value: left,
        }];
        let (fun, args, typ) = self.do_infer_call_with_known_fun(fun, args, location)?;
        Ok(TypedExpr::Call {
            location,
            typ,
            args,
            fun: Box::new(fun),
        })
    }

    /// Attempt to infer a |> b(c) as b(a, c)
    fn infer_insert_pipe(
        &mut self,
        fun: TypedExpr,
        args: Vec<CallArg<UntypedExpr>>,
        left: UntypedExpr,
        right_call_location: SrcSpan,
    ) -> Result<TypedExpr, Error> {
        let location = SrcSpan {
            start: left.location().start,
            end: right_call_location.end,
        };
        let mut new_args = Vec::with_capacity(args.len() + 1);
        new_args.push(CallArg {
            label: None,
            location,
            value: left,
        });
        for arg in args {
            new_args.push(arg.clone());
        }

        let (fun, args, typ) = self.do_infer_call_with_known_fun(fun, new_args, location)?;
        // TODO: Preserve the fact this is a pipe instead of making it a Call
        Ok(TypedExpr::Call {
            location,
            typ,
            args,
            fun: Box::new(fun),
        })
    }

    /// Attempt to infer a |> b as b(a)
    fn infer_apply_pipe(
        &mut self,
        left: UntypedExpr,
        right: UntypedExpr,
        location: SrcSpan,
    ) -> Result<TypedExpr, Error> {
        let left = Box::new(self.infer(left)?);
        let right = Box::new(self.infer(right)?);
        let typ = self.new_unbound_var(self.environment.level);
        let fn_typ = Arc::new(Type::Fn {
            args: vec![left.typ()],
            retrn: typ.clone(),
        });
        self.unify(right.typ(), fn_typ)
            .map_err(|e| convert_unify_error(e, location))?;

        Ok(TypedExpr::Pipe {
            location,
            typ,
            right,
            left,
        })
    }

    fn make_nil(&mut self, location: SrcSpan) -> TypedExpr {
        TypedExpr::ListNil {
            location,
            typ: list(self.new_unbound_var(self.environment.level)),
        }
    }

    fn make_todo(&mut self, location: SrcSpan, label: Option<String>) -> TypedExpr {
        let typ = self.new_unbound_var(self.environment.level);
        self.environment.warnings.push(Warning::Todo {
            location,
            typ: typ.clone(),
        });

        TypedExpr::Todo {
            location,
            label,
            typ,
        }
    }

    fn make_string(value: String, location: SrcSpan) -> TypedExpr {
        TypedExpr::String {
            location,
            value,
            typ: string(),
        }
    }

    fn make_int(value: String, location: SrcSpan) -> TypedExpr {
        TypedExpr::Int {
            location,
            value,
            typ: int(),
        }
    }

    fn make_float(value: String, location: SrcSpan) -> TypedExpr {
        TypedExpr::Float {
            location,
            value,
            typ: float(),
        }
    }

    fn infer_seq(&mut self, first: UntypedExpr, then: UntypedExpr) -> Result<TypedExpr, Error> {
        if first.is_literal() {
            self.environment.warnings.push(Warning::UnusedLiteral {
                location: first.location(),
            });
        }

        let first = self.infer(first)?;
        let then = self.infer(then)?;

        if first.typ().as_ref().is_result() {
            self.environment
                .warnings
                .push(Warning::ImplicitlyDiscardedResult {
                    location: first.location(),
                });
        }

        Ok(TypedExpr::Seq {
            typ: then.typ(),
            first: Box::new(first),
            then: Box::new(then),
        })
    }

    fn infer_fn(
        &mut self,
        args: Vec<UntypedArg>,
        body: UntypedExpr,
        is_capture: bool,
        return_annotation: Option<TypeAst>,
        location: SrcSpan,
    ) -> Result<TypedExpr, Error> {
        let (args, body) = self.do_infer_fn(args, body, &return_annotation)?;
        let args_types = args.iter().map(|a| a.typ.clone()).collect();
        let typ = fn_(args_types, body.typ());
        Ok(TypedExpr::Fn {
            location,
            typ,
            is_capture,
            args,
            body: Box::new(body),
            return_annotation,
        })
    }

    #[allow(clippy::map_unwrap_or)]
    fn infer_arg(&mut self, arg: UntypedArg) -> Result<TypedArg, Error> {
        let Arg {
            names,
            annotation,
            location,
            ..
        } = arg;
        let typ = annotation
            .clone()
            .map(|t| self.type_from_ast(&t))
            .unwrap_or_else(|| Ok(self.new_unbound_var(self.environment.level)))?;
        Ok(Arg {
            names,
            location,
            annotation,
            typ,
        })
    }

    fn infer_call(
        &mut self,
        fun: UntypedExpr,
        args: Vec<CallArg<UntypedExpr>>,
        location: SrcSpan,
    ) -> Result<TypedExpr, Error> {
        let (fun, args, typ) = self.do_infer_call(fun, args, location)?;
        Ok(TypedExpr::Call {
            location,
            typ,
            args,
            fun: Box::new(fun),
        })
    }

    fn infer_cons(
        &mut self,
        head: UntypedExpr,
        tail: UntypedExpr,
        location: SrcSpan,
    ) -> Result<TypedExpr, Error> {
        let head = self.infer(head)?;
        let tail = self.infer(tail)?;
        self.unify(tail.typ(), list(head.typ()))
            .map_err(|e| convert_unify_error(e, location))?;

        Ok(TypedExpr::ListCons {
            location,
            typ: tail.typ(),
            head: Box::new(head),
            tail: Box::new(tail),
        })
    }

    fn infer_tuple(
        &mut self,
        elems: Vec<UntypedExpr>,
        location: SrcSpan,
    ) -> Result<TypedExpr, Error> {
        let elems = elems
            .into_iter()
            .map(|e| self.infer(e))
            .collect::<Result<Vec<_>, _>>()?;
        let typ = tuple(elems.iter().map(|e| e.typ()).collect());
        Ok(TypedExpr::Tuple {
            location,
            elems,
            typ,
        })
    }
    fn infer_var(&mut self, name: String, location: SrcSpan) -> Result<TypedExpr, Error> {
        let constructor = self.infer_value_constructor(&None, &name, &location)?;
        Ok(TypedExpr::Var {
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
    ) -> Result<TypedExpr, Error> {
        // Attempt to infer the container as a record access. If that fails, we may be shadowing the name
        // of an imported module, so attempt to infer the container as a module access.
        // TODO: Remove this cloning
        match self.infer_record_access(container.clone(), &label, access_location) {
            Ok(record_access) => Ok(record_access),
            Err(err) => match container {
                UntypedExpr::Var { name, location, .. } => {
                    let module_access =
                        self.infer_module_access(name.as_ref(), label, &location, access_location);

                    // If the name is in the environment, use the original error from
                    // inferring the record access, so that we can suggest possible
                    // misspellings of field names
                    if self.environment.local_values.contains_key(&name) {
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

        match tuple.typ().as_ref() {
            Type::Tuple { elems } => {
                let typ = elems
                    .get(to_usize(index))
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
                given: tuple.typ(),
            }),
        }
    }

    fn infer_bit_string(
        &mut self,
        segments: Vec<UntypedExprBitStringSegment>,
        location: SrcSpan,
    ) -> Result<TypedExpr, Error> {
        let segments = segments
            .into_iter()
            .map(|s| self.infer_bit_segment(*s.value, s.options, s.location, ExprTyper::infer))
            .collect::<Result<Vec<_>, _>>()?;

        Ok(TypedExpr::BitString {
            location,
            segments,
            typ: bit_string(),
        })
    }

    fn infer_constant_bit_string(
        &mut self,
        segments: Vec<UntypedConstantBitStringSegment>,
        location: SrcSpan,
    ) -> Result<TypedConstant, Error> {
        let segments = segments
            .into_iter()
            .map(|s| {
                self.infer_bit_segment(*s.value, s.options, s.location, |env, expr| {
                    env.infer_const(&None, expr)
                })
            })
            .collect::<Result<Vec<_>, _>>()?;

        Ok(Constant::BitString { location, segments })
    }

    fn infer_bit_segment<UntypedValue, TypedValue, InferFn>(
        &mut self,
        value: UntypedValue,
        options: Vec<BitStringSegmentOption<UntypedValue>>,
        location: SrcSpan,
        mut infer: InferFn,
    ) -> Result<BitStringSegment<TypedValue, Arc<Type>>, Error>
    where
        InferFn: FnMut(&mut Self, UntypedValue) -> Result<TypedValue, Error>,
        TypedValue: HasType + HasLocation + Clone,
    {
        let value = infer(self, value)?;

        let infer_option = |segment_option: BitStringSegmentOption<UntypedValue>| {
            infer_bit_string_segment_option(segment_option, |value, typ| {
                let typed_value = infer(self, value)?;
                self.unify(typ, typed_value.typ())
                    .map_err(|e| convert_unify_error(e, typed_value.location()))?;
                Ok(typed_value)
            })
        };

        let options = options
            .into_iter()
            .map(infer_option)
            .collect::<Result<Vec<_>, _>>()?;

        let type_specifier = BinaryTypeSpecifier::new(&options, false)
            .map_err(|e| convert_binary_error(e, &location))?;
        let typ = type_specifier.typ().unwrap_or_else(int);

        self.unify(typ.clone(), value.typ())
            .map_err(|e| convert_unify_error(e, value.location()))?;

        Ok(BitStringSegment {
            location,
            typ,
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
        let (input_type, output_type) = match name {
            BinOp::Eq | BinOp::NotEq => {
                let left = self.infer(left)?;
                let right = self.infer(right)?;
                self.unify(left.typ(), right.typ())
                    .map_err(|e| convert_unify_error(e, right.location()))?;

                return Ok(TypedExpr::BinOp {
                    location,
                    name,
                    typ: bool(),
                    left: Box::new(left),
                    right: Box::new(right),
                });
            }
            BinOp::And | BinOp::Or => (bool(), bool()),
            BinOp::LtInt | BinOp::LtEqInt | BinOp::GtEqInt | BinOp::GtInt => (int(), bool()),

            BinOp::LtFloat | BinOp::LtEqFloat | BinOp::GtEqFloat | BinOp::GtFloat => {
                (float(), bool())
            }
            BinOp::AddInt | BinOp::SubInt | BinOp::MultInt | BinOp::DivInt | BinOp::ModuloInt => {
                (int(), int())
            }
            BinOp::AddFloat | BinOp::SubFloat | BinOp::MultFloat | BinOp::DivFloat => {
                (float(), float())
            }
        };

        let left = self.infer(left)?;
        self.unify(input_type.clone(), left.typ())
            .map_err(|e| convert_unify_error(e, left.location()))?;
        let right = self.infer(right)?;
        self.unify(input_type, right.typ())
            .map_err(|e| convert_unify_error(e, right.location()))?;

        Ok(TypedExpr::BinOp {
            location,
            name,
            typ: output_type,
            left: Box::new(left),
            right: Box::new(right),
        })
    }

    fn infer_let(
        &mut self,
        pattern: UntypedPattern,
        value: UntypedExpr,
        then: UntypedExpr,
        kind: BindingKind,
        annotation: &Option<TypeAst>,
        location: SrcSpan,
    ) -> Result<TypedExpr, Error> {
        let value = self.in_new_scope(|value_typer| value_typer.infer(value))?;

        let try_value_type = self.new_unbound_var(self.environment.level);
        let try_error_type = self.new_unbound_var(self.environment.level);

        let value_typ = match kind {
            // Ensure that the value is a result if this is a `try` binding
            BindingKind::Try => {
                let v = try_value_type.clone();
                let e = try_error_type.clone();
                self.unify(result(v, e), value.typ())
                    .map_err(|e| convert_unify_error(e, value.location()))?;
                try_value_type
            }
            _ => value.typ(),
        };

        let value_typ = generalise(&value_typ, self.environment.level + 1);

        // Ensure the pattern matches the type of the value
        let pattern =
            pattern::PatternTyper::new(self.environment, &self.hydrator, self.environment.level)
                .unify(pattern, value_typ.clone())?;

        // Check the type of the following code
        let then = self.infer(then)?;
        let typ = then.typ();

        // Ensure that a Result with the right error type is returned for `try`
        if kind == BindingKind::Try {
            let value = self.new_unbound_var(self.environment.level);
            self.unify(result(value, try_error_type), typ.clone())
                .map_err(|e| convert_unify_error(e, then.try_binding_location()))?;
        }

        // Check that any type annotation is accurate.
        if let Some(ann) = annotation {
            let ann_typ = self
                .type_from_ast(ann)
                .map(|t| self.instantiate(&t, self.environment.level, &mut hashmap![]))?;
            self.unify(ann_typ, value_typ)
                .map_err(|e| convert_unify_error(e, value.location()))?;
        }

        Ok(TypedExpr::Let {
            location,
            typ,
            kind,
            pattern,
            value: Box::new(value),
            then: Box::new(then),
        })
    }

    fn infer_case(
        &mut self,
        subjects: &[UntypedExpr],
        clauses: &[UntypedClause],
        location: SrcSpan,
    ) -> Result<TypedExpr, Error> {
        let subjects_count = subjects.len();
        let mut typed_subjects = Vec::with_capacity(subjects_count);
        let mut subject_types = Vec::with_capacity(subjects_count);
        let mut typed_clauses = Vec::with_capacity(clauses.len());

        let return_type = self.new_unbound_var(self.environment.level);

        for subject in subjects {
            let (subject, subject_type) = self.in_new_scope(|subject_typer| {
                let subject = subject_typer.infer(subject.clone())?;
                let subject_type = generalise(&subject.typ(), subject_typer.environment.level);

                Ok((subject, subject_type))
            })?;

            typed_subjects.push(subject);
            subject_types.push(subject_type);
        }

        for clause in clauses {
            let typed_clause = self.infer_clause(clause.clone(), &subject_types)?;
            self.unify(return_type.clone(), typed_clause.then.typ())
                .map_err(|e| e.case_clause_mismatch().into_error(typed_clause.location()))?;
            typed_clauses.push(typed_clause);
        }
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
        let mut pattern_typer =
            pattern::PatternTyper::new(self.environment, &self.hydrator, self.environment.level);
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
                self.unify(bool(), guard.typ())
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
                    ValueConstructorVariant::LocalVariable => (),
                    ValueConstructorVariant::ModuleFn { .. }
                    | ValueConstructorVariant::Record { .. } => {
                        return Err(Error::NonLocalClauseGuardVariable { location, name })
                    }

                    ValueConstructorVariant::ModuleConstant { literal } => {
                        return Ok(ClauseGuard::Constant(literal.clone()))
                    }
                };

                Ok(ClauseGuard::Var {
                    location,
                    name,
                    typ: constructor.typ,
                })
            }

            ClauseGuard::TupleIndex {
                location,
                tuple,
                index,
                ..
            } => {
                let tuple = self.infer_clause_guard(*tuple)?;
                match tuple.typ().as_ref() {
                    Type::Tuple { elems } => {
                        let typ = elems
                            .get(to_usize(index))
                            .ok_or_else(|| Error::OutOfBoundsTupleIndex {
                                location,
                                index,
                                size: elems.len(),
                            })?
                            .clone();
                        Ok(ClauseGuard::TupleIndex {
                            location,
                            index,
                            typ,
                            tuple: Box::new(tuple),
                        })
                    }

                    typ if typ.is_unbound() => Err(Error::NotATupleUnbound {
                        location: tuple.location(),
                    }),

                    _ => Err(Error::NotATuple {
                        location: tuple.location(),
                        given: tuple.typ(),
                    }),
                }
            }

            ClauseGuard::And {
                location,
                left,
                right,
                ..
            } => {
                let left = self.infer_clause_guard(*left)?;
                self.unify(bool(), left.typ())
                    .map_err(|e| convert_unify_error(e, left.location()))?;
                let right = self.infer_clause_guard(*right)?;
                self.unify(bool(), right.typ())
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
                self.unify(bool(), left.typ())
                    .map_err(|e| convert_unify_error(e, left.location()))?;
                let right = self.infer_clause_guard(*right)?;
                self.unify(bool(), right.typ())
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
                self.unify(left.typ(), right.typ())
                    .map_err(|e| convert_unify_error(e, location))?;
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
                self.unify(left.typ(), right.typ())
                    .map_err(|e| convert_unify_error(e, location))?;
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
                self.unify(int(), left.typ())
                    .map_err(|e| convert_unify_error(e, left.location()))?;
                let right = self.infer_clause_guard(*right)?;
                self.unify(int(), right.typ())
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
                self.unify(int(), left.typ())
                    .map_err(|e| convert_unify_error(e, left.location()))?;
                let right = self.infer_clause_guard(*right)?;
                self.unify(int(), right.typ())
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
                self.unify(int(), left.typ())
                    .map_err(|e| convert_unify_error(e, left.location()))?;
                let right = self.infer_clause_guard(*right)?;
                self.unify(int(), right.typ())
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
                self.unify(int(), left.typ())
                    .map_err(|e| convert_unify_error(e, left.location()))?;
                let right = self.infer_clause_guard(*right)?;
                self.unify(int(), right.typ())
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
                self.unify(float(), left.typ())
                    .map_err(|e| convert_unify_error(e, left.location()))?;
                let right = self.infer_clause_guard(*right)?;
                self.unify(float(), right.typ())
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
                self.unify(float(), left.typ())
                    .map_err(|e| convert_unify_error(e, left.location()))?;
                let right = self.infer_clause_guard(*right)?;
                self.unify(float(), right.typ())
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
                self.unify(float(), left.typ())
                    .map_err(|e| convert_unify_error(e, left.location()))?;
                let right = self.infer_clause_guard(*right)?;
                self.unify(float(), right.typ())
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
                self.unify(float(), left.typ())
                    .map_err(|e| convert_unify_error(e, left.location()))?;
                let right = self.infer_clause_guard(*right)?;
                self.unify(float(), right.typ())
                    .map_err(|e| convert_unify_error(e, right.location()))?;
                Ok(ClauseGuard::LtEqFloat {
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
            let module_info = self
                .environment
                .imported_modules
                .get(&*module_alias)
                .ok_or_else(|| Error::UnknownModule {
                    name: module_alias.to_string(),
                    location: *module_location,
                    imported_modules: self
                        .environment
                        .imported_modules
                        .keys()
                        .map(std::string::ToString::to_string)
                        .collect(),
                })?;

            let constructor =
                module_info
                    .1
                    .values
                    .get(&label)
                    .ok_or_else(|| Error::UnknownModuleValue {
                        name: label.clone(),
                        location: SrcSpan {
                            start: module_location.end,
                            end: select_location.end,
                        },
                        module_name: module_info.1.name.clone(),
                        value_constructors: module_info
                            .1
                            .values
                            .keys()
                            .map(std::string::ToString::to_string)
                            .collect(),
                    })?;

            (module_info.1.name.clone(), constructor.clone())
        };

        Ok(TypedExpr::ModuleSelect {
            label,
            typ: self.instantiate(&constructor.typ, self.environment.level, &mut hashmap![]),
            location: select_location,
            module_name,
            module_alias: module_alias.to_string(),
            constructor: constructor.variant.to_module_value_constructor(),
        })
    }

    fn infer_record_access(
        &mut self,
        record: UntypedExpr,
        label: &str,
        location: SrcSpan,
    ) -> Result<TypedExpr, Error> {
        // Infer the type of the (presumed) record
        let record = self.infer(record)?;

        self.infer_known_record_access(record, label, location)
    }

    fn infer_known_record_access(
        &mut self,
        record: TypedExpr,
        label: &str,
        location: SrcSpan,
    ) -> Result<TypedExpr, Error> {
        let record = Box::new(record);

        // If we don't yet know the type of the record then we cannot use any accessors
        if record.typ().is_unbound() {
            return Err(Error::RecordAccessUnknownType {
                location: record.location(),
            });
        }

        // Error constructor helper function
        let unknown_field = |fields| Error::UnknownField {
            typ: record.typ(),
            location: SrcSpan {
                start: record.location().end,
                end: location.end,
            },
            label: label.to_owned(),
            fields,
        };

        // Check to see if it's a Type that can have accessible fields
        let accessors = match collapse_links(record.typ()).as_ref() {
            // A type in the current module which may have fields
            Type::App { module, name, .. }
                if module.as_slice() == self.environment.current_module =>
            {
                self.environment.accessors.get(name)
            }

            // A type in another module which may have fields
            Type::App { module, name, .. } => self
                .environment
                .importable_modules
                .get(&module.join("/"))
                .and_then(|module| module.1.accessors.get(name)),

            _something_without_fields => return Err(unknown_field(vec![])),
        }
        .ok_or_else(|| unknown_field(vec![]))?;

        // Find the accessor, if the type has one with the same label
        let RecordAccessor { index, label, typ } = accessors
            .accessors
            .get(label)
            .ok_or_else(|| {
                unknown_field(
                    accessors
                        .accessors
                        .keys()
                        .map(std::string::ToString::to_string)
                        .collect(),
                )
            })?
            .clone();

        // Unify the record type with the accessor's stored copy of the record type.
        // This ensure that the type parameters of the retrieved value have the correct
        // types for this instance of the record.
        let accessor_record_type = accessors.typ.clone();
        let mut type_vars = hashmap![];
        let accessor_record_type = self.instantiate(&accessor_record_type, 0, &mut type_vars);
        let typ = self.instantiate(&typ, 0, &mut type_vars);
        self.unify(accessor_record_type, record.typ())
            .map_err(|e| convert_unify_error(e, record.location()))?;

        Ok(TypedExpr::RecordAccess {
            record,
            label,
            index,
            location,
            typ,
        })
    }

    fn infer_record_update(
        &mut self,
        constructor: &UntypedExpr,
        spread: RecordUpdateSpread,
        args: &[UntypedRecordUpdateArg],
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
                })
            }
        };

        let value_constructor = self
            .environment
            .get_value_constructor(module.as_ref(), &name)
            .map_err(|e| convert_get_value_constructor_error(e, location))?
            .clone();

        if let ValueConstructor {
            variant:
                ValueConstructorVariant::Record {
                    field_map: Some(field_map),
                    ..
                },
            ..
        } = value_constructor
        {
            if let Type::Fn { retrn, .. } = value_constructor.typ.as_ref() {
                let spread = self.infer_var(spread.name, spread.location)?;
                let return_type = self.instantiate(retrn, self.environment.level, &mut hashmap![]);

                // Check that the spread variable unifies with the return type of the constructor
                self.unify(return_type, spread.typ())
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
                            let spread_field = self.infer_known_record_access(
                                spread.clone(),
                                label,
                               *location,
                            )?;

                            // Check that the update argument unifies with the corresponding
                            // field in the record contained within the spread variable. We
                            // need to check the spread, and not the constructor, in order
                            // to handle polymorphic types.
                            self.unify(spread_field.typ(), value.typ())
                                .map_err(|e| convert_unify_error(e, value.location()))?;

                            match field_map.fields.get(label) {
                                None => crate::error::fatal_compiler_bug(
                                    "Failed to lookup record field after successfully inferring that field",
                                ),
                                Some(p) => Ok(TypedRecordUpdateArg {
                                    location:*location,
                                    label: label.to_string(),
                                    value,
                                    index: *p,
                                }),
                            }
                        },
                    )
                    .collect::<Result<_, _>>()?;

                if args.is_empty() {
                    self.environment
                        .warnings
                        .push(Warning::NoFieldsRecordUpdate { location });
                }

                if args.len() == field_map.arity {
                    self.environment
                        .warnings
                        .push(Warning::AllFieldsRecordUpdate { location });
                }

                return Ok(TypedExpr::RecordUpdate {
                    location,
                    typ: spread.typ(),
                    spread: Box::new(spread),
                    args,
                });
            }
        };

        Err(Error::RecordUpdateInvalidConstructor {
            location: constructor.location(),
        })
    }

    fn infer_value_constructor(
        &mut self,
        module: &Option<String>,
        name: &str,
        location: &SrcSpan,
    ) -> Result<ValueConstructor, Error> {
        let constructor = match module {
            // Look in the local scope for a binding with this name
            None => {
                let constructor = self
                    .environment
                    .get_variable(name)
                    .or_else(|| self.environment.get_module_const(name))
                    .cloned()
                    .ok_or_else(|| Error::UnknownVariable {
                        location: *location,
                        name: name.to_string(),
                        variables: self
                            .environment
                            .local_values
                            .keys()
                            .map(std::string::ToString::to_string)
                            .collect(),
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
                self.environment.value_used(name);

                constructor
            }

            // Look in an imported module for a binding with this name
            Some(module_name) => {
                let imported_module = &self
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
                            .map(std::string::ToString::to_string)
                            .collect(),
                    })?
                    .1;
                imported_module.values.get(name).cloned().ok_or_else(|| {
                    Error::UnknownModuleValue {
                        location: *location,
                        module_name: vec![module_name.to_string()],
                        name: name.to_string(),
                        value_constructors: imported_module
                            .values
                            .keys()
                            .map(std::string::ToString::to_string)
                            .collect(),
                    }
                })?
            }
        };

        let ValueConstructor {
            public,
            variant,
            origin,
            typ,
        } = constructor;

        // Instantiate generic variables into unbound variables for this usage
        let typ = self.instantiate(&typ, self.environment.level, &mut hashmap![]);
        Ok(ValueConstructor {
            public,
            variant,
            origin,
            typ,
        })
    }

    // TODO: extract the type annotation checking into a infer_module_const
    // function that uses this function internally

    pub fn infer_const(
        &mut self,
        annotation: &Option<TypeAst>,
        value: UntypedConstant,
    ) -> Result<TypedConstant, Error> {
        let inferred = match value {
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
            } => self.infer_const_tuple(&elements, location),

            Constant::List {
                elements, location, ..
            } => self.infer_const_list(&elements, location),

            Constant::BitString { location, segments } => {
                self.infer_constant_bit_string(segments, location)
            }

            Constant::Record {
                module,
                location,
                name,
                args,
                ..
            } if args.is_empty() => {
                let constructor = self.infer_value_constructor(&module, &name, &location)?;

                let tag = match &constructor.variant {
                    ValueConstructorVariant::Record { name, .. } => name.clone(),

                    ValueConstructorVariant::ModuleFn { .. }
                    | ValueConstructorVariant::LocalVariable => {
                        return Err(Error::NonLocalClauseGuardVariable { location, name })
                    }

                    ValueConstructorVariant::ModuleConstant { literal } => {
                        return Ok(literal.clone())
                    }
                };

                Ok(Constant::Record {
                    module,
                    location,
                    name,
                    args: vec![],
                    typ: constructor.typ,
                    tag,
                })
            }

            Constant::Record {
                module,
                location,
                name,
                mut args,
                ..
            } => {
                let constructor = self.infer_value_constructor(&module, &name, &location)?;

                let tag = match &constructor.variant {
                    ValueConstructorVariant::Record { name, .. } => name.clone(),

                    ValueConstructorVariant::ModuleFn { .. }
                    | ValueConstructorVariant::LocalVariable => {
                        return Err(Error::NonLocalClauseGuardVariable { location, name })
                    }

                    ValueConstructorVariant::ModuleConstant { literal } => {
                        return Ok(literal.clone())
                    }
                };

                // Pretty much all the other infer functions operate on UntypedExpr
                // or TypedExpr rather than ClauseGuard. To make things easier we
                // build the TypedExpr equivalent of the constructor and use that
                // TODO: resvisit this. It is rather awkward at present how we
                // have to convert to this other data structure.
                let fun = match &module {
                    Some(module_name) => TypedExpr::ModuleSelect {
                        label: name.clone(),
                        module_name: self
                            .environment
                            .importable_modules
                            .get(module_name)
                            .gleam_expect("Failed to find previously located module import")
                            .1
                            .name
                            .clone(),
                        typ: constructor.typ.clone(),
                        module_alias: module_name.clone(),
                        constructor: constructor.variant.to_module_value_constructor(),
                        location,
                    },

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
                    match_fun_type(fun.typ(), args.len(), self.environment)
                        .map_err(|e| convert_not_fun_error(e, fun.location(), location))?;
                let args = args_types
                    .iter_mut()
                    .zip(args)
                    .map(|(typ, arg): (&mut Arc<Type>, _)| {
                        let CallArg {
                            label,
                            value,
                            location,
                        } = arg;
                        let value = self.infer_const(&None, value)?;
                        self.unify(typ.clone(), value.typ())
                            .map_err(|e| convert_unify_error(e, value.location()))?;
                        Ok(CallArg {
                            label,
                            value,
                            location,
                        })
                    })
                    .collect::<Result<_, _>>()?;

                Ok(Constant::Record {
                    module,
                    location,
                    name,
                    args,
                    typ: return_type,
                    tag,
                })
            }
        }?;

        // Check type annotation is accurate.
        if let Some(ann) = annotation {
            let const_ann = self.type_from_ast(ann)?;
            self.unify(const_ann, inferred.typ())
                .map_err(|e| convert_unify_error(e, inferred.location()))?;
        };

        Ok(inferred)
    }

    fn infer_const_tuple(
        &mut self,
        untyped_elements: &[UntypedConstant],
        location: SrcSpan,
    ) -> Result<TypedConstant, Error> {
        let mut elements = Vec::with_capacity(untyped_elements.len());

        for element in untyped_elements {
            let element = self.infer_const(&None, element.clone())?;
            elements.push(element);
        }

        Ok(Constant::Tuple { elements, location })
    }

    fn infer_const_list(
        &mut self,
        untyped_elements: &[UntypedConstant],
        location: SrcSpan,
    ) -> Result<TypedConstant, Error> {
        let typ = self.new_unbound_var(0);
        let mut elements = Vec::with_capacity(untyped_elements.len());

        for element in untyped_elements {
            let element = self.infer_const(&None, element.clone())?;
            self.unify(typ.clone(), element.typ())
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
    ) -> Result<Option<&FieldMap>, GetValueConstructorError> {
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
    ) -> Result<(TypedExpr, Vec<TypedCallArg>, Arc<Type>), Error> {
        let fun = self.infer(fun)?;
        let (fun, args, typ) = self.do_infer_call_with_known_fun(fun, args, location)?;
        Ok((fun, args, typ))
    }

    pub fn do_infer_call_with_known_fun(
        &mut self,
        fun: TypedExpr,
        mut args: Vec<CallArg<UntypedExpr>>,
        location: SrcSpan,
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
        let (mut args_types, return_type) = match_fun_type(fun.typ(), args.len(), self.environment)
            .map_err(|e| convert_not_fun_error(e, fun.location(), location))?;

        // Ensure that the given args have the correct types
        let args = args_types
            .iter_mut()
            .zip(args)
            .map(|(typ, arg): (&mut Arc<Type>, _)| {
                let CallArg {
                    label,
                    value,
                    location,
                } = arg;
                let value = self.infer(value)?;
                self.unify(typ.clone(), value.typ())
                    .map_err(|e| convert_unify_error(e, value.location()))?;
                Ok(CallArg {
                    label,
                    value,
                    location,
                })
            })
            .collect::<Result<_, _>>()?;
        Ok((fun, args, return_type))
    }

    pub fn do_infer_fn(
        &mut self,
        args: Vec<UntypedArg>,
        body: UntypedExpr,
        return_annotation: &Option<TypeAst>,
    ) -> Result<(Vec<TypedArg>, TypedExpr), Error> {
        // Construct an initial type for each argument of the function- either an unbound
        // type variable or a type provided by an annotation.
        let args: Vec<_> = args
            .into_iter()
            .map(|arg| self.infer_arg(arg))
            .collect::<Result<_, _>>()?;

        let return_type = match return_annotation {
            Some(ann) => Some(self.type_from_ast(ann)?),
            None => None,
        };

        self.infer_fn_with_known_types(args, body, return_type)
    }

    pub fn infer_fn_with_known_types(
        &mut self,
        args: Vec<TypedArg>,
        body: UntypedExpr,
        return_type: Option<Arc<Type>>,
    ) -> Result<(Vec<TypedArg>, TypedExpr), Error> {
        let body = self.in_new_scope(|body_typer| {
            for (arg, t) in args.iter().zip(args.iter().map(|arg| arg.typ.clone())) {
                match &arg.names {
                    ArgNames::Named { name } | ArgNames::NamedLabelled { name, .. } => {
                        body_typer.environment.insert_variable(
                            name.to_string(),
                            ValueConstructorVariant::LocalVariable,
                            t,
                        )
                    }
                    ArgNames::Discard { .. } | ArgNames::LabelledDiscard { .. } => (),
                };
            }

            body_typer.infer(body)
        })?;

        // Check that any return type type is accurate.
        if let Some(return_type) = return_type {
            self.unify(return_type, body.typ())
                .map_err(|e| e.return_annotation_mismatch().into_error(body.location()))?;
        }

        Ok((args, body))
    }
}
