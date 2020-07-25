use super::*;
use crate::ast::{
    Arg, BinOp, BindingKind, BitStringSegment, BitStringSegmentOption, CallArg, Clause,
    ClauseGuard, Constant, HasLocation, RecordUpdateSpread, SrcSpan, TypeAst, TypedArg,
    TypedClause, TypedClauseGuard, TypedConstant, TypedExpr, TypedMultiPattern, UntypedArg,
    UntypedClause, UntypedClauseGuard, UntypedConstant, UntypedConstantBitStringSegment,
    UntypedExpr, UntypedExprBitStringSegment, UntypedMultiPattern, UntypedPattern,
};

pub struct ExprTyper<'a> {
    environment: &'a mut Environment<'a>,

    // Type hydrator for creating types from annotations
    hydrator: Hydrator,
}

impl<'a> Typer for ExprTyper<'a> {
    fn get_environment(&mut self) -> &mut Environment {
        &mut self.environment
    }
}

impl<'a> ExprTyper<'a> {
    pub fn new(environment: &'a mut Environment<'a>) -> Self {
        Self {
            hydrator: Hydrator::new(),
            environment,
        }
    }

    fn reset_hydrator(&mut self) {
        self.hydrator = Hydrator::new();
    }

    pub fn type_from_ast(&mut self, ast: &TypeAst) -> Result<Arc<Type>, Error> {
        self.hydrator.type_from_ast(ast, self.environment)
    }

    /// Crawl the AST, annotating each node with the inferred type or
    /// returning an error.
    ///
    pub fn infer(&mut self, expr: UntypedExpr) -> Result<TypedExpr, Error> {
        match expr {
            UntypedExpr::ListNil { location, .. } => self.infer_nil(location),

            UntypedExpr::Todo {
                location, label, ..
            } => self.infer_todo(location, label),

            UntypedExpr::Var { location, name, .. } => self.infer_var(name, location),

            UntypedExpr::Int {
                location, value, ..
            } => self.infer_int(value, location),

            UntypedExpr::Seq { first, then, .. } => self.infer_seq(*first, *then),

            UntypedExpr::Tuple {
                location, elems, ..
            } => self.infer_tuple(elems, location),

            UntypedExpr::Float {
                location, value, ..
            } => self.infer_float(value, location),

            UntypedExpr::String {
                location, value, ..
            } => self.infer_string(value, location),

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
            } => self.infer_case(subjects, clauses, location),

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
            } => self.infer_record_update(*constructor, spread, args, location),
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
            UntypedExpr::Call { fun, args, .. } => {
                let fun = self.infer(*fun)?;
                match fun.typ().fn_arity() {
                    // Rewrite as right(left, ..args)
                    Some(arity) if arity == args.len() + 1 => {
                        self.infer_insert_pipe(fun, args, left)
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
        let (fun, args, typ) = self.do_infer_call_with_known_fun(fun, args, &location)?;
        let fun = TypedExpr::Call {
            location: location.clone(),
            typ,
            args,
            fun: Box::new(fun),
        };
        let args = vec![CallArg {
            label: None,
            location: left.location().clone(),
            value: left,
        }];
        let (fun, args, typ) = self.do_infer_call_with_known_fun(fun, args, &location)?;
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
    ) -> Result<TypedExpr, Error> {
        let location = left.location().clone();
        let mut new_args = Vec::with_capacity(args.len() + 1);
        new_args.push(CallArg {
            label: None,
            location: left.location().clone(),
            value: left,
        });
        for arg in args {
            new_args.push(arg.clone());
        }

        let (fun, args, typ) = self.do_infer_call_with_known_fun(fun, new_args, &location)?;
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
        let typ = self.new_unbound_var(self.level);
        let fn_typ = Arc::new(Type::Fn {
            args: vec![left.typ()],
            retrn: typ.clone(),
        });
        self.unify(right.typ(), fn_typ)
            .map_err(|e| convert_unify_error(e, &location))?;

        Ok(TypedExpr::Pipe {
            location,
            typ,
            right,
            left,
        })
    }

    fn infer_nil(&mut self, location: SrcSpan) -> Result<TypedExpr, Error> {
        Ok(TypedExpr::ListNil {
            location,
            typ: list(self.new_unbound_var(self.level)),
        })
    }

    fn infer_todo(&mut self, location: SrcSpan, label: Option<String>) -> Result<TypedExpr, Error> {
        self.warnings.push(Warning::Todo {
            location: location.clone(),
        });

        Ok(TypedExpr::Todo {
            location,
            label,
            typ: self.new_unbound_var(self.level),
        })
    }

    fn infer_string(&mut self, value: String, location: SrcSpan) -> Result<TypedExpr, Error> {
        Ok(TypedExpr::String {
            location,
            value,
            typ: string(),
        })
    }

    fn infer_int(&mut self, value: String, location: SrcSpan) -> Result<TypedExpr, Error> {
        Ok(TypedExpr::Int {
            location,
            value,
            typ: int(),
        })
    }

    fn infer_float(&mut self, value: String, location: SrcSpan) -> Result<TypedExpr, Error> {
        Ok(TypedExpr::Float {
            location,
            value,
            typ: float(),
        })
    }

    fn infer_seq(&mut self, first: UntypedExpr, then: UntypedExpr) -> Result<TypedExpr, Error> {
        let first = self.infer(first)?;
        let then = self.infer(then)?;

        match first.typ().as_ref() {
            typ if typ.is_result() => {
                self.warnings.push(Warning::ImplicitlyDiscardedResult {
                    location: first.location().clone(),
                });
            }

            _ => {}
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
            .unwrap_or_else(|| Ok(self.new_unbound_var(self.level)))?;
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
        let (fun, args, typ) = self.do_infer_call(fun, args, &location)?;
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
            .map_err(|e| convert_unify_error(e, &location))?;

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
        let constructor = self.infer_value_constructor(&name, &location)?;
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
        match container {
            UntypedExpr::Var { name, location, .. } if !self.local_values.contains_key(&name) => {
                self.infer_module_access(name.as_ref(), label, &location, access_location)
            }

            _ => self.infer_record_access(container, label, access_location),
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
                    .get(index as usize)
                    .ok_or_else(|| Error::OutOfBoundsTupleIndex {
                        location: location.clone(),
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
                location: tuple.location().clone(),
            }),

            _ => Err(Error::NotATuple {
                location: tuple.location().clone(),
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
            .map(|s| {
                self.infer_bit_segment(*s.value, s.options, s.location, |env, expr| env.infer(expr))
            })
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
        let typ = type_specifier.construction_typ().unwrap_or_else(|| int());

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
            BinOp::ModuloInt => (int(), int()),
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

        let try_value_type = self.new_unbound_var(self.level);
        let try_error_type = self.new_unbound_var(self.level);

        let value_typ = match kind {
            // Ensure that the value is a result if this is a `try` binding
            BindingKind::Try => {
                let v = try_value_type.clone();
                let e = try_error_type.clone();
                self.unify(result(v, e), value.typ())
                    .map_err(|e| convert_unify_error(e, value.location()))?;
                try_value_type.clone()
            }
            _ => value.typ(),
        };

        let value_typ = generalise(value_typ, self.level + 1);

        // Ensure the pattern matches the type of the value
        let pattern =
            pattern::PatternTyper::new(self, self.level).unify(pattern, value_typ.clone())?;

        // Check the type of the following code
        let then = self.infer(then)?;
        let typ = then.typ();

        // Ensure that a Result with the right error type is returned for `try`
        if kind == BindingKind::Try {
            let value = self.new_unbound_var(self.level);
            self.unify(result(value, try_error_type), typ.clone())
                .map_err(|e| convert_unify_error(e, then.try_binding_location()))?;
        }

        // Check that any type annotation is accurate.
        if let Some(ann) = annotation {
            let ann_typ = self
                .type_from_ast(ann)
                .map(|t| self.instantiate(t, self.level, &mut hashmap![]))?;
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
        subjects: Vec<UntypedExpr>,
        clauses: Vec<UntypedClause>,
        location: SrcSpan,
    ) -> Result<TypedExpr, Error> {
        let subjects_count = subjects.len();
        let mut typed_subjects = Vec::with_capacity(subjects_count);
        let mut subject_types = Vec::with_capacity(subjects_count);
        let mut typed_clauses = Vec::with_capacity(clauses.len());

        let return_type = self.new_unbound_var(self.level);

        for subject in subjects.into_iter() {
            let (subject, subject_type) = self.in_new_scope(|subject_typer| {
                let subject = subject_typer.infer(subject)?;
                let subject_type = generalise(subject.typ(), subject_typer.level);

                Ok((subject, subject_type))
            })?;

            typed_subjects.push(subject);
            subject_types.push(subject_type);
        }

        for clause in clauses.into_iter() {
            let typed_clause = self.infer_clause(clause, &subject_types)?;
            self.unify(return_type.clone(), typed_clause.then.typ())
                .map_err(|e| convert_unify_error(e, typed_clause.then.location()))?;
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
        let mut pattern_typer = pattern::PatternTyper::new(self, self.level);
        let typed_pattern = pattern_typer.infer_multi_pattern(pattern, subjects, &location)?;

        // Each case clause has one or more patterns that may match the
        // subject in order for the clause to be selected, so we must type
        // check every pattern.
        let mut typed_alternatives = Vec::with_capacity(alternatives.len());
        for m in alternatives {
            typed_alternatives
                .push(pattern_typer.infer_alternative_multi_pattern(m, subjects, &location)?);
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
                let constructor = self.infer_value_constructor(&name, &location)?;

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
                    .map_err(|e| convert_unify_error(e, &location))?;
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
                    .map_err(|e| convert_unify_error(e, &location))?;
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
            let module_info =
                self.imported_modules
                    .get(&*module_alias)
                    .ok_or_else(|| Error::UnknownModule {
                        name: module_alias.to_string(),
                        location: module_location.clone(),
                        imported_modules: self
                            .imported_modules
                            .keys()
                            .map(|t| t.to_string())
                            .collect(),
                    })?;

            let constructor =
                module_info
                    .1
                    .values
                    .get(&label)
                    .ok_or_else(|| Error::UnknownModuleValue {
                        name: label.clone(),
                        location: select_location.clone(),
                        module_name: module_info.1.name.clone(),
                        value_constructors: module_info
                            .1
                            .values
                            .keys()
                            .map(|t| t.to_string())
                            .collect(),
                    })?;

            (module_info.1.name.clone(), constructor.clone())
        };

        Ok(TypedExpr::ModuleSelect {
            label,
            typ: self.instantiate(constructor.typ, self.level, &mut hashmap![]),
            location: select_location,
            module_name,
            module_alias: module_alias.to_string(),
            constructor: constructor.variant.to_module_value_constructor(),
        })
    }

    fn infer_record_access(
        &mut self,
        record: UntypedExpr,
        label: String,
        location: SrcSpan,
    ) -> Result<TypedExpr, Error> {
        // Infer the type of the (presumed) record
        let record = self.infer(record)?;

        self.infer_known_record_access(record, label, location)
    }

    fn infer_known_record_access(
        &mut self,
        record: TypedExpr,
        label: String,
        location: SrcSpan,
    ) -> Result<TypedExpr, Error> {
        let record = Box::new(record);

        // If we don't yet know the type of the record then we cannot use any accessors
        if record.typ().is_unbound() {
            return Err(Error::RecordAccessUnknownType {
                location: record.location().clone(),
            });
        }

        // Error constructor helper function
        let unknown_field = |fields| Error::UnknownField {
            typ: record.typ(),
            location: location.clone(),
            label: label.clone(),
            fields,
        };

        // Check to see if it's a Type that can have accessible fields
        let accessors = match collapse_links(record.typ()).as_ref() {
            // A type in the current module which may have fields
            Type::App { module, name, .. } if module.as_slice() == self.current_module => {
                self.accessors.get(name)
            }

            // A type in another module which may have fields
            Type::App { module, name, .. } => self
                .importable_modules
                .get(&module.join("/"))
                .and_then(|module| module.1.accessors.get(name)),

            _something_without_fields => return Err(unknown_field(vec![])),
        }
        .ok_or_else(|| unknown_field(vec![]))?;

        // Find the accessor, if the type has one with the same label
        let RecordAccessor {
            index, label, typ, ..
        } = accessors
            .accessors
            .get(&label)
            .ok_or_else(|| {
                unknown_field(accessors.accessors.keys().map(|t| t.to_string()).collect())
            })?
            .clone();

        // Unify the record type with the accessor's stored copy of the record type.
        // This ensure that the type parameters of the retrieved value have the correct
        // types for this instance of the record.
        let accessor_record_type = accessors.typ.clone();
        let mut type_vars = hashmap![];
        let accessor_record_type = self.instantiate(accessor_record_type, 0, &mut type_vars);
        let typ = self.instantiate(typ, 0, &mut type_vars);
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
                    location: constructor.location().clone(),
                })
            }
        };

        let value_constructor = self
            .environment
            .get_value_constructor(module.as_ref(), &name)
            .map_err(|e| convert_get_value_constructor_error(e, &location))?
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
                let return_type =
                    self.instantiate(retrn.clone(), self.environment.level, &mut hashmap![]);

                // Check that the spread variable unifies with the return type of the constructor
                self.unify(return_type.clone(), spread.typ())
                    .map_err(|e| convert_unify_error(e, spread.location()))?;

                let args: Vec<TypedRecordUpdateArg> = args
                    .iter()
                    .map(
                        |UntypedRecordUpdateArg {
                             label,
                             value,
                             location,
                             ..
                         }| {
                            let value = self.infer(value.clone())?;
                            let spread_field = self.infer_known_record_access(
                                spread.clone(),
                                label.to_string(),
                                location.clone(),
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
                                    location: location.clone(),
                                    label: label.to_string(),
                                    value,
                                    index: *p,
                                }),
                            }
                        },
                    )
                    .collect::<Result<_, _>>()?;

                return Ok(TypedExpr::RecordUpdate {
                    location,
                    typ: spread.typ(),
                    spread: Box::new(spread),
                    args,
                });
            }
        };

        Err(Error::RecordUpdateInvalidConstructor {
            location: constructor.location().clone(),
        })
    }

    fn infer_value_constructor(
        &mut self,
        name: &str,
        location: &SrcSpan,
    ) -> Result<ValueConstructor, Error> {
        let ValueConstructor {
            public,
            variant,
            origin,
            typ,
        } = self
            .get_variable(name)
            .or_else(|| self.get_module_const(name))
            .cloned()
            .ok_or_else(|| Error::UnknownVariable {
                location: location.clone(),
                name: name.to_string(),
                variables: self.local_values.keys().map(|t| t.to_string()).collect(),
            })?;
        let typ = self.instantiate(typ, self.level, &mut hashmap![]);
        Ok(ValueConstructor {
            public,
            variant,
            origin,
            typ,
        })
    }

    // TODO: extract the type annotation checking into a infer_module_const
    // function that uses this function internally
    fn infer_const(
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
            } => self.infer_const_tuple(elements, location),

            Constant::List {
                elements, location, ..
            } => self.infer_const_list(elements, location),

            Constant::BitString { location, segments } => {
                self.infer_constant_bit_string(segments, location)
            }

            Constant::Record {
                module,
                location,
                name,
                mut args,
                ..
            } => {
                let constructor = self.infer_value_constructor(&name, &location)?;

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
                let fun = TypedExpr::Var {
                    constructor,
                    location: location.clone(),
                    name: name.clone(),
                };

                // This is basically the same code as do_infer_call_with_known_fun()
                // except the args are typed with infer_clause_guard() here.
                // This duplication is a bit awkward but it works!
                // Potentially this could be improved later
                match self
                    .get_field_map(&fun)
                    .map_err(|e| convert_get_value_constructor_error(e, &location))?
                {
                    // The fun has a field map so labelled arguments may be present and need to be reordered.
                    Some(field_map) => field_map.reorder(&mut args, &location)?,

                    // The fun has no field map and so we error if arguments have been labelled
                    None => assert_no_labelled_arguments(&args)?,
                }

                let (mut args_types, return_type) = match_fun_type(fun.typ(), args.len(), self)
                    .map_err(|e| convert_not_fun_error(e, fun.location(), &location))?;
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
            let const_ann = self.type_from_ast(&ann)?;
            self.unify(const_ann, inferred.typ())
                .map_err(|e| convert_unify_error(e, inferred.location()))?;
        };

        Ok(inferred)
    }

    fn infer_const_tuple(
        &mut self,
        untyped_elements: Vec<UntypedConstant>,
        location: SrcSpan,
    ) -> Result<TypedConstant, Error> {
        let mut elements = Vec::with_capacity(untyped_elements.len());

        for element in untyped_elements.into_iter() {
            let element = self.infer_const(&None, element)?;
            elements.push(element);
        }

        Ok(Constant::Tuple { elements, location })
    }

    fn infer_const_list(
        &mut self,
        untyped_elements: Vec<UntypedConstant>,
        location: SrcSpan,
    ) -> Result<TypedConstant, Error> {
        let typ = self.new_unbound_var(0);
        let mut elements = Vec::with_capacity(untyped_elements.len());

        for element in untyped_elements.into_iter() {
            let element = self.infer_const(&None, element)?;
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

        Ok(self.get_value_constructor(module, name)?.field_map())
    }
}
