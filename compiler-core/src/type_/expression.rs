use itertools::Itertools;
use vec1::Vec1;

use super::{pipe::PipeTyper, *};
use crate::{
    analyse::infer_bit_string_segment_option,
    ast::{
        Arg, Assignment, AssignmentKind, BinOp, BitStringSegment, BitStringSegmentOption, CallArg,
        Clause, ClauseGuard, Constant, HasLocation, RecordUpdateSpread, SrcSpan, Statement,
        TodoKind, TypeAst, TypedArg, TypedClause, TypedClauseGuard, TypedConstant, TypedExpr,
        TypedMultiPattern, TypedStatement, UntypedArg, UntypedClause, UntypedClauseGuard,
        UntypedConstant, UntypedConstantBitStringSegment, UntypedExpr, UntypedExprBitStringSegment,
        UntypedMultiPattern, UntypedPattern, UntypedStatement, Use, USE_ASSIGNMENT_VARIABLE,
    },
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

    pub fn type_from_ast(&mut self, ast: &TypeAst) -> Result<Arc<Type>, Error> {
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
    pub fn infer_statements(
        &mut self,
        statements: Vec1<UntypedStatement>,
    ) -> Result<Vec1<TypedStatement>, Error> {
        // TODO: it
        todo!("infer_statements")
    }

    fn infer(&mut self, expr: UntypedExpr) -> Result<TypedExpr, Error> {
        match expr {
            UntypedExpr::Todo {
                location,
                label,
                kind,
                ..
            } => Ok(self.infer_todo(location, kind, label)),

            UntypedExpr::Panic { location } => Ok(self.infer_panic(location)),

            UntypedExpr::Var { location, name, .. } => self.infer_var(name, location),

            UntypedExpr::Int {
                location, value, ..
            } => Ok(self.infer_int(value, location)),

            UntypedExpr::Block {
                expressions,
                location,
            } => self.infer_seq(location, expressions),

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

            UntypedExpr::Assignment {
                location,
                pattern,
                value,
                kind,
                annotation,
                ..
            } => self.infer_assignment(pattern, *value, kind, &annotation, location),

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

            UntypedExpr::NegateBool { location, value } => self.infer_negate_bool(location, *value),

            UntypedExpr::NegateInt { location, value } => self.infer_negate_int(location, *value),

            UntypedExpr::Use(use_) => {
                let location = use_.location;
                self.infer_use(use_, location, vec![])
            }
        }
    }

    fn infer_pipeline(&mut self, expressions: Vec1<UntypedExpr>) -> Result<TypedExpr, Error> {
        PipeTyper::infer(self, expressions)
    }

    fn infer_todo(
        &mut self,
        location: SrcSpan,
        kind: TodoKind,
        label: Option<SmolStr>,
    ) -> TypedExpr {
        let typ = self.new_unbound_var();
        self.environment.warnings.emit(Warning::Todo {
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

    fn infer_panic(&mut self, location: SrcSpan) -> TypedExpr {
        let typ = self.new_unbound_var();
        TypedExpr::Panic { location, typ }
    }

    fn infer_string(&mut self, value: SmolStr, location: SrcSpan) -> TypedExpr {
        TypedExpr::String {
            location,
            value,
            typ: string(),
        }
    }

    fn infer_int(&mut self, value: SmolStr, location: SrcSpan) -> TypedExpr {
        TypedExpr::Int {
            location,
            value,
            typ: int(),
        }
    }

    fn infer_float(&mut self, value: SmolStr, location: SrcSpan) -> TypedExpr {
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
        }
        if discarded.type_().is_result() && !discarded.is_assignment() {
            self.environment
                .warnings
                .emit(Warning::ImplicitlyDiscardedResult {
                    location: discarded.location(),
                });
        }
    }

    fn infer_seq(
        &mut self,
        location: SrcSpan,
        untyped: Vec<UntypedExpr>,
    ) -> Result<TypedExpr, Error> {
        let count = untyped.len();
        let untyped = untyped.into_iter();
        self.infer_iter_seq(location, count, untyped)
    }

    fn infer_iter_seq<Exprs: Iterator<Item = UntypedExpr>>(
        &mut self,
        location: SrcSpan,
        count: usize,
        mut untyped: Exprs,
    ) -> Result<TypedExpr, Error> {
        let mut i = 0;
        let mut expressions = Vec::with_capacity(count);

        while let Some(expression) = untyped.next() {
            i += 1;

            // Special case for `use` expressions, which need the remaining
            // expressions in the sequence to be passed to them during as an
            // implicit anonymous function.
            if let UntypedExpr::Use(use_) = expression {
                let expression = self.infer_use(use_, location, untyped.collect())?;
                expressions.push(expression);
                break; // Inferring the use has consumed the rest of the exprs
            }

            let expression = self.infer(expression)?;
            // This isn't the final expression in the sequence, so call the
            // `expression_discarded` function to see if anything is being
            // discarded that we think shouldn't be.
            if i < count {
                self.expression_discarded(&expression);
            }
            expressions.push(expression);
        }

        Ok(TypedExpr::Block {
            location,
            expressions,
        })
    }

    fn infer_use(
        &mut self,
        use_: Use,
        sequence_location: SrcSpan,
        mut following_expressions: Vec<UntypedExpr>,
    ) -> Result<TypedExpr, Error> {
        let mut call = get_use_expression_call(*use_.call)?;
        let assignments = UseAssignments::from_use_expression(use_.assignments);

        let mut expressions = assignments.body_assignments;

        if following_expressions.is_empty() {
            let todo = UntypedExpr::Todo {
                location: use_.location,
                label: None,
                kind: TodoKind::IncompleteUse,
            };
            expressions.push(todo);
        } else {
            expressions.append(&mut following_expressions);
        }

        let first = expressions
            .get(0)
            .expect("This is safe, a default todo was set above")
            .location();

        // Collect the following expressions into a function to be passed as a
        // callback to the use's call function.
        let callback = UntypedExpr::Fn {
            arguments: assignments.function_arguments,
            location: SrcSpan::new(first.start, sequence_location.end),
            return_annotation: None,
            is_capture: false,
            body: expressions,
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

        self.infer(UntypedExpr::Call {
            location: SrcSpan::new(use_.location.start, sequence_location.end),
            fun: call.function,
            arguments: call.arguments,
        })
    }

    fn infer_negate_bool(
        &mut self,
        location: SrcSpan,
        value: UntypedExpr,
    ) -> Result<TypedExpr, Error> {
        let value = self.infer(value)?;

        unify(bool(), value.type_()).map_err(|e| convert_unify_error(e, value.location()))?;

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
        let typ = fn_(args_types, body.type_());
        Ok(TypedExpr::Fn {
            location,
            typ,
            is_capture,
            args,
            body: Box::new(body),
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
    ) -> Result<TypedExpr, Error> {
        let (fun, args, typ) = self.do_infer_call(fun, args, location)?;
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
                unify(typ.clone(), element.type_())
                    .map_err(|e| convert_unify_error(e, location))?;
                Ok(element)
            })
            .try_collect()?;
        // Type check the ..tail, if there is one
        let typ = list(typ);
        let tail = match tail {
            Some(tail) => {
                let tail = self.infer(*tail)?;
                // Ensure the tail has the same type as the preceeding elements
                unify(typ.clone(), tail.type_()).map_err(|e| convert_unify_error(e, location))?;
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

    fn infer_var(&mut self, name: SmolStr, location: SrcSpan) -> Result<TypedExpr, Error> {
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
        label: SmolStr,
        access_location: SrcSpan,
        usage: FieldAccessUsage,
    ) -> Result<TypedExpr, Error> {
        // Attempt to infer the container as a record access. If that fails, we may be shadowing the name
        // of an imported module, so attempt to infer the container as a module access.
        // TODO: Remove this cloning
        match self.infer_record_access(container.clone(), label.clone(), access_location, usage) {
            Ok(record_access) => Ok(record_access),
            Err(err) => match container {
                UntypedExpr::Var { name, location, .. } => {
                    let module_access =
                        self.infer_module_access(&name, label, &location, access_location);

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
            .try_collect()?;

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
            .try_collect()?;

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
        TypedValue: HasType + HasLocation + Clone + bit_string::GetLiteralValue,
    {
        let value = infer(self, value)?;

        let infer_option = |segment_option: BitStringSegmentOption<UntypedValue>| {
            infer_bit_string_segment_option(segment_option, |value, typ| {
                let typed_value = infer(self, value)?;
                unify(typ, typed_value.type_())
                    .map_err(|e| convert_unify_error(e, typed_value.location()))?;
                Ok(typed_value)
            })
        };

        let options: Vec<_> = options.into_iter().map(infer_option).try_collect()?;

        let typ = crate::bit_string::type_options_for_value(&options).map_err(|error| {
            Error::BitStringSegmentError {
                error: error.error,
                location: error.location,
            }
        })?;

        unify(typ.clone(), value.type_()).map_err(|e| convert_unify_error(e, value.location()))?;

        Ok(BitStringSegment {
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

        Ok(TypedExpr::BinOp {
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
    ) -> Result<TypedExpr, Error> {
        let value = self.in_new_scope(|value_typer| value_typer.infer(value))?;
        let value_typ = value.type_();

        // Ensure the pattern matches the type of the value
        let pattern = pattern::PatternTyper::new(self.environment, &self.hydrator)
            .unify(pattern, value_typ.clone())?;

        // Check that any type annotation is accurate.
        if let Some(ann) = annotation {
            let ann_typ = self
                .type_from_ast(ann)
                .map(|t| self.instantiate(t, &mut hashmap![]))?;
            unify(ann_typ, value_typ.clone())
                .map_err(|e| convert_unify_error(e, value.type_defining_location()))?;
        }

        // We currently only do only limited exhaustiveness checking of custom types
        // at the top level of patterns.
        // Do not perform exhaustiveness checking if user explicitly used `let assert ... = ...`.
        if kind.performs_exhaustiveness_check() {
            if let Err(unmatched) = self
                .environment
                .check_exhaustiveness(vec![pattern.clone()], collapse_links(value_typ.clone()))
            {
                return Err(Error::NotExhaustivePatternMatch {
                    location,
                    unmatched,
                    kind: PatternMatchKind::Assignment,
                });
            }
        }

        Ok(TypedExpr::Assignment {
            location,
            typ: value_typ,
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

        if let Err(unmatched) =
            self.check_case_exhaustiveness(subjects_count, &subject_types, &typed_clauses)
        {
            return Err(Error::NotExhaustivePatternMatch {
                location,
                unmatched,
                kind: PatternMatchKind::Case,
            });
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

                    ValueConstructorVariant::ModuleConstant { literal, .. } => {
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
                self.infer_const(&None, constant).map(ClauseGuard::Constant)
            }
        }
    }

    fn infer_module_access(
        &mut self,
        module_alias: &SmolStr,
        label: SmolStr,
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
                    .values
                    .get(&label)
                    .ok_or_else(|| Error::UnknownModuleValue {
                        name: label.clone(),
                        location: SrcSpan {
                            start: module_location.end,
                            end: select_location.end,
                        },
                        module_name: module.name.clone(),
                        value_constructors: module.values.keys().cloned().collect(),
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
            module_name,
            module_alias: module_alias.clone(),
            constructor,
        })
    }

    fn infer_record_access(
        &mut self,
        record: UntypedExpr,
        label: SmolStr,
        location: SrcSpan,
        usage: FieldAccessUsage,
    ) -> Result<TypedExpr, Error> {
        // Infer the type of the (presumed) record
        let record = self.infer(record)?;

        self.infer_known_record_access(record, label, location, usage)
    }

    fn infer_known_record_access(
        &mut self,
        record: TypedExpr,
        label: SmolStr,
        location: SrcSpan,
        usage: FieldAccessUsage,
    ) -> Result<TypedExpr, Error> {
        let record = Box::new(record);

        // If we don't yet know the type of the record then we cannot use any accessors
        if record.type_().is_unbound() {
            return Err(Error::RecordAccessUnknownType {
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
                self.environment.accessors.get(name)
            }

            // A type in another module which may have fields
            Type::App { module, name, .. } => self
                .environment
                .importable_modules
                .get(module)
                .and_then(|module| module.accessors.get(name)),

            _something_without_fields => return Err(unknown_field(vec![])),
        }
        .ok_or_else(|| unknown_field(vec![]))?;

        // Find the accessor, if the type has one with the same label
        let RecordAccessor {
            index,
            label,
            type_: typ,
        } = accessors
            .accessors
            .get(&label)
            .ok_or_else(|| unknown_field(accessors.accessors.keys().cloned().collect()))?
            .clone();

        // Unify the record type with the accessor's stored copy of the record type.
        // This ensure that the type parameters of the retrieved value have the correct
        // types for this instance of the record.
        let accessor_record_type = accessors.type_.clone();
        let mut type_vars = hashmap![];
        let accessor_record_type = self.instantiate(accessor_record_type, &mut type_vars);
        let typ = self.instantiate(typ, &mut type_vars);
        unify(accessor_record_type, record.type_())
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
                    let spread_field = self.infer_known_record_access(
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
        module: &Option<SmolStr>,
        name: &SmolStr,
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
                        value_constructors: module.values.keys().cloned().collect(),
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

    // TODO: extract the type annotation checking into a crate::analyse::infer_module_const
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
                args,
                // field_map, is always None here because untyped not yet unified
                ..
            } if args.is_empty() => {
                // Register the module as having been used if it was imported
                if let Some(ref module) = &module {
                    _ = self.environment.unused_modules.remove(module);
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
                    ValueConstructorVariant::ModuleConstant { literal, .. } => {
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
                    ValueConstructorVariant::ModuleConstant { literal, .. } => {
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
                    match_fun_type(fun.type_(), args.len(), self.environment)
                        .map_err(|e| convert_not_fun_error(e, fun.location(), location))?;
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
                        let value = self.infer_const(&None, value)?;
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
                }

                // Infer the type of this constant
                let constructor = self.infer_value_constructor(&module, &name, &location)?;
                match constructor.variant {
                    ValueConstructorVariant::ModuleConstant { .. }
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
        }?;

        // Check type annotation is accurate.
        if let Some(ann) = annotation {
            let const_ann = self.type_from_ast(ann)?;
            unify(const_ann, inferred.type_())
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

        for element in untyped_elements {
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
        let typ = self.new_unbound_var();
        let mut elements = Vec::with_capacity(untyped_elements.len());

        for element in untyped_elements {
            let element = self.infer_const(&None, element)?;
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
            } => (Some(SmolStr::from(module_alias.as_str())), label),

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
    ) -> Result<(TypedExpr, Vec<TypedCallArg>, Arc<Type>), Error> {
        let fun = match fun {
            UntypedExpr::FieldAccess {
                location,
                label,
                container,
            } => self.infer_field_access(*container, label, location, FieldAccessUsage::MethodCall),

            fun => self.infer(fun),
        }?;

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
        let (mut args_types, return_type) =
            match_fun_type(fun.type_(), args.len(), self.environment)
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
                    implicit,
                } = arg;
                let value = self.infer_call_argument(value, typ.clone())?;
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

        unify(typ, value.type_()).map_err(|e| convert_unify_error(e, value.location()))?;
        Ok(value)
    }

    pub fn do_infer_fn(
        &mut self,
        args: Vec<UntypedArg>,
        expected_args: &[Arc<Type>],
        body: Vec1<UntypedStatement>,
        return_annotation: &Option<TypeAst>,
    ) -> Result<(Vec<TypedArg>, Vec1<TypedExpr>), Error> {
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
        let (body_rigid_names, body_infer) = self.in_new_scope(|body_typer| {
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

                        // Register the variable in the usage tracker so that we
                        // can identify if it is unused
                        body_typer.environment.init_usage(
                            name.clone(),
                            EntityKind::Variable,
                            arg.location,
                        );
                    }
                    ArgNames::Discard { .. } | ArgNames::LabelledDiscard { .. } => (),
                };
            }

            let body = body_typer.infer_statements(body);
            Ok((body_typer.hydrator.rigid_names(), body))
        })?;

        let body = body_infer.map_err(|e| e.with_unify_error_rigid_names(&body_rigid_names))?;

        // Check that any return type is accurate.
        if let Some(return_type) = return_type {
            unify(return_type, body.last().type_()).map_err(|e| {
                e.return_annotation_mismatch()
                    .into_error(body.last().type_defining_location())
                    .with_unify_error_rigid_names(&body_rigid_names)
            })?;
        }

        Ok((args, body))
    }

    fn check_case_exhaustiveness(
        &mut self,
        subjects_count: usize,
        subjects: &[Arc<Type>],
        typed_clauses: &[Clause<TypedExpr, Arc<Type>, SmolStr>],
    ) -> Result<(), Vec<SmolStr>> {
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
    fn from_use_expression(patterns: Vec<UntypedPattern>) -> UseAssignments {
        let mut assignments = UseAssignments::default();

        for (index, pattern) in patterns.into_iter().enumerate() {
            match pattern {
                // For discards we add a discard function arguments.
                Pattern::Discard { name, location, .. } => {
                    assignments.function_arguments.push(Arg {
                        location,
                        names: ArgNames::Discard { name },
                        annotation: None,
                        type_: (),
                    })
                }

                // For simple patterns of a single variable we add a regular
                // function argument.
                Pattern::Var { location, name, .. } => assignments.function_arguments.push(Arg {
                    location,
                    names: ArgNames::Named { name },
                    annotation: None,
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
                | Pattern::BitString { .. }
                | Pattern::Concatenate { .. }) => {
                    let name: SmolStr = format!("{USE_ASSIGNMENT_VARIABLE}{index}").into();
                    let location = pattern.location();
                    assignments.function_arguments.push(Arg {
                        location,
                        names: ArgNames::Named { name: name.clone() },
                        annotation: None,
                        type_: (),
                    });
                    let assignment = Assignment {
                        location,
                        pattern,
                        kind: AssignmentKind::Let,
                        annotation: None,
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
