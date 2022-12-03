use im::hashmap;

///! Type inference and checking of patterns used in case expressions
///! and variables bindings.
///!
use super::*;
use crate::ast::{AssignName, UntypedPatternBitStringSegment};
use std::sync::Arc;

pub struct PatternTyper<'a, 'b> {
    environment: &'a mut Environment<'b>,
    hydrator: &'a Hydrator,
    mode: PatternMode,
    initial_pattern_vars: HashSet<String>,
}

enum PatternMode {
    Initial,
    Alternative(Vec<String>),
}

impl<'a, 'b> PatternTyper<'a, 'b> {
    pub fn new(environment: &'a mut Environment<'b>, hydrator: &'a Hydrator) -> Self {
        Self {
            environment,
            hydrator,
            mode: PatternMode::Initial,
            initial_pattern_vars: HashSet::new(),
        }
    }

    fn insert_variable(
        &mut self,
        name: &str,
        typ: Arc<Type>,
        location: SrcSpan,
    ) -> Result<(), UnifyError> {
        match &mut self.mode {
            PatternMode::Initial => {
                // Register usage for the unused variable detection
                self.environment
                    .init_usage(name.to_string(), EntityKind::Variable, location);
                // Ensure there are no duplicate variable names in the pattern
                if self.initial_pattern_vars.contains(name) {
                    return Err(UnifyError::DuplicateVarInPattern {
                        name: name.to_string(),
                    });
                }
                // Record that this variable originated in this pattern so any
                // following alternative patterns can be checked to ensure they
                // have the same variables.
                let _ = self.initial_pattern_vars.insert(name.to_string());
                // And now insert the variable for use in the code that comes
                // after the pattern.
                self.environment
                    .insert_local_variable(name.to_string(), location, typ);
                Ok(())
            }

            PatternMode::Alternative(assigned) => {
                match self.environment.scope.get(name) {
                    // This variable was defined in the Initial multi-pattern
                    Some(initial) if self.initial_pattern_vars.contains(name) => {
                        assigned.push(name.to_string());
                        let initial_typ = initial.type_.clone();
                        unify(initial_typ, typ)
                    }

                    // This variable was not defined in the Initial multi-pattern
                    _ => Err(UnifyError::ExtraVarInAlternativePattern {
                        name: name.to_string(),
                    }),
                }
            }
        }
    }

    pub fn infer_alternative_multi_pattern(
        &mut self,
        multi_pattern: UntypedMultiPattern,
        subjects: &[Arc<Type>],
        location: &SrcSpan,
    ) -> FilledResult<Vec<TypedPattern>, Error> {
        let mut ctx = FilledResultContext::new();
        self.mode = PatternMode::Alternative(vec![]);
        let typed_multi =
            ctx.slurp_filled(self.infer_multi_pattern(multi_pattern, subjects, location));
        match &self.mode {
            PatternMode::Initial => panic!("Pattern mode switched from Alternative to Initial"),
            PatternMode::Alternative(assigned)
                if assigned.len() != self.initial_pattern_vars.len() =>
            {
                for name in assigned {
                    let _ = self.initial_pattern_vars.remove(name);
                }
                ctx.register_error(Error::MissingVarInAlternativePattern {
                    location: *location,
                    // It is safe to use expect here as we checked the length above
                    name: self
                        .initial_pattern_vars
                        .iter()
                        .next()
                        .expect("Getting undefined pattern variable")
                        .clone(),
                });
            }
            PatternMode::Alternative(_) => (),
        }
        ctx.finish(typed_multi)
    }

    pub fn infer_multi_pattern(
        &mut self,
        multi_pattern: UntypedMultiPattern,
        subjects: &[Arc<Type>],
        location: &SrcSpan,
    ) -> FilledResult<Vec<TypedPattern>, Error> {
        let mut ctx = FilledResultContext::new();
        // If there are N subjects the multi-pattern is expected to be N patterns
        if subjects.len() != multi_pattern.len() {
            ctx.register_error(Error::IncorrectNumClausePatterns {
                location: *location,
                expected: subjects.len(),
                given: multi_pattern.len(),
            });
        }

        let min_len = subjects.len().min(multi_pattern.len());

        // Unify each pattern in the multi-pattern with the corresponding subject
        let mut typed_multi = Vec::with_capacity(min_len);
        for (pattern, subject_type) in multi_pattern.into_iter().zip(subjects).take(min_len) {
            let pattern = ctx.slurp_filled(self.unify(pattern, subject_type.clone()));
            typed_multi.push(pattern);
        }
        ctx.finish(typed_multi)
    }

    fn infer_pattern_bit_string(
        &mut self,
        mut segments: Vec<UntypedPatternBitStringSegment>,
        location: SrcSpan,
    ) -> FilledResult<TypedPattern, Error> {
        let mut ctx = FilledResultContext::new();
        let last_segment = segments.pop();

        let mut typed_segments: Vec<_> = segments
            .into_iter()
            .map(|s| ctx.slurp_filled(self.infer_pattern_segment(s, false)))
            .collect();

        if let Some(s) = last_segment {
            let typed_last_segment = ctx.slurp_filled(self.infer_pattern_segment(s, true));
            typed_segments.push(typed_last_segment)
        }

        ctx.finish(TypedPattern::BitString {
            location,
            segments: typed_segments,
        })
    }

    fn infer_pattern_segment(
        &mut self,
        segment: UntypedPatternBitStringSegment,
        is_last_segment: bool,
    ) -> FilledResult<TypedPatternBitStringSegment, Error> {
        let mut ctx = FilledResultContext::new();
        let UntypedPatternBitStringSegment {
            location,
            options,
            value,
            ..
        } = segment;

        let options: Vec<_> = options
            .into_iter()
            .map(|o| {
                ctx.slurp_filled(infer_bit_string_segment_option(o, |value, typ| {
                    self.unify(value, typ)
                }))
            })
            .collect();

        let segment_type = ctx
            .slurp_result(
                bit_string::type_options_for_pattern(&options, !is_last_segment).map_err(|error| {
                    Error::BitStringSegmentError {
                        error: error.error,
                        location: error.location,
                    }
                }),
            )
            .unwrap_or_else(|| self.environment.new_unbound_var());

        let typ = segment_type.clone();
        if matches!(value.deref(), Pattern::Var { .. } if segment_type == string()) {
            ctx.register_error(Error::BitStringSegmentError {
                error: bit_string::ErrorType::VariableUtfSegmentInPattern,
                location,
            });
        }
        let typed_value = ctx.slurp_filled(self.unify(*value, typ.clone()));

        ctx.finish(BitStringSegment {
            location,
            value: Box::new(typed_value),
            options,
            type_: typ,
        })
    }

    /// When we have an assignment or a case expression we unify the pattern with the
    /// inferred type of the subject in order to determine what variables to insert
    /// into the environment (or to detect a type error).
    ///
    pub fn unify(
        &mut self,
        pattern: UntypedPattern,
        type_: Arc<Type>,
    ) -> FilledResult<TypedPattern, Error> {
        match pattern {
            Pattern::Discard { name, location } => {
                FilledResult::ok(Pattern::Discard { name, location })
            }

            Pattern::Var { name, location, .. } => {
                let errors = self
                    .insert_variable(&name, type_, location)
                    .err()
                    .map(|e| convert_unify_error(e, location))
                    .into_iter()
                    .collect();

                FilledResult::new(Pattern::Var { name, location }, errors)
            }

            Pattern::VarUsage { name, location, .. } => {
                let mut ctx = FilledResultContext::new();
                let typ = if let Some(typ) = self
                    .environment
                    .get_variable(&name)
                    .map(|v| v.type_.clone())
                {
                    typ
                } else {
                    ctx.register_error(Error::UnknownVariable {
                        location,
                        name: name.to_string(),
                        variables: self.environment.local_value_names(),
                    });
                    self.environment.new_unbound_var()
                };
                self.environment.increment_usage(&name);
                let typ = self
                    .environment
                    .instantiate(typ, &mut hashmap![], self.hydrator);
                let _ = ctx.slurp_result(
                    unify(int(), typ.clone()).map_err(|e| convert_unify_error(e, location)),
                );

                ctx.finish(Pattern::VarUsage {
                    name,
                    location,
                    type_: typ,
                })
            }

            Pattern::Concatenate {
                location,
                left_location,
                right_location,
                left_side_string,
                right_side_assignment,
            } => {
                let mut ctx = FilledResultContext::new();
                // The entire concatenate pattern must be a string
                ctx.just_slurp_result(
                    unify(type_, string()).map_err(|e| convert_unify_error(e, location)),
                );

                // The right hand side may assign a variable, which is the suffix of the string
                if let AssignName::Variable(right) = &right_side_assignment {
                    ctx.just_slurp_result(
                        self.insert_variable(right.as_ref(), string(), right_location)
                            .map_err(|e| convert_unify_error(e, location)),
                    );
                };

                ctx.finish(Pattern::Concatenate {
                    location,
                    left_location,
                    right_location,
                    left_side_string,
                    right_side_assignment,
                })
            }

            Pattern::Assign {
                name,
                pattern,
                location,
            } => {
                let mut ctx = FilledResultContext::new();
                ctx.just_slurp_result(
                    self.insert_variable(&name, type_.clone(), location)
                        .map_err(|e| convert_unify_error(e, pattern.location())),
                );
                let pattern = ctx.slurp_filled(self.unify(*pattern, type_));
                ctx.finish(Pattern::Assign {
                    name,
                    pattern: Box::new(pattern),
                    location,
                })
            }

            Pattern::Int { location, value } => FilledResult::ok(Pattern::Int { location, value })
                .with_check(unify(type_, int()).map_err(|e| convert_unify_error(e, location))),

            Pattern::Float { location, value } => {
                FilledResult::ok(Pattern::Float { location, value })
                    .with_check(unify(type_, float()).map_err(|e| convert_unify_error(e, location)))
            }

            Pattern::String { location, value } => {
                FilledResult::ok(Pattern::String { location, value }).with_check(
                    unify(type_, string()).map_err(|e| convert_unify_error(e, location)),
                )
            }

            Pattern::List {
                location,
                elements,
                tail,
            } => {
                let mut ctx = FilledResultContext::new();

                let typ = if let Some(args) =
                    type_.get_app_args(true, &[], "List", 1, self.environment)
                {
                    args.get(0)
                        .expect("Failed to get type argument of List")
                        .clone()
                } else {
                    let list_contents = self.environment.new_unbound_var();
                    ctx.register_error(Error::CouldNotUnify {
                        location,
                        situation: None,
                        expected: type_.clone(),
                        given: list(list_contents.clone()),
                        rigid_type_names: hashmap![],
                    });
                    list_contents
                };

                let elements = elements
                    .into_iter()
                    .map(|element| ctx.slurp_filled(self.unify(element, typ.clone())))
                    .collect();

                let tail =
                    tail.map(|tail| Box::new(ctx.slurp_filled(self.unify(*tail, list(typ)))));

                ctx.finish(Pattern::List {
                    location,
                    elements,
                    tail,
                })
            }

            Pattern::Tuple { elems, location } => {
                let mut ctx = FilledResultContext::new();

                let type_elems: Vec<Arc<Type>> = match collapse_links(type_.clone()).deref() {
                    Type::Tuple { elems: type_elems } => {
                        if elems.len() != type_elems.len() {
                            ctx.register_error(Error::IncorrectArity {
                                labels: vec![],
                                location,
                                expected: type_elems.len(),
                                given: elems.len(),
                            });
                        }
                        type_elems.clone()
                    }

                    Type::Var { .. } => {
                        let elems_types: Vec<_> = (0..elems.len())
                            .map(|_| self.environment.new_unbound_var())
                            .collect();
                        ctx.just_slurp_result(
                            unify(tuple(elems_types.clone()), type_)
                                .map_err(|e| convert_unify_error(e, location)),
                        );
                        elems_types
                    }

                    _ => {
                        let elems_types: Vec<_> = (0..(elems.len()))
                            .map(|_| self.environment.new_unbound_var())
                            .collect();

                        ctx.register_error(Error::CouldNotUnify {
                            given: tuple(elems_types.clone()),
                            expected: type_,
                            situation: None,
                            location,
                            rigid_type_names: hashmap![],
                        });
                        elems_types
                    }
                };

                let elems = elems
                    .into_iter()
                    .zip(type_elems)
                    .map(|(pattern, type_)| ctx.slurp_filled(self.unify(pattern, type_)))
                    .collect();

                ctx.finish(Pattern::Tuple { elems, location })
            }

            Pattern::BitString { location, segments } => {
                let mut ctx = FilledResultContext::new();
                ctx.just_slurp_result(
                    unify(type_, bit_string()).map_err(|e| convert_unify_error(e, location)),
                );
                let res = ctx.slurp_filled(self.infer_pattern_bit_string(segments, location));
                ctx.finish(res)
            }

            Pattern::Constructor {
                location,
                module,
                name,
                arguments: mut pattern_args,
                with_spread,
                ..
            } => {
                let mut ctx = FilledResultContext::new();
                // Register the value as seen for detection of unused values
                self.environment.increment_usage(&name);

                if let Some(cons) = ctx.slurp_result(
                    self.environment
                        .get_value_constructor(module.as_ref(), &name)
                        .map_err(|e| convert_get_value_constructor_error(e, location)),
                ) {
                    match cons.field_map() {
                        // The fun has a field map so labelled arguments may be present and need to be reordered.
                        Some(field_map) => {
                            if with_spread {
                                // Using the spread operator when you have already provided variables for all of the
                                // record's fields throws an error
                                if pattern_args.len() == field_map.arity as usize {
                                    ctx.register_error(Error::UnnecessarySpreadOperator {
                                        location: SrcSpan {
                                            start: location.end - 3,
                                            end: location.end - 1,
                                        },
                                        arity: field_map.arity as usize,
                                    });
                                }

                                // The location of the spread operator itself
                                let spread_location = SrcSpan {
                                    start: location.end - 3,
                                    end: location.end - 1,
                                };

                                // Insert discard variables to match the unspecified fields
                                // In order to support both positional and labelled arguments we have to insert
                                // them after all positional variables and before the labelled ones. This means
                                // we have calculate that index and then insert() the discards. It would be faster
                                // if we could put the discards anywhere which would let us use push().
                                // Potential future optimisation.
                                let index_of_first_labelled_arg = pattern_args
                                    .iter()
                                    .position(|a| a.label.is_some())
                                    .unwrap_or(pattern_args.len());

                                while pattern_args.len() < field_map.arity as usize {
                                    let new_call_arg = CallArg {
                                        value: Pattern::Discard {
                                            name: "_".to_string(),
                                            location: spread_location,
                                        },
                                        location: spread_location,
                                        label: None,
                                        implicit: false,
                                    };

                                    pattern_args.insert(index_of_first_labelled_arg, new_call_arg);
                                }
                            }

                            ctx.just_slurp_result(field_map.reorder(&mut pattern_args, location))
                        }

                        // The fun has no field map and so we error if arguments have been labelled
                        None => ctx.just_slurp_result(assert_no_labelled_arguments(&pattern_args)),
                    }

                    let constructor_typ = cons.type_.clone();
                    let constructor = match cons.variant {
                        ValueConstructorVariant::Record { ref name, .. } => {
                            PatternConstructor::Record {
                                name: name.clone(),
                                field_map: cons.field_map().cloned(),
                            }
                        }
                        ValueConstructorVariant::LocalVariable { .. }
                        | ValueConstructorVariant::ModuleConstant { .. }
                        | ValueConstructorVariant::ModuleFn { .. } => {
                            panic!("Unexpected value constructor type for a constructor pattern.",)
                        }
                    };

                    let instantiated_constructor_type = self.environment.instantiate(
                        constructor_typ,
                        &mut hashmap![],
                        self.hydrator,
                    );
                    match instantiated_constructor_type.deref() {
                        Type::Fn { args, retrn } => {
                            if args.len() != pattern_args.len() {
                                ctx.register_error(Error::IncorrectArity {
                                    labels: vec![],
                                    location,
                                    expected: args.len(),
                                    given: pattern_args.len(),
                                });
                            }
                            let pattern_args = pattern_args
                                .into_iter()
                                .zip(args)
                                .map(|(arg, typ)| {
                                    let CallArg {
                                        value,
                                        location,
                                        implicit,
                                        label,
                                    } = arg;
                                    let value = ctx.slurp_filled(self.unify(value, typ.clone()));
                                    CallArg {
                                        value,
                                        location,
                                        implicit,
                                        label,
                                    }
                                })
                                .collect();
                            ctx.just_slurp_result(
                                unify(type_, retrn.clone())
                                    .map_err(|e| convert_unify_error(e, location)),
                            );
                            ctx.finish(Pattern::Constructor {
                                location,
                                module,
                                name,
                                arguments: pattern_args,
                                constructor,
                                with_spread,
                                type_: instantiated_constructor_type,
                            })
                        }

                        Type::App { .. } => {
                            if !pattern_args.is_empty() {
                                ctx.register_error(Error::IncorrectArity {
                                    labels: vec![],
                                    location,
                                    expected: 0,
                                    given: pattern_args.len(),
                                });
                            }
                            ctx.just_slurp_result(
                                unify(type_, instantiated_constructor_type.clone())
                                    .map_err(|e| convert_unify_error(e, location)),
                            );
                            ctx.finish(Pattern::Constructor {
                                location,
                                module,
                                name,
                                arguments: vec![],
                                constructor,
                                with_spread,
                                type_: instantiated_constructor_type,
                            })
                        }

                        _ => panic!("Unexpected constructor type for a constructor pattern.",),
                    }
                } else {
                    let field_map = if pattern_args.is_empty() {
                        None
                    } else {
                        let mut field_map = FieldMap::new(pattern_args.len() as u32);

                        for (i, location, label) in
                            pattern_args.iter().enumerate().filter_map(|(i, arg)| {
                                arg.label.as_ref().map(|l| (i, arg.location, l.clone()))
                            })
                        {
                            ctx.just_slurp_result(
                                field_map
                                    .insert(label.clone(), i as u32)
                                    .map_err(|_| Error::DuplicateField { location, label }),
                            );
                        }
                        Some(field_map)
                    };

                    ctx.finish(Pattern::Constructor {
                        location,
                        module,
                        name: name.clone(),
                        arguments: vec![],
                        constructor: PatternConstructor::Record { name, field_map },
                        with_spread,
                        type_,
                    })
                }
            }
        }
    }
}
