///! Type inference and checking of patterns used in case expressions
///! and variables bindings.
///!
use super::*;
use crate::ast::UntypedPatternBitStringSegment;
use std::sync::Arc;

pub struct PatternTyper<'a, 'b, 'c> {
    environment: &'a mut Environment<'b, 'c>,
    hydrator: &'a Hydrator,
    level: usize,
    mode: PatternMode,
    initial_pattern_vars: HashSet<String>,
}

enum PatternMode {
    Initial,
    Alternative,
}

impl<'a, 'b, 'c> PatternTyper<'a, 'b, 'c> {
    pub fn new(
        environment: &'a mut Environment<'b, 'c>,
        hydrator: &'a Hydrator,
        level: usize,
    ) -> Self {
        Self {
            environment,
            hydrator,
            level,
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
        self.environment
            .init_usage(name.to_string(), EntityKind::Variable, location);

        match self.mode {
            PatternMode::Initial => {
                if self.initial_pattern_vars.contains(name) {
                    return Err(UnifyError::DuplicateVarInPattern {
                        name: name.to_string(),
                    });
                }
                let _ = self.initial_pattern_vars.insert(name.to_string());
                self.environment.insert_variable(
                    name.to_string(),
                    ValueConstructorVariant::LocalVariable,
                    typ,
                    location,
                );
                Ok(())
            }

            PatternMode::Alternative => match self.environment.local_values.get(name) {
                // This variable was defined in the Initial multi-pattern
                Some(initial) if self.initial_pattern_vars.contains(name) => {
                    let initial_typ = initial.typ.clone();
                    self.environment.unify(initial_typ, typ)
                }

                // This variable was not defined in the Initial multi-pattern
                _ => Err(UnifyError::ExtraVarInAlternativePattern {
                    name: name.to_string(),
                }),
            },
        }
    }

    pub fn infer_alternative_multi_pattern(
        &mut self,
        multi_pattern: UntypedMultiPattern,
        subjects: &[Arc<Type>],
        location: &SrcSpan,
    ) -> Result<Vec<TypedPattern>, Error> {
        self.mode = PatternMode::Alternative;
        let typed_multi = self.infer_multi_pattern(multi_pattern, subjects, location)?;
        Ok(typed_multi)
    }

    pub fn infer_multi_pattern(
        &mut self,
        multi_pattern: UntypedMultiPattern,
        subjects: &[Arc<Type>],
        location: &SrcSpan,
    ) -> Result<Vec<TypedPattern>, Error> {
        // If there are N subjects the multi-pattern is expected to be N patterns
        if subjects.len() != multi_pattern.len() {
            return Err(Error::IncorrectNumClausePatterns {
                location: *location,
                expected: subjects.len(),
                given: multi_pattern.len(),
            });
        }

        // Unify each pattern in the multi-pattern with the corresponding subject
        let mut typed_multi = Vec::with_capacity(multi_pattern.len());
        for (pattern, subject_type) in multi_pattern.into_iter().zip(subjects.iter()) {
            let pattern = self.unify(pattern, subject_type.clone())?;
            typed_multi.push(pattern);
        }
        Ok(typed_multi)
    }

    fn infer_pattern_bit_string(
        &mut self,
        mut segments: Vec<UntypedPatternBitStringSegment>,
        location: SrcSpan,
    ) -> Result<TypedPattern, Error> {
        let last_segment = segments.pop();

        let mut typed_segments = segments
            .into_iter()
            .map(|s| self.infer_pattern_segment(s, false))
            .collect::<Result<Vec<_>, _>>()?;

        if let Some(s) = last_segment {
            let typed_last_segment = self.infer_pattern_segment(s, true)?;
            typed_segments.push(typed_last_segment)
        }

        Ok(TypedPattern::BitString {
            location,
            segments: typed_segments,
        })
    }

    fn infer_pattern_segment(
        &mut self,
        segment: UntypedPatternBitStringSegment,
        is_last_segment: bool,
    ) -> Result<TypedPatternBitStringSegment, Error> {
        let UntypedPatternBitStringSegment {
            location,
            options,
            value,
            ..
        } = segment;

        let options = options
            .into_iter()
            .map(|o| infer_bit_string_segment_option(o, |value, typ| self.unify(value, typ)))
            .collect::<Result<Vec<_>, _>>()?;

        let segment_type = bit_string::type_options_for_pattern(&options, !is_last_segment)
            .map_err(|error| Error::BitStringSegmentError {
                error: error.error,
                location: error.location,
            })?;

        let typ = {
            match &*value {
                Pattern::Var { .. } if segment_type == string() => {
                    Err(Error::BitStringSegmentError {
                        error: bit_string::ErrorType::VaribleUTFSegmentInPatten,
                        location: location,
                    })
                }
                _ => Ok(segment_type),
            }
        }?;
        let typed_value = self.unify(*value, typ.clone())?;

        Ok(BitStringSegment {
            location,
            value: Box::new(typed_value),
            options,
            typ,
        })
    }

    /// When we have an assignment or a case expression we unify the pattern with the
    /// inferred type of the subject in order to determine what variables to insert
    /// into the environment (or to detect a type error).
    ///
    pub fn unify(
        &mut self,
        pattern: UntypedPattern,
        typ: Arc<Type>,
    ) -> Result<TypedPattern, Error> {
        match pattern {
            Pattern::Discard { name, location } => Ok(Pattern::Discard { name, location }),

            Pattern::Var { name, location } => {
                self.insert_variable(name.as_ref(), typ, location)
                    .map_err(|e| convert_unify_error(e, location))?;
                Ok(Pattern::Var { name, location })
            }

            Pattern::VarUsage { name, location, .. } => {
                let ValueConstructor { typ, .. } = self
                    .environment
                    .get_variable(&name)
                    .cloned()
                    .ok_or_else(|| Error::UnknownVariable {
                        location,
                        name: name.to_string(),
                        variables: self
                            .environment
                            .local_values
                            .keys()
                            .map(|t| t.to_string())
                            .collect(),
                    })?;
                let typ = self.environment.instantiate(
                    typ,
                    self.environment.level,
                    &mut hashmap![],
                    self.hydrator,
                );
                self.environment
                    .unify(int(), typ.clone())
                    .map_err(|e| convert_unify_error(e, location))?;

                Ok(Pattern::VarUsage {
                    name,
                    location,
                    typ,
                })
            }

            Pattern::Let {
                name,
                pattern,
                location,
            } => {
                self.insert_variable(name.as_ref(), typ.clone(), location)
                    .map_err(|e| convert_unify_error(e, pattern.location()))?;
                let pattern = self.unify(*pattern, typ)?;
                Ok(Pattern::Let {
                    name,
                    pattern: Box::new(pattern),
                    location,
                })
            }

            Pattern::Int { location, value } => {
                self.environment
                    .unify(typ, int())
                    .map_err(|e| convert_unify_error(e, location))?;
                Ok(Pattern::Int { location, value })
            }

            Pattern::Float { location, value } => {
                self.environment
                    .unify(typ, float())
                    .map_err(|e| convert_unify_error(e, location))?;
                Ok(Pattern::Float { location, value })
            }

            Pattern::String { location, value } => {
                self.environment
                    .unify(typ, string())
                    .map_err(|e| convert_unify_error(e, location))?;
                Ok(Pattern::String { location, value })
            }

            Pattern::Nil { location } => {
                let typ2 = list(self.environment.new_unbound_var(self.level));
                self.environment
                    .unify(typ, typ2)
                    .map_err(|e| convert_unify_error(e, location))?;
                Ok(Pattern::Nil { location })
            }

            Pattern::Cons {
                location,
                head,
                tail,
            } => match typ.get_app_args(true, &[], "List", 1, self.environment) {
                Some(args) => {
                    let head = Box::new(self.unify(*head, args[0].clone())?);
                    let tail = Box::new(self.unify(*tail, typ)?);

                    Ok(Pattern::Cons {
                        location,
                        head,
                        tail,
                    })
                }

                None => Err(Error::CouldNotUnify {
                    given: list(self.environment.new_unbound_var(self.level)),
                    expected: typ.clone(),
                    situation: None,
                    location,
                }),
            },

            Pattern::Tuple { elems, location } => match &*collapse_links(typ.clone()) {
                Type::Tuple { elems: type_elems } => {
                    if elems.len() != type_elems.len() {
                        return Err(Error::IncorrectArity {
                            labels: vec![],
                            location,
                            expected: type_elems.len(),
                            given: elems.len(),
                        });
                    }

                    let elems = elems
                        .into_iter()
                        .zip(type_elems)
                        .map(|(pattern, typ)| self.unify(pattern, typ.clone()))
                        .collect::<Result<Vec<_>, _>>()?;
                    Ok(Pattern::Tuple { elems, location })
                }

                Type::Var { .. } => {
                    let elems_types = (0..(elems.len()))
                        .map(|_| self.environment.new_unbound_var(self.level))
                        .collect();
                    self.environment
                        .unify(tuple(elems_types), typ.clone())
                        .map_err(|e| convert_unify_error(e, location))?;
                    self.unify(Pattern::Tuple { elems, location }, typ)
                }

                _ => {
                    let elems_types = (0..(elems.len()))
                        .map(|_| self.environment.new_unbound_var(self.level))
                        .collect();

                    Err(Error::CouldNotUnify {
                        given: tuple(elems_types),
                        expected: typ,
                        situation: None,
                        location,
                    })
                }
            },

            Pattern::BitString { location, segments } => {
                self.environment
                    .unify(typ, bit_string())
                    .map_err(|e| convert_unify_error(e, location))?;
                self.infer_pattern_bit_string(segments, location)
            }

            Pattern::Constructor {
                location,
                module,
                name,
                args: mut pattern_args,
                with_spread,
                ..
            } => {
                // Register the value as seen for detection of unused values
                self.environment.increment_usage(name.as_str());

                let cons = self
                    .environment
                    .get_value_constructor(module.as_ref(), &name)
                    .map_err(|e| convert_get_value_constructor_error(e, location))?;

                match cons.field_map() {
                    // The fun has a field map so labelled arguments may be present and need to be reordered.
                    Some(field_map) => {
                        if with_spread {
                            // Using the spread operator when you have already provided variables for all of the
                            // record's fields throws an error
                            if pattern_args.len() == field_map.arity {
                                return Err(Error::UnnecessarySpreadOperator {
                                    location: SrcSpan {
                                        start: location.end - 3,
                                        end: location.end - 1,
                                    },
                                    arity: field_map.arity,
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
                                .unwrap_or_else(|| pattern_args.len());

                            while pattern_args.len() < field_map.arity {
                                let new_call_arg = CallArg {
                                    value: Pattern::Discard {
                                        name: "_".to_string(),
                                        location: spread_location.clone(),
                                    },
                                    location: spread_location.clone(),
                                    label: None,
                                };

                                pattern_args.insert(index_of_first_labelled_arg, new_call_arg);
                            }
                        }

                        field_map.reorder(&mut pattern_args, location)?
                    }

                    // The fun has no field map and so we error if arguments have been labelled
                    None => assert_no_labelled_arguments(&pattern_args)?,
                }

                let constructor_typ = cons.typ.clone();
                let constructor = match cons.variant {
                    ValueConstructorVariant::Record { ref name, .. } => {
                        PatternConstructor::Record { name: name.clone() }
                    }
                    ValueConstructorVariant::LocalVariable
                    | ValueConstructorVariant::ModuleConstant { .. }
                    | ValueConstructorVariant::ModuleFn { .. } => crate::error::fatal_compiler_bug(
                        "Unexpected value constructor type for a constructor pattern.",
                    ),
                };

                let instantiated_constructor_type = self.environment.instantiate(
                    constructor_typ,
                    self.level,
                    &mut hashmap![],
                    self.hydrator,
                );
                match &*instantiated_constructor_type {
                    Type::Fn { args, retrn } => {
                        if args.len() == pattern_args.len() {
                            let pattern_args = pattern_args
                                .into_iter()
                                .zip(args)
                                .map(|(arg, typ)| {
                                    let CallArg {
                                        value,
                                        location,
                                        label,
                                    } = arg;
                                    let value = self.unify(value, typ.clone())?;
                                    Ok(CallArg {
                                        value,
                                        location,
                                        label,
                                    })
                                })
                                .collect::<Result<Vec<_>, _>>()?;
                            self.environment
                                .unify(typ, retrn.clone())
                                .map_err(|e| convert_unify_error(e, location))?;
                            Ok(Pattern::Constructor {
                                location,
                                module,
                                name,
                                args: pattern_args,
                                constructor,
                                with_spread,
                            })
                        } else {
                            Err(Error::IncorrectArity {
                                labels: vec![],
                                location,
                                expected: args.len(),
                                given: pattern_args.len(),
                            })
                        }
                    }

                    Type::App { .. } => {
                        if pattern_args.is_empty() {
                            self.environment
                                .unify(typ, instantiated_constructor_type)
                                .map_err(|e| convert_unify_error(e, location))?;
                            Ok(Pattern::Constructor {
                                location,
                                module,
                                name,
                                args: vec![],
                                constructor,
                                with_spread,
                            })
                        } else {
                            Err(Error::IncorrectArity {
                                labels: vec![],
                                location,
                                expected: 0,
                                given: pattern_args.len(),
                            })
                        }
                    }

                    _ => crate::error::fatal_compiler_bug(
                        "Unexpected constructor type for a constructor pattern.",
                    ),
                }
            }
        }
    }
}
