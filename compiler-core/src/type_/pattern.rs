use im::hashmap;
use itertools::Itertools;

///! Type inference and checking of patterns used in case expressions
///! and variables bindings.
///!
use super::*;
use crate::ast::UntypedPatternBitStringSegment;
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
                self.environment.insert_variable(
                    name.to_string(),
                    ValueConstructorVariant::LocalVariable { location },
                    typ,
                );
                Ok(())
            }

            PatternMode::Alternative(assigned) => {
                match self.environment.local_values.get(name) {
                    // This variable was defined in the Initial multi-pattern
                    Some(initial) if self.initial_pattern_vars.contains(name) => {
                        assigned.push(name.to_string());
                        let initial_typ = initial.type_.clone();
                        self.environment.unify(initial_typ, typ)
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
    ) -> Result<Vec<TypedPattern>, Error> {
        self.mode = PatternMode::Alternative(vec![]);
        let typed_multi = self.infer_multi_pattern(multi_pattern, subjects, location)?;
        match &self.mode {
            PatternMode::Initial => panic!("Pattern mode switched from Alternative to Initial"),
            PatternMode::Alternative(assigned)
                if assigned.len() != self.initial_pattern_vars.len() =>
            {
                for name in assigned {
                    let _ = self.initial_pattern_vars.remove(name);
                }
                Err(Error::MissingVarInAlternativePattern {
                    location: *location,
                    // It is safe to use expect here as we checked the length above
                    name: self
                        .initial_pattern_vars
                        .iter()
                        .next()
                        .expect("Getting undefined pattern variable")
                        .clone(),
                })
            }
            PatternMode::Alternative(_) => Ok(typed_multi),
        }
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
        for (pattern, subject_type) in multi_pattern.into_iter().zip(subjects) {
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

        let mut typed_segments: Vec<_> = segments
            .into_iter()
            .map(|s| self.infer_pattern_segment(s, false))
            .try_collect()?;

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

        let options: Vec<_> = options
            .into_iter()
            .map(|o| infer_bit_string_segment_option(o, |value, typ| self.unify(value, typ)))
            .try_collect()?;

        let segment_type = bit_string::type_options_for_pattern(&options, !is_last_segment)
            .map_err(|error| Error::BitStringSegmentError {
                error: error.error,
                location: error.location,
            })?;

        let typ = {
            match value.deref() {
                Pattern::Var { .. } if segment_type == string() => {
                    Err(Error::BitStringSegmentError {
                        error: bit_string::ErrorType::VariableUtfSegmentInPattern,
                        location,
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
    ) -> Result<TypedPattern, Error> {
        match pattern {
            Pattern::Discard { name, location } => Ok(Pattern::Discard { name, location }),

            Pattern::Var { name, location, .. } => {
                self.insert_variable(&name, type_, location)
                    .map_err(|e| convert_unify_error(e, location))?;
                Ok(Pattern::Var { name, location })
            }

            Pattern::VarUsage { name, location, .. } => {
                let ValueConstructor { type_: typ, .. } = self
                    .environment
                    .get_variable(&name)
                    .cloned()
                    .ok_or_else(|| Error::UnknownVariable {
                        location,
                        name: name.to_string(),
                        variables: self.environment.local_value_names(),
                    })?;
                self.environment.increment_usage(&name);
                let typ = self
                    .environment
                    .instantiate(typ, &mut hashmap![], self.hydrator);
                self.environment
                    .unify(int(), typ.clone())
                    .map_err(|e| convert_unify_error(e, location))?;

                Ok(Pattern::VarUsage {
                    name,
                    location,
                    type_: typ,
                })
            }

            Pattern::Assign {
                name,
                pattern,
                location,
            } => {
                self.insert_variable(&name, type_.clone(), location)
                    .map_err(|e| convert_unify_error(e, pattern.location()))?;
                let pattern = self.unify(*pattern, type_)?;
                Ok(Pattern::Assign {
                    name,
                    pattern: Box::new(pattern),
                    location,
                })
            }

            Pattern::Int { location, value } => {
                self.environment
                    .unify(type_, int())
                    .map_err(|e| convert_unify_error(e, location))?;
                Ok(Pattern::Int { location, value })
            }

            Pattern::Float { location, value } => {
                self.environment
                    .unify(type_, float())
                    .map_err(|e| convert_unify_error(e, location))?;
                Ok(Pattern::Float { location, value })
            }

            Pattern::String { location, value } => {
                self.environment
                    .unify(type_, string())
                    .map_err(|e| convert_unify_error(e, location))?;
                Ok(Pattern::String { location, value })
            }

            Pattern::List {
                location,
                elements,
                tail,
            } => match type_.get_app_args(true, &[], "List", 1, self.environment) {
                Some(args) => {
                    let typ = args
                        .get(0)
                        .expect("Failed to get type argument of List")
                        .clone();
                    let elements = elements
                        .into_iter()
                        .map(|element| self.unify(element, typ.clone()))
                        .try_collect()?;

                    let tail = match tail {
                        Some(tail) => Some(Box::new(self.unify(*tail, list(typ))?)),
                        None => None,
                    };

                    Ok(Pattern::List {
                        location,
                        elements,
                        tail,
                    })
                }

                None => Err(Error::CouldNotUnify {
                    given: list(self.environment.new_unbound_var()),
                    expected: type_.clone(),
                    situation: None,
                    location,
                    rigid_type_names: hashmap![],
                }),
            },

            Pattern::Tuple { elems, location } => match collapse_links(type_.clone()).deref() {
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
                        .try_collect()?;
                    Ok(Pattern::Tuple { elems, location })
                }

                Type::Var { .. } => {
                    let elems_types: Vec<_> = (0..(elems.len()))
                        .map(|_| self.environment.new_unbound_var())
                        .collect();
                    self.environment
                        .unify(tuple(elems_types.clone()), type_.clone())
                        .map_err(|e| convert_unify_error(e, location))?;
                    let elems = elems
                        .into_iter()
                        .zip(elems_types)
                        .map(|(pattern, typ)| self.unify(pattern, typ))
                        .try_collect()?;
                    Ok(Pattern::Tuple { elems, location })
                }

                _ => {
                    let elems_types = (0..(elems.len()))
                        .map(|_| self.environment.new_unbound_var())
                        .collect();

                    Err(Error::CouldNotUnify {
                        given: tuple(elems_types),
                        expected: type_,
                        situation: None,
                        location,
                        rigid_type_names: hashmap![],
                    })
                }
            },

            Pattern::BitString { location, segments } => {
                self.environment
                    .unify(type_, bit_string())
                    .map_err(|e| convert_unify_error(e, location))?;
                self.infer_pattern_bit_string(segments, location)
            }

            Pattern::Constructor {
                location,
                module,
                name,
                arguments: mut pattern_args,
                with_spread,
                ..
            } => {
                // Register the value as seen for detection of unused values
                self.environment.increment_usage(&name);

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
                                .unwrap_or(pattern_args.len());

                            while pattern_args.len() < field_map.arity {
                                let new_call_arg = CallArg {
                                    value: Pattern::Discard {
                                        name: "_".to_string(),
                                        location: spread_location,
                                    },
                                    location: spread_location,
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

                let instantiated_constructor_type =
                    self.environment
                        .instantiate(constructor_typ, &mut hashmap![], self.hydrator);
                match instantiated_constructor_type.deref() {
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
                                .try_collect()?;
                            self.environment
                                .unify(type_, retrn.clone())
                                .map_err(|e| convert_unify_error(e, location))?;
                            Ok(Pattern::Constructor {
                                location,
                                module,
                                name,
                                arguments: pattern_args,
                                constructor,
                                with_spread,
                                type_: instantiated_constructor_type,
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
                                .unify(type_, instantiated_constructor_type.clone())
                                .map_err(|e| convert_unify_error(e, location))?;
                            Ok(Pattern::Constructor {
                                location,
                                module,
                                name,
                                arguments: vec![],
                                constructor,
                                with_spread,
                                type_: instantiated_constructor_type,
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

                    _ => panic!("Unexpected constructor type for a constructor pattern.",),
                }
            }
        }
    }
}
