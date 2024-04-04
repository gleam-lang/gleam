use im::hashmap;
use itertools::Itertools;

/// Type inference and checking of patterns used in case expressions
/// and variables bindings.
///
use super::*;
use crate::{
    analyse::Inferred,
    ast::{AssignName, Layer, UntypedPatternBitArraySegment},
};
use std::sync::Arc;

pub struct PatternTyper<'a, 'b> {
    environment: &'a mut Environment<'b>,
    hydrator: &'a Hydrator,
    mode: PatternMode,
    initial_pattern_vars: HashSet<EcoString>,
}

enum PatternMode {
    Initial,
    Alternative(Vec<EcoString>),
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
                    .init_usage(name.into(), EntityKind::Variable, location);
                // Ensure there are no duplicate variable names in the pattern
                if self.initial_pattern_vars.contains(name) {
                    return Err(UnifyError::DuplicateVarInPattern { name: name.into() });
                }
                // Record that this variable originated in this pattern so any
                // following alternative patterns can be checked to ensure they
                // have the same variables.
                let _ = self.initial_pattern_vars.insert(name.into());
                // And now insert the variable for use in the code that comes
                // after the pattern.
                self.environment
                    .insert_local_variable(name.into(), location, typ);
                Ok(())
            }

            PatternMode::Alternative(assigned) => {
                match self.environment.scope.get(name) {
                    // This variable was defined in the Initial multi-pattern
                    Some(initial) if self.initial_pattern_vars.contains(name) => {
                        assigned.push(name.into());
                        let initial_typ = initial.type_.clone();
                        unify(initial_typ, typ)
                    }

                    // This variable was not defined in the Initial multi-pattern
                    _ => Err(UnifyError::ExtraVarInAlternativePattern { name: name.into() }),
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

    fn infer_pattern_bit_array(
        &mut self,
        mut segments: Vec<UntypedPatternBitArraySegment>,
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

        Ok(TypedPattern::BitArray {
            location,
            segments: typed_segments,
        })
    }

    fn infer_pattern_segment(
        &mut self,
        segment: UntypedPatternBitArraySegment,
        is_last_segment: bool,
    ) -> Result<TypedPatternBitArraySegment, Error> {
        let UntypedPatternBitArraySegment {
            location,
            options,
            value,
            ..
        } = segment;

        let options: Vec<_> = options
            .into_iter()
            .map(|o| crate::analyse::infer_bit_array_option(o, |value, typ| self.unify(value, typ)))
            .try_collect()?;

        let segment_type = bit_array::type_options_for_pattern(&options, !is_last_segment)
            .map_err(|error| Error::BitArraySegmentError {
                error: error.error,
                location: error.location,
            })?;

        let typ = {
            match value.deref() {
                Pattern::Variable { .. } if segment_type == string() => {
                    Err(Error::BitArraySegmentError {
                        error: bit_array::ErrorType::VariableUtfSegmentInPattern,
                        location,
                    })
                }
                _ => Ok(segment_type),
            }
        }?;
        let typed_value = self.unify(*value, typ.clone())?;

        Ok(BitArraySegment {
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
            Pattern::Discard { name, location, .. } => Ok(Pattern::Discard {
                type_,
                name,
                location,
            }),

            Pattern::Variable { name, location, .. } => {
                self.insert_variable(&name, type_.clone(), location)
                    .map_err(|e| convert_unify_error(e, location))?;
                Ok(Pattern::Variable {
                    type_,
                    name,
                    location,
                })
            }

            Pattern::VarUsage { name, location, .. } => {
                let vc = self
                    .environment
                    .get_variable(&name)
                    .cloned()
                    .ok_or_else(|| Error::UnknownVariable {
                        location,
                        name: name.clone(),
                        variables: self.environment.local_value_names(),
                    })?;
                self.environment.increment_usage(&name);
                let typ =
                    self.environment
                        .instantiate(vc.type_.clone(), &mut hashmap![], self.hydrator);
                unify(int(), typ.clone()).map_err(|e| convert_unify_error(e, location))?;

                Ok(Pattern::VarUsage {
                    name,
                    location,
                    constructor: Some(vc),
                    type_: typ,
                })
            }

            Pattern::StringPrefix {
                location,
                left_location,
                right_location,
                left_side_string,
                left_side_assignment,
                right_side_assignment,
            } => {
                // The entire concatenate pattern must be a string
                unify(type_, string()).map_err(|e| convert_unify_error(e, location))?;

                // The left hand side may assign a variable, which is the prefix of the string
                if let Some((left, left_location)) = &left_side_assignment {
                    self.insert_variable(left.as_ref(), string(), *left_location)
                        .map_err(|e| convert_unify_error(e, location))?;
                }

                // The right hand side may assign a variable, which is the suffix of the string
                if let AssignName::Variable(right) = &right_side_assignment {
                    self.insert_variable(right.as_ref(), string(), right_location)
                        .map_err(|e| convert_unify_error(e, location))?;
                };

                Ok(Pattern::StringPrefix {
                    location,
                    left_location,
                    right_location,
                    left_side_string,
                    left_side_assignment,
                    right_side_assignment,
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
                unify(type_, int()).map_err(|e| convert_unify_error(e, location))?;
                Ok(Pattern::Int { location, value })
            }

            Pattern::Float { location, value } => {
                unify(type_, float()).map_err(|e| convert_unify_error(e, location))?;
                Ok(Pattern::Float { location, value })
            }

            Pattern::String { location, value } => {
                unify(type_, string()).map_err(|e| convert_unify_error(e, location))?;
                Ok(Pattern::String { location, value })
            }

            Pattern::List {
                location,
                elements,
                tail,
                ..
            } => match type_.get_app_args(
                true,
                PRELUDE_PACKAGE_NAME,
                PRELUDE_MODULE_NAME,
                "List",
                1,
                self.environment,
            ) {
                Some(args) => {
                    let type_ = args
                        .first()
                        .expect("Failed to get type argument of List")
                        .clone();
                    let elements = elements
                        .into_iter()
                        .map(|element| self.unify(element, type_.clone()))
                        .try_collect()?;
                    let type_ = list(type_);

                    let tail = match tail {
                        Some(tail) => Some(Box::new(self.unify(*tail, type_.clone())?)),
                        None => None,
                    };

                    Ok(Pattern::List {
                        location,
                        elements,
                        tail,
                        type_,
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
                    unify(tuple(elems_types.clone()), type_)
                        .map_err(|e| convert_unify_error(e, location))?;
                    let elems = elems
                        .into_iter()
                        .zip(elems_types)
                        .map(|(pattern, type_)| self.unify(pattern, type_))
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

            Pattern::BitArray { location, segments } => {
                unify(type_, bits()).map_err(|e| convert_unify_error(e, location))?;
                self.infer_pattern_bit_array(segments, location)
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
                            if pattern_args.len() == field_map.arity as usize {
                                return Err(Error::UnnecessarySpreadOperator {
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
                                        name: "_".into(),
                                        location: spread_location,
                                        type_: (),
                                    },
                                    location: spread_location,
                                    label: None,
                                    implicit: false,
                                };

                                pattern_args.insert(index_of_first_labelled_arg, new_call_arg);
                            }
                        }

                        field_map.reorder(&mut pattern_args, location)?
                    }

                    None => {
                        // The fun has no field map and so we error if arguments have been labelled
                        assert_no_labelled_arguments(&pattern_args)?;

                        if with_spread {
                            // The location of the spread operator itself
                            let spread_location = SrcSpan {
                                start: location.end - 3,
                                end: location.end - 1,
                            };

                            if let ValueConstructorVariant::Record { arity, .. } = &cons.variant {
                                while pattern_args.len() < usize::from(*arity) {
                                    pattern_args.push(CallArg {
                                        value: Pattern::Discard {
                                            name: "_".into(),
                                            location: spread_location,
                                            type_: (),
                                        },
                                        location: spread_location,
                                        label: None,
                                        implicit: true,
                                    });
                                }
                            };
                        }
                    }
                }

                let constructor_typ = cons.type_.clone();
                let constructor = match &cons.variant {
                    ValueConstructorVariant::Record {
                        name,
                        documentation,
                        module,
                        location,
                        constructor_index,
                        ..
                    } => PatternConstructor {
                        documentation: documentation.clone(),
                        name: name.clone(),
                        field_map: cons.field_map().cloned(),
                        module: Some(module.clone()),
                        location: *location,
                        constructor_index: *constructor_index,
                    },
                    ValueConstructorVariant::LocalVariable { .. }
                    | ValueConstructorVariant::LocalConstant { .. }
                    | ValueConstructorVariant::ModuleConstant { .. }
                    | ValueConstructorVariant::ModuleFn { .. } => {
                        panic!("Unexpected value constructor type for a constructor pattern.")
                    }
                };

                let constructor_deprecation = cons.deprecation.clone();
                match constructor_deprecation {
                    Deprecation::NotDeprecated => {}
                    Deprecation::Deprecated { message } => {
                        self.environment.warnings.emit(Warning::DeprecatedItem {
                            location,
                            message: message.clone(),
                            layer: Layer::Value,
                        })
                    }
                }

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
                                        implicit,
                                        label,
                                    } = arg;
                                    let value = self.unify(value, typ.clone())?;
                                    Ok(CallArg {
                                        value,
                                        location,
                                        implicit,
                                        label,
                                    })
                                })
                                .try_collect()?;
                            unify(type_, retrn.clone())
                                .map_err(|e| convert_unify_error(e, location))?;
                            Ok(Pattern::Constructor {
                                location,
                                module,
                                name,
                                arguments: pattern_args,
                                constructor: Inferred::Known(constructor),
                                with_spread,
                                type_: retrn.clone(),
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

                    Type::Named { .. } => {
                        if pattern_args.is_empty() {
                            unify(type_, instantiated_constructor_type.clone())
                                .map_err(|e| convert_unify_error(e, location))?;
                            Ok(Pattern::Constructor {
                                location,
                                module,
                                name,
                                arguments: vec![],
                                constructor: Inferred::Known(constructor),
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

                    _ => panic!("Unexpected constructor type for a constructor pattern."),
                }
            }
        }
    }
}
