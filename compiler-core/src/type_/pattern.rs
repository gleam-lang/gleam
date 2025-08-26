use ecow::eco_format;
use hexpm::version::{LowestVersion, Version};
use im::hashmap;
use itertools::Itertools;
use num_bigint::BigInt;

/// Type inference and checking of patterns used in case expressions
/// and variables bindings.
///
use super::*;
use crate::{
    analyse::{self, Inferred, name::check_name_case},
    ast::{
        AssignName, BitArrayOption, BitArraySize, ImplicitCallArgOrigin, Layer, TypedBitArraySize,
        UntypedPatternBitArraySegment,
    },
    parse::PatternPosition,
    reference::ReferenceKind,
    type_::expression::FunctionDefinition,
};
use std::sync::Arc;

pub struct PatternTyper<'a, 'b> {
    environment: &'a mut Environment<'b>,
    implementations: &'a Implementations,
    current_function: &'a FunctionDefinition,
    hydrator: &'a Hydrator,
    mode: PatternMode,
    initial_pattern_vars: HashSet<EcoString>,
    /// Variables which have been inferred to a specific variant of their type
    /// from this pattern-matching. Key is the variable name, Value is the inferred variant index.
    inferred_variant_variables: HashMap<EcoString, u16>,
    problems: &'a mut Problems,

    /// The minimum Gleam version required to compile the typed pattern.
    pub minimum_required_version: Version,

    pub error_encountered: bool,

    /// Variables which have been assigned in the current pattern. We can't
    /// register them immediately. If we're in a bit array, variables that are
    /// assigned in the pattern can be used as part of the pattern, e.g.
    /// `<<a, b:size(a)>>`. However, if we are not in a bit array pattern,
    /// variables cannot be used within the pattern. This is invalid:
    /// `#(size, <<a:size(size)>>)`. This is due to a limitation of Erlang.
    ///
    /// What we do instead is store the variables in this map. Each variable
    /// keeps track of whether it is in scope, so that we can correctly detect
    /// valid/invalid uses.
    variables: HashMap<EcoString, LocalVariable>,

    /// What kind of pattern we are typing
    position: PatternPosition,
}

#[derive(Debug)]
struct LocalVariable {
    location: SrcSpan,
    origin: VariableOrigin,
    type_: Arc<Type>,
    usage: Usage,
    scope: Scope,
}

impl LocalVariable {
    fn in_scope(&self) -> bool {
        match self.scope {
            Scope::CurrentBitArrayPattern => true,
            Scope::OtherPattern => false,
        }
    }

    fn was_used(&self) -> bool {
        match self.usage {
            Usage::UsedInPattern => true,
            Usage::UnusedSoFar => false,
        }
    }
}

#[derive(Debug, Clone, Copy)]
enum Usage {
    UsedInPattern,
    UnusedSoFar,
}

#[derive(Debug, Clone, Copy)]
enum Scope {
    CurrentBitArrayPattern,
    OtherPattern,
}

enum PatternMode {
    Initial,
    Alternative(Vec<EcoString>),
}

impl<'a, 'b> PatternTyper<'a, 'b> {
    pub fn new(
        environment: &'a mut Environment<'b>,
        implementations: &'a Implementations,
        current_function: &'a FunctionDefinition,
        hydrator: &'a Hydrator,
        problems: &'a mut Problems,
        position: PatternPosition,
    ) -> Self {
        Self {
            environment,
            implementations,
            current_function,
            hydrator,
            mode: PatternMode::Initial,
            initial_pattern_vars: HashSet::new(),
            inferred_variant_variables: HashMap::new(),
            minimum_required_version: Version::new(0, 1, 0),
            problems,
            error_encountered: false,
            variables: HashMap::new(),
            position,
        }
    }

    fn insert_variable(
        &mut self,
        name: &EcoString,
        type_: Arc<Type>,
        location: SrcSpan,
        origin: VariableOrigin,
    ) {
        self.check_name_case(location, name, Named::Variable);

        match &mut self.mode {
            PatternMode::Initial => {
                // Ensure there are no duplicate variable names in the pattern
                if self.initial_pattern_vars.contains(name) {
                    self.error(convert_unify_error(
                        UnifyError::DuplicateVarInPattern { name: name.clone() },
                        location,
                    ));
                    return;
                }
                // We no longer have access to the variable from the subject of the pattern
                // so it doesn't need to be inferred any more.
                let _ = self.inferred_variant_variables.remove(name);
                // Record that this variable originated in this pattern so any
                // following alternative patterns can be checked to ensure they
                // have the same variables.
                let _ = self.initial_pattern_vars.insert(name.clone());

                _ = self.variables.insert(
                    name.clone(),
                    LocalVariable {
                        location,
                        origin: origin.clone(),
                        type_: type_.clone(),
                        usage: Usage::UnusedSoFar,
                        scope: Scope::CurrentBitArrayPattern,
                    },
                );
            }

            PatternMode::Alternative(assigned) => {
                match self.environment.scope.get_mut(name) {
                    // This variable was defined in the Initial multi-pattern
                    Some(initial) if self.initial_pattern_vars.contains(name) => {
                        if assigned.contains(name) {
                            self.error(convert_unify_error(
                                UnifyError::DuplicateVarInPattern { name: name.clone() },
                                location,
                            ));
                            return;
                        }

                        assigned.push(name.clone());
                        let initial_type = initial.type_.clone();
                        match unify(initial_type, type_.clone()) {
                            Ok(()) => {}
                            Err(error) => {
                                self.problems.error(convert_unify_error(error, location));
                                self.error_encountered = true;
                            }
                        };
                        unify_constructor_variants(Arc::make_mut(&mut initial.type_), &type_);
                    }

                    // This variable was not defined in the Initial multi-pattern
                    _ => self.error(convert_unify_error(
                        UnifyError::ExtraVarInAlternativePattern { name: name.clone() },
                        location,
                    )),
                }
            }
        }
    }

    fn set_subject_variable_variant(&mut self, name: EcoString, variant_index: u16) {
        match &self.mode {
            PatternMode::Initial => {
                // If this name is reassigned in the pattern itself, we don't need to infer
                // it, since it isn't accessible in this scope anymore.
                if self.initial_pattern_vars.contains(&name) {
                    return;
                }

                let variable = self
                    .environment
                    .scope
                    .get(&name)
                    .expect("Variable already exists in the case subjects");

                // The type in this scope is now separate from the parent scope, so we
                // remove any links to ensure that they aren't linked in any way and that
                // we don't accidentally set the variant of the variable outside of this scope
                let mut type_ = collapse_links(variable.type_.clone());
                Arc::make_mut(&mut type_).set_custom_type_variant(variant_index);
                // Mark this variable as having been inferred
                let _ = self
                    .inferred_variant_variables
                    .insert(name.clone(), variant_index);

                let origin = match &variable.variant {
                    ValueConstructorVariant::LocalVariable { origin, .. } => origin.clone(),
                    ValueConstructorVariant::ModuleConstant { .. }
                    | ValueConstructorVariant::LocalConstant { .. }
                    | ValueConstructorVariant::ModuleFn { .. }
                    | ValueConstructorVariant::Record { .. } => VariableOrigin::generated(),
                };

                // This variable is only inferred in this branch of the case expression
                self.environment.insert_local_variable(
                    name.clone(),
                    variable.definition_location().span,
                    origin,
                    type_,
                );
            }

            PatternMode::Alternative(_) => {
                // If we haven't inferred this variable in all alternative patterns so far,
                // we can't set its variant here
                let Some(inferred_variant) = self.inferred_variant_variables.get(&name) else {
                    return;
                };

                // If multiple variants are possible in this pattern, we can't infer it at all
                // and we have to remove the variant index
                if *inferred_variant != variant_index {
                    // This variable's variant is no longer known
                    let _ = self.inferred_variant_variables.remove(&name);
                    let variable = self
                        .environment
                        .scope
                        .get_mut(&name)
                        .expect("Variable already exists in the case subjects");

                    Arc::make_mut(&mut variable.type_).generalise_custom_type_variant();
                }
            }
        }
    }

    pub fn infer_alternative_multi_pattern(
        &mut self,
        multi_pattern: UntypedMultiPattern,
        subjects: &[TypedExpr],
        location: &SrcSpan,
    ) -> Vec<TypedPattern> {
        self.mode = PatternMode::Alternative(vec![]);
        let typed_multi = self.infer_multi_pattern(multi_pattern, subjects, location);

        if self.error_encountered {
            return typed_multi;
        }

        match &self.mode {
            PatternMode::Initial => panic!("Pattern mode switched from Alternative to Initial"),
            PatternMode::Alternative(assigned)
                if assigned.len() < self.initial_pattern_vars.len() =>
            {
                for name in assigned {
                    let _ = self.initial_pattern_vars.remove(name);
                }
                self.error(Error::MissingVarInAlternativePattern {
                    location: *location,
                    // It is safe to use expect here as we checked the length above
                    name: self
                        .initial_pattern_vars
                        .iter()
                        .next()
                        .expect("Getting undefined pattern variable")
                        .clone(),
                });
                typed_multi
            }
            PatternMode::Alternative(_) => typed_multi,
        }
    }

    pub fn infer_multi_pattern(
        &mut self,
        multi_pattern: UntypedMultiPattern,
        subjects: &[TypedExpr],
        location: &SrcSpan,
    ) -> Vec<TypedPattern> {
        // If there are N subjects the multi-pattern is expected to be N patterns
        if subjects.len() != multi_pattern.len() {
            self.error(Error::IncorrectNumClausePatterns {
                location: *location,
                expected: subjects.len(),
                given: multi_pattern.len(),
            });
            return Vec::new();
        }

        // Unify each pattern in the multi-pattern with the corresponding subject
        let mut typed_multi = Vec::with_capacity(multi_pattern.len());
        for (pattern, subject) in multi_pattern.into_iter().zip(subjects) {
            let subject_variable = Self::subject_variable(subject);

            let pattern = self.unify(pattern, subject.type_(), subject_variable);
            typed_multi.push(pattern);
        }

        self.register_variables();

        typed_multi
    }

    pub fn infer_single_pattern(
        &mut self,
        pattern: UntypedPattern,
        subject: &TypedExpr,
    ) -> TypedPattern {
        let subject_variable = Self::subject_variable(subject);

        let typed_pattern = self.unify(pattern, subject.type_(), subject_variable);
        self.register_variables();
        typed_pattern
    }

    fn subject_variable(subject: &TypedExpr) -> Option<EcoString> {
        match subject {
            TypedExpr::Var {
                constructor:
                    ValueConstructor {
                        // Records should not be considered local variables
                        // See: https://github.com/gleam-lang/gleam/issues/3861
                        variant: ValueConstructorVariant::Record { .. },
                        ..
                    },
                ..
            } => None,
            TypedExpr::Var { name, .. } => Some(name.clone()),
            // If the subject of a `case` expression is something like
            // `echo some_variable`, we still want to narrow the variant for
            // `some_variable`.
            TypedExpr::Echo {
                expression: Some(subject),
                ..
            } => Self::subject_variable(subject),
            _ => None,
        }
    }

    /// Register the variables bound in this pattern in the environment
    fn register_variables(&mut self) {
        for (name, variable) in std::mem::take(&mut self.variables) {
            let was_used = variable.was_used();

            let LocalVariable {
                location,
                origin,
                type_,
                usage: _,
                scope: _,
            } = variable;

            // If this variable has already been referenced in another part of
            // the pattern, we don't need to register it for usage tracking as
            // it has already been used.
            if !was_used {
                self.environment
                    .init_usage(name.clone(), origin.clone(), location, self.problems);
            }

            self.environment
                .insert_local_variable(name, location, origin, type_);
        }
    }

    fn infer_pattern_bit_array(
        &mut self,
        mut segments: Vec<UntypedPatternBitArraySegment>,
        location: SrcSpan,
    ) -> TypedPattern {
        // Any variables from other parts of the pattern are no longer in scope.
        // Only variables from the bit array pattern itself can be used.
        for (_, variable) in self.variables.iter_mut() {
            variable.scope = Scope::OtherPattern;
        }

        let last_segment = segments.pop();

        let mut typed_segments: Vec<_> = segments
            .into_iter()
            .map(|s| self.infer_pattern_segment(s, false))
            .collect();

        if let Some(s) = last_segment {
            let typed_last_segment = self.infer_pattern_segment(s, true);
            typed_segments.push(typed_last_segment)
        }

        TypedPattern::BitArray {
            location,
            segments: typed_segments,
        }
    }

    fn infer_pattern_segment(
        &mut self,
        mut segment: UntypedPatternBitArraySegment,
        is_last_segment: bool,
    ) -> TypedPatternBitArraySegment {
        // If the segment doesn't have an explicit type option we add a default
        // one ourselves if the pattern is unambiguous: literal strings are
        // implicitly considered utf-8 encoded strings, while floats are
        // implicitly given the float type option.
        if !segment.has_type_option() {
            match segment.value_unwrapping_assign() {
                Pattern::String { location, .. } => {
                    self.track_feature_usage(FeatureKind::UnannotatedUtf8StringSegment, *location);
                    segment.options.push(BitArrayOption::Utf8 {
                        location: SrcSpan::default(),
                    });
                }

                Pattern::Float { location, .. } => {
                    self.track_feature_usage(FeatureKind::UnannotatedFloatSegment, *location);
                    segment.options.push(BitArrayOption::Float {
                        location: SrcSpan::default(),
                    })
                }

                _ => (),
            }
        }

        let has_non_utf8_string_option = segment.has_utf16_option() || segment.has_utf32_option();

        let options: Vec<_> = segment
            .options
            .into_iter()
            .map(|option| {
                analyse::infer_bit_array_option(option, |value, type_| {
                    Ok(self.unify(value, type_, None))
                })
            })
            .try_collect()
            .expect("The function always returns Ok");

        let segment_type = match bit_array::type_options_for_pattern(
            &options,
            !is_last_segment,
            self.environment.target,
        ) {
            Ok(type_) => type_,
            Err(error) => {
                self.error(Error::BitArraySegmentError {
                    error: error.error,
                    location: error.location,
                });
                self.environment.new_unbound_var()
            }
        };

        // Track usage of the unaligned bit arrays feature on JavaScript so that
        // warnings can be emitted if the Gleam version constraint is too low
        if self.environment.target == Target::JavaScript
            && !self.current_function.has_javascript_external
        {
            for option in options.iter() {
                match option {
                    // Use of the `bits` segment type
                    BitArrayOption::<TypedPattern>::Bits { location } => {
                        self.track_feature_usage(
                            FeatureKind::JavaScriptUnalignedBitArray,
                            *location,
                        );
                    }

                    // Int segments that aren't a whole number of bytes
                    BitArrayOption::<TypedPattern>::Size { value, .. } if segment_type.is_int() => {
                        match &**value {
                            Pattern::BitArraySize(BitArraySize::Int {
                                location,
                                int_value,
                                ..
                            }) if int_value % 8 != BigInt::ZERO => {
                                self.track_feature_usage(
                                    FeatureKind::JavaScriptUnalignedBitArray,
                                    *location,
                                );
                            }
                            _ => (),
                        }
                    }

                    _ => (),
                }
            }
        }

        let type_ = match segment.value.deref() {
            Pattern::Assign { pattern, .. } if pattern.is_discard() && segment_type.is_string() => {
                self.error(Error::BitArraySegmentError {
                    error: bit_array::ErrorType::VariableUtfSegmentInPattern,
                    location: segment.location,
                });
                self.environment.new_unbound_var()
            }
            Pattern::Variable { .. } if segment_type.is_string() => {
                self.error(Error::BitArraySegmentError {
                    error: bit_array::ErrorType::VariableUtfSegmentInPattern,
                    location: segment.location,
                });
                self.environment.new_unbound_var()
            }
            _ => segment_type,
        };

        let typed_value = self.unify(*segment.value, type_.clone(), None);

        match &typed_value {
            // We can't directly match on the contents of a `Box`, so we must
            // use a guard here.
            Pattern::Assign {
                location, pattern, ..
            } if pattern.is_variable() => {
                // It is tricky to generate code on Erlang for a pattern like
                // `<<a as b>>`, since assignment patterns are not allowed in
                // bit array patterns in Erlang. Since there is basically no
                // reason to ever need to do this anyway, we simply emit an error
                // here.
                self.error(Error::DoubleVariableAssignmentInBitArray {
                    location: *location,
                });
            }
            Pattern::Assign { location, .. } if has_non_utf8_string_option => {
                self.error(Error::NonUtf8StringAssignmentInBitArray {
                    location: *location,
                });
            }
            _ => {}
        };

        BitArraySegment {
            location: segment.location,
            value: Box::new(typed_value),
            options,
            type_,
        }
    }

    /// When we have an assignment or a case expression we unify the pattern with the
    /// inferred type of the subject in order to determine what variables to insert
    /// into the environment (or to detect a type error).
    ///
    fn unify(
        &mut self,
        pattern: UntypedPattern,
        type_: Arc<Type>,
        // The name of the variable this pattern matches on, if any. Used for variant inference.
        //
        // Example:
        // ```gleam
        // case some_wibble {
        //   Wibble(..) -> {
        //     some_wibble.field_only_present_in_wibble
        //   }
        //   _ -> panic
        // }
        // ```
        //
        // Here, the pattern `Wibble(..)` has the subject variable `some_wibble`, meaning that
        // in the inner scope, we can infer that the `some_wibble` variable is the `Wibble` variant
        //
        subject_variable: Option<EcoString>,
    ) -> TypedPattern {
        match pattern {
            Pattern::Discard { name, location, .. } => {
                self.check_name_case(location, &name, Named::Discard);
                let _ = self
                    .environment
                    .discarded_names
                    .insert(name.clone(), location);
                Pattern::Discard {
                    type_,
                    name,
                    location,
                }
            }

            Pattern::Invalid { location, .. } => Pattern::Invalid { type_, location },

            Pattern::Variable {
                name,
                location,
                origin,
                ..
            } => {
                self.insert_variable(&name, type_.clone(), location, origin.clone());

                Pattern::Variable {
                    type_,
                    name,
                    location,
                    origin,
                }
            }

            Pattern::BitArraySize(size) => {
                let location = size.location();
                match self.bit_array_size(size, type_.clone()) {
                    Ok(size) => Pattern::BitArraySize(size),
                    Err(error) => {
                        self.error(error);
                        Pattern::Invalid { location, type_ }
                    }
                }
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
                self.unify_types(type_, string(), location);

                // The left hand side may assign a variable, which is the prefix of the string
                if let Some((left, left_location)) = &left_side_assignment {
                    self.insert_variable(
                        left,
                        string(),
                        *left_location,
                        VariableOrigin {
                            syntax: VariableSyntax::AssignmentPattern,
                            declaration: self.position.to_declaration(),
                        },
                    );
                }

                // The right hand side may assign a variable, which is the suffix of the string
                match &right_side_assignment {
                    AssignName::Variable(right) => {
                        self.insert_variable(
                            right,
                            string(),
                            right_location,
                            VariableOrigin {
                                syntax: VariableSyntax::Variable(right.clone()),
                                declaration: self.position.to_declaration(),
                            },
                        );
                    }
                    AssignName::Discard(right) => {
                        let _ = self
                            .environment
                            .discarded_names
                            .insert(right.clone(), right_location);
                        self.check_name_case(right_location, right, Named::Discard);
                    }
                };

                Pattern::StringPrefix {
                    location,
                    left_location,
                    right_location,
                    left_side_string,
                    left_side_assignment,
                    right_side_assignment,
                }
            }

            Pattern::Assign {
                name,
                pattern,
                location,
            } => {
                let pattern = self.unify(*pattern, type_, subject_variable);

                if pattern.is_discard() {
                    self.problems.warning(Warning::UnusedDiscardPattern {
                        location,
                        name: name.clone(),
                    });
                }
                self.insert_variable(
                    &name,
                    pattern.type_().clone(),
                    location,
                    VariableOrigin {
                        syntax: VariableSyntax::AssignmentPattern,
                        declaration: self.position.to_declaration(),
                    },
                );
                Pattern::Assign {
                    name,
                    pattern: Box::new(pattern),
                    location,
                }
            }

            Pattern::Int {
                location,
                value,
                int_value,
            } => {
                self.unify_types(type_, int(), location);

                if self.environment.target == Target::JavaScript
                    && !self.current_function.has_javascript_external
                {
                    check_javascript_int_safety(&int_value, location, self.problems);
                }

                Pattern::Int {
                    location,
                    value,
                    int_value,
                }
            }

            Pattern::Float { location, value } => {
                self.unify_types(type_, float(), location);

                if self.environment.target == Target::Erlang
                    && !self.implementations.uses_erlang_externals
                {
                    check_erlang_float_safety(&value, location, self.problems)
                }

                Pattern::Float { location, value }
            }

            Pattern::String { location, value } => {
                self.unify_types(type_, string(), location);
                Pattern::String { location, value }
            }

            Pattern::List {
                location,
                elements,
                tail,
                ..
            } => match type_.get_app_arguments(
                Publicity::Public,
                PRELUDE_PACKAGE_NAME,
                PRELUDE_MODULE_NAME,
                "List",
                1,
                self.environment,
            ) {
                Some(arguments) => {
                    let type_ = arguments
                        .first()
                        .expect("Failed to get type argument of List")
                        .clone();
                    let elements = elements
                        .into_iter()
                        .map(|element| self.unify(element, type_.clone(), None))
                        .collect();
                    let type_ = list(type_);

                    let tail = tail.map(|tail| Box::new(self.unify(*tail, type_.clone(), None)));

                    Pattern::List {
                        location,
                        elements,
                        tail,
                        type_,
                    }
                }

                None => {
                    self.problems.error(Error::CouldNotUnify {
                        given: list(self.environment.new_unbound_var()),
                        expected: type_.clone(),
                        situation: None,
                        location,
                    });
                    self.error_encountered = true;

                    Pattern::Invalid { location, type_ }
                }
            },

            Pattern::Tuple { elements, location } => match collapse_links(type_.clone()).deref() {
                Type::Tuple {
                    elements: type_elements,
                } => {
                    if elements.len() != type_elements.len() {
                        self.error(Error::IncorrectArity {
                            labels: vec![],
                            location,
                            context: IncorrectArityContext::Pattern,
                            expected: type_elements.len(),
                            given: elements.len(),
                        });
                        return Pattern::Invalid { location, type_ };
                    }

                    let elements = elements
                        .into_iter()
                        .zip(type_elements)
                        .map(|(pattern, type_)| self.unify(pattern, type_.clone(), None))
                        .collect();
                    Pattern::Tuple { elements, location }
                }

                Type::Var { .. } => {
                    let elements_types: Vec<_> = (0..(elements.len()))
                        .map(|_| self.environment.new_unbound_var())
                        .collect();
                    self.unify_types(tuple(elements_types.clone()), type_, location);
                    let elements = elements
                        .into_iter()
                        .zip(elements_types)
                        .map(|(pattern, type_)| self.unify(pattern, type_, None))
                        .collect();
                    Pattern::Tuple { elements, location }
                }

                _ => {
                    let elements_types = (0..(elements.len()))
                        .map(|_| self.environment.new_unbound_var())
                        .collect();

                    self.error(Error::CouldNotUnify {
                        given: tuple(elements_types),
                        expected: type_.clone(),
                        situation: None,
                        location,
                    });
                    Pattern::Invalid { location, type_ }
                }
            },

            Pattern::BitArray { location, segments } => {
                self.unify_types(type_, bit_array(), location);
                self.infer_pattern_bit_array(segments, location)
            }

            Pattern::Constructor {
                location,
                module,
                name_location,
                name,
                arguments: mut pattern_arguments,
                spread,
                ..
            } => {
                // Register the value as seen for detection of unused values
                self.environment.increment_usage(&name);

                let constructor = self
                    .environment
                    .get_value_constructor(module.as_ref().map(|(module, _)| module), &name);

                let constructor = match constructor {
                    Ok(constructor) => constructor,
                    Err(error) => {
                        self.error(convert_get_value_constructor_error(
                            error,
                            location,
                            module.as_ref().map(|(_, location)| *location),
                        ));

                        // If there's no constructor we still try and infer all
                        // the pattern arguments and produce an unknown constructor.
                        return Pattern::Constructor {
                            location,
                            name_location,
                            name,
                            arguments: self.infer_pattern_call_arguments(pattern_arguments, &[]),
                            module,
                            constructor: Inferred::Unknown,
                            spread,
                            type_,
                        };
                    }
                };

                let mut incorrect_arity_error = false;
                match constructor.field_map() {
                    // The fun has a field map so labelled arguments may be present and need to be reordered.
                    Some(field_map) => {
                        if let Some(spread_location) = spread {
                            // Using the spread operator when you have already provided variables for all of the
                            // record's fields throws an error
                            if pattern_arguments.len() == field_map.arity as usize {
                                {
                                    self.problems.error(Error::UnnecessarySpreadOperator {
                                        location: spread_location,
                                        arity: field_map.arity as usize,
                                    });
                                    self.error_encountered = true;
                                };
                            }

                            // Insert discard variables to match the unspecified fields
                            // In order to support both positional and labelled arguments we have to insert
                            // them after all positional variables and before the labelled ones. This means
                            // we have calculate that index and then insert() the discards. It would be faster
                            // if we could put the discards anywhere which would let us use push().
                            // Potential future optimisation.
                            let index_of_first_labelled_arg = pattern_arguments
                                .iter()
                                .position(|argument| argument.label.is_some())
                                .unwrap_or(pattern_arguments.len());

                            // In Gleam we can pass in positional unlabelled args to a constructor
                            // even if the field was defined as labelled
                            //
                            //     pub type Wibble {
                            //       Wibble(Int, two: Int, three: Int, four: Int)
                            //     }
                            //     Wibble(1, 2, 3, 4)
                            //
                            // When using `..` to ignore some fields the compiler needs to add a
                            // placeholder implicit discard pattern for each one of the ignored
                            // arguments. To give those discards the proper missing label we need to
                            // know how many of the labelled fields were provided as unlabelled.
                            //
                            // That's why we want to keep track of the number of unlabelled argument
                            // that have been supplied to the pattern and all the labels that have
                            // been explicitly supplied.
                            //
                            //     Wibble(a, b, four: c, ..)
                            //            ┬───  ┬──────
                            //            │     ╰ We supplied 1 labelled arg
                            //            ╰ We supplied 2 unlabelled args
                            //
                            let supplied_unlabelled_arguments = index_of_first_labelled_arg;
                            let supplied_labelled_arguments = pattern_arguments
                                .iter()
                                .filter_map(|argument| argument.label.clone())
                                .collect::<HashSet<_>>();
                            let constructor_unlabelled_arguments =
                                field_map.arity - field_map.fields.len() as u32;
                            let labelled_arguments_supplied_as_unlabelled =
                                supplied_unlabelled_arguments
                                    .saturating_sub(constructor_unlabelled_arguments as usize);

                            let mut missing_labels = field_map
                                .fields
                                .iter()
                                // We take the labels in order of definition in the constructor...
                                .sorted_by_key(|(_, position)| *position)
                                .map(|(label, _)| label.clone())
                                // ...and then remove the ones that were supplied as unlabelled
                                // positional arguments...
                                .skip(labelled_arguments_supplied_as_unlabelled)
                                // ... lastly we still need to remove all those labels that
                                // were explicitly supplied in the pattern.
                                .filter(|label| !supplied_labelled_arguments.contains(label));

                            while pattern_arguments.len() < field_map.arity as usize {
                                let new_call_arg = CallArg {
                                    value: Pattern::Discard {
                                        name: "_".into(),
                                        location: spread_location,
                                        type_: (),
                                    },
                                    location: spread_location,
                                    label: missing_labels.next(),
                                    implicit: Some(ImplicitCallArgOrigin::PatternFieldSpread),
                                };

                                pattern_arguments.insert(index_of_first_labelled_arg, new_call_arg);
                            }
                        }

                        if let Err(error) = field_map.reorder(
                            &mut pattern_arguments,
                            location,
                            IncorrectArityContext::Pattern,
                        ) {
                            incorrect_arity_error = true;
                            self.problems.error(error);
                            self.error_encountered = true;
                        }
                    }

                    None => {
                        // The fun has no field map and so we error if arguments have been labelled
                        match assert_no_labelled_arguments(&pattern_arguments) {
                            Ok(()) => {}
                            Err(error) => {
                                self.problems.error(error);
                                self.error_encountered = true;
                            }
                        }

                        if let Some(spread_location) = spread
                            && let ValueConstructorVariant::Record { arity, .. } =
                                &constructor.variant
                        {
                            while pattern_arguments.len() < usize::from(*arity) {
                                pattern_arguments.push(CallArg {
                                    value: Pattern::Discard {
                                        name: "_".into(),
                                        location: spread_location,
                                        type_: (),
                                    },
                                    location: spread_location,
                                    label: None,
                                    implicit: Some(ImplicitCallArgOrigin::PatternFieldSpread),
                                });
                            }
                        };
                    }
                }

                let constructor_type = constructor.type_.clone();
                let constructor_deprecation = constructor.deprecation.clone();
                let pattern_constructor = match &constructor.variant {
                    ValueConstructorVariant::Record {
                        name,
                        documentation,
                        module,
                        location,
                        variant_index: constructor_index,
                        ..
                    } => {
                        let constructor_index = *constructor_index;
                        let constructor = PatternConstructor {
                            documentation: documentation.clone(),
                            name: name.clone(),
                            field_map: constructor.field_map().cloned(),
                            module: module.clone(),
                            location: *location,
                            constructor_index,
                        };

                        if let Some(ref variable_name) = subject_variable {
                            self.set_subject_variable_variant(
                                variable_name.clone(),
                                constructor_index,
                            );
                        }

                        constructor
                    }
                    ValueConstructorVariant::LocalVariable { .. }
                    | ValueConstructorVariant::LocalConstant { .. }
                    | ValueConstructorVariant::ModuleConstant { .. }
                    | ValueConstructorVariant::ModuleFn { .. } => {
                        panic!("Unexpected value constructor type for a constructor pattern.")
                    }
                };

                match constructor_deprecation {
                    Deprecation::NotDeprecated => {}
                    Deprecation::Deprecated { message } => {
                        self.problems.warning(Warning::DeprecatedItem {
                            location,
                            message: message.clone(),
                            layer: Layer::Value,
                        })
                    }
                }

                self.environment.references.register_value_reference(
                    pattern_constructor.module.clone(),
                    pattern_constructor.name.clone(),
                    &name,
                    name_location,
                    if module.is_some() {
                        ReferenceKind::Qualified
                    } else {
                        ReferenceKind::Unqualified
                    },
                );

                let instantiated_constructor_type =
                    self.environment
                        .instantiate(constructor_type, &mut hashmap![], self.hydrator);
                match instantiated_constructor_type.deref() {
                    Type::Fn { arguments, return_ } => {
                        self.unify_types(type_.clone(), return_.clone(), location);

                        if let Some((variable_to_infer, inferred_variant)) =
                            subject_variable.zip(return_.custom_type_inferred_variant())
                        {
                            self.set_subject_variable_variant(variable_to_infer, inferred_variant);
                        }

                        // We're emitting the incorrect arity error only if we haven't emitted
                        // one already. This might happen when we can't reorder the field map
                        // of a constructor because there's not enough labels.
                        if arguments.len() != pattern_arguments.len() && !incorrect_arity_error {
                            self.error(Error::IncorrectArity {
                                labels: vec![],
                                location,
                                context: IncorrectArityContext::Pattern,
                                expected: arguments.len(),
                                given: pattern_arguments.len(),
                            });
                        }

                        let pattern_arguments =
                            self.infer_pattern_call_arguments(pattern_arguments, arguments);

                        Pattern::Constructor {
                            location,
                            name_location,
                            name,
                            module,
                            constructor: Inferred::Known(pattern_constructor),
                            arguments: pattern_arguments,
                            spread,
                            type_: return_.clone(),
                        }
                    }

                    Type::Named {
                        inferred_variant, ..
                    } => {
                        self.unify_types(type_, instantiated_constructor_type.clone(), location);

                        if let Some((variable_to_infer, inferred_variant)) =
                            subject_variable.zip(*inferred_variant)
                        {
                            self.set_subject_variable_variant(variable_to_infer, inferred_variant);
                        }

                        if !pattern_arguments.is_empty() {
                            self.error(Error::IncorrectArity {
                                labels: vec![],
                                location,
                                context: IncorrectArityContext::Pattern,
                                expected: 0,
                                given: pattern_arguments.len(),
                            });
                        }
                        Pattern::Constructor {
                            location,
                            name_location,
                            module,
                            name,
                            arguments: vec![],
                            constructor: Inferred::Known(pattern_constructor),
                            spread,
                            type_: instantiated_constructor_type,
                        }
                    }

                    _ => panic!("Unexpected constructor type for a constructor pattern."),
                }
            }
        }
    }

    fn infer_pattern_call_arguments(
        &mut self,
        pattern_arguments: Vec<CallArg<UntypedPattern>>,
        expected_types: &[Arc<Type>],
    ) -> Vec<CallArg<TypedPattern>> {
        pattern_arguments
            .into_iter()
            .enumerate()
            .map(|(index, arg)| {
                if !arg.is_implicit() && arg.uses_label_shorthand() {
                    self.track_feature_usage(FeatureKind::LabelShorthandSyntax, arg.location);
                }

                let CallArg {
                    value,
                    location,
                    implicit,
                    label,
                } = arg;

                let type_ = expected_types
                    .get(index)
                    .cloned()
                    .unwrap_or_else(|| self.environment.new_unbound_var());

                let value = self.unify(value, type_, None);
                CallArg {
                    value,
                    location,
                    implicit,
                    label,
                }
            })
            .collect()
    }

    fn bit_array_size(
        &mut self,
        size: BitArraySize<()>,
        type_: Arc<Type>,
    ) -> Result<TypedBitArraySize, Error> {
        let typed_size = match size {
            BitArraySize::Int {
                location,
                value,
                int_value,
            } => {
                self.unify_types(type_, int(), location);

                if self.environment.target == Target::JavaScript
                    && !self.current_function.has_javascript_external
                {
                    check_javascript_int_safety(&int_value, location, self.problems);
                }

                BitArraySize::Int {
                    location,
                    value,
                    int_value,
                }
            }
            BitArraySize::Variable { name, location, .. } => {
                let constructor = match self.variables.get_mut(&name) {
                    // If we've bound a variable in the current bit array pattern,
                    // we want to use that.
                    Some(variable) if variable.in_scope() => {
                        variable.usage = Usage::UsedInPattern;
                        ValueConstructor::local_variable(
                            variable.location,
                            variable.origin.clone(),
                            variable.type_.clone(),
                        )
                    }
                    // Otherwise, we check the local scope.
                    Some(_) | None => match self.environment.get_variable(&name) {
                        Some(constructor) => constructor.clone(),
                        None => {
                            return Err(Error::UnknownVariable {
                                location,
                                name: name.clone(),
                                variables: self.environment.local_value_names(),
                                discarded_location: self
                                    .environment
                                    .discarded_names
                                    .get(&eco_format!("_{name}"))
                                    .cloned(),
                                type_with_name_in_scope: self
                                    .environment
                                    .module_types
                                    .keys()
                                    .any(|type_| type_ == &name),
                            });
                        }
                    },
                };

                self.environment.increment_usage(&name);
                let type_ = self.environment.instantiate(
                    constructor.type_.clone(),
                    &mut hashmap![],
                    self.hydrator,
                );
                self.unify_types(int(), type_.clone(), location);

                BitArraySize::Variable {
                    name,
                    location,
                    constructor: Some(Box::new(constructor)),
                    type_,
                }
            }
            BitArraySize::BinaryOperator {
                location,
                operator,
                left,
                right,
            } => BitArraySize::BinaryOperator {
                location,
                operator,
                left: Box::new(self.bit_array_size(*left, type_.clone())?),
                right: Box::new(self.bit_array_size(*right, type_)?),
            },
            BitArraySize::Block { location, inner } => BitArraySize::Block {
                location,
                inner: Box::new(self.bit_array_size(*inner, type_)?),
            },
        };

        Ok(typed_size)
    }

    fn check_name_case(&mut self, location: SrcSpan, name: &EcoString, kind: Named) {
        if let Err(error) = check_name_case(location, name, kind) {
            self.problems.error(error);
        }
    }

    fn unify_types(&mut self, first: Arc<Type>, second: Arc<Type>, location: SrcSpan) {
        match unify(first, second) {
            Ok(()) => {}
            Err(error) => self.error(convert_unify_error(error, location)),
        }
    }

    fn error(&mut self, error: Error) {
        self.problems.error(error);
        self.error_encountered = true;
    }

    fn track_feature_usage(&mut self, feature_kind: FeatureKind, location: SrcSpan) {
        let minimum_required_version = feature_kind.required_version();

        // Then if the required version is not in the specified version for the
        // range we emit a warning highlighting the usage of the feature.
        if let Some(gleam_version) = &self.environment.gleam_version
            && let Some(lowest_allowed_version) = gleam_version.lowest_version()
        {
            // There is a version in the specified range that is lower than
            // the one required by this feature! This means that the
            // specified range is wrong and would allow someone to run a
            // compiler that is too old to know of this feature.
            if minimum_required_version > lowest_allowed_version {
                self.problems
                    .warning(Warning::FeatureRequiresHigherGleamVersion {
                        location,
                        feature_kind,
                        minimum_required_version: minimum_required_version.clone(),
                        wrongfully_allowed_version: lowest_allowed_version,
                    })
            }
        }

        if minimum_required_version > self.minimum_required_version {
            self.minimum_required_version = minimum_required_version;
        }
    }
}

/// Unifies the variants of two variables declared in alternative patterns.
///
/// This ensures that constructor variant information is only stored if
/// all alternate pattern variables have the same variant. For example:
///
/// ```gleam
/// type Wibble {
///   Wibble(wibble: Int, wobble: Float)
///   Wobble(wubble: String, wooble, Bool)
/// }
///
/// case some_value {
///   Wibble(..) as wibble | Wobble(..) as wibble ->
///     Wibble(..wibble, wobble: 1.3)
/// }
/// ```
///
/// The `wibble` variable will not have the constructor variant stored,
/// since it can be one of two possible variants.
///
fn unify_constructor_variants(into: &mut Type, from: &Type) {
    match (into, from) {
        (
            Type::Named {
                inferred_variant: into_index,
                ..
            },
            Type::Named {
                inferred_variant: from_index,
                ..
            },
        ) if from_index != into_index => *into_index = None,
        // If the variants are the same, or they aren't both named types,
        // no modifications are needed
        _ => {}
    }
}
