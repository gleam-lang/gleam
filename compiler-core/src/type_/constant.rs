// SPDX-License-Identifier: Apache-2.0
// SPDX-FileCopyrightText: 2026 The Gleam contributors

use std::sync::Arc;

use ecow::EcoString;
use im::{HashSet, hashmap};
use itertools::Itertools;
use src_span::SrcSpan;

use crate::{
    analyse::Inferred,
    ast::{
        BinOp, BitArrayOption, CallArg, Constant, InvalidExpression, TypedConstant,
        UntypedConstant, UntypedConstantBitArraySegment,
    },
    build::Target,
    type_::{
        Error, ExprTyper, FieldAccessUsage, HasType, Type, ValueConstructorVariant, ValueUsage,
        Warning, assert_no_labelled_arguments, bit_array,
        error::{
            FeatureKind, IncorrectArityContext, RecordField, UnexpectedLabelledArgKind, UnifyError,
            UnsafeRecordUpdateReason, check_float_safety, check_javascript_int_safety,
            convert_unify_error,
        },
        expression::{CallKind, FunctionTypeMatch, IgnoredLabelledArgument},
        list, string, tuple, unify,
    },
};

pub struct ConstantTyper<'expression_typer, 'env, 'module> {
    typer: &'expression_typer mut ExprTyper<'env, 'module>,
}

impl<'expression_typer, 'env, 'module> ConstantTyper<'expression_typer, 'env, 'module> {
    pub fn new(typer: &'expression_typer mut ExprTyper<'env, 'module>) -> Self {
        Self { typer }
    }

    pub fn infer(mut self, constant: UntypedConstant) -> TypedConstant {
        self.do_infer(constant)
    }

    fn do_infer(&mut self, value: UntypedConstant) -> TypedConstant {
        match value {
            Constant::Int {
                location,
                value,
                int_value,
            } => {
                if self.typer.environment.target == Target::JavaScript {
                    check_javascript_int_safety(&int_value, location, self.typer.problems);
                }

                Constant::Int {
                    location,
                    value,
                    int_value,
                }
            }

            Constant::Float {
                location,
                value,
                float_value,
            } => {
                check_float_safety(float_value, location, self.typer.problems);
                Constant::Float {
                    location,
                    value,
                    float_value,
                }
            }

            Constant::String {
                location, value, ..
            } => Constant::String { location, value },

            Constant::Tuple {
                elements, location, ..
            } => self.infer_tuple(elements, location),

            Constant::List {
                elements,
                location,
                tail,
                ..
            } => self.infer_list(elements, location, tail),

            Constant::BitArray { location, segments } => {
                match self.infer_bit_array(segments, location) {
                    Ok(inferred) => inferred,
                    Err(error) => {
                        self.typer.problems.error(error);
                        Constant::Invalid {
                            location,
                            type_: bit_array(),
                            extra_information: None,
                        }
                    }
                }
            }

            Constant::RecordUpdate {
                constructor_location,
                module,
                location,
                name,
                record,
                arguments,
                ..
            } => {
                self.typer
                    .track_feature_usage(FeatureKind::ConstantRecordUpdate, location);
                let first_argument_start =
                    arguments.first().map(|argument| argument.location.start);
                let constructor = match self.typer.infer_value_constructor(
                    &module,
                    &name,
                    &location,
                    ValueUsage::Call {
                        arity: arguments.len(),
                    },
                ) {
                    Ok(constructor) => constructor,
                    Err(error) => {
                        self.typer.problems.error(error);
                        return self.new_invalid_constant(location);
                    }
                };

                let (constructor_tag, field_map) = match &constructor.variant {
                    ValueConstructorVariant::Record {
                        name,
                        field_map: Some(field_map),
                        ..
                    } => (name.clone(), field_map.clone()),

                    ValueConstructorVariant::Record {
                        field_map: None, ..
                    } => {
                        self.typer
                            .problems
                            .error(Error::RecordUpdateVariantWithNoFields {
                                location: constructor_location,
                            });
                        return self.new_invalid_constant(location);
                    }

                    ValueConstructorVariant::ModuleFn { .. }
                    | ValueConstructorVariant::LocalVariable { .. } => {
                        self.typer
                            .problems
                            .error(Error::NonLocalClauseGuardVariable { location, name });
                        return self.new_invalid_constant(location);
                    }

                    ValueConstructorVariant::ModuleConstant { .. } => {
                        unreachable!("constant called as a record constructor")
                    }
                };

                // Type-check the record being updated
                let typed_record = self.do_infer(*record.base.clone());
                let typed_record_type = typed_record.type_();

                // Instantiate the constructor type to enable generic re-specialization.
                let instantiated_constructor_type = self
                    .typer
                    .instantiate(constructor.type_.clone(), &mut hashmap![]);

                // Extract field types and return type from the instantiated constructor
                let (field_types, expected_type) = match instantiated_constructor_type.as_ref() {
                    Type::Fn { arguments, return_ } => (arguments.clone(), return_.clone()),
                    Type::Named { .. } | Type::Var { .. } | Type::Tuple { .. } => {
                        self.typer
                            .problems
                            .error(Error::RecordUpdateInvalidConstructor {
                                location: constructor_location,
                            });
                        return self.new_invalid_constant(location);
                    }
                };

                // If the record being updated is a reference to a constant variable, resolve
                // it to get the actual record value
                let resolved_record = match &typed_record {
                    Constant::Var {
                        constructor: Some(value_constructor),
                        ..
                    } => match &value_constructor.variant {
                        ValueConstructorVariant::ModuleConstant { literal, .. } => literal.clone(),
                        ValueConstructorVariant::LocalVariable { .. }
                        | ValueConstructorVariant::ModuleFn { .. }
                        | ValueConstructorVariant::Record { .. } => typed_record,
                    },
                    Constant::Int { .. }
                    | Constant::Float { .. }
                    | Constant::String { .. }
                    | Constant::Tuple { .. }
                    | Constant::List { .. }
                    | Constant::Record { .. }
                    | Constant::RecordUpdate { .. }
                    | Constant::BitArray { .. }
                    | Constant::Var { .. }
                    | Constant::BinaryOperator { .. }
                    | Constant::Todo { .. }
                    | Constant::Invalid { .. } => typed_record,
                };

                // Get the field arguments from the record that we'll use as the base.
                let (base_arguments, updated_record_tag) = if let Constant::Record {
                    arguments,
                    record_constructor: Some(resolved_record_constructor),
                    ..
                } = resolved_record
                    && let ValueConstructorVariant::Record { name, .. } =
                        resolved_record_constructor.variant
                {
                    (arguments.unwrap_or(vec![]), name)
                } else {
                    self.typer.problems.error(convert_unify_error(
                        UnifyError::CouldNotUnify {
                            expected: expected_type,
                            given: typed_record_type,
                            situation: None,
                        },
                        record.base.location(),
                    ));
                    return self.new_invalid_constant(location);
                };

                // Check that the variant being spread matches the constructor variant
                // For multi-variant custom types, you can't spread Dog to create Cat
                if constructor_tag != updated_record_tag {
                    self.typer.problems.error(Error::UnsafeRecordUpdate {
                        location: record.base.location(),
                        reason: UnsafeRecordUpdateReason::WrongVariant {
                            constructed_variant: constructor_tag,
                            spread_variant: updated_record_tag,
                        },
                    });
                    return self.new_invalid_constant(location);
                }

                // Emit warning if no fields are being overridden
                if arguments.is_empty() {
                    self.typer
                        .problems
                        .warning(Warning::NoFieldsRecordUpdate { location });
                }

                let mut implicit_labelled_arguments = field_map.fields.clone();
                let mut update_argument_indices = HashSet::new();

                let mut final_arguments = base_arguments;
                for argument in arguments {
                    let syntax = argument.label_syntax();
                    let label_location = argument.label_location();
                    if argument.uses_label_shorthand() {
                        self.typer.track_feature_usage(
                            FeatureKind::LabelShorthandSyntax,
                            argument.location,
                        );
                    }

                    let label = &argument.label;
                    let typed_value = self.do_infer(argument.value);

                    let Some(index) = implicit_labelled_arguments.remove(label) else {
                        if field_map.fields.contains_key(label) {
                            self.typer.problems.error(Error::DuplicateArgument {
                                location: argument.location,
                                label: label.clone(),
                            });
                        } else {
                            self.typer.problems.error(self.typer.unknown_field_error(
                                field_map.fields.keys().cloned().collect(),
                                expected_type,
                                argument.location,
                                label.clone(),
                                FieldAccessUsage::Other,
                            ));
                        }

                        return self.new_invalid_constant(location);
                    };

                    // Record update argument value must match the field type
                    if let Some(expected_type) = field_types.get(index as usize)
                        && let Err(error) = unify(expected_type.clone(), typed_value.type_())
                    {
                        self.typer
                            .problems
                            .error(convert_unify_error(error, typed_value.location()));
                        return self.new_invalid_constant(location);
                    }

                    if let Some(type_name) = expected_type.named_type_name() {
                        self.typer.environment.references.register_label_reference(
                            type_name,
                            label.clone(),
                            label_location,
                            syntax,
                        );
                    }

                    let _ = update_argument_indices.insert(index as usize);

                    *final_arguments
                        .get_mut(index as usize)
                        .expect("Index out of bounds") = CallArg {
                        label: Some(label.clone()),
                        value: typed_value,
                        location: argument.location,
                        implicit: None,
                    };
                }

                // Emit warning if all fields are being overridden
                if implicit_labelled_arguments.is_empty() {
                    self.typer.problems.warning(Warning::AllFieldsRecordUpdate {
                        location,
                        record_location: SrcSpan::new(
                            record.location.start,
                            first_argument_start.unwrap_or(record.location.end),
                        ),
                    });
                }

                // Check that fields implicitly overridden (including unlabelled ones) have compatible types.
                for (index, field_arg) in final_arguments.iter().enumerate() {
                    // Skip fields that were record update arguments, as they've already been type-checked above
                    if update_argument_indices.contains(&index) {
                        continue;
                    }

                    if let Some(expected_field_type) = field_types.get(index)
                        && let Err(unify_error) =
                            unify(expected_field_type.clone(), field_arg.value.type_())
                    {
                        let field = field_map
                            .fields
                            .iter()
                            .find(|(_, i)| **i == index as u32)
                            .map(|(name, _)| RecordField::Labelled(name.clone()))
                            .unwrap_or_else(|| RecordField::Unlabelled(index as u32));

                        self.typer.problems.error(
                            if let UnifyError::CouldNotUnify {
                                expected, given, ..
                            } = unify_error
                            {
                                Error::UnsafeRecordUpdate {
                                    location: record.base.location(),
                                    reason: UnsafeRecordUpdateReason::IncompatibleFieldTypes {
                                        constructed_variant: expected_type,
                                        record_variant: typed_record_type,
                                        expected_field_type: expected,
                                        record_field_type: given,
                                        field,
                                    },
                                }
                            } else {
                                convert_unify_error(unify_error, location)
                            },
                        );
                        return self.new_invalid_constant(location);
                    }
                }

                Constant::Record {
                    module,
                    location,
                    arguments_start_position: constructor_location.end,
                    name,
                    arguments: Some(final_arguments),
                    type_: expected_type,
                    field_map: Inferred::Known(field_map),
                    record_constructor: Some(Box::new(constructor)),
                }
            }

            Constant::Record {
                module,
                location,
                arguments_start_position,
                name,
                arguments,
                ..
            } => self.infer_record(module, location, arguments_start_position, name, arguments),

            Constant::Var {
                location,
                module,
                name,
                ..
            } => {
                // Infer the type of this constant
                let constructor = match self.typer.infer_value_constructor(
                    &module,
                    &name,
                    &location,
                    ValueUsage::Other,
                ) {
                    Ok(constructor) => constructor,
                    Err(error) => {
                        self.typer.problems.error(error);
                        return Constant::Invalid {
                            location,
                            type_: self.typer.new_unbound_var(),
                            extra_information: Some(match module {
                                Some((module_name, _)) => InvalidExpression::ModuleSelect {
                                    module_name,
                                    label: name,
                                },
                                None => InvalidExpression::UnknownVariable { name },
                            }),
                        };
                    }
                };

                match constructor.variant {
                    ValueConstructorVariant::ModuleConstant { .. }
                    | ValueConstructorVariant::ModuleFn { .. }
                    | ValueConstructorVariant::LocalVariable { .. } => Constant::Var {
                        location,
                        module,
                        name,
                        type_: Arc::clone(&constructor.type_),
                        constructor: Some(Box::from(constructor)),
                    },
                    // It cannot be a Record because then this constant would have been
                    // parsed as a Constant::Record. Therefore this code is unreachable.
                    ValueConstructorVariant::Record { .. } => unreachable!(),
                }
            }

            Constant::BinaryOperator {
                location,
                left,
                right,
                operator,
                operator_start,
                type_: (),
            } => match operator {
                BinOp::And
                | BinOp::Or
                | BinOp::Eq
                | BinOp::NotEq
                | BinOp::LtInt
                | BinOp::LtEqInt
                | BinOp::LtFloat
                | BinOp::LtEqFloat
                | BinOp::GtEqInt
                | BinOp::GtInt
                | BinOp::GtEqFloat
                | BinOp::GtFloat
                | BinOp::AddInt
                | BinOp::AddFloat
                | BinOp::SubInt
                | BinOp::SubFloat
                | BinOp::MultInt
                | BinOp::MultFloat
                | BinOp::DivInt
                | BinOp::DivFloat
                | BinOp::RemainderInt => {
                    // These operators are not currently allowed in constants.
                    // We keep inferring the left and right values to catch
                    // other invalid usages of this kind but we don't try and
                    // type check those against some expected type!
                    let left = self.do_infer(*left);
                    let right = self.do_infer(*right);
                    self.typer
                        .problems
                        .error(Error::InvalidConstantBinaryOperator {
                            operator_start,
                            operator,
                        });

                    Constant::BinaryOperator {
                        location,
                        operator_start,
                        operator,
                        left: Box::new(left),
                        right: Box::new(right),
                        // We use an unbound type so we don't get type errors
                        // for this invalid binary operator. We only want an
                        // error message saying "this is not supported", other
                        // type errors like "Expected String, got Int" wouldn't
                        // be all that useful.
                        type_: self.typer.new_unbound_var(),
                    }
                }

                BinOp::Concatenate => self.infer_string_concatenation(
                    location,
                    operator_start,
                    *left,
                    *right,
                    operator,
                ),
            },

            Constant::Todo {
                location, message, ..
            } => {
                let type_ = self.typer.new_unbound_var();
                let message = message.map(|message| {
                    let message = self.do_infer(*message);
                    if let Err(error) = unify(string(), message.type_()) {
                        self.typer
                            .problems
                            .error(convert_unify_error(error, message.location()));
                    }
                    Box::new(message)
                });

                // Constant todos always result in a compile time error, this
                // way the developer has to remember to change them before
                // running their code!
                self.typer.problems.error(Error::TodoConstant { location });

                Constant::Todo {
                    location,
                    type_,
                    message,
                }
            }

            Constant::Invalid { .. } => panic!("invalid constants can not be in an untyped ast"),
        }
    }

    fn infer_string_concatenation(
        &mut self,
        location: SrcSpan,
        operator_start: u32,
        left: UntypedConstant,
        right: UntypedConstant,
        operator: BinOp,
    ) -> TypedConstant {
        self.typer
            .track_feature_usage(FeatureKind::ConstantStringConcatenation, location);
        let left = self.do_infer(left);

        if let Err(error) = unify(string(), left.type_()) {
            self.typer.problems.error(
                error
                    .operator_situation(BinOp::Concatenate)
                    .into_error(left.location()),
            );
        }

        let right = self.do_infer(right);
        if let Err(error) = unify(string(), right.type_()) {
            self.typer.problems.error(
                error
                    .operator_situation(BinOp::Concatenate)
                    .into_error(right.location()),
            );
        }

        Constant::BinaryOperator {
            location,
            operator_start,
            operator,
            type_: string(),
            left: Box::new(left),
            right: Box::new(right),
        }
    }

    fn infer_record(
        &mut self,
        module: Option<(EcoString, SrcSpan)>,
        location: SrcSpan,
        arguments_start_position: u32,
        name: EcoString,
        arguments: Option<Vec<CallArg<UntypedConstant>>>,
    ) -> TypedConstant {
        // We start by inferring the value constructor. If we can't do that we
        // immediately fail and return an invalid node.
        // TODO: in future we might want to make this more fault tolerant and
        //       still check the arguments even if the constructor itself cannot
        //       be inferred, like we do for expressions!

        // The usage counts as a call only if there's actually an arguments list!
        // `Wibble()` and `Wibble(1, 2)` are calls, but `Wibble` is not!
        let usage = arguments
            .as_ref()
            .map_or(ValueUsage::Other, |arguments| ValueUsage::Call {
                arity: arguments.len(),
            });

        let constructor_location = SrcSpan {
            start: location.start,
            end: arguments_start_position,
        };
        let constructor =
            match self
                .typer
                .infer_value_constructor(&module, &name, &constructor_location, usage)
            {
                Ok(constructor) => constructor,
                Err(error) => {
                    self.typer.problems.error(error);
                    return self.new_invalid_constant(location);
                }
            };

        let field_map = match &constructor.variant {
            ValueConstructorVariant::Record { field_map, .. } => field_map.clone(),

            ValueConstructorVariant::ModuleFn { .. }
            | ValueConstructorVariant::LocalVariable { .. } => {
                self.typer
                    .problems
                    .error(Error::NonLocalClauseGuardVariable { location, name });
                return self.new_invalid_constant(location);
            }

            ValueConstructorVariant::ModuleConstant { .. } => {
                unreachable!("module constant called as a record is a syntax error")
            }
        };

        // If the arguments are none, then there's nothing else left to type, we
        // can just return.
        // Otherwise we'll have to go on and also check the arguments.
        let Some(mut arguments) = arguments else {
            return Constant::Record {
                module,
                location,
                arguments_start_position,
                name,
                arguments: None,
                type_: constructor.type_.clone(),
                field_map: match field_map {
                    Some(field_map) => Inferred::Known(field_map),
                    None => Inferred::Unknown,
                },
                record_constructor: Some(Box::new(constructor)),
            };
        };

        // This is basically the same code as do_infer_call_with_known_fun()
        // except the args are typed with infer_clause_guard() here.
        // This duplication is a bit awkward but it works!
        // Potentially this could be improved later
        let result = match &field_map {
            // The fun has a field map so labelled arguments may be present
            // and need to be reordered.
            Some(field_map) => {
                field_map.reorder(&mut arguments, location, IncorrectArityContext::Function)
            }
            // The fun or constructor has no field map and so we error
            // if arguments have been labelled.
            None if constructor.variant.is_record_constructor_function() => {
                assert_no_labelled_arguments(
                    &arguments,
                    UnexpectedLabelledArgKind::RecordConstructorArgument,
                )
            }

            None => assert_no_labelled_arguments(
                &arguments,
                UnexpectedLabelledArgKind::FunctionParameter,
            ),
        };

        // If there's an error with the constructor being passed the wrong
        // number of arguments we keep track of it, but don't immediately return
        // an invalid node!
        // We still want to analyse the passed arguments.
        let mut labelled_arity_error = false;
        if let Err(error) = result {
            if let Error::IncorrectArity { .. } = error {
                labelled_arity_error = true;
            }
            self.typer.problems.error(error);
        }

        let called_location = module.as_ref().map_or(location, |(_, module_location)| {
            module_location.merge(&location)
        });

        let FunctionTypeMatch {
            mut expected_arguments,
            expected_return,
            missing_arguments: _,
            ignored_labelled_arguments,
        } = self.typer.fault_tolerant_match_function_type(
            labelled_arity_error,
            CallKind::Function,
            constructor.type_.clone(),
            called_location,
            location,
            &arguments,
        );

        let mut typed_arguments = expected_arguments
            .iter_mut()
            .zip(arguments)
            .map(|(type_, argument): (&mut Arc<Type>, _)| {
                if argument.uses_label_shorthand() {
                    self.typer
                        .track_feature_usage(FeatureKind::LabelShorthandSyntax, argument.location);
                }
                let CallArg {
                    label,
                    value,
                    location,
                    implicit,
                } = argument;
                let value = self.do_infer(value);
                if let Err(error) = unify(type_.clone(), value.type_()) {
                    self.typer
                        .problems
                        .error(convert_unify_error(error, value.location()));
                }
                CallArg {
                    label,
                    value,
                    implicit,
                    location,
                }
            })
            .collect_vec();

        // Register a reference to each labelled field so the language server can
        // offer go-to-definition, find-references and rename on record fields. We
        // do this before adding back the ignored arguments below, as those are
        // synthetic placeholders without a real value: their labels are
        // registered using the locations captured before the values were
        // discarded.
        if let Some(type_name) = expected_return.named_type_name() {
            for argument in &typed_arguments {
                if let Some(label) = &argument.label
                    && let Some(label_location) = argument.label_location()
                {
                    self.typer.environment.references.register_label_reference(
                        type_name.clone(),
                        label.clone(),
                        label_location,
                        argument.label_syntax(),
                    );
                }
            }

            for argument in &ignored_labelled_arguments {
                if let Some(label) = &argument.label
                    && let Some(label_location) = argument.label_location
                    && argument.implicit.is_none()
                {
                    self.typer.environment.references.register_label_reference(
                        type_name.clone(),
                        label.clone(),
                        label_location,
                        argument.syntax,
                    );
                }
            }
        }

        // Now if we had supplied less arguments than required and some of those
        // were labelled, in the previous step we would have got rid of those
        // _before_ typing.
        // That is because we can only reliably type positional arguments in
        // case of mismatched arity, as labelled arguments cannot be reordered.
        //
        // So now what we want to do is add back those labelled arguments to
        // make sure the LS can still see that those were explicitly supplied.
        for IgnoredLabelledArgument {
            label,
            location,
            implicit,
            ..
        } in ignored_labelled_arguments
        {
            typed_arguments.push(CallArg {
                label,
                value: TypedConstant::Invalid {
                    location,
                    type_: self.typer.new_unbound_var(),
                    extra_information: None,
                },
                implicit,
                location,
            });
        }

        Constant::Record {
            module,
            location,
            arguments_start_position,
            name,
            arguments: Some(typed_arguments),
            type_: expected_return,
            field_map: match field_map {
                Some(field_map) => Inferred::Known(field_map),
                None => Inferred::Unknown,
            },
            record_constructor: Some(Box::new(constructor)),
        }
    }

    /// Returns an invalid constant with an unbound type and no extra information
    /// attached.
    fn new_invalid_constant(&mut self, location: SrcSpan) -> TypedConstant {
        Constant::Invalid {
            location,
            type_: self.typer.new_unbound_var(),
            extra_information: None,
        }
    }

    fn infer_tuple(
        &mut self,
        untyped_elements: Vec<UntypedConstant>,
        location: SrcSpan,
    ) -> TypedConstant {
        let mut elements = Vec::with_capacity(untyped_elements.len());

        for element in untyped_elements {
            let element = self.do_infer(element);
            elements.push(element);
        }

        let type_ = tuple(elements.iter().map(HasType::type_).collect_vec());

        Constant::Tuple {
            elements,
            location,
            type_,
        }
    }

    fn infer_list(
        &mut self,
        untyped_elements: Vec<UntypedConstant>,
        location: SrcSpan,
        tail: Option<Box<UntypedConstant>>,
    ) -> TypedConstant {
        let element_type = self.typer.new_unbound_var();
        let mut elements = Vec::with_capacity(untyped_elements.len());

        for element in untyped_elements {
            let element = self.do_infer(element);
            if let Err(error) = unify(element_type.clone(), element.type_()) {
                self.typer
                    .problems
                    .error(convert_unify_error(error, element.location()));
            }

            elements.push(element);
        }

        let type_ = list(element_type);

        let tail = if let Some(tail) = tail {
            self.typer
                .track_feature_usage(FeatureKind::ConstantListWithTail, location);
            let tail = self.do_infer(*tail);
            if let Err(error) = unify(type_.clone(), tail.type_()) {
                self.typer
                    .problems
                    .error(convert_unify_error(error, tail.location()));
            }
            Some(Box::new(tail))
        } else {
            None
        };

        // We're checking if we have a prepend but no elements, so we can
        // provide an error telling the developer to just use the list directly.
        // Like this: `[..tail]`.
        if elements.is_empty() && tail.is_some() {
            self.typer
                .problems.error(Error::ListPrependWithoutElements { location })
        }

        Constant::List {
            elements,
            location,
            type_,
            tail,
        }
    }

    fn infer_bit_array(
        &mut self,
        segments: Vec<UntypedConstantBitArraySegment>,
        location: SrcSpan,
    ) -> Result<TypedConstant, Error> {
        let segments = segments
            .into_iter()
            .map(|mut segment| {
                // If the segment doesn't have an explicit type option we add a default
                // one ourselves if the pattern is unambiguous: literal strings are
                // implicitly considered utf-8 encoded strings, while floats are
                // implicitly given the float type option.
                if !segment.has_type_option() {
                    match segment.value.as_ref() {
                        Constant::String { location, .. } => {
                            self.typer.track_feature_usage(
                                FeatureKind::UnannotatedUtf8StringSegment,
                                *location,
                            );
                            segment.options.push(BitArrayOption::Utf8 {
                                location: SrcSpan::default(),
                            });
                        }

                        Constant::Float { location, .. } => {
                            self.typer.track_feature_usage(
                                FeatureKind::UnannotatedFloatSegment,
                                *location,
                            );
                            segment.options.push(BitArrayOption::Float {
                                location: SrcSpan::default(),
                            });
                        }

                        Constant::Int { .. }
                        | Constant::Todo { .. }
                        | Constant::Tuple { .. }
                        | Constant::List { .. }
                        | Constant::Record { .. }
                        | Constant::RecordUpdate { .. }
                        | Constant::BitArray { .. }
                        | Constant::Var { .. }
                        | Constant::BinaryOperator { .. }
                        | Constant::Invalid { .. } => (),
                    }
                }

                let segment = self.typer.infer_bit_segment(
                    *segment.value,
                    segment.options,
                    segment.location,
                    |env, expr| Ok(env.infer_const(&None, expr)),
                );

                if let Ok(segment) = &segment {
                    // If we could successfully infer the segment we need to
                    // check if it's `size` option uses any feature that has to
                    // be tracked!
                    self.check_constant_segment_size_expression(&segment.options);
                }

                segment
            })
            .try_collect()?;

        Ok(Constant::BitArray { location, segments })
    }

    /// Checks if one of the options is a size option using an expression.
    /// This needs to be tracked as it was introduced in Gleam 1.12.0.
    ///
    /// This is basically the same as the function above working on expressions!
    fn check_constant_segment_size_expression(&self, options: &[BitArrayOption<TypedConstant>]) {
        let Some(size_value) = options.iter().find_map(|option| match option {
            BitArrayOption::Size { value, .. } => Some(value),

            BitArrayOption::Bytes { .. }
            | BitArrayOption::Int { .. }
            | BitArrayOption::Float { .. }
            | BitArrayOption::Bits { .. }
            | BitArrayOption::Utf8 { .. }
            | BitArrayOption::Utf16 { .. }
            | BitArrayOption::Utf32 { .. }
            | BitArrayOption::Utf8Codepoint { .. }
            | BitArrayOption::Utf16Codepoint { .. }
            | BitArrayOption::Utf32Codepoint { .. }
            | BitArrayOption::Signed { .. }
            | BitArrayOption::Unsigned { .. }
            | BitArrayOption::Big { .. }
            | BitArrayOption::Little { .. }
            | BitArrayOption::Native { .. }
            | BitArrayOption::Unit { .. } => None,
        }) else {
            return;
        };

        // Expressions are not allowed in constants so for now nothing needs
        // tracking. Though this is handy to have already in place if we were to
        // lift this restriction for constant bit arrays as well!
        match size_value.as_ref() {
            // Ints and vars were always allowed from the start
            TypedConstant::Int { .. } | TypedConstant::Var { .. } => (),

            // None of these are currently supported... for now!
            Constant::Float { .. }
            | Constant::String { .. }
            | Constant::Tuple { .. }
            | Constant::List { .. }
            | Constant::Record { .. }
            | Constant::RecordUpdate { .. }
            | Constant::BitArray { .. }
            | Constant::BinaryOperator { .. }
            | Constant::Todo { .. }
            | Constant::Invalid { .. } => (),
        }
    }
}
