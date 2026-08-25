// SPDX-License-Identifier: Apache-2.0
// SPDX-FileCopyrightText: 2026 The Gleam contributors

use crate::{
    ast::{BinOp, ClauseGuard, TypedClauseGuard, TypedExpr, UntypedClauseGuard},
    reference::ReferenceKind,
    type_::{
        Error, ExprTyper, FieldAccessUsage, ModuleValueConstructor, RecordAccessor, Type,
        ValueConstructorVariant, ValueUsage, bool,
        error::{FeatureKind, convert_unify_error},
        float, int, string, unify,
    },
};
use ecow::EcoString;
use src_span::SrcSpan;
use std::collections::HashSet;

pub struct InferredGuard {
    pub guard: TypedClauseGuard,
    /// These are all the remote constants that are referenced by the guard
    /// being inferred.
    /// Remote constants (constants from other modules) are turned into
    /// function calls on the Erlang target, and those are not allowed in
    /// clause guards.
    /// This will be used by code generation to actually bind those needed
    /// values to variables that come before the case expression so the
    /// guards can reference these variables.
    ///
    /// For example, in this case:
    ///
    /// ```gleam
    /// import other_module.{some_constant}
    ///
    /// case wibble {
    ///   2 if other_module.some_other_constant || some_constant -> todo
    ///   _ -> todo
    /// }
    /// ```
    ///
    /// We need to keep track of `(other_module, some_constant)` and
    /// `(other_module.some_other_constant)`.
    ///
    pub remote_constants: HashSet<(EcoString, EcoString)>,
}

pub struct GuardTyper<'expression_typer, 'env, 'module> {
    typer: &'expression_typer mut ExprTyper<'env, 'module>,
    remote_constants: HashSet<(EcoString, EcoString)>,
}

impl<'expression_typer, 'env, 'module> GuardTyper<'expression_typer, 'env, 'module> {
    pub fn new(typer: &'expression_typer mut ExprTyper<'env, 'module>) -> Self {
        Self {
            typer,
            remote_constants: HashSet::new(),
        }
    }

    pub fn infer(mut self, guard: UntypedClauseGuard) -> InferredGuard {
        InferredGuard {
            guard: self.do_infer(guard),
            remote_constants: self.remote_constants,
        }
    }

    fn do_infer(&mut self, guard: UntypedClauseGuard) -> TypedClauseGuard {
        match guard {
            ClauseGuard::Invalid { .. } => {
                unreachable!("untyped guard should never be invalid")
            }

            ClauseGuard::LocalVariable { location, name, .. } => {
                match self.infer_clause_guard_variable(name, location) {
                    Ok(variable) => variable,
                    Err(error) => {
                        self.typer.problems.error(error);
                        ClauseGuard::Invalid {
                            location,
                            type_: self.typer.new_unbound_var(),
                        }
                    }
                }
            }

            ClauseGuard::TupleIndex {
                location,
                tuple,
                index,
                ..
            } => {
                let tuple = self.do_infer(*tuple);
                let index_type = match tuple.type_().as_ref() {
                    Type::Tuple { elements } => match elements.get(index as usize) {
                        Some(type_) => type_.clone(),
                        // If the index is outside the tuple range, then we
                        // report the error and return an unbound type to keep
                        // going.
                        None => {
                            self.typer.problems.error(Error::OutOfBoundsTupleIndex {
                                location,
                                index,
                                size: elements.len(),
                            });
                            self.typer.new_unbound_var()
                        }
                    },

                    tuple_type if tuple_type.is_unbound() => {
                        self.typer.problems.error(Error::NotATupleUnbound {
                            location: tuple.location(),
                        });
                        self.typer.new_unbound_var()
                    }

                    Type::Named { .. } | Type::Fn { .. } | Type::Var { .. } => {
                        self.typer.problems.error(Error::NotATuple {
                            location: tuple.location(),
                            given: tuple.type_(),
                        });
                        self.typer.new_unbound_var()
                    }
                };

                ClauseGuard::TupleIndex {
                    location,
                    index,
                    type_: index_type,
                    tuple: Box::new(tuple),
                }
            }

            ClauseGuard::FieldAccess {
                label_location,
                label,
                container,
                index: _,
                type_: (),
            } => {
                let container_location = container.location();
                let result = if let ClauseGuard::LocalVariable { name, location, .. } = *container {
                    // If the container looks like a regular variable, then this
                    // could either be a module select, or a record access.
                    match self.infer_clause_guard_variable(name.clone(), location) {
                        // If the variable itself cannot be inferred as one, then
                        // it could really be a module select. We try that one
                        // as an alternative.
                        Err(error) => self.infer_guard_module_access(
                            name,
                            label,
                            location,
                            label_location,
                            error,
                        ),
                        // Otherwise that's a proper variable and not a module name,
                        // so the whole expression has to be inferred as a regular
                        // record access.
                        Ok(variable) => {
                            self.infer_guard_record_access(variable, label, label_location)
                        }
                    }
                } else {
                    // If it doesn't this has to be a regular record access and
                    // we try and infer it as such.
                    let inferred_container = self.do_infer(*container.clone());
                    self.infer_guard_record_access(inferred_container, label, label_location)
                };

                match result {
                    Ok(inferred) => inferred,
                    Err(error) => {
                        self.typer.problems.error(error);
                        ClauseGuard::Invalid {
                            location: container_location.merge(&label_location),
                            type_: self.typer.new_unbound_var(),
                        }
                    }
                }
            }

            ClauseGuard::ModuleSelect { .. } => {
                unreachable!("untyped guard should never be module select")
            }

            ClauseGuard::Not {
                location,
                expression,
            } => {
                let expression = self.do_infer(*expression);
                match unify(bool(), expression.type_()) {
                    Err(error) => {
                        self.typer
                            .problems
                            .error(convert_unify_error(error, expression.location()));
                    }
                    _ => (),
                }
                ClauseGuard::Not {
                    location,
                    expression: Box::new(expression),
                }
            }

            ClauseGuard::Constant(constant) => {
                let inferred = self.typer.infer_const(&None, constant);
                self.remote_constants.extend(inferred.remote_constants);
                ClauseGuard::Constant(inferred.constant)
            }

            ClauseGuard::Block { value, location } => ClauseGuard::Block {
                location,
                value: Box::new(self.do_infer(*value)),
            },

            ClauseGuard::BinaryOperator {
                location,
                operator,
                operator_start,
                left,
                right,
            } => {
                let left = self.do_infer(*left);
                let right = self.do_infer(*right);

                match operator {
                    BinOp::And | BinOp::Or => {
                        if let Err(error) = unify(bool(), left.type_()) {
                            self.typer
                                .problems
                                .error(convert_unify_error(error, left.location()));
                        }
                        if let Err(error) = unify(bool(), right.type_()) {
                            self.typer
                                .problems
                                .error(convert_unify_error(error, right.location()));
                        }
                    }

                    BinOp::Eq | BinOp::NotEq => {
                        if let Err(error) = unify(left.type_(), right.type_()) {
                            self.typer
                                .problems
                                .error(convert_unify_error(error, right.location()));
                        }
                    }

                    BinOp::GtInt
                    | BinOp::GtEqInt
                    | BinOp::LtInt
                    | BinOp::LtEqInt
                    | BinOp::AddInt
                    | BinOp::SubInt
                    | BinOp::DivInt
                    | BinOp::MultInt
                    | BinOp::RemainderInt => {
                        self.typer
                            .track_feature_usage(FeatureKind::ArithmeticInGuards, location);
                        // If both operands are floats, then we use a more specialised
                        // error.
                        if left.type_().is_float() && right.type_().is_float() {
                            self.typer.problems.error(Error::IntOperatorOnFloats {
                                operator,
                                location: SrcSpan::new(
                                    operator_start,
                                    operator_start + operator.size(),
                                ),
                            });
                        } else {
                            if let Err(error) = unify(int(), left.type_()) {
                                self.typer
                                    .problems
                                    .error(convert_unify_error(error, left.location()));
                            }
                            if let Err(error) = unify(int(), right.type_()) {
                                self.typer
                                    .problems
                                    .error(convert_unify_error(error, right.location()));
                            }
                        }
                    }

                    BinOp::GtFloat
                    | BinOp::GtEqFloat
                    | BinOp::LtFloat
                    | BinOp::LtEqFloat
                    | BinOp::AddFloat
                    | BinOp::SubFloat
                    | BinOp::DivFloat
                    | BinOp::MultFloat => {
                        self.typer
                            .track_feature_usage(FeatureKind::ArithmeticInGuards, location);

                        // If both operands are int then we use a more specialised
                        // error
                        if left.type_().is_int() && right.type_().is_int() {
                            self.typer.problems.error(Error::FloatOperatorOnInts {
                                operator,
                                location: SrcSpan::new(
                                    operator_start,
                                    operator_start + operator.size(),
                                ),
                            });
                        } else {
                            if let Err(error) = unify(float(), left.type_()) {
                                self.typer
                                    .problems
                                    .error(convert_unify_error(error, left.location()));
                            }
                            if let Err(error) = unify(float(), right.type_()) {
                                self.typer
                                    .problems
                                    .error(convert_unify_error(error, right.location()));
                            }
                        }
                    }

                    BinOp::Concatenate => {
                        self.typer
                            .track_feature_usage(FeatureKind::ConcatenateInGuards, location);

                        if let Err(error) = unify(string(), left.type_()) {
                            self.typer
                                .problems
                                .error(convert_unify_error(error, left.location()));
                        }
                        if let Err(error) = unify(string(), right.type_()) {
                            self.typer
                                .problems
                                .error(convert_unify_error(error, right.location()));
                        }
                    }
                }

                ClauseGuard::BinaryOperator {
                    location,
                    operator,
                    operator_start,
                    left: Box::new(left),
                    right: Box::new(right),
                }
            }
        }
    }

    fn infer_clause_guard_variable(
        &mut self,
        name: EcoString,
        location: SrcSpan,
    ) -> Result<TypedClauseGuard, Error> {
        let constructor =
            self.typer
                .infer_value_constructor(&None, &name, &location, ValueUsage::Other)?;

        // We cannot support all values in guard expressions as the BEAM does not
        let (definition_location, origin) = match &constructor.variant {
            ValueConstructorVariant::LocalVariable { location, origin } => {
                (*location, origin.clone())
            }

            ValueConstructorVariant::ModuleFn { .. } | ValueConstructorVariant::Record { .. } => {
                return Err(Error::NonLocalClauseGuardVariable { location, name });
            }

            ValueConstructorVariant::ModuleConstant {
                literal,
                module,
                name,
                remote_constants,
                ..
            } => {
                if *module == self.typer.environment.current_module {
                    // If we're referencing a constant from the current module,
                    // then we will referencing the same remote constants it is
                    // referencing.
                    self.remote_constants
                        .extend(remote_constants.iter().cloned());
                } else {
                    // If it is a constant from a different module then we need
                    // to add it to the list of remote constants!
                    let _ = self.remote_constants.insert((module.clone(), name.clone()));
                }
                return Ok(ClauseGuard::Constant(literal.clone()));
            }
        };

        Ok(ClauseGuard::LocalVariable {
            location,
            name,
            origin,
            type_: constructor.type_,
            definition_location,
        })
    }

    fn infer_guard_record_access(
        &mut self,
        container: TypedClauseGuard,
        label: EcoString,
        location: SrcSpan,
    ) -> Result<TypedClauseGuard, Error> {
        let container = Box::new(container);
        let container_type = container.type_();
        let RecordAccessor {
            index,
            label,
            type_,
            documentation: _,
        } = self.typer.infer_known_record_access(
            container_type,
            container.location(),
            FieldAccessUsage::Other,
            location,
            label,
        )?;
        Ok(ClauseGuard::FieldAccess {
            container,
            label,
            index: Some(index),
            label_location: location,
            type_,
        })
    }

    fn infer_guard_module_access(
        &mut self,
        name: EcoString,
        label: EcoString,
        module_location: SrcSpan,
        label_location: SrcSpan,
        record_access_error: Error,
    ) -> Result<TypedClauseGuard, Error> {
        let module_access = self
            .typer
            .infer_module_access(&name, label, &module_location, label_location)
            .and_then(|module_select| {
                if let TypedExpr::ModuleSelect {
                    location,
                    field_start: _,
                    type_,
                    label,
                    module_name,
                    module_alias,
                    constructor,
                } = module_select
                {
                    match constructor {
                        ModuleValueConstructor::Constant {
                            literal,
                            location: definition_location,
                            ..
                        } => {
                            self.typer.environment.references.register_value_reference(
                                module_name.clone(),
                                label.clone(),
                                &label,
                                label_location,
                                ReferenceKind::Qualified {
                                    module_alias: module_alias.clone(),
                                    module_location,
                                },
                            );

                            // We have established we are referencing a remote
                            // constant here!
                            let _ = self
                                .remote_constants
                                .insert((module_name.clone(), label.clone()));

                            Ok(ClauseGuard::ModuleSelect {
                                location,
                                field_start: label_location.start,
                                definition_location,
                                type_,
                                label,
                                module_name,
                                module_alias,
                                literal,
                            })
                        }

                        ModuleValueConstructor::Record { .. }
                        | ModuleValueConstructor::Fn { .. } => {
                            Err(Error::RecordAccessUnknownType { location })
                        }
                    }
                } else {
                    Err(Error::RecordAccessUnknownType {
                        location: module_location,
                    })
                }
            });

        // If the name is in the environment, use the original error from
        // inferring the record access, so that we can suggest possible
        // misspellings of field names
        if self.typer.environment.scope.contains_key(&name) {
            module_access.map_err(|_| record_access_error)
        } else {
            module_access
        }
    }
}
