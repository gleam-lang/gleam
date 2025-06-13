use super::*;
use crate::{
    analyse::name::check_name_case,
    ast::{Layer, TypeAst, TypeAstConstructor, TypeAstFn, TypeAstHole, TypeAstTuple, TypeAstVar},
    reference::ReferenceKind,
};
use std::sync::Arc;

use im::hashmap;

/// The Hydrator takes an AST representing a type (i.e. a type annotation
/// for a function argument) and returns a Type for that annotation.
///
/// If a valid Type cannot be constructed it returns an error.
///
/// It keeps track of any type variables created. This is useful for:
///
/// - Determining if a generic type variable should be made into an
///   unbound type variable during type instantiation.
/// - Ensuring that the same type is constructed if the programmer
///   uses the same name for a type variable multiple times.
///
#[derive(Debug)]
pub struct Hydrator {
    created_type_variables: im::HashMap<EcoString, CreatedTypeVariable>,
    /// A rigid type is a generic type that was specified as being generic in
    /// an annotation. As such it should never be instantiated into an unbound
    /// variable. This type_id => name map is used for reporting the original
    /// annotated name on error.
    rigid_type_names: im::HashMap<u64, EcoString>,
    permit_new_type_variables: bool,
    permit_holes: bool,
}

#[derive(Debug)]
pub struct ScopeResetData {
    created_type_variables: im::HashMap<EcoString, CreatedTypeVariable>,
    rigid_type_names: im::HashMap<u64, EcoString>,
}

impl Default for Hydrator {
    fn default() -> Self {
        Self::new()
    }
}

impl Hydrator {
    pub fn new() -> Self {
        Self {
            created_type_variables: hashmap![],
            rigid_type_names: hashmap![],
            permit_new_type_variables: true,
            permit_holes: false,
        }
    }

    pub fn named_type_variables(&self) -> im::HashMap<EcoString, CreatedTypeVariable> {
        self.created_type_variables.clone()
    }

    pub fn open_new_scope(&mut self) -> ScopeResetData {
        let created_type_variables = self.created_type_variables.clone();
        let rigid_type_names = self.rigid_type_names.clone();
        ScopeResetData {
            created_type_variables,
            rigid_type_names,
        }
    }

    pub fn close_scope(&mut self, data: ScopeResetData) {
        self.created_type_variables = data.created_type_variables;
        self.rigid_type_names = data.rigid_type_names;
    }

    pub fn disallow_new_type_variables(&mut self) {
        self.permit_new_type_variables = false
    }

    pub fn permit_holes(&mut self, flag: bool) {
        self.permit_holes = flag
    }

    /// A rigid type is a generic type that was specified as being generic in
    /// an annotation. As such it should never be instantiated into an unbound
    /// variable.
    pub fn is_rigid(&self, id: &u64) -> bool {
        self.rigid_type_names.contains_key(id)
    }

    pub fn rigid_names(&self) -> im::HashMap<u64, EcoString> {
        self.rigid_type_names.clone()
    }

    pub fn type_from_option_ast(
        &mut self,
        ast: &Option<TypeAst>,
        environment: &mut Environment<'_>,
        problems: &mut Problems,
    ) -> Result<Arc<Type>, Error> {
        match ast {
            Some(ast) => self.type_from_ast(ast, environment, problems),
            None => Ok(environment.new_unbound_var()),
        }
    }

    /// Construct a Type from an AST Type annotation.
    ///
    pub fn type_from_ast(
        &mut self,
        ast: &TypeAst,
        environment: &mut Environment<'_>,
        problems: &mut Problems,
    ) -> Result<Arc<Type>, Error> {
        match ast {
            TypeAst::Constructor(TypeAstConstructor {
                location,
                name_location,
                module,
                name,
                arguments: args,
            }) => {
                // Hydrate the type argument AST into types
                let mut argument_types = Vec::with_capacity(args.len());
                for t in args {
                    let type_ = self.type_from_ast(t, environment, problems)?;
                    argument_types.push((t.location(), type_));
                }

                // Look up the constructor
                let TypeConstructor {
                    parameters,
                    type_: return_type,
                    deprecation,
                    ..
                } = environment
                    .get_type_constructor(module, name, Some(args.len()))
                    .map_err(|e| {
                        convert_get_type_constructor_error(
                            e,
                            location,
                            module.as_ref().map(|(_, location)| *location),
                        )
                    })?
                    .clone();

                if let Some((type_module, type_name)) = return_type.named_type_name() {
                    let reference_kind = if module.is_some() {
                        ReferenceKind::Qualified
                    } else if name != &type_name {
                        ReferenceKind::Alias
                    } else {
                        ReferenceKind::Unqualified
                    };
                    environment.references.register_type_reference(
                        type_module,
                        type_name,
                        name,
                        *name_location,
                        reference_kind,
                    );
                } else {
                    environment
                        .references
                        .register_type_reference_in_call_graph(name.clone());
                }

                match deprecation {
                    Deprecation::NotDeprecated => {}
                    Deprecation::Deprecated { message } => {
                        problems.warning(Warning::DeprecatedItem {
                            location: *location,
                            message: message.clone(),
                            layer: Layer::Type,
                        })
                    }
                }

                // Register the type constructor as being used if it is unqualified.
                // We do not track use of qualified type constructors as they may be
                // used in another module.
                if module.is_none() {
                    environment.increment_usage(name);
                }

                // Ensure that the correct number of arguments have been given to the constructor
                if args.len() != parameters.len() {
                    return Err(Error::IncorrectTypeArity {
                        location: *location,
                        name: name.clone(),
                        expected: parameters.len(),
                        given: args.len(),
                    });
                }

                // Instantiate the constructor type for this specific usage
                let mut type_vars = hashmap![];
                #[allow(clippy::needless_collect)] // Not needless, used for side effects
                let parameter_types: Vec<_> = parameters
                    .into_iter()
                    .map(|type_| environment.instantiate(type_, &mut type_vars, self))
                    .collect();

                let return_type = environment.instantiate(return_type, &mut type_vars, self);

                // Unify argument types with instantiated parameter types so that the correct types
                // are inserted into the return type
                for (parameter, (location, argument)) in
                    parameter_types.into_iter().zip(argument_types)
                {
                    unify(parameter, argument).map_err(|e| convert_unify_error(e, location))?;
                }

                Ok(return_type)
            }

            TypeAst::Tuple(TypeAstTuple { elements, .. }) => Ok(tuple(
                elements
                    .iter()
                    .map(|type_| self.type_from_ast(type_, environment, problems))
                    .try_collect()?,
            )),

            TypeAst::Fn(TypeAstFn {
                arguments: args,
                return_,
                ..
            }) => {
                let args = args
                    .iter()
                    .map(|type_| self.type_from_ast(type_, environment, problems))
                    .try_collect()?;
                let return_ = self.type_from_ast(return_, environment, problems)?;
                Ok(fn_(args, return_))
            }

            TypeAst::Var(TypeAstVar { name, location }) => {
                match self.created_type_variables.get_mut(name) {
                    Some(var) => {
                        var.usage_count += 1;
                        Ok(var.type_.clone())
                    }

                    None if self.permit_new_type_variables => {
                        if let Err(error) = check_name_case(*location, name, Named::TypeVariable) {
                            problems.error(error);
                        }
                        let t = environment.new_generic_var();
                        let _ = self
                            .rigid_type_names
                            .insert(environment.previous_uid(), name.clone());
                        environment
                            .names
                            .type_variable_in_scope(environment.previous_uid(), name.clone());
                        let _ = self.created_type_variables.insert(
                            name.clone(),
                            CreatedTypeVariable {
                                type_: t.clone(),
                                usage_count: 1,
                            },
                        );
                        Ok(t)
                    }

                    None => {
                        let hint = match environment.scope.contains_key(name) {
                            true => UnknownTypeHint::ValueInScopeWithSameName,
                            false => UnknownTypeHint::AlternativeTypes(
                                environment.module_types.keys().cloned().collect(),
                            ),
                        };

                        Err(Error::UnknownType {
                            name: name.clone(),
                            location: *location,
                            hint,
                            suggestions: environment.suggest_modules_for_type_or_value(
                                name,
                                Layer::Type,
                                None,
                            ),
                        })
                    }
                }
            }

            TypeAst::Hole(TypeAstHole { .. }) if self.permit_holes => {
                Ok(environment.new_unbound_var())
            }

            TypeAst::Hole(TypeAstHole { location, .. }) => Err(Error::UnexpectedTypeHole {
                location: *location,
            }),
        }
    }

    pub fn clear_ridgid_type_names(&mut self) {
        self.rigid_type_names.clear();
    }

    /// All the type variables that were created but never used.
    pub fn unused_type_variables(&self) -> impl Iterator<Item = &EcoString> {
        self.created_type_variables
            .iter()
            .filter(|(_, var)| var.usage_count == 0)
            .map(|(name, _)| name)
    }

    /// Create a new type variable with the given name.
    pub fn add_type_variable(
        &mut self,
        name: &EcoString,
        environment: &mut Environment<'_>,
    ) -> Result<Arc<Type>, Arc<Type>> {
        let t = environment.new_generic_var();
        let v = CreatedTypeVariable {
            type_: t.clone(),
            usage_count: 0,
        };

        environment
            .names
            .type_variable_in_scope(environment.previous_uid(), name.clone());
        match self.created_type_variables.insert(name.clone(), v) {
            Some(_) => Err(t),
            None => Ok(t),
        }
    }
}

#[derive(Debug, Clone)]
pub struct CreatedTypeVariable {
    pub type_: Arc<Type>,
    pub usage_count: usize,
}
