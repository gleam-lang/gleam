use super::*;
use crate::ast::TypeAst;
use std::sync::Arc;

/// The Hydrator takes an AST representing a type (i.e. a type annotation
/// for a function argument) and returns a Type for that annotation.
///
/// If a valid Type cannot be constructed it returns an error.
///
/// It keeps track of any type variables created. This is useful for:
///
/// - Determining if a generic type variable should be made into an
///   unbound type varable during type instantiation.
/// - Ensuring that the same type is constructed if the programmer
///   uses the same name for a type variable multiple times.
///
#[derive(Debug)]
pub struct Hydrator {
    created_type_variables: im::HashMap<String, Arc<Type>>,
    created_type_variable_ids: im::HashSet<usize>,
    permit_new_type_variables: bool,
    permit_holes: bool,
}

pub struct ScopeResetData {
    created_type_variables: im::HashMap<String, Arc<Type>>,
    created_type_variable_ids: im::HashSet<usize>,
}

impl Hydrator {
    pub fn new() -> Self {
        Self {
            created_type_variables: im::hashmap![],
            created_type_variable_ids: im::hashset![],
            permit_new_type_variables: true,
            permit_holes: false,
        }
    }

    pub fn open_new_scope(&mut self) -> ScopeResetData {
        let created_type_variables = self.created_type_variables.clone();
        let created_type_variable_ids = self.created_type_variable_ids.clone();
        ScopeResetData {
            created_type_variables,
            created_type_variable_ids,
        }
    }

    pub fn close_scope(&mut self, data: ScopeResetData) {
        self.created_type_variables = data.created_type_variables;
        self.created_type_variable_ids = data.created_type_variable_ids;
    }

    pub fn register_type_as_created(&mut self, name: String, typ: Arc<Type>) {
        // Note we don't record the id. This is OK currently as we only
        // use this for custom type constructors, but in future we may
        // want to do this.
        self.created_type_variables.insert(name, typ);
    }

    pub fn disallow_new_type_variables(&mut self) {
        self.permit_new_type_variables = false
    }

    pub fn permit_holes(&mut self, flag: bool) {
        self.permit_holes = flag
    }

    pub fn is_created_generic_type(&self, id: &usize) -> bool {
        self.created_type_variable_ids.contains(id)
    }

    /// Construct a Type from an AST Type annotation.
    ///
    pub fn type_from_ast<'a, 'b>(
        &mut self,
        ast: &TypeAst,
        environment: &mut Environment<'a, 'b>,
    ) -> Result<Arc<Type>, Error> {
        match ast {
            TypeAst::Constructor {
                location,
                module,
                name,
                args,
            } => {
                // Hydrate the type argument AST into types
                let mut argument_types = Vec::with_capacity(args.len());
                for t in args {
                    let typ = self.type_from_ast(t, environment)?;
                    argument_types.push((t.location(), typ));
                }

                // Look up the constructor
                let TypeConstructor {
                    parameters,
                    typ: return_type,
                    ..
                } = environment
                    .get_type_constructor(module, name)
                    .map_err(|e| convert_get_type_constructor_error(e, &location))?
                    .clone();

                // Register the type constructor as being used if it is unqualifed.
                // We do not track use of qualified type constructors as they may be
                // used in another module.
                if module.is_none() {
                    environment.unused_private_types.remove(name.as_str());
                }

                // Ensure that the correct number of arguments have been given to the constructor
                if args.len() != parameters.len() {
                    return Err(Error::IncorrectTypeArity {
                        location: location.clone(),
                        name: name.to_string(),
                        expected: parameters.len(),
                        given: args.len(),
                    });
                }

                // Instantiate the constructor type for this specific usage
                let mut type_vars = hashmap![];
                let mut parameter_types = Vec::with_capacity(parameters.len());
                for typ in parameters {
                    let t = environment.instantiate(typ, 0, &mut type_vars, &self);

                    parameter_types.push(t);
                }
                let return_type = environment.instantiate(return_type, 0, &mut type_vars, &self);

                // Unify argument types with instantiated parameter types so that the correct types
                // are inserted into the return type
                for (parameter, (location, argument)) in
                    parameter_types.iter().zip(argument_types.iter())
                {
                    environment
                        .unify(parameter.clone(), argument.clone())
                        .map_err(|e| convert_unify_error(e, &location))?;
                }

                Ok(return_type)
            }

            TypeAst::Tuple { elems, .. } => Ok(tuple(
                elems
                    .iter()
                    .map(|t| self.type_from_ast(t, environment))
                    .collect::<Result<_, _>>()?,
            )),

            TypeAst::Fn { args, retrn, .. } => {
                let args = args
                    .iter()
                    .map(|t| self.type_from_ast(t, environment))
                    .collect::<Result<_, _>>()?;
                let retrn = self.type_from_ast(retrn, environment)?;
                Ok(fn_(args, retrn))
            }

            TypeAst::Var { name, location, .. } => {
                match self.created_type_variables.get(name.as_str()) {
                    Some(var) => Ok(var.clone()),

                    None if self.permit_new_type_variables => {
                        let var = environment.new_generic_var();
                        self.created_type_variable_ids
                            .insert(environment.previous_uid());
                        self.created_type_variables
                            .insert(name.clone(), var.clone());
                        Ok(var)
                    }

                    None => Err(Error::UnknownType {
                        name: name.to_string(),
                        location: location.clone(),
                        types: environment
                            .module_types
                            .keys()
                            .map(|t| t.to_string())
                            .collect(),
                    }),
                }
            }

            TypeAst::Hole { .. } if self.permit_holes => {
                Ok(environment.new_unbound_var(environment.level))
            }

            TypeAst::Hole { location, .. } => Err(Error::UnexpectedTypeHole {
                location: location.clone(),
            }),
        }
    }
}
