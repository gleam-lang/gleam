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
    new_type_behaviour: NewTypeAction,
}

impl Hydrator {
    pub fn new() -> Self {
        Self {
            created_type_variables: im::hashmap![],
            created_type_variable_ids: im::hashset![],
            new_type_behaviour: NewTypeAction::MakeGeneric,
        }
    }

    pub fn register_type_as_created(&mut self, name: String, typ: Arc<Type>) {
        // Note we don't record the id. This is OK currently as we only
        // use this for custom type constructors, but in future we may
        // want to do this.
        self.created_type_variables.insert(name, typ);
    }

    pub fn disallow_new_type_variables(&mut self) {
        self.new_type_behaviour = NewTypeAction::Disallow
    }

    pub fn is_created_generic_type(&self, id: &usize) -> bool {
        self.created_type_variable_ids.contains(id)
    }

    /// Construct a Type from an AST Type annotation.
    ///
    pub fn type_from_ast<'a>(
        &mut self,
        ast: &TypeAst,
        environment: &mut Environment<'a>,
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

                    None => match self.new_type_behaviour {
                        NewTypeAction::MakeGeneric => {
                            let var = environment.new_generic_var();
                            self.created_type_variable_ids
                                .insert(environment.previous_uid());
                            self.created_type_variables
                                .insert(name.clone(), var.clone());
                            Ok(var)
                        }
                        NewTypeAction::Disallow => Err(Error::UnknownType {
                            name: name.to_string(),
                            location: location.clone(),
                            types: environment
                                .module_types
                                .keys()
                                .map(|t| t.to_string())
                                .collect(),
                        }),
                    },
                }
            }
        }
    }
}

#[derive(Debug, Clone, Copy)]
enum NewTypeAction {
    Disallow,
    MakeGeneric,
}
