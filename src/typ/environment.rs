use super::*;
use std::collections::HashMap;

#[derive(Debug)]
pub struct Environment<'a> {
    pub current_module: &'a [String],
    pub uid: usize,
    pub level: usize,
    pub importable_modules: &'a HashMap<String, (Origin, Module)>,
    pub imported_modules: HashMap<String, (Origin, Module)>,
    pub annotated_generic_types: im::HashSet<usize>,

    // Values defined in the current function (or the prelude)
    pub local_values: im::HashMap<String, ValueConstructor>,

    // Types defined in the current module (or the prelude)
    pub module_types: HashMap<String, TypeConstructor>,

    // Values defined in the current module
    pub module_values: HashMap<String, ValueConstructor>,

    // Accessors defined in the current module
    pub accessors: HashMap<String, AccessorsMap>,

    // Warnings
    pub warnings: &'a mut Vec<Warning>,
}

impl<'a> Environment<'a> {
    pub fn new(
        current_module: &'a [String],
        importable_modules: &'a HashMap<String, (Origin, Module)>,
        warnings: &'a mut Vec<Warning>,
    ) -> Self {
        let typer = Self {
            uid: 0,
            level: 1,
            annotated_generic_types: im::HashSet::new(),
            module_types: HashMap::new(),
            module_values: HashMap::new(),
            imported_modules: HashMap::new(),
            accessors: HashMap::new(),
            local_values: hashmap![],
            importable_modules,
            current_module,
            warnings,
        };
        register_prelude(typer)
    }
}

impl<'a> Environment<'a> {
    pub fn in_new_scope<T, O: FnOnce(&mut Self) -> Result<T, Error>>(
        &mut self,
        process_scope: O,
    ) -> Result<T, Error> {
        // Record initial scope state
        let initial_local_values = self.local_values.clone();
        // TODO: introduce scope for the hydrator
        // let initial_annotated_type_vars = self.annotated_type_vars.clone();
        let initial_annotated_generic_types = self.annotated_generic_types.clone();

        // Create state for new scope
        self.level += 1;

        // Process scope
        let result = process_scope(self);

        // Discard local state now scope is over
        self.level -= 1;
        self.local_values = initial_local_values;
        // TODO
        // self.annotated_type_vars = initial_annotated_type_vars;
        self.annotated_generic_types = initial_annotated_generic_types;

        // Return result of typing the scope
        result
    }

    pub fn next_uid(&mut self) -> usize {
        let i = self.uid;
        self.uid += 1;
        i
    }

    pub fn previous_uid(&self) -> usize {
        self.uid - 1
    }

    /// Create a new unbound type that is a specific type, we just don't
    /// know which one yet.
    ///
    pub fn new_unbound_var(&mut self, level: usize) -> Arc<Type> {
        Arc::new(Type::Var {
            typ: Arc::new(RefCell::new(TypeVar::Unbound {
                id: self.next_uid(),
                level,
            })),
        })
    }

    /// Create a new generic type that can stand in for any type.
    ///
    pub fn new_generic_var(&mut self) -> Arc<Type> {
        Arc::new(Type::Var {
            typ: Arc::new(RefCell::new(TypeVar::Generic {
                id: self.next_uid(),
            })),
        })
    }

    /// Insert a variable in the current scope.
    ///
    pub fn insert_variable(
        &mut self,
        name: String,
        variant: ValueConstructorVariant,
        typ: Arc<Type>,
    ) {
        self.local_values.insert(
            name,
            ValueConstructor {
                public: false,
                origin: Default::default(), // TODO: use the real one
                variant,
                typ,
            },
        );
    }

    /// Insert a value into the current module.
    /// Errors if the module already has a value with that name.
    ///
    pub fn insert_module_value(
        &mut self,
        name: &str,
        value: ValueConstructor,
    ) -> Result<(), Error> {
        let location = value.origin.clone();
        match self.module_values.insert(name.to_string(), value) {
            None => Ok(()),
            Some(previous) => Err(Error::DuplicateName {
                location,
                previous_location: previous.origin,
                name: name.to_string(),
            }),
        }
    }

    /// Lookup a variable in the current scope.
    ///
    pub fn get_variable(&self, name: &str) -> Option<&ValueConstructor> {
        self.local_values.get(name)
    }

    /// Lookup a module constant in the current scope.
    ///
    pub fn get_module_const(&self, name: &str) -> Option<&ValueConstructor> {
        self.module_values
            .get(name)
            .filter(|ValueConstructor { variant, .. }| {
                if let ValueConstructorVariant::ModuleConstant { .. } = variant {
                    true
                } else {
                    false
                }
            })
    }

    /// Map a type in the current scope.
    /// Errors if the module already has a type with that name, unless the type is from the
    /// prelude.
    ///
    pub fn insert_type_constructor(
        &mut self,
        type_name: String,
        info: TypeConstructor,
    ) -> Result<(), Error> {
        let name = type_name.clone();
        let location = info.origin.clone();
        match self.module_types.insert(type_name, info) {
            None => Ok(()),
            Some(prelude_type) if prelude_type.module.is_empty() => Ok(()),
            Some(previous) => Err(Error::DuplicateTypeName {
                name,
                location,
                previous_location: previous.origin,
            }),
        }
    }

    /// Lookup a type in the current scope.
    ///
    pub fn get_type_constructor(
        &self,
        module_alias: &Option<String>,
        name: &str,
    ) -> Result<&TypeConstructor, GetTypeConstructorError> {
        match module_alias {
            None => {
                self.module_types
                    .get(name)
                    .ok_or_else(|| GetTypeConstructorError::UnknownType {
                        name: name.to_string(),
                        type_constructors: self
                            .module_types
                            .keys()
                            .map(|t| t.to_string())
                            .collect(),
                    })
            }

            Some(m) => {
                let module = &self.imported_modules.get(m).ok_or_else(|| {
                    GetTypeConstructorError::UnknownModule {
                        name: name.to_string(),
                        imported_modules: self
                            .importable_modules
                            .keys()
                            .map(|t| t.to_string())
                            .collect(),
                    }
                })?;
                module
                    .1
                    .types
                    .get(name)
                    .ok_or_else(|| GetTypeConstructorError::UnknownModuleType {
                        name: name.to_string(),
                        module_name: module.1.name.clone(),
                        type_constructors: module.1.types.keys().map(|t| t.to_string()).collect(),
                    })
            }
        }
    }

    /// Lookup a value constructor in the current scope.
    ///
    pub fn get_value_constructor(
        &self,
        module: Option<&String>,
        name: &str,
    ) -> Result<&ValueConstructor, GetValueConstructorError> {
        match module {
            None => self.local_values.get(name).ok_or_else(|| {
                GetValueConstructorError::UnknownVariable {
                    name: name.to_string(),
                    variables: self.local_values.keys().map(|t| t.to_string()).collect(),
                }
            }),

            Some(module) => {
                let module = self.imported_modules.get(&*module).ok_or_else(|| {
                    GetValueConstructorError::UnknownModule {
                        name: name.to_string(),
                        imported_modules: self
                            .importable_modules
                            .keys()
                            .map(|t| t.to_string())
                            .collect(),
                    }
                })?;
                module.1.values.get(&*name).ok_or_else(|| {
                    GetValueConstructorError::UnknownModuleValue {
                        name: name.to_string(),
                        module_name: module.1.name.clone(),
                        value_constructors: module.1.values.keys().map(|t| t.to_string()).collect(),
                    }
                })
            }
        }
    }

    pub fn insert_accessors(&mut self, type_name: &str, accessors: AccessorsMap) {
        self.accessors.insert(type_name.to_string(), accessors);
    }

    pub fn do_infer_fn(
        &mut self,
        args: Vec<UntypedArg>,
        body: UntypedExpr,
        return_annotation: &Option<TypeAst>,
    ) -> Result<(Vec<TypedArg>, TypedExpr), Error> {
        // Construct an initial type for each argument of the function- either an unbound type variable
        // or a type provided by an annotation.
        let args: Vec<_> = args
            .into_iter()
            .map(|arg| self.infer_arg(arg))
            .collect::<Result<_, _>>()?;

        let body = self.in_new_scope(|body_typer| {
            for (arg, t) in args.iter().zip(args.iter().map(|arg| arg.typ.clone())) {
                match &arg.names {
                    ArgNames::Named { name } | ArgNames::NamedLabelled { name, .. } => body_typer
                        .insert_variable(
                            name.to_string(),
                            ValueConstructorVariant::LocalVariable,
                            t,
                        ),
                    ArgNames::Discard { .. } | ArgNames::LabelledDiscard { .. } => (),
                };
            }

            body_typer.infer(body)
        })?;

        // Check that any return type annotation is accurate.
        if let Some(ann) = return_annotation {
            let ret_typ = self.type_from_ast(ann)?;
            self.unify(ret_typ, body.typ())
                .map_err(|e| convert_unify_error(e, body.location()))?;
        }

        Ok((args, body))
    }

    pub fn do_infer_call(
        &mut self,
        fun: UntypedExpr,
        args: Vec<CallArg<UntypedExpr>>,
        location: &SrcSpan,
    ) -> Result<(TypedExpr, Vec<TypedCallArg>, Arc<Type>), Error> {
        let fun = self.infer(fun)?;
        let (fun, args, typ) = self.do_infer_call_with_known_fun(fun, args, location)?;
        Ok((fun, args, typ))
    }

    pub fn do_infer_call_with_known_fun(
        &mut self,
        fun: TypedExpr,
        mut args: Vec<CallArg<UntypedExpr>>,
        location: &SrcSpan,
    ) -> Result<(TypedExpr, Vec<TypedCallArg>, Arc<Type>), Error> {
        // Check to see if the function accepts labelled arguments
        match self
            .get_field_map(&fun)
            .map_err(|e| convert_get_value_constructor_error(e, location))?
        {
            // The fun has a field map so labelled arguments may be present and need to be reordered.
            Some(field_map) => field_map.reorder(&mut args, location)?,

            // The fun has no field map and so we error if arguments have been labelled
            None => assert_no_labelled_arguments(&args)?,
        }

        // Extract the type of the fun, ensuring it actually is a function
        let (mut args_types, return_type) = match_fun_type(fun.typ(), args.len(), self)
            .map_err(|e| convert_not_fun_error(e, fun.location(), location))?;

        // Ensure that the given args have the correct types
        let args = args_types
            .iter_mut()
            .zip(args)
            .map(|(typ, arg): (&mut Arc<Type>, _)| {
                let CallArg {
                    label,
                    value,
                    location,
                } = arg;
                let value = self.infer(value)?;
                self.unify(typ.clone(), value.typ())
                    .map_err(|e| convert_unify_error(e, value.location()))?;
                Ok(CallArg {
                    label,
                    value,
                    location,
                })
            })
            .collect::<Result<_, _>>()?;
        Ok((fun, args, return_type))
    }

    /// Instantiate converts generic variables into unbound ones.
    ///
    pub fn instantiate(
        &mut self,
        t: Arc<Type>,
        ctx_level: usize,
        ids: &mut im::HashMap<usize, Arc<Type>>,
    ) -> Arc<Type> {
        match &*t {
            Type::App {
                public,
                name,
                module,
                args,
            } => {
                let args = args
                    .iter()
                    .map(|t| self.instantiate(t.clone(), ctx_level, ids))
                    .collect();
                Arc::new(Type::App {
                    public: *public,
                    name: name.clone(),
                    module: module.clone(),
                    args,
                })
            }

            Type::Var { typ } => {
                match &*typ.borrow() {
                    TypeVar::Link { typ } => return self.instantiate(typ.clone(), ctx_level, ids),

                    TypeVar::Unbound { .. } => return Arc::new(Type::Var { typ: typ.clone() }),

                    TypeVar::Generic { id } => match ids.get(id) {
                        Some(t) => return t.clone(),
                        None => {
                            if !self.annotated_generic_types.contains(id) {
                                // Check this in the hydrator, i.e. is it a created type
                                let v = self.new_unbound_var(ctx_level);
                                ids.insert(*id, v.clone());
                                return v;
                            }
                        }
                    },
                }
                Arc::new(Type::Var { typ: typ.clone() })
            }

            Type::Fn { args, retrn, .. } => fn_(
                args.iter()
                    .map(|t| self.instantiate(t.clone(), ctx_level, ids))
                    .collect(),
                self.instantiate(retrn.clone(), ctx_level, ids),
            ),

            Type::Tuple { elems } => tuple(
                elems
                    .iter()
                    .map(|t| self.instantiate(t.clone(), ctx_level, ids))
                    .collect(),
            ),
        }
    }

    pub fn make_type_vars(
        &mut self,
        args: &[String],
        location: &SrcSpan,
    ) -> Result<Vec<Arc<Type>>, Error> {
        args.iter()
            .map(|arg| TypeAst::Var {
                location: location.clone(),
                name: arg.to_string(),
            })
            .map(|ast| self.type_from_ast(&ast))
            .collect::<Result<_, _>>()
    }

    pub fn custom_type_accessors(
        &mut self,
        constructors: &[RecordConstructor],
    ) -> Result<Option<HashMap<String, RecordAccessor>>, Error> {
        // Get the constructor for this custom type.
        let args = match constructors {
            [constructor] if !constructor.args.is_empty() => &constructor.args,
            // If there is not exactly 1 constructor we return as we cannot
            // build any constructors.
            _ => return Ok(None),
        };

        let mut fields = HashMap::with_capacity(args.len());
        self.hydrator.disallow_new_type_variables();
        for (index, (label, arg, ..)) in args.iter().enumerate() {
            if let Some(label) = label {
                let typ = self.type_from_ast(arg)?;
                fields.insert(
                    label.to_string(),
                    RecordAccessor {
                        index: index as u64,
                        label: label.to_string(),
                        typ,
                    },
                );
            }
        }
        Ok(Some(fields))
    }

    /// Unify two types that should be the same.
    /// Any unbound type variables will be linked to the other type as they are the same.
    ///
    /// It two types are found to not be the same an error is returned.
    ///
    pub fn unify(&mut self, t1: Arc<Type>, t2: Arc<Type>) -> Result<(), UnifyError> {
        if t1 == t2 {
            return Ok(());
        }

        // Collapse right hand side type links. Left hand side will be collapsed in the next block.
        if let Type::Var { typ } = &*t2 {
            if let TypeVar::Link { typ } = &*typ.borrow() {
                return self.unify(t1, typ.clone());
            }
        }

        if let Type::Var { typ } = &*t1 {
            enum Action {
                Unify(Arc<Type>),
                CouldNotUnify,
                Link,
            }

            let action = match &*typ.borrow() {
                TypeVar::Link { typ } => Action::Unify(typ.clone()),

                TypeVar::Unbound { id, level } => {
                    update_levels(t2.clone(), *level, *id)?;
                    Action::Link
                }

                TypeVar::Generic { id } => {
                    if let Type::Var { typ } = &*t2 {
                        if typ.borrow().is_unbound() {
                            *typ.borrow_mut() = TypeVar::Generic { id: *id };
                            return Ok(());
                        }
                    }
                    Action::CouldNotUnify
                }
            };

            return match action {
                Action::Link => {
                    *typ.borrow_mut() = TypeVar::Link { typ: t2 };
                    Ok(())
                }

                Action::Unify(t) => self.unify(t, t2),

                Action::CouldNotUnify => Err(UnifyError::CouldNotUnify {
                    expected: t1.clone(),
                    given: t2,
                }),
            };
        }

        if let Type::Var { .. } = *t2 {
            return self.unify(t2, t1).map_err(flip_unify_error);
        }

        match (&*t1, &*t2) {
            (
                Type::App {
                    module: m1,
                    name: n1,
                    args: args1,
                    ..
                },
                Type::App {
                    module: m2,
                    name: n2,
                    args: args2,
                    ..
                },
            ) if m1 == m2 && n1 == n2 && args1.len() == args2.len() => {
                for (a, b) in args1.iter().zip(args2) {
                    unify_enclosed_type(t1.clone(), t2.clone(), self.unify(a.clone(), b.clone()))?;
                }
                Ok(())
            }

            (Type::Tuple { elems: elems1, .. }, Type::Tuple { elems: elems2, .. })
                if elems1.len() == elems2.len() =>
            {
                for (a, b) in elems1.iter().zip(elems2) {
                    unify_enclosed_type(t1.clone(), t2.clone(), self.unify(a.clone(), b.clone()))?;
                }
                Ok(())
            }

            (
                Type::Fn {
                    args: args1,
                    retrn: retrn1,
                    ..
                },
                Type::Fn {
                    args: args2,
                    retrn: retrn2,
                    ..
                },
            ) if args1.len() == args2.len() => {
                for (a, b) in args1.iter().zip(args2) {
                    self.unify(a.clone(), b.clone())
                        .map_err(|_| UnifyError::CouldNotUnify {
                            expected: t1.clone(),
                            given: t2.clone(),
                        })?;
                }
                self.unify(retrn1.clone(), retrn2.clone())
                    .map_err(|_| UnifyError::CouldNotUnify {
                        expected: t1.clone(),
                        given: t2.clone(),
                    })
            }

            (_, _) => Err(UnifyError::CouldNotUnify {
                expected: t1.clone(),
                given: t2.clone(),
            }),
        }
    }

    pub fn register_import(&mut self, s: &UntypedStatement) -> Result<(), Error> {
        match s {
            Statement::Import {
                module,
                as_name,
                unqualified,
                ..
            } => {
                // Find imported module
                let module_info = self
                    .importable_modules
                    .get(&module.join("/"))
                    .gleam_expect("Typer could not find a module being imported.");

                // Determine local alias of imported module
                let module_name = match &as_name {
                    None => module[module.len() - 1].clone(),
                    Some(name) => name.clone(),
                };

                // Insert unqualified imports into scope
                for UnqualifiedImport {
                    name,
                    location,
                    as_name,
                } in unqualified
                {
                    let mut imported = false;

                    let imported_name = match &as_name {
                        None => name,
                        Some(alias) => alias,
                    };

                    if let Some(value) = module_info.1.values.get(name) {
                        self.insert_variable(
                            imported_name.clone(),
                            value.variant.clone(),
                            value.typ.clone(),
                        );
                        imported = true;
                    }

                    if let Some(typ) = module_info.1.types.get(name) {
                        match self.insert_type_constructor(imported_name.clone(), typ.clone()) {
                            Ok(_) => (),
                            Err(e) => return Err(e),
                        }

                        imported = true;
                    }

                    if !imported {
                        return Err(Error::UnknownModuleField {
                            location: location.clone(),
                            name: name.clone(),
                            module_name: module.clone(),
                            value_constructors: module_info
                                .1
                                .values
                                .keys()
                                .map(|t| t.to_string())
                                .collect(),
                            type_constructors: module_info
                                .1
                                .types
                                .keys()
                                .map(|t| t.to_string())
                                .collect(),
                        });
                    }
                }

                // Insert imported module into scope
                // TODO: use a refernce to the module to avoid copying
                self.imported_modules
                    .insert(module_name, module_info.clone());
                Ok(())
            }

            _ => Ok(()),
        }
    }

    /// Iterate over a module, registering any new types created by the module into the typer
    pub fn register_types(
        &mut self,
        statement: &UntypedStatement,
        module: &[String],
    ) -> Result<(), Error> {
        match statement {
            Statement::ExternalType {
                name,
                public,
                args,
                location,
                ..
            } => {
                self.reset_hydrator();
                let parameters = self.make_type_vars(args, location)?;
                let typ = Arc::new(Type::App {
                    public: *public,
                    module: module.to_owned(),
                    name: name.clone(),
                    args: parameters.clone(),
                });

                self.insert_type_constructor(
                    name.clone(),
                    TypeConstructor {
                        origin: location.clone(),
                        module: module.to_owned(),
                        public: *public,
                        parameters,
                        typ,
                    },
                )?;
            }

            Statement::CustomType {
                name,
                public,
                opaque,
                args,
                constructors,
                location,
                ..
            } => {
                self.reset_hydrator();
                let parameters = self.make_type_vars(args, location)?;
                let typ = Arc::new(Type::App {
                    public: *public,
                    module: module.to_owned(),
                    name: name.clone(),
                    args: parameters.clone(),
                });

                self.insert_type_constructor(
                    name.clone(),
                    TypeConstructor {
                        origin: location.clone(),
                        module: module.to_owned(),
                        public: *public,
                        parameters,
                        typ: typ.clone(),
                    },
                )?;

                // If the custom type only has a single constructor then we can access the
                // fields using the record.field syntax, so store any fields accessors.
                if let Some(accessors) = self.custom_type_accessors(constructors.as_slice())? {
                    let map = AccessorsMap {
                        public: (*public && !*opaque),
                        accessors,
                        typ: typ.clone(),
                    };
                    self.insert_accessors(name.as_ref(), map)
                }
            }

            Statement::TypeAlias {
                location,
                public,
                args,
                alias: name,
                resolved_type,
                ..
            } => {
                self.reset_hydrator();
                // Register the paramerterised types
                let parameters = self.make_type_vars(args, location)?;

                // Disallow creation of new types outside the paramerterised types
                self.hydrator.disallow_new_type_variables();

                // Create the type that the alias resolves to
                let typ = self.type_from_ast(&resolved_type)?;
                self.insert_type_constructor(
                    name.clone(),
                    TypeConstructor {
                        origin: location.clone(),
                        module: module.to_owned(),
                        public: *public,
                        parameters,
                        typ,
                    },
                )?;
            }

            _ => {}
        }

        Ok(())
    }
}
