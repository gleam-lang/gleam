use super::*;
use std::collections::HashMap;

#[derive(Debug)]
pub struct Environment<'a, 'b> {
    pub current_module: &'a [String],
    pub uid: &'b mut usize,
    pub level: usize,
    pub importable_modules: &'a HashMap<String, (Origin, Module)>,
    pub imported_modules: HashMap<String, (Origin, Module)>,

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

    // Functions that have not yet been inferred then generalised.
    // We use this to determine whether functions that call this one
    // can safely be generalised.
    pub ungeneralised_functions: HashSet<String>,

    // entity_usages is a stack of scopes. When an entity is created it is
    // added to the top scope. When an entity is used we crawl down the scope
    // stack for an entity with that name and mark it as used.
    pub entity_usages: Vec<HashMap<String, (EntityKind, SrcSpan, bool)>>,
}

/// For Keeping track of entity usages and knowing which error to display.
#[derive(Debug, Clone)]
pub enum EntityKind {
    PrivateConstant,
    // String here is the type constructor's type name
    PrivateTypeConstructor(String),
    PrivateFunction,
    ImportedConstructor,
    ImportedType,
    ImportedTypeAndConstructor,
    ImportedValue,
    PrivateType,
    Variable,
}

impl<'a, 'b> Environment<'a, 'b> {
    pub fn new(
        uid: &'b mut usize,
        current_module: &'a [String],
        importable_modules: &'a HashMap<String, (Origin, Module)>,
        warnings: &'a mut Vec<Warning>,
    ) -> Self {
        let mut entity_usages = vec![];
        entity_usages.push(HashMap::new());
        let typer = Self {
            uid,
            level: 1,
            ungeneralised_functions: HashSet::new(),
            module_types: HashMap::new(),
            module_values: HashMap::new(),
            imported_modules: HashMap::new(),
            accessors: HashMap::new(),
            local_values: hashmap![],
            importable_modules,
            current_module,
            warnings,
            entity_usages,
        };
        register_prelude(typer)
    }
}

#[derive(Debug)]
pub struct ScopeResetData {
    local_values: im::HashMap<String, ValueConstructor>,
}

impl<'a, 'b> Environment<'a, 'b> {
    pub fn in_new_scope<T>(&mut self, process_scope: impl FnOnce(&mut Self) -> T) -> T {
        // Record initial scope state
        let initial = self.open_new_scope();

        // Process scope
        let result = process_scope(self);

        self.close_scope(initial);

        // Return result of typing the scope
        result
    }

    pub fn open_new_scope(&mut self) -> ScopeResetData {
        let local_values = self.local_values.clone();
        self.entity_usages.push(HashMap::new());
        self.level += 1;
        ScopeResetData { local_values }
    }

    pub fn close_scope(&mut self, data: ScopeResetData) {
        self.level -= 1;
        let unused = self
            .entity_usages
            .pop()
            .gleam_expect("There was no top entity scope.");
        self.handle_unused(unused);
        self.local_values = data.local_values;
    }

    pub fn next_uid(&mut self) -> usize {
        let i = *self.uid;
        *self.uid += 1;
        i
    }

    pub fn previous_uid(&self) -> usize {
        *self.uid - 1
    }

    /// Create a new unbound type that is a specific type, we just don't
    /// know which one yet.
    ///
    pub fn new_unbound_var(&mut self, level: usize) -> Arc<Type> {
        unbound_var(self.next_uid(), level)
    }

    /// Create a new generic type that can stand in for any type.
    ///
    pub fn new_generic_var(&mut self) -> Arc<Type> {
        generic_var(self.next_uid())
    }

    /// Insert a variable in the current scope.
    ///
    pub fn insert_variable(
        &mut self,
        name: String,
        variant: ValueConstructorVariant,
        typ: Arc<Type>,
        origin: SrcSpan,
    ) {
        let _ = self.local_values.insert(
            name,
            ValueConstructor {
                public: false,
                origin,
                variant,
                typ,
            },
        );
    }

    /// Insert a value into the current module.
    /// Errors if the module already has a value with that name.
    ///
    pub fn insert_module_value(&mut self, name: &str, value: ValueConstructor) {
        let _ = self.module_values.insert(name.to_string(), value);
    }

    /// Lookup a variable in the current scope.
    ///
    pub fn get_variable(&self, name: &str) -> Option<&ValueConstructor> {
        self.local_values.get(name)
    }

    /// Lookup a module constant in the current scope.
    ///
    pub fn get_module_const(&mut self, name: &str) -> Option<&ValueConstructor> {
        self.increment_usage(name);
        self.module_values
            .get(name)
            .filter(|ValueConstructor { variant, .. }| {
                matches!(variant, ValueConstructorVariant::ModuleConstant { .. })
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
        let _ = self.accessors.insert(type_name.to_string(), accessors);
    }

    /// Instantiate converts generic variables into unbound ones.
    ///
    pub fn instantiate(
        &mut self,
        t: Arc<Type>,
        ctx_level: usize,
        ids: &mut im::HashMap<usize, Arc<Type>>,
        hydrator: &Hydrator,
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
                    .map(|t| self.instantiate(t.clone(), ctx_level, ids, hydrator))
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
                    TypeVar::Link { typ } => {
                        return self.instantiate(typ.clone(), ctx_level, ids, hydrator)
                    }

                    TypeVar::Unbound { .. } => return Arc::new(Type::Var { typ: typ.clone() }),

                    TypeVar::Generic { id } => match ids.get(id) {
                        Some(t) => return t.clone(),
                        None => {
                            if !hydrator.is_created_generic_type(id) {
                                // Check this in the hydrator, i.e. is it a created type
                                let v = self.new_unbound_var(ctx_level);
                                let _ = ids.insert(*id, v.clone());
                                return v;
                            }
                        }
                    },
                }
                Arc::new(Type::Var { typ: typ.clone() })
            }

            Type::Fn { args, retrn, .. } => fn_(
                args.iter()
                    .map(|t| self.instantiate(t.clone(), ctx_level, ids, hydrator))
                    .collect(),
                self.instantiate(retrn.clone(), ctx_level, ids, hydrator),
            ),

            Type::Tuple { elems } => tuple(
                elems
                    .iter()
                    .map(|t| self.instantiate(t.clone(), ctx_level, ids, hydrator))
                    .collect(),
            ),
        }
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
                    situation: None,
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
                            situation: None,
                        })?;
                }
                self.unify(retrn1.clone(), retrn2.clone())
                    .map_err(|_| UnifyError::CouldNotUnify {
                        expected: t1.clone(),
                        given: t2.clone(),
                        situation: None,
                    })
            }

            (_, _) => Err(UnifyError::CouldNotUnify {
                expected: t1.clone(),
                given: t2.clone(),
                situation: None,
            }),
        }
    }

    /// Inserts an entity at the current scope for usage tracking.
    pub fn init_usage(&mut self, name: String, kind: EntityKind, location: SrcSpan) {
        match self
            .entity_usages
            .last_mut()
            .gleam_expect("Attempted to access non-existant entity usages scope")
            .insert(name.to_string(), (kind, location, false))
        {
            // Private types can be shadowed by a constructor with the same name
            //
            // TODO: Improve this so that we can tell if an imported overriden
            // type is actually used or not by tracking whether usages apply to
            // the value or type scope
            Some((EntityKind::ImportedTypeAndConstructor, _, _))
            | Some((EntityKind::ImportedType, _, _))
            | Some((EntityKind::PrivateType, _, _)) => {}
            Some((kind, location, false)) => {
                // an entity was overwritten in the top most scope without being used
                let mut unused = HashMap::with_capacity(1);
                let _ = unused.insert(name, (kind, location, false));
                self.handle_unused(unused);
            }
            _ => {}
        }
    }

    /// Increments an entity's usage in the current or nearest enclosing scope
    pub fn increment_usage(&mut self, name: &str) {
        let mut cascade = None;
        for scope in self.entity_usages.iter_mut().rev() {
            match scope.get_mut(name) {
                Some((EntityKind::PrivateTypeConstructor(type_name), _, usages)) => {
                    // If a type constructor is used, we consider
                    // its type also used
                    if type_name != name {
                        cascade = Some(type_name.clone());
                    }
                    *usages = true;
                    break;
                }
                Some(val) => {
                    val.2 = true;
                    break;
                }
                None => {}
            }
        }
        if let Some(cascade) = cascade {
            self.increment_usage(&cascade)
        };
    }

    /// Converts entities with a usage count of 0 to warnings
    pub fn convert_unused_to_warnings(&mut self) {
        let unused = self
            .entity_usages
            .pop()
            .gleam_expect("Expected a bottom level of entity usages.");
        self.handle_unused(unused);
    }

    fn handle_unused(&mut self, unused: HashMap<String, (EntityKind, SrcSpan, bool)>) {
        for val in unused {
            match val {
                (name, (EntityKind::ImportedType, location, false)) => {
                    self.warnings.push(Warning::UnusedType {
                        name,
                        imported: true,
                        location,
                    })
                }
                (name, (EntityKind::ImportedConstructor, location, false)) => {
                    self.warnings.push(Warning::UnusedConstructor {
                        name,
                        imported: true,
                        location,
                    })
                }
                (name, (EntityKind::ImportedTypeAndConstructor, location, false)) => {
                    self.warnings.push(Warning::UnusedType {
                        name,
                        imported: true,
                        location,
                    })
                }
                (name, (EntityKind::PrivateConstant, location, false)) => self
                    .warnings
                    .push(Warning::UnusedPrivateModuleConstant { name, location }),
                (name, (EntityKind::PrivateTypeConstructor(_), location, false)) => {
                    self.warnings.push(Warning::UnusedConstructor {
                        name,
                        imported: false,
                        location,
                    })
                }
                (name, (EntityKind::PrivateFunction, location, false)) => self
                    .warnings
                    .push(Warning::UnusedPrivateFunction { name, location }),
                (name, (EntityKind::PrivateType, location, false)) => {
                    self.warnings.push(Warning::UnusedType {
                        name,
                        imported: false,
                        location,
                    })
                }
                (name, (EntityKind::ImportedValue, location, false)) => self
                    .warnings
                    .push(Warning::UnusedImportedValue { name, location }),

                (name, (EntityKind::Variable, location, false)) => self
                    .warnings
                    .push(Warning::UnusedVariable { name, location }),

                _ => {}
            };
        }
    }
}
