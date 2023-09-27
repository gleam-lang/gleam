use crate::{
    analyse::Inferred, ast::PIPE_VARIABLE, build::Target, uid::UniqueIdGenerator,
    warning::TypeWarningEmitter,
};

use super::*;
use std::collections::HashMap;

#[derive(Debug)]
pub struct Environment<'a> {
    pub current_module: &'a str,
    pub target: Target,
    pub ids: UniqueIdGenerator,
    previous_id: u64,
    /// Names of types or values that have been imported an unqualified fashion
    /// from other modules. Used to prevent multiple imports using the same name.
    pub unqualified_imported_names: HashMap<SmolStr, SrcSpan>,
    pub importable_modules: &'a im::HashMap<SmolStr, ModuleInterface>,

    /// Modules that have been imported by the current module, along with the
    /// location of the import statement where they were imported.
    pub imported_modules: HashMap<SmolStr, (SrcSpan, &'a ModuleInterface)>,
    pub unused_modules: HashMap<SmolStr, SrcSpan>,
    pub imported_types: HashSet<SmolStr>,

    /// Values defined in the current function (or the prelude)
    pub scope: im::HashMap<SmolStr, ValueConstructor>,

    /// Types defined in the current module (or the prelude)
    pub module_types: HashMap<SmolStr, TypeConstructor>,

    /// Mapping from types to constructor names in the current module (or the prelude)
    pub module_types_constructors: HashMap<SmolStr, Vec<SmolStr>>,

    /// Values defined in the current module (or the prelude)
    pub module_values: HashMap<SmolStr, ValueConstructor>,

    /// Accessors defined in the current module
    pub accessors: HashMap<SmolStr, AccessorsMap>,

    /// Warnings
    pub warnings: &'a TypeWarningEmitter,

    /// entity_usages is a stack of scopes. When an entity is created it is
    /// added to the top scope. When an entity is used we crawl down the scope
    /// stack for an entity with that name and mark it as used.
    /// NOTE: The bool in the tuple here tracks if the entity has been used
    pub entity_usages: Vec<HashMap<SmolStr, (EntityKind, SrcSpan, bool)>>,
}

impl<'a> Environment<'a> {
    pub fn new(
        ids: UniqueIdGenerator,
        current_module: &'a str,
        target: Target,
        importable_modules: &'a im::HashMap<SmolStr, ModuleInterface>,
        warnings: &'a TypeWarningEmitter,
    ) -> Self {
        let prelude = importable_modules
            .get(PRELUDE_MODULE_NAME)
            .expect("Unable to find prelude in importable modules");
        Self {
            previous_id: ids.next(),
            ids,
            target,
            module_types: prelude.types.clone(),
            module_types_constructors: prelude.types_constructors.clone(),
            module_values: HashMap::new(),
            imported_modules: HashMap::new(),
            unused_modules: HashMap::new(),
            unqualified_imported_names: HashMap::new(),
            accessors: prelude.accessors.clone(),
            scope: prelude.values.clone().into(),
            importable_modules,
            imported_types: HashSet::new(),
            current_module,
            warnings,
            entity_usages: vec![HashMap::new()],
        }
    }
}

/// For Keeping track of entity usages and knowing which error to display.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum EntityKind {
    PrivateConstant,
    // String here is the type constructor's type name
    PrivateTypeConstructor(SmolStr),
    PrivateFunction,
    ImportedConstructor,
    ImportedType,
    ImportedTypeAndConstructor,
    ImportedValue,
    PrivateType,
    Variable,
}

#[derive(Debug)]
pub struct ScopeResetData {
    local_values: im::HashMap<SmolStr, ValueConstructor>,
}

impl<'a> Environment<'a> {
    pub fn in_new_scope<T, E>(
        &mut self,
        process_scope: impl FnOnce(&mut Self) -> Result<T, E>,
    ) -> Result<T, E> {
        // Record initial scope state
        let initial = self.open_new_scope();

        // Process scope
        let result = process_scope(self);

        self.close_scope(initial, result.is_ok());

        // Return result of typing the scope
        result
    }

    pub fn open_new_scope(&mut self) -> ScopeResetData {
        let local_values = self.scope.clone();
        self.entity_usages.push(HashMap::new());
        ScopeResetData { local_values }
    }

    pub fn close_scope(&mut self, data: ScopeResetData, was_successful: bool) {
        let unused = self
            .entity_usages
            .pop()
            .expect("There was no top entity scope.");

        // We only check for unused entities if the scope was successfully
        // processed. If it was not then any seemingly unused entities may have
        // been used beyond the point where the error occured, so we don't want
        // to incorrectly warn about them.
        if was_successful {
            self.handle_unused(unused);
        }
        self.scope = data.local_values;
    }

    pub fn next_uid(&mut self) -> u64 {
        let id = self.ids.next();
        self.previous_id = id;
        id
    }

    pub fn previous_uid(&self) -> u64 {
        self.previous_id
    }

    /// Create a new unbound type that is a specific type, we just don't
    /// know which one yet.
    ///
    pub fn new_unbound_var(&mut self) -> Arc<Type> {
        unbound_var(self.next_uid())
    }

    /// Create a new generic type that can stand in for any type.
    ///
    pub fn new_generic_var(&mut self) -> Arc<Type> {
        generic_var(self.next_uid())
    }

    /// Insert a variable in the current scope.
    ///
    pub fn insert_local_variable(&mut self, name: SmolStr, location: SrcSpan, typ: Arc<Type>) {
        let _ = self.scope.insert(
            name,
            ValueConstructor {
                deprecation: Deprecation::NotDeprecated,
                public: false,
                variant: ValueConstructorVariant::LocalVariable { location },
                type_: typ,
            },
        );
    }

    /// Insert a constant in the current scope
    pub fn insert_local_constant(&mut self, name: SmolStr, literal: Constant<Arc<Type>, SmolStr>) {
        let _ = self.scope.insert(
            name,
            ValueConstructor {
                deprecation: Deprecation::NotDeprecated,
                public: false,
                variant: ValueConstructorVariant::LocalConstant {
                    literal: literal.clone(),
                },
                type_: literal.type_(),
            },
        );
    }

    /// Insert a variable in the current scope.
    ///
    pub fn insert_variable(
        &mut self,
        name: SmolStr,
        variant: ValueConstructorVariant,
        typ: Arc<Type>,
        public: bool,
        deprecation: Deprecation,
    ) {
        let _ = self.scope.insert(
            name,
            ValueConstructor {
                public,
                deprecation,
                variant,
                type_: typ,
            },
        );
    }

    /// Insert a value into the current module.
    /// Errors if the module already has a value with that name.
    ///
    pub fn insert_module_value(&mut self, name: SmolStr, value: ValueConstructor) {
        let _ = self.module_values.insert(name, value);
    }

    /// Lookup a variable in the current scope.
    ///
    pub fn get_variable(&self, name: &SmolStr) -> Option<&ValueConstructor> {
        self.scope.get(name)
    }

    /// Lookup a module constant in the current scope.
    ///
    pub fn get_module_const(&mut self, name: &SmolStr) -> Option<&ValueConstructor> {
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
        type_name: SmolStr,
        info: TypeConstructor,
    ) -> Result<(), Error> {
        let name = type_name.clone();
        let location = info.origin;
        match self.module_types.insert(type_name, info) {
            None => Ok(()),
            Some(prelude_type) if is_prelude_module(&prelude_type.module) => Ok(()),
            Some(previous) => Err(Error::DuplicateTypeName {
                name,
                location,
                previous_location: previous.origin,
            }),
        }
    }

    /// Map a type to constructors in the current scope.
    ///
    pub fn insert_type_to_constructors(&mut self, type_name: SmolStr, constructors: Vec<SmolStr>) {
        let _ = self
            .module_types_constructors
            .insert(type_name, constructors);
    }

    /// Lookup a type in the current scope.
    ///
    pub fn get_type_constructor(
        &mut self,
        module_alias: &Option<SmolStr>,
        name: &SmolStr,
    ) -> Result<&TypeConstructor, UnknownTypeConstructorError> {
        match module_alias {
            None => self
                .module_types
                .get(name)
                .ok_or_else(|| UnknownTypeConstructorError::Type {
                    name: name.clone(),
                    type_constructors: self.module_types.keys().cloned().collect(),
                }),

            Some(module_name) => {
                let (_, module) = self.imported_modules.get(module_name).ok_or_else(|| {
                    UnknownTypeConstructorError::Module {
                        name: module_name.clone(),
                        imported_modules: self.importable_modules.keys().cloned().collect(),
                    }
                })?;
                let _ = self.unused_modules.remove(module_name);
                module
                    .types
                    .get(name)
                    .ok_or_else(|| UnknownTypeConstructorError::ModuleType {
                        name: name.clone(),
                        module_name: module.name.clone(),
                        type_constructors: module.public_type_names(),
                    })
            }
        }
    }

    /// Lookup constructors for type in the current scope.
    ///
    pub fn get_constructors_for_type(
        &mut self,
        full_module_name: Option<&str>,
        name: &SmolStr,
    ) -> Result<&Vec<SmolStr>, UnknownTypeConstructorError> {
        match full_module_name {
            None => self.module_types_constructors.get(name).ok_or_else(|| {
                UnknownTypeConstructorError::Type {
                    name: name.clone(),
                    type_constructors: self.module_types.keys().cloned().collect(),
                }
            }),

            Some(m) => {
                let module = self.importable_modules.get(m).ok_or_else(|| {
                    UnknownTypeConstructorError::Module {
                        name: name.clone(),
                        imported_modules: self.importable_modules.keys().cloned().collect(),
                    }
                })?;
                let _ = self.unused_modules.remove(m);
                module.types_constructors.get(name).ok_or_else(|| {
                    UnknownTypeConstructorError::ModuleType {
                        name: name.clone(),
                        module_name: module.name.clone(),
                        type_constructors: module.public_type_names(),
                    }
                })
            }
        }
    }

    /// Lookup a value constructor in the current scope.
    ///
    pub fn get_value_constructor(
        &mut self,
        module: Option<&SmolStr>,
        name: &SmolStr,
    ) -> Result<&ValueConstructor, UnknownValueConstructorError> {
        match module {
            None => self
                .scope
                .get(name)
                .ok_or_else(|| UnknownValueConstructorError::Variable {
                    name: name.clone(),
                    variables: self.local_value_names(),
                }),

            Some(module_name) => {
                let (_, module) = self.imported_modules.get(module_name).ok_or_else(|| {
                    UnknownValueConstructorError::Module {
                        name: module_name.clone(),
                        imported_modules: self.importable_modules.keys().cloned().collect(),
                    }
                })?;
                let _ = self.unused_modules.remove(module_name);
                module.get_public_value(name).ok_or_else(|| {
                    UnknownValueConstructorError::ModuleValue {
                        name: name.clone(),
                        module_name: module.name.clone(),
                        value_constructors: module.public_value_names(),
                    }
                })
            }
        }
    }

    pub fn insert_accessors(&mut self, type_name: SmolStr, accessors: AccessorsMap) {
        let _ = self.accessors.insert(type_name, accessors);
    }

    /// Instantiate converts generic variables into unbound ones.
    ///
    pub fn instantiate(
        &mut self,
        t: Arc<Type>,
        ids: &mut im::HashMap<u64, Arc<Type>>,
        hydrator: &Hydrator,
    ) -> Arc<Type> {
        match t.deref() {
            Type::Named {
                public,
                name,
                module,
                args,
            } => {
                let args = args
                    .iter()
                    .map(|t| self.instantiate(t.clone(), ids, hydrator))
                    .collect();
                Arc::new(Type::Named {
                    public: *public,
                    name: name.clone(),
                    module: module.clone(),
                    args,
                })
            }

            Type::Var { type_: typ } => {
                match typ.borrow().deref() {
                    TypeVar::Link { type_: typ } => {
                        return self.instantiate(typ.clone(), ids, hydrator)
                    }

                    TypeVar::Unbound { .. } => return Arc::new(Type::Var { type_: typ.clone() }),

                    TypeVar::Generic { id } => match ids.get(id) {
                        Some(t) => return t.clone(),
                        None => {
                            if !hydrator.is_rigid(id) {
                                // Check this in the hydrator, i.e. is it a created type
                                let v = self.new_unbound_var();
                                let _ = ids.insert(*id, v.clone());
                                return v;
                            }
                        }
                    },
                }
                Arc::new(Type::Var { type_: typ.clone() })
            }

            Type::Fn { args, retrn, .. } => fn_(
                args.iter()
                    .map(|t| self.instantiate(t.clone(), ids, hydrator))
                    .collect(),
                self.instantiate(retrn.clone(), ids, hydrator),
            ),

            Type::Tuple { elems } => tuple(
                elems
                    .iter()
                    .map(|t| self.instantiate(t.clone(), ids, hydrator))
                    .collect(),
            ),
        }
    }

    /// Inserts an entity at the current scope for usage tracking.
    pub fn init_usage(&mut self, name: SmolStr, kind: EntityKind, location: SrcSpan) {
        use EntityKind::*;

        match self
            .entity_usages
            .last_mut()
            .expect("Attempted to access non-existant entity usages scope")
            .insert(name.clone(), (kind, location, false))
        {
            // Private types can be shadowed by a constructor with the same name
            //
            // TODO: Improve this so that we can tell if an imported overriden
            // type is actually used or not by tracking whether usages apply to
            // the value or type scope
            Some((ImportedType | ImportedTypeAndConstructor | PrivateType, _, _)) => {}

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
    pub fn increment_usage(&mut self, name: &SmolStr) {
        let mut name = name.clone();

        while let Some((kind, _, used)) = self
            .entity_usages
            .iter_mut()
            .rev()
            .find_map(|scope| scope.get_mut(&name))
        {
            *used = true;

            match kind {
                // If a type constructor is used, we consider its type also used
                EntityKind::PrivateTypeConstructor(type_name) if *type_name != name => {
                    name = type_name.clone();
                }
                _ => return,
            }
        }
    }

    /// Converts entities with a usage count of 0 to warnings
    pub fn convert_unused_to_warnings(&mut self) {
        let unused = self
            .entity_usages
            .pop()
            .expect("Expected a bottom level of entity usages.");
        self.handle_unused(unused);

        for (name, location) in self.unused_modules.clone().into_iter() {
            self.warnings
                .emit(Warning::UnusedImportedModule { name, location });
        }
    }

    fn handle_unused(&mut self, unused: HashMap<SmolStr, (EntityKind, SrcSpan, bool)>) {
        for (name, (kind, location, _)) in unused.into_iter().filter(|(_, (_, _, used))| !used) {
            let warning = match kind {
                EntityKind::ImportedType | EntityKind::ImportedTypeAndConstructor => {
                    Warning::UnusedType {
                        name,
                        imported: true,
                        location,
                    }
                }
                EntityKind::ImportedConstructor => Warning::UnusedConstructor {
                    name,
                    imported: true,
                    location,
                },
                EntityKind::PrivateConstant => {
                    Warning::UnusedPrivateModuleConstant { name, location }
                }
                EntityKind::PrivateTypeConstructor(_) => Warning::UnusedConstructor {
                    name,
                    imported: false,
                    location,
                },
                EntityKind::PrivateFunction => Warning::UnusedPrivateFunction { name, location },
                EntityKind::PrivateType => Warning::UnusedType {
                    name,
                    imported: false,
                    location,
                },
                EntityKind::ImportedValue => Warning::UnusedImportedValue { name, location },
                EntityKind::Variable => Warning::UnusedVariable { name, location },
            };

            self.warnings.emit(warning);
        }
    }

    pub fn local_value_names(&self) -> Vec<SmolStr> {
        self.scope
            .keys()
            .filter(|&t| PIPE_VARIABLE != t)
            .cloned()
            .collect()
    }

    /// Checks that the given patterns are exhaustive for given type.
    /// Currently only performs exhaustiveness checking for custom types,
    /// only at the top level (without recursing into constructor arguments).
    pub fn check_exhaustiveness(
        &mut self,
        patterns: Vec<Pattern<Arc<Type>>>,
        value_typ: Arc<Type>,
    ) -> Result<(), Vec<SmolStr>> {
        match &*value_typ {
            Type::Named {
                name: type_name,
                module: module_name,
                ..
            } => {
                let m = if module_name.is_empty() || module_name == self.current_module {
                    None
                } else {
                    Some(module_name.as_str())
                };

                if let Ok(constructors) = self.get_constructors_for_type(m, type_name) {
                    let mut unmatched_constructors: HashSet<SmolStr> =
                        constructors.iter().cloned().collect();

                    for p in &patterns {
                        // ignore Assign patterns
                        let mut pattern = p;
                        while let Pattern::Assign {
                            pattern: assign_pattern,
                            ..
                        } = pattern
                        {
                            pattern = assign_pattern;
                        }

                        match pattern {
                            // If the pattern is a Discard or Var, all constructors are covered by it
                            Pattern::Discard { .. } => return Ok(()),
                            Pattern::Var { .. } => return Ok(()),

                            // If the pattern is a constructor, remove it from unmatched patterns
                            Pattern::Constructor {
                                constructor:
                                    Inferred::Known(PatternConstructor::Record { name, .. }),
                                ..
                            } => {
                                let _ = unmatched_constructors.remove(name);
                            }

                            _ => return Ok(()),
                        }
                    }

                    if !unmatched_constructors.is_empty() {
                        return Err(unmatched_constructors.into_iter().sorted().collect());
                    }
                }
                Ok(())
            }
            _ => Ok(()),
        }
    }
}

/// Unify two types that should be the same.
/// Any unbound type variables will be linked to the other type as they are the same.
///
/// It two types are found to not be the same an error is returned.
///
pub fn unify(t1: Arc<Type>, t2: Arc<Type>) -> Result<(), UnifyError> {
    if t1 == t2 {
        return Ok(());
    }

    // Collapse right hand side type links. Left hand side will be collapsed in the next block.
    if let Type::Var { type_: typ } = t2.deref() {
        if let TypeVar::Link { type_: typ } = typ.borrow().deref() {
            return unify(t1, typ.clone());
        }
    }

    if let Type::Var { type_: typ } = t1.deref() {
        enum Action {
            Unify(Arc<Type>),
            CouldNotUnify,
            Link,
        }

        let action = match typ.borrow().deref() {
            TypeVar::Link { type_: typ } => Action::Unify(typ.clone()),

            TypeVar::Unbound { id } => {
                unify_unbound_type(t2.clone(), *id)?;
                Action::Link
            }

            TypeVar::Generic { id } => {
                if let Type::Var { type_: typ } = t2.deref() {
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
                *typ.borrow_mut() = TypeVar::Link { type_: t2 };
                Ok(())
            }

            Action::Unify(t) => unify(t, t2),

            Action::CouldNotUnify => Err(UnifyError::CouldNotUnify {
                expected: t1.clone(),
                given: t2,
                situation: None,
            }),
        };
    }

    if let Type::Var { .. } = t2.deref() {
        return unify(t2, t1).map_err(flip_unify_error);
    }

    match (t1.deref(), t2.deref()) {
        (
            Type::Named {
                module: m1,
                name: n1,
                args: args1,
                ..
            },
            Type::Named {
                module: m2,
                name: n2,
                args: args2,
                ..
            },
        ) if m1 == m2 && n1 == n2 && args1.len() == args2.len() => {
            for (a, b) in args1.iter().zip(args2) {
                unify_enclosed_type(t1.clone(), t2.clone(), unify(a.clone(), b.clone()))?;
            }
            Ok(())
        }

        (Type::Tuple { elems: elems1, .. }, Type::Tuple { elems: elems2, .. })
            if elems1.len() == elems2.len() =>
        {
            for (a, b) in elems1.iter().zip(elems2) {
                unify_enclosed_type(t1.clone(), t2.clone(), unify(a.clone(), b.clone()))?;
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
                unify(a.clone(), b.clone()).map_err(|_| UnifyError::CouldNotUnify {
                    expected: t1.clone(),
                    given: t2.clone(),
                    situation: None,
                })?;
            }
            unify(retrn1.clone(), retrn2.clone()).map_err(|_| UnifyError::CouldNotUnify {
                expected: t1.clone(),
                given: t2.clone(),
                situation: None,
            })
        }

        _ => Err(UnifyError::CouldNotUnify {
            expected: t1.clone(),
            given: t2.clone(),
            situation: None,
        }),
    }
}
