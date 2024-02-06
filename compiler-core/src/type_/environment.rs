use crate::{
    analyse::TargetSupport, ast::PIPE_VARIABLE, build::Target, uid::UniqueIdGenerator,
    warning::TypeWarningEmitter,
};

use super::*;
use std::collections::HashMap;

#[derive(Debug)]
pub struct Environment<'a> {
    pub current_package: EcoString,
    pub current_module: EcoString,
    pub target: Target,
    pub ids: UniqueIdGenerator,
    previous_id: u64,
    /// Names of types or values that have been imported an unqualified fashion
    /// from other modules. Used to prevent multiple imports using the same name.
    pub unqualified_imported_names: HashMap<EcoString, SrcSpan>,
    pub unqualified_imported_types: HashMap<EcoString, SrcSpan>,
    pub importable_modules: &'a im::HashMap<EcoString, ModuleInterface>,

    /// Modules that have been imported by the current module, along with the
    /// location of the import statement where they were imported.
    pub imported_modules: HashMap<EcoString, (SrcSpan, &'a ModuleInterface)>,
    pub unused_modules: HashMap<EcoString, SrcSpan>,

    /// Names of modules that have been imported with as name.
    pub imported_module_aliases: HashMap<EcoString, SrcSpan>,
    pub unused_module_aliases: HashMap<EcoString, UnusedModuleAlias>,

    /// Values defined in the current function (or the prelude)
    pub scope: im::HashMap<EcoString, ValueConstructor>,

    /// Types defined in the current module (or the prelude)
    pub module_types: HashMap<EcoString, TypeConstructor>,

    /// Mapping from types to constructor names in the current module (or the prelude)
    pub module_types_constructors: HashMap<EcoString, TypeVariantConstructors>,

    /// Values defined in the current module (or the prelude)
    pub module_values: HashMap<EcoString, ValueConstructor>,

    /// Accessors defined in the current module
    pub accessors: HashMap<EcoString, AccessorsMap>,

    /// Warnings
    pub warnings: &'a TypeWarningEmitter,

    /// entity_usages is a stack of scopes. When an entity is created it is
    /// added to the top scope. When an entity is used we crawl down the scope
    /// stack for an entity with that name and mark it as used.
    /// NOTE: The bool in the tuple here tracks if the entity has been used
    pub entity_usages: Vec<HashMap<EcoString, (EntityKind, SrcSpan, bool)>>,

    /// Used to determine if all functions/constants need to support the current
    /// compilation target.
    pub target_support: TargetSupport,

    /// Whether a `todo` expression has been encountered in this module.
    /// This is used by the build tool to refuse to publish packages that are unfinished.
    pub todo_encountered: bool,
}

impl<'a> Environment<'a> {
    pub fn new(
        ids: UniqueIdGenerator,
        current_package: EcoString,
        current_module: EcoString,
        target: Target,
        importable_modules: &'a im::HashMap<EcoString, ModuleInterface>,
        warnings: &'a TypeWarningEmitter,
        target_support: TargetSupport,
    ) -> Self {
        let prelude = importable_modules
            .get(PRELUDE_MODULE_NAME)
            .expect("Unable to find prelude in importable modules");
        Self {
            current_package: current_package.clone(),
            previous_id: ids.next(),
            ids,
            target,
            module_types: prelude.types.clone(),
            module_types_constructors: prelude.types_value_constructors.clone(),
            module_values: HashMap::new(),
            imported_modules: HashMap::new(),
            unused_modules: HashMap::new(),
            unqualified_imported_names: HashMap::new(),
            unqualified_imported_types: HashMap::new(),
            accessors: prelude.accessors.clone(),
            scope: prelude.values.clone().into(),
            importable_modules,
            imported_module_aliases: HashMap::new(),
            unused_module_aliases: HashMap::new(),
            current_module,
            warnings,
            entity_usages: vec![HashMap::new()],
            target_support,
            todo_encountered: false,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct UnusedModuleAlias {
    pub location: SrcSpan,
    pub module_name: EcoString,
}

/// For Keeping track of entity usages and knowing which error to display.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum EntityKind {
    PrivateConstant,
    // String here is the type constructor's type name
    PrivateTypeConstructor(EcoString),
    PrivateFunction,
    ImportedConstructor,
    ImportedType,
    ImportedValue,
    PrivateType,
    Variable,
}

#[derive(Debug)]
pub struct ScopeResetData {
    local_values: im::HashMap<EcoString, ValueConstructor>,
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
        // been used beyond the point where the error occurred, so we don't want
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
    pub fn insert_local_variable(&mut self, name: EcoString, location: SrcSpan, typ: Arc<Type>) {
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
    pub fn insert_local_constant(
        &mut self,
        name: EcoString,
        literal: Constant<Arc<Type>, EcoString>,
    ) {
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
        name: EcoString,
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
    pub fn insert_module_value(&mut self, name: EcoString, value: ValueConstructor) {
        let _ = self.module_values.insert(name, value);
    }

    /// Lookup a variable in the current scope.
    ///
    pub fn get_variable(&self, name: &EcoString) -> Option<&ValueConstructor> {
        self.scope.get(name)
    }

    /// Lookup a module constant in the current scope.
    ///
    pub fn get_module_const(&mut self, name: &EcoString) -> Option<&ValueConstructor> {
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
        type_name: EcoString,
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
    pub fn insert_type_to_constructors(
        &mut self,
        type_name: EcoString,
        constructors: TypeVariantConstructors,
    ) {
        let _ = self
            .module_types_constructors
            .insert(type_name, constructors);
    }

    /// Lookup a type in the current scope.
    ///
    pub fn get_type_constructor(
        &mut self,
        module_alias: &Option<EcoString>,
        name: &EcoString,
    ) -> Result<&TypeConstructor, UnknownTypeConstructorError> {
        let t = match module_alias {
            None => self
                .module_types
                .get(name)
                .ok_or_else(|| UnknownTypeConstructorError::Type {
                    name: name.clone(),
                    hint: self.unknown_type_hint(name),
                }),

            Some(module_name) => {
                let (_, module) = self.imported_modules.get(module_name).ok_or_else(|| {
                    UnknownTypeConstructorError::Module {
                        name: module_name.clone(),
                        imported_modules: self.importable_modules.keys().cloned().collect(),
                    }
                })?;
                let _ = self.unused_modules.remove(module_name);
                let _ = self.unused_module_aliases.remove(module_name);
                module
                    .types
                    .get(name)
                    .ok_or_else(|| UnknownTypeConstructorError::ModuleType {
                        name: name.clone(),
                        module_name: module.name.clone(),
                        type_constructors: module.public_type_names(),
                    })
            }
        }?;

        Ok(t)
    }

    fn unknown_type_hint(&self, type_name: &EcoString) -> UnknownTypeHint {
        match self.scope.contains_key(type_name) {
            true => UnknownTypeHint::ValueInScopeWithSameName,
            false => UnknownTypeHint::AlternativeTypes(self.module_types.keys().cloned().collect()),
        }
    }

    /// Lookup constructors for type in the current scope.
    ///
    pub fn get_constructors_for_type(
        &self,
        module: &EcoString,
        name: &EcoString,
    ) -> Result<&TypeVariantConstructors, UnknownTypeConstructorError> {
        let module = if module.is_empty() || *module == self.current_module {
            None
        } else {
            Some(module)
        };
        match module {
            None => self.module_types_constructors.get(name).ok_or_else(|| {
                UnknownTypeConstructorError::Type {
                    name: name.clone(),
                    hint: self.unknown_type_hint(name),
                }
            }),

            Some(m) => {
                let module = self.importable_modules.get(m).ok_or_else(|| {
                    UnknownTypeConstructorError::Module {
                        name: name.clone(),
                        imported_modules: self.importable_modules.keys().cloned().collect(),
                    }
                })?;
                module.types_value_constructors.get(name).ok_or_else(|| {
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
        module: Option<&EcoString>,
        name: &EcoString,
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

    pub fn insert_accessors(&mut self, type_name: EcoString, accessors: AccessorsMap) {
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
                package,
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
                    package: package.clone(),
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
    pub fn init_usage(&mut self, name: EcoString, kind: EntityKind, location: SrcSpan) {
        use EntityKind::*;

        match self
            .entity_usages
            .last_mut()
            .expect("Attempted to access non-existent entity usages scope")
            .insert(name.clone(), (kind, location, false))
        {
            // Private types can be shadowed by a constructor with the same name
            //
            // TODO: Improve this so that we can tell if an imported overriden
            // type is actually used or not by tracking whether usages apply to
            // the value or type scope
            Some((ImportedType | PrivateType, _, _)) => {}

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
    pub fn increment_usage(&mut self, name: &EcoString) {
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
                _ => break,
            }
        }
    }

    /// Converts entities with a usage count of 0 to warnings.
    /// Returns the list of unused imported module location for the removed unused lsp action.
    pub fn convert_unused_to_warnings(&mut self) -> Vec<SrcSpan> {
        let unused = self
            .entity_usages
            .pop()
            .expect("Expected a bottom level of entity usages.");
        self.handle_unused(unused);

        let mut locations = Vec::new();
        for (name, location) in self.unused_modules.clone().into_iter() {
            self.warnings.emit(Warning::UnusedImportedModule {
                name: name.clone(),
                location,
            });
            locations.push(location);
        }

        for (name, info) in self.unused_module_aliases.iter() {
            if self.unused_modules.get(name).is_none() {
                self.warnings.emit(Warning::UnusedImportedModuleAlias {
                    alias: name.clone(),
                    location: info.location,
                    module_name: info.module_name.clone(),
                });
                locations.push(info.location);
            }
        }
        locations
    }

    fn handle_unused(&mut self, unused: HashMap<EcoString, (EntityKind, SrcSpan, bool)>) {
        for (name, (kind, location, _)) in unused.into_iter().filter(|(_, (_, _, used))| !used) {
            let warning = match kind {
                EntityKind::ImportedType => Warning::UnusedType {
                    name,
                    imported: true,
                    location,
                },
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

    pub fn local_value_names(&self) -> Vec<EcoString> {
        self.scope
            .keys()
            .filter(|&t| PIPE_VARIABLE != t)
            .cloned()
            .collect()
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
