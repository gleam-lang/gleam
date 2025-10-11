use pubgrub::Range;

use crate::{
    analyse::TargetSupport,
    ast::{PIPE_VARIABLE, Publicity},
    build::Target,
    error::edit_distance,
    reference::{EntityKind, ReferenceTracker},
    uid::UniqueIdGenerator,
};

use super::*;
use std::collections::HashMap;

#[derive(Debug)]
pub struct EnvironmentArguments<'a> {
    pub ids: UniqueIdGenerator,
    pub current_package: EcoString,
    pub gleam_version: Option<Range<Version>>,
    pub current_module: EcoString,
    pub target: Target,
    pub importable_modules: &'a im::HashMap<EcoString, ModuleInterface>,
    pub target_support: TargetSupport,
    pub current_origin: Origin,
    pub dev_dependencies: &'a HashSet<EcoString>,
}

impl<'a> EnvironmentArguments<'a> {
    pub fn build(self) -> Environment<'a> {
        Environment::new(self)
    }
}

#[derive(Debug)]
pub struct Environment<'a> {
    pub current_package: EcoString,
    pub origin: Origin,

    /// The gleam version range required by the current package as stated in its
    /// gleam.toml
    pub gleam_version: Option<Range<Version>>,

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

    /// Values defined in the current function (or the prelude)
    pub scope: im::HashMap<EcoString, ValueConstructor>,

    // The names of all the ignored variables and arguments in scope:
    // `let _var = 10` `pub fn main(_var) { todo }`.
    pub discarded_names: im::HashMap<EcoString, SrcSpan>,

    /// Types defined in the current module (or the prelude)
    pub module_types: HashMap<EcoString, TypeConstructor>,

    /// Mapping from types to constructor names in the current module (or the prelude)
    pub module_types_constructors: HashMap<EcoString, TypeVariantConstructors>,

    pub module_type_aliases: HashMap<EcoString, TypeAliasConstructor>,

    /// Values defined in the current module (or the prelude)
    pub module_values: HashMap<EcoString, ValueConstructor>,

    /// Accessors defined in the current module
    pub accessors: HashMap<EcoString, AccessorsMap>,

    /// local_variable_usages is a stack of scopes. When a local variable is created it is
    /// added to the top scope. When a local variable is used we crawl down the scope
    /// stack for a variable with that name and mark it as used.
    pub local_variable_usages: Vec<HashMap<EcoString, VariableUsage>>,

    /// Used to determine if all functions/constants need to support the current
    /// compilation target.
    pub target_support: TargetSupport,

    pub names: Names,

    /// Wether we ran into an `echo` or not while analysing the current module.
    pub echo_found: bool,

    pub references: ReferenceTracker,

    pub dev_dependencies: &'a HashSet<EcoString>,
}

#[derive(Debug)]
pub struct VariableUsage {
    origin: VariableOrigin,
    location: SrcSpan,
    usages: usize,
    recursive_usages: usize,
}

impl<'a> Environment<'a> {
    pub fn new(
        EnvironmentArguments {
            ids,
            current_package,
            gleam_version,
            current_module,
            target,
            importable_modules,
            target_support,
            current_origin: origin,
            dev_dependencies,
        }: EnvironmentArguments<'a>,
    ) -> Self {
        let prelude = importable_modules
            .get(PRELUDE_MODULE_NAME)
            .expect("Unable to find prelude in importable modules");

        let names = Self::build_names(prelude, importable_modules);

        Self {
            current_package,
            gleam_version,
            previous_id: ids.next(),
            ids,
            origin,
            target,
            module_types: prelude.types.clone(),
            module_types_constructors: prelude.types_value_constructors.clone(),
            module_values: HashMap::new(),
            imported_modules: HashMap::new(),
            unqualified_imported_names: HashMap::new(),
            unqualified_imported_types: HashMap::new(),
            accessors: prelude.accessors.clone(),
            scope: prelude.values.clone().into(),
            discarded_names: im::HashMap::new(),
            importable_modules,
            current_module,
            local_variable_usages: vec![HashMap::new()],
            target_support,
            names,
            module_type_aliases: HashMap::new(),
            echo_found: false,
            references: ReferenceTracker::new(),
            dev_dependencies,
        }
    }

    fn build_names(
        prelude: &ModuleInterface,
        importable_modules: &im::HashMap<EcoString, ModuleInterface>,
    ) -> Names {
        let mut names = Names::new();

        // Insert prelude types and values into scope
        for name in prelude.values.keys() {
            names.named_constructor_in_scope(
                PRELUDE_MODULE_NAME.into(),
                name.clone(),
                name.clone(),
            );
        }
        for name in prelude.types.keys() {
            names.named_type_in_scope(PRELUDE_MODULE_NAME.into(), name.clone(), name.clone());
        }

        // Find potential type aliases which reexport internal types
        for module in importable_modules.values() {
            // Internal modules are not part of the public API so they are also
            // not considered.
            if module.is_internal {
                continue;
            }
            for (alias_name, alias) in module.type_aliases.iter() {
                // An alias can only be a public reexport if it is public.
                if alias.publicity.is_public() {
                    names.maybe_register_reexport_alias(&module.package, alias_name, alias);
                }
            }
        }

        names
    }
}

#[derive(Debug)]
pub struct ScopeResetData {
    local_values: im::HashMap<EcoString, ValueConstructor>,
    discarded_names: im::HashMap<EcoString, SrcSpan>,
}

impl Environment<'_> {
    pub fn in_new_scope<T, E>(
        &mut self,
        problems: &mut Problems,
        process_scope: impl FnOnce(&mut Self, &mut Problems) -> Result<T, E>,
    ) -> Result<T, E> {
        // Record initial scope state
        let initial = self.open_new_scope();

        // Process scope
        let result = process_scope(self, problems);

        self.close_scope(initial, result.is_ok(), problems);

        // Return result of typing the scope
        result
    }

    pub fn open_new_scope(&mut self) -> ScopeResetData {
        let local_values = self.scope.clone();
        let discarded_names = self.discarded_names.clone();
        self.local_variable_usages.push(HashMap::new());
        ScopeResetData {
            local_values,
            discarded_names,
        }
    }

    pub fn close_scope(
        &mut self,
        data: ScopeResetData,
        was_successful: bool,
        problems: &mut Problems,
    ) {
        let ScopeResetData {
            local_values,
            discarded_names,
        } = data;

        let unused = self
            .local_variable_usages
            .pop()
            .expect("There was no top entity scope.");

        // We only check for unused entities if the scope was successfully
        // processed. If it was not then any seemingly unused entities may have
        // been used beyond the point where the error occurred, so we don't want
        // to incorrectly warn about them.
        if was_successful {
            self.handle_unused_variables(unused, problems);
        }
        self.scope = local_values;
        self.discarded_names = discarded_names;
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
    pub fn insert_local_variable(
        &mut self,
        name: EcoString,
        location: SrcSpan,
        origin: VariableOrigin,
        type_: Arc<Type>,
    ) {
        let _ = self.scope.insert(
            name,
            ValueConstructor::local_variable(location, origin, type_),
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
                publicity: Publicity::Private,
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
        type_: Arc<Type>,
        publicity: Publicity,
        deprecation: Deprecation,
    ) {
        let _ = self.scope.insert(
            name,
            ValueConstructor {
                publicity,
                deprecation,
                variant,
                type_,
            },
        );
    }

    /// Insert (or overwrites) a value into the current module.
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

    /// Map a type alias in the current scope.
    /// Errors if the module already has a type with that name, unless the type is from the
    /// prelude.
    ///
    pub fn insert_type_alias(
        &mut self,
        type_name: EcoString,
        info: TypeAliasConstructor,
    ) -> Result<(), Error> {
        let name = type_name.clone();
        let location = info.origin;
        match self.module_type_aliases.insert(type_name, info) {
            None => Ok(()),
            Some(prelude_type) if is_prelude_module(&prelude_type.module) => Ok(()),
            Some(previous) => Err(Error::DuplicateTypeName {
                name,
                location,
                previous_location: previous.origin,
            }),
        }
    }

    pub fn assert_unique_type_name(
        &mut self,
        name: &EcoString,
        location: SrcSpan,
    ) -> Result<(), Error> {
        match self.module_types.get(name) {
            None => Ok(()),
            Some(prelude_type) if is_prelude_module(&prelude_type.module) => Ok(()),
            Some(previous) => Err(Error::DuplicateTypeName {
                name: name.clone(),
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
        module: &Option<(EcoString, SrcSpan)>,
        name: &EcoString,
    ) -> Result<&TypeConstructor, UnknownTypeConstructorError> {
        match module {
            None => self
                .module_types
                .get(name)
                .ok_or_else(|| UnknownTypeConstructorError::Type {
                    name: name.clone(),
                    hint: self.unknown_type_hint(name),
                }),

            Some((module_name, _)) => {
                let (_, module) = self.imported_modules.get(module_name).ok_or_else(|| {
                    UnknownTypeConstructorError::Module {
                        name: module_name.clone(),
                        suggestions: self
                            .suggest_modules(module_name, Imported::Type(name.clone())),
                    }
                })?;
                self.references
                    .register_module_reference(module_name.clone());
                module.get_public_type(name).ok_or_else(|| {
                    UnknownTypeConstructorError::ModuleType {
                        name: name.clone(),
                        module_name: module.name.clone(),
                        type_constructors: module.public_type_names(),
                        imported_type_as_value: false,
                    }
                })
            }
        }
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
                        suggestions: self.suggest_modules(m, Imported::Type(name.clone())),
                    }
                })?;
                module.types_value_constructors.get(name).ok_or_else(|| {
                    UnknownTypeConstructorError::ModuleType {
                        name: name.clone(),
                        module_name: module.name.clone(),
                        type_constructors: module.public_type_names(),
                        imported_type_as_value: false,
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
            None => self.scope.get(name).ok_or_else(|| {
                let type_with_name_in_scope = self.module_types.keys().any(|type_| type_ == name);
                UnknownValueConstructorError::Variable {
                    name: name.clone(),
                    variables: self.local_value_names(),
                    type_with_name_in_scope,
                }
            }),

            Some(module_name) => {
                let (_, module) = self.imported_modules.get(module_name).ok_or_else(|| {
                    UnknownValueConstructorError::Module {
                        name: module_name.clone(),
                        suggestions: self
                            .suggest_modules(module_name, Imported::Value(name.clone())),
                    }
                })?;
                self.references
                    .register_module_reference(module_name.clone());
                module.get_public_value(name).ok_or_else(|| {
                    UnknownValueConstructorError::ModuleValue {
                        name: name.clone(),
                        module_name: module.name.clone(),
                        value_constructors: module.public_value_names(),
                        imported_value_as_type: false,
                    }
                })
            }
        }
    }

    pub fn get_type_variants_fields(
        &self,
        module: &EcoString,
        name: &EcoString,
    ) -> Vec<&EcoString> {
        self.get_constructors_for_type(module, name)
            .iter()
            .flat_map(|c| &c.variants)
            .filter_map(|variant| {
                self.type_value_constructor_to_constructor(module, variant)?
                    .variant
                    .record_field_map()
            })
            .flat_map(|field_map| field_map.fields.keys())
            .collect_vec()
    }

    fn type_value_constructor_to_constructor(
        &self,
        module: &EcoString,
        variant: &TypeValueConstructor,
    ) -> Option<&ValueConstructor> {
        if *module == self.current_module {
            self.scope.get(&variant.name)
        } else {
            let (_, module) = self.imported_modules.get(module)?;
            module.get_public_value(&variant.name)
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
                publicity,
                name,
                package,
                module,
                arguments,
                inferred_variant,
            } => {
                let arguments = arguments
                    .iter()
                    .map(|type_| self.instantiate(type_.clone(), ids, hydrator))
                    .collect();
                Arc::new(Type::Named {
                    publicity: *publicity,
                    name: name.clone(),
                    package: package.clone(),
                    module: module.clone(),
                    arguments,
                    inferred_variant: *inferred_variant,
                })
            }

            Type::Var { type_ } => {
                match type_.borrow().deref() {
                    TypeVar::Link { type_ } => {
                        return self.instantiate(type_.clone(), ids, hydrator);
                    }

                    TypeVar::Unbound { .. } => {
                        return Arc::new(Type::Var {
                            type_: type_.clone(),
                        });
                    }

                    TypeVar::Generic { id } => match ids.get(id) {
                        Some(t) => return t.clone(),
                        None => {
                            if !hydrator.is_rigid(id) {
                                // Check this in the hydrator, i.e. is it a created type
                                let v = self.new_unbound_var();
                                let _ = ids.insert(*id, v.clone());

                                // Preserve any user-provided name from the original generic variable
                                // for the new unbound variable. This ensures error messages and type
                                // displays continue to use meaningful names (e.g., "something") rather
                                // than auto-generated ones (e.g., "a", "b").
                                let v_id = self.previous_uid();
                                self.names.reassign_type_variable_alias(*id, v_id);

                                return v;
                            }
                        }
                    },
                }
                Arc::new(Type::Var {
                    type_: type_.clone(),
                })
            }

            Type::Fn {
                arguments, return_, ..
            } => fn_(
                arguments
                    .iter()
                    .map(|type_| self.instantiate(type_.clone(), ids, hydrator))
                    .collect(),
                self.instantiate(return_.clone(), ids, hydrator),
            ),

            Type::Tuple { elements } => tuple(
                elements
                    .iter()
                    .map(|type_| self.instantiate(type_.clone(), ids, hydrator))
                    .collect(),
            ),
        }
    }

    /// Inserts a local variable at the current scope for usage tracking.
    pub fn init_usage(
        &mut self,
        name: EcoString,
        origin: VariableOrigin,
        location: SrcSpan,
        problems: &mut Problems,
    ) {
        if let Some(VariableUsage {
            origin,
            location,
            usages: 0,
            recursive_usages,
        }) = self
            .local_variable_usages
            .last_mut()
            .expect("Attempted to access non-existent entity usages scope")
            .insert(
                name.clone(),
                VariableUsage {
                    origin,
                    location,
                    usages: 0,
                    recursive_usages: 0,
                },
            )
        {
            // an entity was overwritten in the top most scope without being used
            let mut unused = HashMap::with_capacity(1);
            let _ = unused.insert(
                name,
                VariableUsage {
                    origin,
                    location,
                    usages: 0,
                    recursive_usages,
                },
            );
            self.handle_unused_variables(unused, problems);
        }
    }

    /// Increments an entity's usage in the current or nearest enclosing scope
    pub fn increment_usage(&mut self, name: &EcoString) {
        if let Some(VariableUsage { usages, .. }) = self
            .local_variable_usages
            .iter_mut()
            .rev()
            .find_map(|scope| scope.get_mut(name))
        {
            *usages += 1;
        }
    }

    /// Marks an argument as being passed recursively to a function call.
    pub fn increment_recursive_usage(&mut self, name: &EcoString) {
        if let Some(VariableUsage {
            recursive_usages, ..
        }) = self
            .local_variable_usages
            .iter_mut()
            .rev()
            .find_map(|scope| scope.get_mut(name))
        {
            *recursive_usages += 1;
        }
    }

    /// Emit warnings for unused definitions, imports, expressions, etc.
    ///
    /// Returns the source byte start positions of all unused definitions.
    ///
    pub fn handle_unused(&mut self, problems: &mut Problems) -> HashSet<u32> {
        let mut unused_positions = HashSet::new();
        let unused = self
            .local_variable_usages
            .pop()
            .expect("Expected a bottom level of entity usages.");
        self.handle_unused_variables(unused, problems);

        // We have to handle unused imported entites a bit differently when
        // emitting warning: when an import list is unused all its items and
        // the import itself are unused:
        //
        // ```
        // import wibble.{unused, also_unused}
        //        ^^^^^^  ^^^^^^  ^^^^^^^^^^^ Everything is unused here
        // ```
        //
        // But instead of emitting three warnings, what we really want is to
        // emit just a single warning encompassing the entire line! So we have
        // to hold on all unused imported entities and emit a warning for those
        // only if the module they come from is not also unused.
        let mut unused_modules = HashSet::new();
        let mut unused_imported_items = vec![];

        for (entity, info) in self.references.unused() {
            let name = entity.name;
            let location = info.origin;

            let warning = match info.kind {
                EntityKind::Function => {
                    let _ = unused_positions.insert(location.start);
                    Warning::UnusedPrivateFunction { location, name }
                }
                EntityKind::Constant => {
                    let _ = unused_positions.insert(location.start);
                    Warning::UnusedPrivateModuleConstant { location, name }
                }
                EntityKind::Constructor => Warning::UnusedConstructor {
                    location,
                    name,
                    imported: false,
                },
                EntityKind::Type => {
                    let _ = unused_positions.insert(location.start);
                    Warning::UnusedType {
                        name,
                        imported: false,
                        location,
                    }
                }
                EntityKind::ImportedModule { module_name } => {
                    let _ = unused_modules.insert(module_name.clone());
                    Warning::UnusedImportedModule { name, location }
                }
                EntityKind::ImportedType { module } => {
                    unused_imported_items.push((
                        module,
                        Warning::UnusedType {
                            name,
                            imported: true,
                            location,
                        },
                    ));
                    continue;
                }
                EntityKind::ImportedConstructor { module } => {
                    unused_imported_items.push((
                        module,
                        Warning::UnusedConstructor {
                            name,
                            imported: true,
                            location,
                        },
                    ));
                    continue;
                }
                EntityKind::ImportedValue { module } => {
                    unused_imported_items
                        .push((module, Warning::UnusedImportedValue { name, location }));
                    continue;
                }
                EntityKind::ModuleAlias { module } => {
                    unused_imported_items.push((
                        module.clone(),
                        Warning::UnusedImportedModuleAlias {
                            module_name: module.clone(),
                            alias: name,
                            location,
                        },
                    ));
                    continue;
                }
            };
            problems.warning(warning);
        }

        unused_imported_items
            .into_iter()
            .filter(|(module, _)| !unused_modules.contains(module))
            .for_each(|(_, warning)| problems.warning(warning));

        unused_positions
    }

    fn handle_unused_variables(
        &mut self,
        unused: HashMap<EcoString, VariableUsage>,
        problems: &mut Problems,
    ) {
        for VariableUsage {
            origin,
            location,
            usages,
            recursive_usages,
        } in unused.into_values()
        {
            if usages == 0 {
                problems.warning(Warning::UnusedVariable { location, origin });
            }
            // If the function parameter is actually used somewhere, but all the
            // usages are just passing it along in a recursive call, then it
            // counts as being unused too.
            else if origin.is_function_parameter() && recursive_usages == usages {
                problems.warning(Warning::UnusedRecursiveArgument { location });
            }
        }
    }

    pub fn local_value_names(&self) -> Vec<EcoString> {
        self.scope
            .keys()
            .filter(|&t| PIPE_VARIABLE != t)
            .cloned()
            .collect()
    }

    /// Suggest modules to import or use, for an unknown module
    pub fn suggest_modules(&self, module: &str, imported: Imported) -> Vec<ModuleSuggestion> {
        let mut suggestions = self
            .importable_modules
            .iter()
            .filter_map(|(importable, module_info)| {
                if module_info.is_internal && module_info.package != self.current_package {
                    return None;
                }

                match &imported {
                    // Don't suggest importing modules if they are already imported
                    _ if self
                        .imported_modules
                        .contains_key(importable.split('/').next_back().unwrap_or(importable)) =>
                    {
                        None
                    }
                    Imported::Type(name) if module_info.get_public_type(name).is_some() => {
                        Some(ModuleSuggestion::Importable(importable.clone()))
                    }
                    Imported::Value(name) if module_info.get_public_value(name).is_some() => {
                        Some(ModuleSuggestion::Importable(importable.clone()))
                    }
                    _ => None,
                }
            })
            .collect_vec();

        suggestions.extend(
            self.imported_modules
                .keys()
                .map(|module| ModuleSuggestion::Imported(module.clone())),
        );

        let threshold = std::cmp::max(module.chars().count() / 3, 1);

        // Filter and sort options based on edit distance.
        suggestions
            .into_iter()
            .sorted()
            .filter_map(|suggestion| {
                edit_distance(module, suggestion.last_name_component(), threshold)
                    .map(|distance| (suggestion, distance))
            })
            .sorted_by_key(|&(_, distance)| distance)
            .map(|(suggestion, _)| suggestion)
            .collect()
    }

    pub fn type_variant_name(
        &self,
        type_module: &EcoString,
        type_name: &EcoString,
        variant_index: u16,
    ) -> Option<&EcoString> {
        let type_constructors = if type_module == &self.current_module {
            &self.module_types_constructors
        } else {
            &self
                .importable_modules
                .get(type_module)?
                .types_value_constructors
        };

        type_constructors
            .get(type_name)
            .and_then(|type_constructors| type_constructors.variants.get(variant_index as usize))
            .map(|variant| &variant.name)
    }
}

#[derive(Debug)]
/// An imported name, for looking up a module which exports it
pub enum Imported {
    /// An imported module, with no extra information
    Module,
    /// An imported type
    Type(EcoString),
    /// An imported value
    Value(EcoString),
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
    if let Type::Var { type_ } = t2.deref()
        && let TypeVar::Link { type_ } = type_.borrow().deref()
    {
        return unify(t1, type_.clone());
    }

    if let Type::Var { type_ } = t1.deref() {
        enum Action {
            Unify(Arc<Type>),
            CouldNotUnify,
            Link,
        }

        let action = match type_.borrow().deref() {
            TypeVar::Link { type_ } => Action::Unify(type_.clone()),

            TypeVar::Unbound { id } => {
                unify_unbound_type(t2.clone(), *id)?;
                Action::Link
            }

            TypeVar::Generic { id } => {
                if let Type::Var { type_ } = t2.deref()
                    && type_.borrow().is_unbound()
                {
                    *type_.borrow_mut() = TypeVar::Generic { id: *id };
                    return Ok(());
                }
                Action::CouldNotUnify
            }
        };

        return match action {
            Action::Link => {
                let mut t2 = t2.deref().clone();
                t2.generalise_custom_type_variant();
                *type_.borrow_mut() = TypeVar::Link {
                    type_: Arc::new(t2),
                };
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
                arguments: arguments1,
                ..
            },
            Type::Named {
                module: m2,
                name: n2,
                arguments: arguments2,
                ..
            },
        ) if m1 == m2 && n1 == n2 && arguments1.len() == arguments2.len() => {
            for (a, b) in arguments1.iter().zip(arguments2) {
                unify_enclosed_type(t1.clone(), t2.clone(), unify(a.clone(), b.clone()))?;
            }
            Ok(())
        }

        (
            Type::Tuple {
                elements: elements1,
                ..
            },
            Type::Tuple {
                elements: elements2,
                ..
            },
        ) if elements1.len() == elements2.len() => {
            for (a, b) in elements1.iter().zip(elements2) {
                unify_enclosed_type(t1.clone(), t2.clone(), unify(a.clone(), b.clone()))?;
            }
            Ok(())
        }

        (
            Type::Fn {
                arguments: arguments1,
                return_: return1,
                ..
            },
            Type::Fn {
                arguments: arguments2,
                return_: return2,
                ..
            },
        ) => {
            if arguments1.len() != arguments2.len() {
                Err(unify_wrong_arity(
                    &t1,
                    arguments1.len(),
                    &t2,
                    arguments2.len(),
                ))?
            }

            for (i, (a, b)) in arguments1.iter().zip(arguments2).enumerate() {
                unify(a.clone(), b.clone())
                    .map_err(|_| unify_wrong_arguments(&t1, a, &t2, b, i))?;
            }

            unify(return1.clone(), return2.clone())
                .map_err(|_| unify_wrong_returns(&t1, return1, &t2, return2))
        }

        _ => Err(UnifyError::CouldNotUnify {
            expected: t1.clone(),
            given: t2.clone(),
            situation: None,
        }),
    }
}
