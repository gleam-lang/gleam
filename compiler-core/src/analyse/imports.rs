use ecow::EcoString;

use crate::{
    ast::{SrcSpan, UnqualifiedImport, UntypedImport},
    build::Origin,
    type_::{
        EntityKind, Environment, Error, ModuleInterface, Problems, UnusedModuleAlias,
        ValueConstructorVariant,
    },
};

use super::Imported;

#[derive(Debug)]
pub struct Importer<'context, 'problems> {
    origin: Origin,
    environment: Environment<'context>,
    problems: &'problems mut Problems,
}

impl<'context, 'problems> Importer<'context, 'problems> {
    pub fn new(
        origin: Origin,
        environment: Environment<'context>,
        problems: &'problems mut Problems,
    ) -> Self {
        Self {
            origin,
            environment,
            problems,
        }
    }

    pub fn run<'code>(
        origin: Origin,
        env: Environment<'context>,
        imports: &'code [UntypedImport],
        problems: &'problems mut Problems,
    ) -> Environment<'context> {
        let mut importer = Self::new(origin, env, problems);
        for import in imports {
            importer.register_import(import)
        }
        importer.environment
    }

    fn register_import(&mut self, import: &UntypedImport) {
        let location = import.location;
        let name = import.module.clone();

        // Find imported module
        let Some(module_info) = self.environment.importable_modules.get(&name) else {
            self.problems.error(Error::UnknownModule {
                location,
                name: name.clone(),
                suggestions: self.environment.suggest_modules(&name, Imported::Module),
            });
            return;
        };

        if let Err(e) = self.check_src_does_not_import_test(module_info, location, name.clone()) {
            self.problems.error(e);
            return;
        }

        if let Err(e) = self.register_module(import, module_info) {
            self.problems.error(e);
            return;
        }

        // Insert unqualified imports into scope
        for type_ in &import.unqualified_types {
            self.register_unqualified_type(type_, module_info);
        }
        for value in &import.unqualified_values {
            self.register_unqualified_value(value, module_info);
        }
    }

    fn register_unqualified_type(&mut self, import: &UnqualifiedImport, module: &ModuleInterface) {
        let imported_name = import.as_name.as_ref().unwrap_or(&import.name);

        // Register the unqualified import if it is a type constructor
        let Some(type_info) = module.get_public_type(&import.name) else {
            // TODO: refine to a type specific error
            self.problems.error(Error::UnknownModuleType {
                location: import.location,
                name: import.name.clone(),
                module_name: module.name.clone(),
                type_constructors: module.public_type_names(),
                value_with_same_name: module.get_public_value(&import.name).is_some(),
            });
            return;
        };

        let type_info = type_info.clone().with_location(import.location);

        self.environment.names.type_in_scope(
            imported_name.clone(),
            type_info.type_.as_ref(),
            &type_info.parameters,
        );

        if let Err(e) = self
            .environment
            .insert_type_constructor(imported_name.clone(), type_info)
        {
            self.problems.error(e);
            return;
        }

        self.environment.init_usage(
            imported_name.clone(),
            EntityKind::ImportedType,
            import.location,
            self.problems,
        );
    }

    fn register_unqualified_value(&mut self, import: &UnqualifiedImport, module: &ModuleInterface) {
        let import_name = &import.name;
        let location = import.location;
        let used_name = import.as_name.as_ref().unwrap_or(&import.name);

        // Register the unqualified import if it is a value
        let variant = match module.get_public_value(import_name) {
            Some(value) => {
                let implementations = value.variant.implementations();
                // Check the target support of the imported value
                if self.environment.target_support.is_enforced()
                    && !implementations.supports(self.environment.target)
                {
                    self.problems.error(Error::UnsupportedExpressionTarget {
                        target: self.environment.target,
                        location,
                    })
                }

                self.environment.insert_variable(
                    used_name.clone(),
                    value.variant.clone(),
                    value.type_.clone(),
                    value.publicity,
                    value.deprecation.clone(),
                );
                &value.variant
            }
            None => {
                self.problems.error(Error::UnknownModuleValue {
                    location,
                    name: import_name.clone(),
                    module_name: module.name.clone(),
                    value_constructors: module.public_value_names(),
                    type_with_same_name: module.get_public_type(import_name).is_some(),
                });
                return;
            }
        };

        match variant {
            ValueConstructorVariant::Record { name, module, .. } => {
                self.environment.init_usage(
                    used_name.clone(),
                    EntityKind::ImportedConstructor,
                    location,
                    self.problems,
                );
                self.environment.names.named_constructor_in_scope(
                    module.clone(),
                    name.clone(),
                    used_name.clone(),
                );
            }
            _ => self.environment.init_usage(
                used_name.clone(),
                EntityKind::ImportedValue,
                location,
                self.problems,
            ),
        };

        // Check if value already was imported
        if let Some(previous) = self.environment.unqualified_imported_names.get(used_name) {
            self.problems.error(Error::DuplicateImport {
                location,
                previous_location: *previous,
                name: import_name.clone(),
            });
            return;
        }

        // Register the name as imported so it can't be imported a
        // second time in future
        let _ = self
            .environment
            .unqualified_imported_names
            .insert(used_name.clone(), location);
    }

    fn check_src_does_not_import_test(
        &mut self,
        module_info: &ModuleInterface,
        location: SrcSpan,
        imported_module: EcoString,
    ) -> Result<(), Error> {
        if self.origin.is_src() && !module_info.origin.is_src() {
            return Err(Error::SrcImportingTest {
                location,
                src_module: self.environment.current_module.clone(),
                test_module: imported_module,
            });
        }
        Ok(())
    }

    fn register_module(
        &mut self,
        import: &UntypedImport,
        import_info: &'context ModuleInterface,
    ) -> Result<(), Error> {
        if let Some(used_name) = import.used_name() {
            self.check_not_a_duplicate_import(&used_name, import.location)?;

            if import.unqualified_types.is_empty() && import.unqualified_values.is_empty() {
                // When the module has no unqualified imports, we track its usage
                // so we can warn if not used by the end of the type checking
                let _ = self
                    .environment
                    .unused_modules
                    .insert(used_name.clone(), import.location);
            }

            if let Some(alias_location) = import.alias_location() {
                // We also register it's name to differentiate between unused module
                // and unused module name. See 'convert_unused_to_warnings'.
                let _ = self
                    .environment
                    .imported_module_aliases
                    .insert(used_name.clone(), alias_location);

                let _ = self.environment.unused_module_aliases.insert(
                    used_name.clone(),
                    UnusedModuleAlias {
                        location: alias_location,
                        module_name: import.module.clone(),
                    },
                );
            }

            // Insert imported module into scope
            let _ = self
                .environment
                .imported_modules
                .insert(used_name.clone(), (import.location, import_info));

            self.environment
                .names
                .imported_module(import.module.clone(), used_name);
        };

        Ok(())
    }

    fn check_not_a_duplicate_import(
        &self,
        used_name: &EcoString,
        location: SrcSpan,
    ) -> Result<(), Error> {
        // Check if a module was already imported with this name
        if let Some((previous_location, _)) = self.environment.imported_modules.get(used_name) {
            return Err(Error::DuplicateImport {
                location,
                previous_location: *previous_location,
                name: used_name.clone(),
            });
        }
        Ok(())
    }
}
