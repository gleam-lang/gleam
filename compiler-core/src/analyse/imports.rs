use ecow::EcoString;

use crate::{
    ast::{Import, SrcSpan, UnqualifiedImport},
    build::Origin,
    type_::{
        EntityKind, Environment, Error, ModuleInterface, UnusedModuleAlias, ValueConstructorVariant,
    },
};

#[derive(Debug)]
pub struct Importer<'a> {
    origin: Origin,
    environment: Environment<'a>,
}

impl<'a> Importer<'a> {
    pub fn new(origin: Origin, environment: Environment<'a>) -> Self {
        Self {
            origin,
            environment,
        }
    }

    pub fn run<'b>(
        origin: Origin,
        env: Environment<'a>,
        imports: &'b [Import<()>],
    ) -> Result<Environment<'a>, Error> {
        let mut importer = Self::new(origin, env);
        for import in imports {
            importer.register_import(import)?;
        }
        Ok(importer.environment)
    }

    fn register_import(&mut self, import: &Import<()>) -> Result<(), Error> {
        let location = import.location;
        let imported_module_name = import.module.clone();

        // Find imported module
        let module_info = self
            .environment
            .importable_modules
            .get(&imported_module_name)
            .ok_or_else(|| Error::UnknownModule {
                location,
                name: imported_module_name.clone(),
                imported_modules: self.environment.imported_modules.keys().cloned().collect(),
            })?;

        self.check_src_does_not_import_test(module_info, location, imported_module_name.clone())?;
        self.register_module(import, module_info)?;

        // Insert unqualified imports into scope
        for type_ in &import.unqualified_types {
            self.register_unqualified_type(type_, module_info)?;
        }
        for value in &import.unqualified_values {
            self.register_unqualified_value(value, module_info)?;
        }
        Ok(())
    }

    fn register_unqualified_type(
        &mut self,
        import: &UnqualifiedImport,
        module: &ModuleInterface,
    ) -> Result<(), Error> {
        let imported_name = import.as_name.as_ref().unwrap_or(&import.name);

        // Register the unqualified import if it is a type constructor
        let type_info = module
            .get_public_type(&import.name)
            // TODO: refine to a type specific error
            .ok_or_else(|| Error::UnknownModuleType {
                location: import.location,
                name: import.name.clone(),
                module_name: module.name.clone(),
                type_constructors: module.public_type_names(),
            })?
            .clone()
            .with_location(import.location);

        self.environment
            .insert_type_constructor(imported_name.clone(), type_info)?;

        self.environment.init_usage(
            imported_name.clone(),
            EntityKind::ImportedType,
            import.location,
        );

        Ok(())
    }

    fn register_unqualified_value(
        &mut self,
        import: &UnqualifiedImport,
        module: &ModuleInterface,
    ) -> Result<(), Error> {
        let import_name = &import.name;
        let location = import.location;
        let used_name = import.as_name.as_ref().unwrap_or(&import.name);

        // Register the unqualified import if it is a value
        let variant = match module.get_public_value(import_name) {
            Some(value) => {
                self.environment.insert_variable(
                    used_name.clone(),
                    value.variant.clone(),
                    value.type_.clone(),
                    true,
                    value.deprecation.clone(),
                );
                &value.variant
            }
            None => {
                return Err(Error::UnknownModuleValue {
                    location,
                    name: import_name.clone(),
                    module_name: module.name.clone(),
                    value_constructors: module.public_value_names(),
                });
            }
        };

        match variant {
            &ValueConstructorVariant::Record { .. } => self.environment.init_usage(
                used_name.clone(),
                EntityKind::ImportedConstructor,
                location,
            ),
            _ => {
                self.environment
                    .init_usage(used_name.clone(), EntityKind::ImportedValue, location)
            }
        };

        // Check if value already was imported
        if let Some(previous) = self.environment.unqualified_imported_names.get(used_name) {
            return Err(Error::DuplicateImport {
                location,
                previous_location: *previous,
                name: import_name.clone(),
            });
        }

        // Register the name as imported so it can't be imported a
        // second time in future
        let _ = self
            .environment
            .unqualified_imported_names
            .insert(used_name.clone(), location);

        Ok(())
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
        import: &Import<()>,
        import_info: &'a ModuleInterface,
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
                .insert(used_name, (import.location, import_info));
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
