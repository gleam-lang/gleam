use std::collections::HashSet;

use ecow::EcoString;

use crate::{
    ast::{Import, SrcSpan, UnqualifiedImport},
    build::Origin,
    type_::{
        self, EntityKind, Environment, Error, LayerUsage, ModuleInterface, ValueConstructorVariant,
    },
    warning::TypeWarningEmitter,
};

#[derive(Debug)]
pub struct Importer<'a> {
    origin: Origin,
    warnings: &'a TypeWarningEmitter,
    environment: Environment<'a>,
}

impl<'a> Importer<'a> {
    pub fn new(
        origin: Origin,
        warnings: &'a TypeWarningEmitter,
        environment: Environment<'a>,
    ) -> Self {
        Self {
            origin,
            warnings,
            environment,
        }
    }

    pub fn run<'b>(
        origin: Origin,
        warnings: &'a TypeWarningEmitter,
        env: Environment<'a>,
        imports: &'b [Import<()>],
    ) -> Result<Environment<'a>, Error> {
        let mut importer = Self::new(origin, warnings, env);
        for import in imports {
            importer.register_import(import)?;
        }
        Ok(importer.environment)
    }

    fn register_import(&mut self, import: &Import<()>) -> Result<(), Error> {
        // Determine local alias of imported module
        let used_name = import.used_name();

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

        self.check_not_a_duplicate_import(&used_name, location)?;
        self.check_src_does_not_import_test(module_info, location, imported_module_name.clone())?;

        // TODO: remove this when we remove the old syntax
        let mut types = HashSet::with_capacity(import.unqualified_types.len());
        // Insert unqualified imports into scope
        for type_ in &import.unqualified_types {
            self.register_unqualified_type(type_, module_info, &mut types)?;
        }
        for value in &import.unqualified_values {
            self.register_unqualified_value(value, module_info, &mut types)?;
        }

        self.register_module_usage(import);

        // Register the name as imported so it can't be imported a
        // second time in future
        let _ = self
            .environment
            .unqualified_imported_names
            .insert(used_name.clone(), location);

        // Insert imported module into scope
        let _ = self
            .environment
            .imported_modules
            .insert(used_name, (location, module_info));
        Ok(())
    }

    fn register_unqualified_type(
        &mut self,
        import: &UnqualifiedImport,
        module: &ModuleInterface,
        imported_types: &mut HashSet<EcoString>,
    ) -> Result<(), Error> {
        self.check_if_deprecated_bit_string(import, module, import.location);

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

        let _ = imported_types.insert(imported_name.clone());

        Ok(())
    }

    fn register_unqualified_value(
        &mut self,
        import: &UnqualifiedImport,
        module: &ModuleInterface,
        imported_types: &mut HashSet<EcoString>,
    ) -> Result<(), Error> {
        self.check_if_deprecated_bit_string(import, module, import.location);

        let import_name = &import.name;
        let location = import.location;
        let used_name = import.as_name.as_ref().unwrap_or(&import.name);
        let mut type_imported = false;
        let mut value_imported = false;
        let mut variant = None;

        // Register the unqualified import if it is a value
        if let Some(value) = module.get_public_value(import_name) {
            self.environment.insert_variable(
                used_name.clone(),
                value.variant.clone(),
                value.type_.clone(),
                true,
                value.deprecation.clone(),
            );
            variant = Some(&value.variant);
            value_imported = true;
        }

        // Register the unqualified import if it is a type constructor
        if !imported_types.contains(used_name) {
            if let Some(typ) = module.get_public_type(import_name) {
                self.environment.insert_type_constructor(
                    used_name.clone(),
                    typ.clone().with_location(location),
                )?;
                type_imported = true;
            }
        }

        if value_imported && type_imported {
            self.environment.init_usage(
                used_name.clone(),
                EntityKind::ImportedTypeAndValue(location),
                location,
            );
        } else if type_imported {
            self.environment
                .init_usage(used_name.clone(), EntityKind::ImportedType, location);
            let _ = self.environment.ambiguous_imported_items.insert(
                import_name.clone(),
                LayerUsage {
                    import_location: location,
                    type_: true,
                    value: false,
                },
            );
        } else if value_imported {
            match variant {
                Some(&ValueConstructorVariant::Record { .. }) => self.environment.init_usage(
                    used_name.clone(),
                    EntityKind::ImportedConstructor,
                    location,
                ),
                _ => self.environment.init_usage(
                    used_name.clone(),
                    EntityKind::ImportedValue,
                    location,
                ),
            };
        } else if !value_imported {
            // Error if no type or value was found with that name
            return Err(Error::UnknownModuleValue {
                location,
                name: import_name.clone(),
                module_name: module.name.clone(),
                value_constructors: module.public_value_names(),
            });
        }

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

    fn register_module_usage(&mut self, import: &Import<()>) {
        if import.unqualified_types.is_empty() && import.unqualified_values.is_empty() {
            // When the module has no unqualified imports, we track its usage
            // so we can warn if not used by the end of the type checking
            let _ = self
                .environment
                .unused_modules
                .insert(import.used_name(), import.location);
        } else if let Some(as_name) = &import.as_name {
            // When the module has a name, we also track its as_name usage
            // so we can warn if not used by the end of the type checking
            let _ = self
                .environment
                .unused_modules
                .insert(as_name.name.clone(), as_name.location);
            // We also register it's name to differentiate between unused module
            // and unused module name. See 'convert_unused_to_warnings'.
            let _ = self
                .environment
                .imported_module_aliases
                .insert(as_name.name.clone());
        }
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

    fn check_if_deprecated_bit_string(
        &self,
        import: &UnqualifiedImport,
        module: &ModuleInterface,
        location: SrcSpan,
    ) {
        if import.name == "BitString" && module.name == "gleam" {
            self.warnings
                .emit(type_::Warning::DeprecatedBitString { location });
        }
    }
}
