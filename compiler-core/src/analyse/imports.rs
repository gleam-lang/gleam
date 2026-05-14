use ecow::EcoString;

use crate::{
    ast::{Publicity, SrcSpan, UnqualifiedImport, UntypedImport},
    build::Origin,
    reference::{EntityKind, ReferenceKind},
    type_::{
        Environment, Error, ModuleInterface, Problems, ValueConstructorVariant, Warning,
        error::InvalidImportKind,
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

        if let Err(e) = self.check_for_invalid_imports(module_info, location) {
            self.problems.error(e);
        }

        if let Err(e) = self.register_module(import, module_info) {
            self.problems.error(e);
            return;
        }

        // Insert unqualified imports into scope
        let module_name = &module_info.name;
        for type_ in &import.unqualified_types {
            self.register_unqualified_type(type_, module_name.clone(), module_info);
        }
        for value in &import.unqualified_values {
            self.register_unqualified_value(value, module_name.clone(), module_info);
        }
    }

    fn register_unqualified_type(
        &mut self,
        import: &UnqualifiedImport,
        module_name: EcoString,
        module: &ModuleInterface,
    ) {
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

        self.environment.references.register_type(
            imported_name.clone(),
            EntityKind::ImportedType {
                module: module_name,
            },
            import.location,
            Publicity::Private,
        );

        self.environment.references.register_type_reference(
            type_info.module.clone(),
            import.name.clone(),
            imported_name,
            import.imported_name_location,
            ReferenceKind::Import,
        );

        if let Err(e) = self
            .environment
            .insert_type_constructor(imported_name.clone(), type_info)
        {
            self.problems.error(e);
        }
    }

    fn register_unqualified_value(
        &mut self,
        import: &UnqualifiedImport,
        module_name: EcoString,
        module: &ModuleInterface,
    ) {
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
                    context: crate::type_::error::ModuleValueUsageContext::UnqualifiedImport,
                });
                return;
            }
        };

        match variant {
            ValueConstructorVariant::Record { name, module, .. } => {
                self.environment.names.named_constructor_in_scope(
                    module.clone(),
                    name.clone(),
                    used_name.clone(),
                );
                self.environment.references.register_value(
                    used_name.clone(),
                    EntityKind::ImportedConstructor {
                        module: module_name,
                    },
                    location,
                    Publicity::Private,
                );

                self.environment.references.register_value_reference(
                    module.clone(),
                    import_name.clone(),
                    used_name,
                    import.imported_name_location,
                    ReferenceKind::Import,
                );
            }
            ValueConstructorVariant::ModuleConstant { module, .. }
            | ValueConstructorVariant::ModuleFn { module, .. } => {
                self.environment.references.register_value(
                    used_name.clone(),
                    EntityKind::ImportedValue {
                        module: module_name,
                    },
                    location,
                    Publicity::Private,
                );
                self.environment.references.register_value_reference(
                    module.clone(),
                    import_name.clone(),
                    used_name,
                    import.imported_name_location,
                    ReferenceKind::Import,
                );
            }
            ValueConstructorVariant::LocalVariable { .. } => {}
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

    /// Check for invalid imports, such as `src` importing `test` or `dev`.
    fn check_for_invalid_imports(
        &mut self,
        module_info: &ModuleInterface,
        location: SrcSpan,
    ) -> Result<(), Error> {
        if self.origin.is_src()
            && self
                .environment
                .dev_dependencies
                .contains(&module_info.package)
        {
            return Err(Error::SrcImportingDevDependency {
                importing_module: self.environment.current_module.clone(),
                imported_module: module_info.name.clone(),
                package: module_info.package.clone(),
                location,
            });
        }

        let kind = match (self.origin, module_info.origin) {
            // `src` cannot import `test` or `dev`
            (Origin::Src, Origin::Test) => InvalidImportKind::SrcImportingTest,
            (Origin::Src, Origin::Dev) => InvalidImportKind::SrcImportingDev,
            // `dev` cannot import `test`
            (Origin::Dev, Origin::Test) => InvalidImportKind::DevImportingTest,
            _ => return Ok(()),
        };

        Err(Error::InvalidImport {
            location,
            importing_module: self.environment.current_module.clone(),
            imported_module: module_info.name.clone(),
            kind,
        })
    }

    fn register_module(
        &mut self,
        import: &UntypedImport,
        import_info: &'context ModuleInterface,
    ) -> Result<(), Error> {
        let Some(used_name) = import.used_name() else {
            return Ok(());
        };

        self.check_not_a_duplicate_import(&used_name, import.location)?;

        if let Some(alias_location) = import.alias_location() {
            self.environment.references.register_aliased_module(
                used_name.clone(),
                import.module.clone(),
                alias_location,
                import.location,
            );
        } else {
            self.environment.references.register_module(
                used_name.clone(),
                import.module.clone(),
                import.location,
            );
        }

        // Insert imported module into scope
        let _ = self
            .environment
            .imported_modules
            .insert(used_name.clone(), (import.location, import_info));

        // Register this module as being imported
        //
        // Emit a warning if the module had already been imported.
        // This isn't an error so long as the modules have different local aliases. In Gleam v2
        // this will likely become an error.
        if let Some(previous) = self.environment.names.imported_module(
            import.module.clone(),
            used_name,
            import.location,
        ) {
            self.problems.warning(Warning::ModuleImportedTwice {
                name: import.module.clone(),
                first: previous,
                second: import.location,
            });
        }

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
