// SPDX-License-Identifier: Apache-2.0
// SPDX-FileCopyrightText: 2020 The Gleam contributors

#[cfg(test)]
mod tests;

use crate::analyse::TargetSupport;
use crate::ast::TypedModule;
use crate::build::package_loader::CacheFiles;
use crate::build::{ErlangOutput, module_erlang_name};

use crate::error::{DefinedModuleOrigin, FailedModule, SkipReason, SkippedModule};

use crate::metadata;
use crate::requirement::Requirement;
use crate::type_::PRELUDE_MODULE_NAME;
use crate::type_::printer::Names;
use crate::{
    Error, Result, Warning,
    ast::UntypedModule,
    build::{
        Mode, Module, Origin, Outcome, SourceFingerprint, Target,
        elixir_libraries::ElixirLibraries,
        native_file_copier::NativeFileCopier,
        package_loader::{CodegenRequired, PackageLoader, StaleTracker},
    },
    codegen::{Erlang, ErlangApp, JavaScript, TypeScriptDeclarations},
    config::PackageConfig,
    io::{BeamCompilerIO, CommandExecutor, FileSystemReader, FileSystemWriter, Stdio},
    parse::extra::ModuleExtra,
    paths, type_,
    uid::UniqueIdGenerator,
    warning::{TypeWarningEmitter, WarningEmitter},
};
use askama::Template;
use ecow::{EcoString, eco_format};
use src_span::{LineNumbers, SrcSpan};
use std::{
    collections::{HashMap, HashSet},
    time::SystemTime,
};

use camino::{Utf8Path, Utf8PathBuf};

use super::{ErlangAppCodegenConfiguration, TargetCodegenConfiguration, Telemetry};

#[derive(Debug)]
pub struct Compiled {
    /// The modules which were just compiled
    pub modules: Vec<Module>,
    /// The names of all cached modules, which are not present in the `modules` field.
    pub cached_module_names: Vec<EcoString>,
}

#[derive(Debug)]
pub struct PackageCompiler<'a, IO> {
    pub io: IO,
    pub out: &'a Utf8Path,
    pub lib: &'a Utf8Path,
    pub root: &'a Utf8Path,
    pub mode: Mode,
    pub target: &'a TargetCodegenConfiguration,
    pub config: &'a PackageConfig,
    pub ids: UniqueIdGenerator,
    pub write_metadata: bool,
    pub perform_codegen: bool,
    /// If set to false the compiler won't load and analyse any of the package's
    /// modules and always succeed compilation returning no compile modules.
    ///
    /// Code generation is still carried out so that a root package will have an
    /// entry point nonetheless.
    ///
    pub compile_modules: bool,
    pub write_entrypoint: bool,
    pub copy_native_files: bool,
    pub compile_beam_bytecode: bool,
    pub subprocess_stdio: Stdio,
    pub target_support: TargetSupport,
    pub cached_warnings: CachedWarnings,
    pub check_module_conflicts: CheckModuleConflicts,
}

impl<'a, IO> PackageCompiler<'a, IO>
where
    IO: FileSystemReader + FileSystemWriter + CommandExecutor + BeamCompilerIO + Clone,
{
    #[expect(clippy::too_many_arguments)]
    pub fn new(
        config: &'a PackageConfig,
        mode: Mode,
        root: &'a Utf8Path,
        out: &'a Utf8Path,
        lib: &'a Utf8Path,
        target: &'a TargetCodegenConfiguration,
        ids: UniqueIdGenerator,
        io: IO,
    ) -> Self {
        Self {
            io,
            ids,
            out,
            lib,
            root,
            mode,
            config,
            target,
            write_metadata: true,
            perform_codegen: true,
            compile_modules: true,
            write_entrypoint: false,
            copy_native_files: true,
            compile_beam_bytecode: true,
            subprocess_stdio: Stdio::Inherit,
            target_support: TargetSupport::NotEnforced,
            cached_warnings: CachedWarnings::Ignore,
            check_module_conflicts: CheckModuleConflicts::DoNotCheck,
        }
    }

    /// Compile the package.
    /// Returns a list of modules that were compiled. Any modules that were read
    /// from the cache will not be returned.
    // TODO: return the cached modules.
    pub fn compile(
        mut self,
        warnings: &WarningEmitter,
        existing_modules: &mut im::HashMap<EcoString, type_::ModuleInterface>,
        already_defined_modules: &mut im::HashMap<EcoString, DefinedModuleOrigin>,
        stale_modules: &mut StaleTracker,
        incomplete_modules: &mut HashSet<EcoString>,
        telemetry: &dyn Telemetry,
    ) -> Outcome<Compiled, Error> {
        let span = tracing::info_span!("compile", package = %self.config.name.as_str());
        let _enter = span.enter();

        // Ensure that the package is compatible with this version of Gleam
        if let Err(error) = self.config.check_gleam_compatibility() {
            return error.into();
        }

        let artefact_directory = self.out.join(paths::ARTEFACT_DIRECTORY_NAME);
        let codegen_required = if self.perform_codegen {
            CodegenRequired::Yes
        } else {
            CodegenRequired::No
        };

        let loader = PackageLoader::new(
            self.io.clone(),
            self.ids.clone(),
            self.mode,
            self.root,
            self.cached_warnings,
            warnings,
            codegen_required,
            &artefact_directory,
            self.target.target(),
            &self.config.name,
            stale_modules,
            already_defined_modules,
            incomplete_modules,
        );

        let loaded = if self.compile_modules {
            match loader.run() {
                Ok(loaded) => loaded,
                Err(error) => return error.into(),
            }
        } else {
            Loaded::empty()
        };

        let mut cached_module_names = Vec::new();

        // Load the cached modules that have previously been compiled
        for module in loaded.cached.into_iter() {
            // Emit any cached warnings.
            // Note that `self.cached_warnings` is set to `Ignore` (such as for
            // dependency packages) then this field will not be populated.
            if let Err(error) = self.emit_warnings(warnings, &module) {
                return error.into();
            }

            cached_module_names.push(module.name.clone());

            // Register the cached module so its type information etc can be
            // used for compiling futher modules.
            _ = existing_modules.insert(module.name.clone(), module);
        }

        if !loaded.to_compile.is_empty() {
            // Print that work is being done
            if self.perform_codegen {
                telemetry.compiling_package(&self.config.name);
            } else {
                telemetry.checking_package(&self.config.name)
            }
        }

        // Type check the modules that are new or have changed
        tracing::info!(count=%loaded.to_compile.len(), "analysing_modules");
        let outcome =
            PackageModulesAnalyser::new(&self, warnings, existing_modules, incomplete_modules)
                .analyse(loaded.to_compile);

        let mut modules = match outcome {
            Outcome::Ok(modules) => modules,
            Outcome::PartialFailure(modules, errors) => {
                return Outcome::PartialFailure(
                    Compiled {
                        modules,
                        cached_module_names,
                    },
                    errors,
                );
            }
            Outcome::TotalFailure(errors) => return Outcome::TotalFailure(errors),
        };

        tracing::debug!("performing_code_generation");

        // Inlining is currently disabled. See
        // https://github.com/gleam-lang/gleam/pull/5010 for information.

        // let modules = if self.perform_codegen {
        //     modules
        //         .into_iter()
        //         .map(|mut module| {
        //             module.ast = inline::module(module.ast, &existing_modules);
        //             module
        //         })
        //         .collect()
        // } else {
        //     modules
        // };

        if let Err(error) = self.perform_codegen(&modules, &cached_module_names) {
            return error.into();
        }

        if let Err(error) = self.encode_and_write_metadata(&mut modules) {
            return error.into();
        }

        Outcome::Ok(Compiled {
            modules,
            cached_module_names,
        })
    }

    fn compile_erlang_to_beam(
        &mut self,
        modules: &HashSet<Utf8PathBuf>,
    ) -> Result<Vec<EcoString>, Error> {
        if modules.is_empty() {
            tracing::debug!("no_erlang_to_compile");
            return Ok(Vec::new());
        }

        tracing::debug!("compiling_erlang");

        self.io
            .compile_beam(self.out, self.lib, modules, self.subprocess_stdio)
            .map(|modules| modules.iter().map(EcoString::from).collect())
    }

    fn copy_project_native_files(
        &mut self,
        destination_dir: &Utf8Path,
        precompiled_erlang_file_names: HashSet<EcoString>,
        to_compile_modules: &mut HashSet<Utf8PathBuf>,
    ) -> Result<(), Error> {
        tracing::debug!("copying_native_source_files");

        // TODO: unit test
        let priv_source = self.root.join("priv");
        let priv_build = self.out.join("priv");
        if self.io.is_directory(&priv_source) && !self.io.is_directory(&priv_build) {
            tracing::debug!("linking_priv_to_build");
            self.io.symlink_dir(&priv_source, &priv_build)?;
        }

        let copier = NativeFileCopier::new(
            self.io.clone(),
            self.root,
            destination_dir,
            self.check_module_conflicts,
            precompiled_erlang_file_names,
        );
        let copied = copier.run()?;

        to_compile_modules.extend(copied.to_compile);

        // If there are any Elixir files then we need to locate Elixir
        // installed on this system for use in compilation.
        if copied.any_elixir {
            ElixirLibraries::make_available(
                &self.io,
                &self.lib.to_path_buf(),
                self.subprocess_stdio,
            )?;
        }

        Ok(())
    }

    fn encode_and_write_metadata(&mut self, modules: &mut [Module]) -> Result<()> {
        if !self.write_metadata {
            tracing::debug!("package_metadata_writing_disabled");
            return Ok(());
        }
        if modules.is_empty() {
            return Ok(());
        }

        let artefact_dir = self.out.join(paths::ARTEFACT_DIRECTORY_NAME);

        tracing::debug!("writing_module_caches");
        for module in modules {
            let cache_files = CacheFiles::new(&artefact_dir, &module.name);

            let result = if self.cached_warnings.should_use() {
                metadata::encode(&module.ast.type_info)
            } else {
                // Dependency packages don't get warnings persisted as the
                // programmer doesn't want to be told every time about warnings they
                // cannot fix directly.
                let warnings = std::mem::take(&mut module.ast.type_info.warnings);
                let result = metadata::encode(&module.ast.type_info);
                module.ast.type_info.warnings = warnings;
                result
            };

            // Write cache file
            let bytes = result.expect("Failed to serialise module cache");
            self.io.write_bytes(&cache_files.cache_path, &bytes)?;

            // Write cache metadata
            let info = CacheMetadata {
                mtime: module.mtime,
                codegen_performed: self.perform_codegen,
                dependencies: module.dependencies.clone(),
                fingerprint: SourceFingerprint::new(&module.code),
                line_numbers: module.ast.type_info.line_numbers.clone(),
            };
            self.io
                .write_bytes(&cache_files.meta_path, &info.to_binary())?;
        }
        Ok(())
    }

    fn perform_codegen(
        &mut self,
        modules: &[Module],
        cached_module_names: &[EcoString],
    ) -> Result<()> {
        if !self.perform_codegen {
            tracing::debug!("skipping_codegen");
            return Ok(());
        }

        match self.target {
            TargetCodegenConfiguration::JavaScript {
                emit_typescript_definitions,
                emit_source_maps,
                prelude_location,
            } => self.perform_javascript_codegen(
                modules,
                *emit_typescript_definitions,
                *emit_source_maps,
                prelude_location,
            ),
            TargetCodegenConfiguration::Erlang { app_file, output } => self.perform_erlang_codegen(
                *output,
                modules,
                cached_module_names,
                app_file.as_ref(),
            ),
        }
    }

    fn perform_erlang_codegen(
        &mut self,
        output: ErlangOutput,
        modules: &[Module],
        cached_module_names: &[EcoString],
        app_file_config: Option<&ErlangAppCodegenConfiguration>,
    ) -> Result<(), Error> {
        let mut written = HashSet::new();
        let build_dir = self.out.join(paths::ARTEFACT_DIRECTORY_NAME);
        let include_dir = self.out.join("include");
        let io = self.io.clone();

        io.mkdir(&build_dir)?;

        // We don't want to copy the precompiled Erlang files of deps over, so
        // we make sure the native file copier will ignore those.
        let precompiled_erlang_file_names = modules
            .iter()
            .map(|module| &module.name)
            .chain(cached_module_names.iter())
            .map(|module_name| eco_format!("{}.erl", module_erlang_name(module_name)))
            .collect();

        if self.copy_native_files {
            self.copy_project_native_files(
                &build_dir,
                precompiled_erlang_file_names,
                &mut written,
            )?;
        } else {
            tracing::debug!("skipping_native_file_copying");
        }

        if self.compile_beam_bytecode && self.write_entrypoint {
            self.render_erlang_entrypoint_module(&build_dir, &mut written)?;
        } else {
            tracing::debug!("skipping_entrypoint_generation");
        }

        // NOTE: This must come after `copy_project_native_files` to ensure that
        // we overwrite any precompiled Erlang that was included in the Hex
        // package. Otherwise we will build the potentially outdated precompiled
        // version and not the newly compiled version.
        Erlang::new(&build_dir, &include_dir).render(output, io.clone(), modules, self.root)?;

        let native_modules: Vec<EcoString> = if self.compile_beam_bytecode {
            written.extend(modules.iter().map(match output {
                ErlangOutput::Binary => Module::compiled_erlang_path,
                ErlangOutput::Textual => Module::compiled_textual_erlang_path,
            }));
            self.compile_erlang_to_beam(&written)?
        } else {
            tracing::debug!("skipping_erlang_bytecode_compilation");
            Vec::new()
        };

        if let Some(config) = app_file_config {
            ErlangApp::new(&self.out.join("ebin"), config).render(
                io,
                self.config,
                modules,
                cached_module_names,
                native_modules,
            )?;
        }
        Ok(())
    }

    fn perform_javascript_codegen(
        &mut self,
        modules: &[Module],
        typescript: bool,
        sourcemaps: bool,
        prelude_location: &Utf8Path,
    ) -> Result<(), Error> {
        let mut written = HashSet::new();
        let typescript = if typescript {
            TypeScriptDeclarations::Emit
        } else {
            TypeScriptDeclarations::None
        };
        JavaScript::new(
            self.out,
            typescript,
            sourcemaps,
            prelude_location,
            self.root,
        )
        .render(&self.io, modules, self.stdlib_package())?;

        if self.copy_native_files {
            self.copy_project_native_files(self.out, HashSet::new(), &mut written)?;
        } else {
            tracing::debug!("skipping_native_file_copying");
        }

        // Source maps files include a reference to the source Gleam file, so if
        // we are generating source maps we need to make the source file available
        // in the build directory next to the JavaScript file and the source map.
        if sourcemaps {
            for module in modules {
                self.link_module_source_file_to_out(module)?;
            }
        }

        Ok(())
    }

    /// Link the module source .gleam file to the output directory.
    /// This is done if we are generating source maps because source
    /// maps include a reference to the source file, so the source
    /// file needs to be accessible to the user of the source file.
    ///
    fn link_module_source_file_to_out(&mut self, module: &Module) -> Result<(), Error> {
        let source = module.input_path.as_path();
        let destination = self.out.join(module.name.as_str()).with_extension("gleam");
        let file_at_destination = self.io.exists(&destination);

        // If the file does not exist then linking is needed. It could already
        // exist due to having been linked by a previous compilation run.
        if !file_at_destination {
            return self.io.hardlink(source, &destination);
        }

        // If file is already there then there is nothing to do.
        //
        // If there is already a file there but it is not a link that means
        // it is a link to the source file for a previous module that had
        // the same name.
        if self.io.is_same_file(source, &destination)? {
            return Ok(());
        }

        self.io.delete_file(&destination)?;
        self.io.hardlink(source, &destination)
    }

    fn render_erlang_entrypoint_module(
        &mut self,
        out: &Utf8Path,
        modules_to_compile: &mut HashSet<Utf8PathBuf>,
    ) -> Result<(), Error> {
        let name = format!("{name}@@main.erl", name = self.config.name);
        let path = out.join(&name);

        // If the entrypoint module has already been created then we don't need
        // to write and compile it again.
        if self.io.is_file(&path) {
            tracing::debug!("erlang_entrypoint_already_exists");
            return Ok(());
        }

        let template = ErlangEntrypointModule {
            application: &self.config.name,
        };
        let module = template.render().expect("Erlang entrypoint rendering");
        self.io.write(&path, &module)?;
        let _ = modules_to_compile.insert(name.into());
        tracing::debug!("erlang_entrypoint_written");
        Ok(())
    }

    fn emit_warnings(
        &self,
        warnings: &WarningEmitter,
        module: &type_::ModuleInterface,
    ) -> Result<()> {
        for warning in &module.warnings {
            let src = self.io.read(&module.src_path)?;
            warnings.emit(Warning::Type {
                path: module.src_path.clone(),
                src: src.into(),
                warning: Box::new(warning.clone()),
            });
        }

        Ok(())
    }

    fn stdlib_package(&self) -> StdlibPackage {
        if self.config.dependencies.contains_key("gleam_stdlib")
            || self.config.dev_dependencies.contains_key("gleam_stdlib")
        {
            StdlibPackage::Present
        } else {
            StdlibPackage::Missing
        }
    }
}

#[derive(Debug, Clone, Copy, Eq, PartialEq)]
pub enum StdlibPackage {
    Present,
    Missing,
}

/// A structure we use to hold data about used to analyse the packages of a
/// module.
struct PackageModulesAnalyser<'a> {
    package_config: &'a PackageConfig,
    target_support: TargetSupport,
    target: Target,
    ids: &'a UniqueIdGenerator,
    warnings: &'a WarningEmitter,

    module_interfaces: &'a mut im::HashMap<EcoString, type_::ModuleInterface>,
    incomplete_modules: &'a mut HashSet<EcoString>,

    /// The direct dependencies of this package, as needed by the
    /// `ModuleAnalyzerConstructor`.
    direct_dependencies: HashMap<EcoString, Requirement>,

    /// The dev dependencies of this package, as needed by the
    /// `ModuleAnalyzerConstructor`.
    dev_dependencies: HashSet<EcoString>,

    /// Keeps track of the modules whose analysis had to be skipped because
    /// other modules they depend on failed to compile.
    skipped_modules: HashMap<EcoString, SkippedModule>,
    /// Keeps track of the modules whose compilation failed because of some
    /// error.
    failed_modules: HashMap<EcoString, FailedModule>,
    /// Keeps track of the modules that could be analysed successfully.
    analysed_modules: Vec<Module>,
}

impl<'a> PackageModulesAnalyser<'a> {
    pub fn new<'package_compiler, IO>(
        package_compiler: &'a PackageCompiler<'package_compiler, IO>,
        warnings: &'a WarningEmitter,
        module_interfaces: &'a mut im::HashMap<EcoString, type_::ModuleInterface>,
        incomplete_modules: &'a mut HashSet<EcoString>,
    ) -> Self {
        let direct_dependencies = package_compiler
            .config
            .dependencies_for(package_compiler.mode)
            .expect("Package deps");

        let dev_dependencies = package_compiler
            .config
            .dev_dependencies
            .keys()
            .cloned()
            .collect();

        Self {
            package_config: package_compiler.config,
            target_support: package_compiler.target_support,
            target: package_compiler.target.target(),
            ids: &package_compiler.ids,
            warnings,
            module_interfaces,
            incomplete_modules,

            direct_dependencies,
            dev_dependencies,

            skipped_modules: HashMap::new(),
            failed_modules: HashMap::new(),
            analysed_modules: Vec::new(),
        }
    }

    pub fn analyse(
        mut self,
        uncompiled_modules: Vec<UncompiledModule>,
    ) -> Outcome<Vec<Module>, Error> {
        // Insert the prelude
        // DUPE: preludeinsertion
        // TODO: Currently we do this here and also in the tests. It would be better
        // to have one place where we create all this required state for use in each
        // place.
        let _ = self
            .module_interfaces
            .insert(PRELUDE_MODULE_NAME.into(), type_::build_prelude(self.ids));

        for uncompiled_module in uncompiled_modules {
            let UncompiledModule {
                name,
                code,
                ast,
                path,
                mtime,
                origin,
                dependencies,
                extra,
                ..
            } = uncompiled_module;

            tracing::debug!(module = ?name, "Type checking");

            // We first need to check if the module can actually be compiled.
            // If we weren't able to compile one of the modules it depends on, then
            // we have to skip this one to avoid reporting false errors.
            if self.skip_if_erroring_dependencies(
                path.clone(),
                name.clone(),
                code.clone(),
                &dependencies,
            ) {
                continue;
            }

            match self.infer_module(path.clone(), code.clone(), origin, ast) {
                // In case of success we register the typed module and its
                // interface, making sure the module is no longer considered
                // incomplete.
                Outcome::Ok(ast) => {
                    let mut module = Module {
                        name,
                        code,
                        input_path: path,
                        mtime,
                        origin,
                        dependencies,
                        extra,
                        ast,
                    };
                    module.attach_doc_and_module_comments();

                    let _ = self.incomplete_modules.remove(&module.name);
                    self.register_module_interface(&module);
                    self.analysed_modules.push(module);
                }

                // If there's some errors in the module we mark it as incomplete
                // (so it won't be reloaded from the cache) and register the
                // failure.
                // We still register the successfully type checked interface so
                // that it can be used by the language server, even in case of
                // errors.
                Outcome::PartialFailure(ast, errors) => {
                    let names = ast.names.clone();
                    let mut module = Module {
                        name: name.clone(),
                        code: code.clone(),
                        input_path: path.clone(),
                        mtime,
                        origin,
                        dependencies,
                        extra,
                        ast,
                    };
                    module.attach_doc_and_module_comments();

                    let _ = self.incomplete_modules.insert(module.name.clone());
                    self.register_failed_module(path, name, code, names, errors);
                    self.register_module_interface(&module);
                    self.analysed_modules.push(module);
                }

                // In case of a total failure the module interface could not be
                // built at all! We just record that analysis failed.
                Outcome::TotalFailure(errors) => {
                    self.register_failed_module(path, name, code, Names::new(), errors);
                }
            };
        }

        self.into_outcome()
    }

    /// After typing all the modules, turn this struct into an outcome, based
    /// on the error we've ran into during the analysis of the given modules.
    fn into_outcome(self) -> Outcome<Vec<Module>, Error> {
        // There's three possible outcomes:
        // - all modules could be analysed, success
        // - no module could be analysed, total failure
        // - there's a mix of analysed and failed modules, partial failure

        // All modules could be analysed: there's no failure at all.
        if self.failed_modules.is_empty() {
            return Outcome::Ok(self.analysed_modules);
        }

        // No module could be analysed.
        if self.analysed_modules.is_empty() {
            return Outcome::TotalFailure(Error::Type {
                skipped_modules: self.skipped_modules.into_values().collect(),
                failed_modules: self.failed_modules,
            });
        }

        // There's a mix of analysed and failed modules.
        Outcome::PartialFailure(
            self.analysed_modules,
            Error::Type {
                skipped_modules: self.skipped_modules.into_values().collect(),
                failed_modules: self.failed_modules,
            },
        )
    }

    /// This adds the compiled module's interface to all the module interfaces
    /// so that its types can be imported into other modules.
    ///
    /// This also cheks the module is not empty and raises a warning if it is.
    fn register_module_interface(&mut self, module: &Module) {
        // Emit the empty module warning if the module has no definitions at all.
        // Modules with only private definitions already emit their own warnings.
        let interface = &module.ast.type_info;
        if interface.values.is_empty() && interface.types.is_empty() {
            self.warnings.emit(Warning::EmptyModule {
                path: module.input_path.clone(),
                name: module.name.clone(),
            });
        }

        let _ = self
            .module_interfaces
            .insert(module.name.clone(), interface.clone());
    }

    /// Registers the given module has failed with some errors.
    fn register_failed_module(
        &mut self,
        path: Utf8PathBuf,
        name: EcoString,
        code: EcoString,
        names: Names,
        errors: vec1::Vec1<type_::Error>,
    ) {
        let _ = self.failed_modules.insert(
            name,
            FailedModule {
                names: Box::new(names),
                path,
                src: code,
                errors,
            },
        );
    }

    /// Performs type inference for the given untyped module.
    fn infer_module(
        &self,
        path: Utf8PathBuf,
        code: EcoString,
        origin: Origin,
        module_ast: UntypedModule,
    ) -> Outcome<TypedModule, vec1::Vec1<type_::Error>> {
        let line_numbers = LineNumbers::new(&code);
        crate::analyse::ModuleAnalyzerConstructor {
            target: self.target,
            ids: self.ids,
            origin,
            importable_modules: self.module_interfaces,
            warnings: &TypeWarningEmitter::new(path.clone(), code, self.warnings.clone()),
            direct_dependencies: &self.direct_dependencies,
            dev_dependencies: &self.dev_dependencies,
            target_support: self.target_support,
            package_config: self.package_config,
        }
        .infer_module(module_ast, line_numbers, path)
    }

    /// Given an uncompiled module, this goes through the modules it depends on
    /// to see if any of them was skipped because of errors.
    /// If the module should be skipped because of this, it is marked as skipped
    /// and this will return `true`.
    ///
    fn skip_if_erroring_dependencies(
        &mut self,
        path: Utf8PathBuf,
        name: EcoString,
        code: EcoString,
        dependencies: &[(EcoString, SrcSpan)],
    ) -> bool {
        let reason_to_skip = dependencies
            .iter()
            .find_map(|(dependency, import_location)| {
                if self.failed_modules.contains_key(dependency) {
                    // We import a failing module directly, this module must be
                    // skipped too.
                    let reason = SkipReason::DependencyHasError {
                        name: dependency.clone(),
                    };
                    Some((*import_location, reason))
                } else if let Some(skipped_module) = self.skipped_modules.get(dependency) {
                    // We import a module that's not directly failing, but had
                    // to be skipped. This module must be skipped too!
                    let reason = SkipReason::DependencyWasSkipped {
                        name: dependency.clone(),
                        erroring_module: skipped_module.reason.erroring_module(),
                    };
                    Some((*import_location, reason))
                } else {
                    None
                }
            });

        match reason_to_skip {
            None => false,
            Some((location, reason)) => {
                let _ = self.skipped_modules.insert(
                    name.clone(),
                    SkippedModule {
                        path,
                        name,
                        code,
                        location,
                        reason,
                    },
                );
                true
            }
        }
    }
}

#[derive(Debug)]
pub(crate) enum Input {
    New { module: Box<UncompiledModule> },
    Cached { module: CachedModule },
}

impl Input {
    pub fn name(&self) -> &EcoString {
        match self {
            Input::New { module } => &module.name,
            Input::Cached { module } => &module.name,
        }
    }

    pub fn source_path(&self) -> &Utf8Path {
        match self {
            Input::New { module } => &module.path,
            Input::Cached { module } => &module.source_path,
        }
    }

    pub fn dependencies(&self) -> Vec<EcoString> {
        match self {
            Input::New { module } => module
                .dependencies
                .iter()
                .map(|(name, _)| name.clone())
                .collect(),
            Input::Cached { module } => module
                .dependencies
                .iter()
                .map(|(name, _)| name.clone())
                .collect(),
        }
    }

    /// Returns `true` if the input is [`New`].
    ///
    /// [`New`]: Input::New
    #[cfg(test)]
    #[must_use]
    pub(crate) fn is_new(&self) -> bool {
        matches!(self, Self::New { .. })
    }

    /// Returns `true` if the input is [`Cached`].
    ///
    /// [`Cached`]: Input::Cached
    #[cfg(test)]
    #[must_use]
    pub(crate) fn is_cached(&self) -> bool {
        matches!(self, Self::Cached { .. })
    }
}

#[derive(Debug)]
pub(crate) struct CachedModule {
    pub name: EcoString,
    pub origin: Origin,
    pub dependencies: Vec<(EcoString, SrcSpan)>,
    pub source_path: Utf8PathBuf,
}

#[derive(Debug, serde::Serialize, serde::Deserialize)]
pub(crate) struct CacheMetadata {
    pub mtime: SystemTime,
    pub codegen_performed: bool,
    pub dependencies: Vec<(EcoString, SrcSpan)>,
    pub fingerprint: SourceFingerprint,
    pub line_numbers: LineNumbers,
}

impl CacheMetadata {
    pub fn to_binary(&self) -> Vec<u8> {
        bitcode::serialize(self).expect("Serializing cache info")
    }

    pub fn from_binary(bytes: &[u8]) -> Result<Self, String> {
        match bitcode::deserialize(bytes) {
            Ok(data) => Ok(data),
            Err(e) => Err(e.to_string()),
        }
    }
}

#[derive(Debug, Default, PartialEq, Eq)]
pub(crate) struct Loaded {
    pub to_compile: Vec<UncompiledModule>,
    pub cached: Vec<type_::ModuleInterface>,
}

impl Loaded {
    fn empty() -> Self {
        Self {
            to_compile: vec![],
            cached: vec![],
        }
    }
}

#[derive(Debug, PartialEq, Eq)]
pub(crate) struct UncompiledModule {
    pub path: Utf8PathBuf,
    pub name: EcoString,
    pub code: EcoString,
    pub mtime: SystemTime,
    pub origin: Origin,
    pub package: EcoString,
    pub dependencies: Vec<(EcoString, SrcSpan)>,
    pub ast: UntypedModule,
    pub extra: ModuleExtra,
}

#[derive(Template)]
#[template(path = "gleam@@main.erl", escape = "none")]
struct ErlangEntrypointModule<'a> {
    application: &'a str,
}

#[derive(Debug, Clone, Copy)]
pub enum CachedWarnings {
    Use,
    Ignore,
}
impl CachedWarnings {
    pub(crate) fn should_use(&self) -> bool {
        match self {
            CachedWarnings::Use => true,
            CachedWarnings::Ignore => false,
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub enum CheckModuleConflicts {
    Check,
    DoNotCheck,
}
impl CheckModuleConflicts {
    pub(crate) fn should_check(&self) -> bool {
        match self {
            CheckModuleConflicts::Check => true,
            CheckModuleConflicts::DoNotCheck => false,
        }
    }
}
