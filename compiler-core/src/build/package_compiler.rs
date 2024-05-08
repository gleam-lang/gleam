use crate::analyse::{ModuleAnalyzer, TargetSupport};
use crate::line_numbers::{self, LineNumbers};
use crate::type_::PRELUDE_MODULE_NAME;
use crate::{
    ast::{SrcSpan, TypedModule, UntypedModule},
    build::{
        elixir_libraries::ElixirLibraries,
        module_loader::SourceFingerprint,
        native_file_copier::NativeFileCopier,
        package_loader::{CodegenRequired, PackageLoader, StaleTracker},
        Mode, Module, Origin, Package, Target,
    },
    codegen::{Erlang, ErlangApp, JavaScript, TypeScriptDeclarations},
    config::PackageConfig,
    dep_tree, error,
    io::{CommandExecutor, FileSystemReader, FileSystemWriter, Stdio},
    metadata::ModuleEncoder,
    parse::extra::ModuleExtra,
    paths, type_,
    uid::UniqueIdGenerator,
    warning::{TypeWarningEmitter, WarningEmitter},
    Error, Result, Warning,
};
use askama::Template;
use ecow::EcoString;
use std::collections::HashSet;
use std::{collections::HashMap, fmt::write, time::SystemTime};

use camino::{Utf8Path, Utf8PathBuf};

use super::{ErlangAppCodegenConfiguration, TargetCodegenConfiguration, Telemetry};

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
    pub write_entrypoint: bool,
    pub copy_native_files: bool,
    pub compile_beam_bytecode: bool,
    pub subprocess_stdio: Stdio,
    pub target_support: TargetSupport,
}

impl<'a, IO> PackageCompiler<'a, IO>
where
    IO: FileSystemReader + FileSystemWriter + CommandExecutor + Clone,
{
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
            write_entrypoint: false,
            copy_native_files: true,
            compile_beam_bytecode: true,
            subprocess_stdio: Stdio::Inherit,
            target_support: TargetSupport::NotEnforced,
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
        already_defined_modules: &mut im::HashMap<EcoString, Utf8PathBuf>,
        stale_modules: &mut StaleTracker,
        telemetry: &dyn Telemetry,
    ) -> Result<Vec<Module>, Error> {
        let span = tracing::info_span!("compile", package = %self.config.name.as_str());
        let _enter = span.enter();

        // Ensure that the package is compatible with this version of Gleam
        self.config.check_gleam_compatibility()?;

        let artefact_directory = self.out.join(paths::ARTEFACT_DIRECTORY_NAME);
        let codegen_required = if self.perform_codegen {
            CodegenRequired::Yes
        } else {
            CodegenRequired::No
        };
        let loaded = PackageLoader::new(
            self.io.clone(),
            self.ids.clone(),
            self.mode,
            self.root,
            warnings,
            codegen_required,
            &artefact_directory,
            self.target.target(),
            &self.config.name,
            stale_modules,
            already_defined_modules,
        )
        .run()?;

        // Load the cached modules that have previously been compiled
        for module in loaded.cached.into_iter() {
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
        let modules = analyse(
            &self.config,
            self.target.target(),
            self.mode,
            &self.ids,
            loaded.to_compile,
            existing_modules,
            warnings,
            self.target_support,
        )?;

        tracing::debug!("performing_code_generation");
        self.perform_codegen(&modules)?;
        self.encode_and_write_metadata(&modules)?;
        Ok(modules)
    }

    fn compile_erlang_to_beam(&mut self, modules: &HashSet<Utf8PathBuf>) -> Result<(), Error> {
        if modules.is_empty() {
            tracing::debug!("no_erlang_to_compile");
            return Ok(());
        }

        tracing::debug!("compiling_erlang");

        let escript_path = self
            .out
            .join(paths::ARTEFACT_DIRECTORY_NAME)
            .join("gleam@@compile.erl");
        if !self.io.is_file(&escript_path) {
            let escript_source = std::include_str!("../../templates/gleam@@compile.erl");
            self.io.write(&escript_path, escript_source)?;
        }

        let mut args = vec![
            escript_path.to_string(),
            // Tell the compiler where to find other libraries
            "--lib".into(),
            self.lib.to_string(),
            // Write compiled .beam to ./ebin
            "--out".into(),
            self.out.join("ebin").to_string(),
        ];
        // Add the list of modules to compile
        for module in modules {
            let path = self.out.join(paths::ARTEFACT_DIRECTORY_NAME).join(module);
            args.push(path.to_string());
        }
        // Compile Erlang and Elixir modules
        let status = self
            .io
            .exec("escript", &args, &[], None, self.subprocess_stdio)?;

        if status == 0 {
            Ok(())
        } else {
            Err(Error::ShellCommand {
                program: "escript".into(),
                err: None,
            })
        }
    }

    fn copy_project_native_files(
        &mut self,
        destination_dir: &Utf8Path,
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

        let copier = NativeFileCopier::new(self.io.clone(), self.root.clone(), destination_dir);
        let copied = copier.run()?;

        to_compile_modules.extend(copied.to_compile.into_iter());

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

    fn encode_and_write_metadata(&mut self, modules: &[Module]) -> Result<()> {
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
            let module_name = module.name.replace("/", "@");

            // Write metadata file
            let name = format!("{}.cache", &module_name);
            let path = artefact_dir.join(name);
            let bytes = ModuleEncoder::new(&module.ast.type_info).encode()?;
            self.io.write_bytes(&path, &bytes)?;

            // Write cache info
            let name = format!("{}.cache_meta", &module_name);
            let path = artefact_dir.join(name);
            let info = CacheMetadata {
                mtime: module.mtime,
                codegen_performed: self.perform_codegen,
                dependencies: module.dependencies_list(),
                fingerprint: SourceFingerprint::new(&module.code),
                line_numbers: module.ast.type_info.line_numbers.clone(),
            };
            self.io.write_bytes(&path, &info.to_binary())?;
        }
        Ok(())
    }

    fn perform_codegen(&mut self, modules: &[Module]) -> Result<()> {
        if !self.perform_codegen {
            tracing::debug!("skipping_codegen");
            return Ok(());
        }

        match self.target {
            TargetCodegenConfiguration::JavaScript {
                emit_typescript_definitions,
                prelude_location,
            } => self.perform_javascript_codegen(
                modules,
                *emit_typescript_definitions,
                prelude_location,
            ),
            TargetCodegenConfiguration::Erlang { app_file } => {
                self.perform_erlang_codegen(modules, app_file.as_ref())
            }
        }
    }

    fn perform_erlang_codegen(
        &mut self,
        modules: &[Module],
        app_file_config: Option<&ErlangAppCodegenConfiguration>,
    ) -> Result<(), Error> {
        let mut written = HashSet::new();
        let build_dir = self.out.join(paths::ARTEFACT_DIRECTORY_NAME);
        let include_dir = self.out.join("include");
        let io = self.io.clone();

        io.mkdir(&build_dir)?;

        if self.copy_native_files {
            self.copy_project_native_files(&build_dir, &mut written)?;
        } else {
            tracing::debug!("skipping_native_file_copying");
        }

        if let Some(config) = app_file_config {
            ErlangApp::new(&self.out.join("ebin"), config).render(
                io.clone(),
                &self.config,
                modules,
            )?;
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
        Erlang::new(&build_dir, &include_dir).render(io, modules)?;

        if self.compile_beam_bytecode {
            written.extend(modules.iter().map(Module::compiled_erlang_path));
            self.compile_erlang_to_beam(&written)?;
        } else {
            tracing::debug!("skipping_erlang_bytecode_compilation");
        }
        Ok(())
    }

    fn perform_javascript_codegen(
        &mut self,
        modules: &[Module],
        typescript: bool,
        prelude_location: &Utf8Path,
    ) -> Result<(), Error> {
        let mut written = HashSet::new();
        let typescript = if typescript {
            TypeScriptDeclarations::Emit
        } else {
            TypeScriptDeclarations::None
        };

        JavaScript::new(&self.out, typescript, prelude_location, self.target_support)
            .render(&self.io, modules)?;

        if self.copy_native_files {
            self.copy_project_native_files(&self.out, &mut written)?;
        } else {
            tracing::debug!("skipping_native_file_copying");
        }

        Ok(())
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
}

fn analyse(
    package_config: &PackageConfig,
    target: Target,
    mode: Mode,
    ids: &UniqueIdGenerator,
    mut parsed_modules: Vec<UncompiledModule>,
    module_types: &mut im::HashMap<EcoString, type_::ModuleInterface>,
    warnings: &WarningEmitter,
    target_support: TargetSupport,
) -> Result<Vec<Module>, Error> {
    let mut modules = Vec::with_capacity(parsed_modules.len() + 1);
    let direct_dependencies = package_config.dependencies_for(mode).expect("Package deps");

    // Insert the prelude
    // DUPE: preludeinsertion
    // TODO: Currently we do this here and also in the tests. It would be better
    // to have one place where we create all this required state for use in each
    // place.
    let _ = module_types.insert(PRELUDE_MODULE_NAME.into(), type_::build_prelude(ids));

    for UncompiledModule {
        name,
        code,
        ast,
        path,
        mtime,
        origin,
        package,
        dependencies,
        extra,
    } in parsed_modules
    {
        tracing::debug!(module = ?name, "Type checking");

        let line_numbers = LineNumbers::new(&code);

        let inference_result = crate::analyse::ModuleAnalyzer {
            target,
            ids,
            origin,
            importable_modules: module_types,
            warnings: &TypeWarningEmitter::new(path.clone(), code.clone(), warnings.clone()),
            direct_dependencies: &direct_dependencies,
            target_support,
            package_config,
        }
        .infer_module(ast, line_numbers, path.clone());

        // TODO: Once we are guaranteed to get an AST back we should change this
        // so that instead of returning immediately we bubble up and
        // do the error check prior to code generation.
        let ast = match inference_result {
            Err(crate::analyse::InferenceFailure { errors, .. }) => {
                return Err(Error::Type {
                    path: path.clone(),
                    src: code.clone(),
                    errors,
                })
            }
            Ok(ast) => ast,
        };

        // Register the types from this module so they can be imported into
        // other modules.
        let _ = module_types.insert(name.clone(), ast.type_info.clone());

        // Register the successfully type checked module data so that it can be
        // used for code generation
        modules.push(Module {
            dependencies,
            origin,
            extra,
            mtime,
            name,
            code,
            ast,
            input_path: path,
        });
    }

    Ok(modules)
}

pub(crate) fn module_name(package_path: &Utf8Path, full_module_path: &Utf8Path) -> EcoString {
    // /path/to/project/_build/default/lib/the_package/src/my/module.gleam

    // my/module.gleam
    let mut module_path = full_module_path
        .strip_prefix(package_path)
        .expect("Stripping package prefix from module path")
        .to_path_buf();

    // my/module
    let _ = module_path.set_extension("");

    // Stringify
    let name = module_path.to_string();

    // normalise windows paths
    name.replace("\\", "/").into()
}

#[derive(Debug)]
pub(crate) enum Input {
    New(UncompiledModule),
    Cached(CachedModule),
}

impl Input {
    pub fn name(&self) -> &EcoString {
        match self {
            Input::New(m) => &m.name,
            Input::Cached(m) => &m.name,
        }
    }

    pub fn source_path(&self) -> &Utf8Path {
        match self {
            Input::New(m) => &m.path,
            Input::Cached(m) => &m.source_path,
        }
    }

    pub fn dependencies(&self) -> Vec<EcoString> {
        match self {
            Input::New(m) => m.dependencies.iter().map(|(n, _)| n.clone()).collect(),
            Input::Cached(m) => m.dependencies.clone(),
        }
    }

    /// Returns `true` if the input is [`New`].
    ///
    /// [`New`]: Input::New
    #[must_use]
    pub(crate) fn is_new(&self) -> bool {
        matches!(self, Self::New(..))
    }

    /// Returns `true` if the input is [`Cached`].
    ///
    /// [`Cached`]: Input::Cached
    #[must_use]
    pub(crate) fn is_cached(&self) -> bool {
        matches!(self, Self::Cached(..))
    }
}

#[derive(Debug)]
pub(crate) struct CachedModule {
    pub name: EcoString,
    pub origin: Origin,
    pub dependencies: Vec<EcoString>,
    pub source_path: Utf8PathBuf,
    pub line_numbers: LineNumbers,
}

#[derive(Debug, serde::Serialize, serde::Deserialize)]
pub(crate) struct CacheMetadata {
    pub mtime: SystemTime,
    pub codegen_performed: bool,
    pub dependencies: Vec<EcoString>,
    pub fingerprint: SourceFingerprint,
    pub line_numbers: LineNumbers,
}

impl CacheMetadata {
    pub fn to_binary(&self) -> Vec<u8> {
        bincode::serialize(self).expect("Serializing cache info")
    }

    pub fn from_binary(bytes: &[u8]) -> Result<Self, String> {
        bincode::deserialize(bytes).map_err(|e| e.to_string())
    }
}

#[derive(Debug, Default, PartialEq, Eq)]
pub(crate) struct Loaded {
    pub to_compile: Vec<UncompiledModule>,
    pub cached: Vec<type_::ModuleInterface>,
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
