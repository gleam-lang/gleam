use debug_ignore::DebugIgnore;
use ecow::EcoString;
use itertools::Itertools;

use crate::{
    analyse::TargetSupport,
    build::{self, Mode, Module, NullTelemetry, ProjectCompiler},
    config::PackageConfig,
    io::{CommandExecutor, FileSystemReader, FileSystemWriter, Stdio},
    language_server::Locker,
    line_numbers::LineNumbers,
    manifest::Manifest,
    paths::ProjectPaths,
    type_::ModuleInterface,
    warning::VectorWarningEmitterIO,
    Error, Result, Warning,
};
use std::{collections::HashMap, sync::Arc};

use camino::Utf8PathBuf;

/// A wrapper around the project compiler which makes it possible to repeatedly
/// recompile the top level package, reusing the information about the already
/// compiled dependency packages.
///
#[derive(Debug)]
pub struct LspProjectCompiler<IO> {
    pub project_compiler: ProjectCompiler<IO>,

    /// Information on compiled modules.
    pub modules: HashMap<EcoString, Module>,
    pub sources: HashMap<EcoString, ModuleSourceInformation>,

    /// The storage for the warning emitter.
    pub warnings: Arc<VectorWarningEmitterIO>,

    /// A lock to ensure that multiple instances of the LSP don't try and use
    /// build directory at the same time.
    pub locker: DebugIgnore<Box<dyn Locker>>,
}

impl<IO> LspProjectCompiler<IO>
where
    IO: CommandExecutor + FileSystemWriter + FileSystemReader + Clone,
{
    pub fn new(
        manifest: Manifest,
        config: PackageConfig,
        paths: ProjectPaths,
        io: IO,
        locker: Box<dyn Locker>,
    ) -> Result<Self> {
        let telemetry = NullTelemetry;
        let target = config.target;
        let name = config.name.clone();
        let warnings = Arc::new(VectorWarningEmitterIO::default());

        // The build caches do not contain all the information we need in the
        // LSP (e.g. the typed AST) so delete the caches for the top level
        // package before we run for the first time.
        // TODO: remove this once the caches have contain all the information
        {
            let _guard = locker.lock_for_build();
            let path = paths.build_directory_for_package(Mode::Lsp, target, &name);
            io.delete_directory(&path)?;
        }

        let options = build::Options {
            warnings_as_errors: false,
            mode: build::Mode::Lsp,
            target: None,
            codegen: build::Codegen::None,
            root_target_support: TargetSupport::Enforced,
        };
        let mut project_compiler = ProjectCompiler::new(
            config,
            options,
            manifest.packages,
            Box::new(telemetry),
            warnings.clone(),
            paths,
            io,
        );

        // To avoid the Erlang compiler printing to stdout (and thus
        // violating LSP which is currently using stdout) we silence it.
        project_compiler.subprocess_stdio = Stdio::Null;

        Ok(Self {
            locker: locker.into(),
            warnings,
            project_compiler,
            modules: HashMap::new(),
            sources: HashMap::new(),
        })
    }

    pub fn compile(&mut self) -> Result<Vec<Utf8PathBuf>, Error> {
        // Lock the build directory to ensure to ensure we are the only one compiling
        let _lock_guard = self.locker.lock_for_build();

        // Verify that the build directory was created using the same version of
        // Gleam as we are running. If it is not then we discard the build
        // directory as the cache files may be in a different format.
        self.project_compiler.check_gleam_version()?;

        let compiled_dependencies = self.project_compiler.compile_dependencies()?;

        // Warnings from dependencies are not fixable by the programmer so
        // we don't bother them with diagnostics for them.
        let _ = self.take_warnings();

        // Do that there compilation. We don't use `?` to return early in the
        // event of an error because we _always_ want to do the restoration of
        // state afterwards.
        let result = self.project_compiler.compile_root_package();

        // Return any error
        let package = result?;

        // Record the compiled dependency modules
        let mut compiled_modules = compiled_dependencies
            .into_iter()
            .map(|m| m.input_path)
            .collect_vec();

        // Store the compiled module information
        for module in package.modules {
            let path = module.input_path.as_os_str().to_string_lossy().to_string();
            let line_numbers = LineNumbers::new(&module.code);
            let source = ModuleSourceInformation { path, line_numbers };
            compiled_modules.push(module.input_path.clone());
            _ = self.sources.insert(module.name.clone(), source);
            _ = self.modules.insert(module.name.clone(), module);
        }

        Ok(compiled_modules)
    }

    pub fn get_module_inferface(&self, name: &str) -> Option<&ModuleInterface> {
        self.project_compiler.get_importable_modules().get(name)
    }
}

impl<IO> LspProjectCompiler<IO> {
    pub fn take_warnings(&mut self) -> Vec<Warning> {
        self.warnings.take()
    }

    pub fn get_source(&self, module: &str) -> Option<&ModuleSourceInformation> {
        self.sources.get(module)
    }
}

#[derive(Debug)]
pub struct ModuleSourceInformation {
    /// The path to the source file from within the project root
    pub path: String,

    /// Useful for converting from Gleam's byte index offsets to the LSP line
    /// and column number positions.
    pub line_numbers: LineNumbers,
}
