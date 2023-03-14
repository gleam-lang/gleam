use crate::{
    build::{self, Mode, Module, NullTelemetry, ProjectCompiler},
    config::PackageConfig,
    io::{CommandExecutor, FileSystemReader, FileSystemWriter, Stdio},
    language_server::Locker,
    line_numbers::LineNumbers,
    manifest::Manifest,
    paths::ProjectPaths,
    warning::VectorWarningEmitterIO,
    Error, Result, Warning,
};
use std::{collections::HashMap, path::PathBuf, sync::Arc};

/// A wrapper around the project compiler which makes it possible to repeatedly
/// recompile the top level package, reusing the information about the already
/// compiled dependency packages.
///
#[derive(Debug)]
pub struct LspProjectCompiler<IO, LockerImpl> {
    pub project_compiler: ProjectCompiler<IO>,

    /// Whether the dependencies have been compiled previously.
    pub dependencies_compiled: bool,

    /// Information on compiled modules.
    pub modules: HashMap<String, Module>,
    pub sources: HashMap<String, ModuleSourceInformation>,

    /// The storage for the warning emitter.
    pub warnings: Arc<VectorWarningEmitterIO>,

    /// A lock to ensure that multiple instances of the LSP don't try and use
    /// build directory at the same time.
    pub locker: LockerImpl,
}

impl<IO, LockerImpl> LspProjectCompiler<IO, LockerImpl>
where
    IO: CommandExecutor + FileSystemWriter + FileSystemReader + Clone,
    LockerImpl: Locker,
{
    pub fn new(
        manifest: Manifest,
        config: PackageConfig,
        paths: ProjectPaths,
        io: IO,
        locker: LockerImpl,
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
            io.delete(&path)?;
        }

        let options = build::Options {
            warnings_as_errors: false,
            mode: build::Mode::Lsp,
            target: None,
            codegen: build::Codegen::None,
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

        // TODO: remove the LSP's ability to create subprocesses. Have the
        // injected IO panic perhaps?
        //
        // To avoid the Erlang compiler printing to stdout (and thus
        // violating LSP which is currently using stdout) we silence it.
        project_compiler.subprocess_stdio = Stdio::Null;

        Ok(Self {
            locker,
            warnings,
            project_compiler,
            modules: HashMap::new(),
            sources: HashMap::new(),
            dependencies_compiled: false,
        })
    }

    pub fn compile(&mut self) -> Result<Vec<PathBuf>, Error> {
        // Lock the build directory to ensure to ensure we are the only one compiling
        let _lock_guard = self.locker.lock_for_build();

        if !self.dependencies_compiled {
            // TODO: store compiled module info
            self.project_compiler.compile_dependencies()?;
            self.dependencies_compiled = true;
        }

        // Save the state prior to compilation of the root package
        let checkpoint = self.project_compiler.checkpoint();

        // Do that there compilation. We don't use `?` to return early in the
        // event of an error because we _always_ want to do the restoration of
        // state afterwards.
        let result = self.project_compiler.compile_root_package();

        // Restore the state so that later we can compile the root again
        self.project_compiler.restore(checkpoint);

        // Return any error
        let package = result?;
        let mut compiled_modules = Vec::with_capacity(package.modules.len());

        // Store the compiled module information
        for module in package.modules {
            let pathbuf = module.input_path.canonicalize().expect("Canonicalize");
            let path = pathbuf.as_os_str().to_string_lossy().to_string();
            let line_numbers = LineNumbers::new(&module.code);
            let source = ModuleSourceInformation { path, line_numbers };
            _ = self.sources.insert(module.name.to_string(), source);
            _ = self.modules.insert(module.name.to_string(), module);
            compiled_modules.push(pathbuf);
        }

        Ok(compiled_modules)
    }
}

impl<IO, LockerImpl> LspProjectCompiler<IO, LockerImpl> {
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
