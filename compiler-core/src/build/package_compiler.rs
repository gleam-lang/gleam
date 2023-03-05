use crate::{
    ast::{SrcSpan, TypedModule, UntypedModule},
    build::{
        dep_tree,
        module_loader::SourceDigest,
        native_file_copier::NativeFileCopier,
        package_loader::{CodegenRequired, PackageLoader},
        Mode, Module, Origin, Package, Target,
    },
    codegen::{Erlang, ErlangApp, JavaScript, TypeScriptDeclarations},
    config::PackageConfig,
    error,
    io::{CommandExecutor, FileSystemIO, FileSystemReader, FileSystemWriter, Stdio},
    metadata::ModuleEncoder,
    parse::extra::ModuleExtra,
    paths, type_,
    uid::UniqueIdGenerator,
    Error, Result, Warning,
};
use askama::Template;
use smol_str::SmolStr;
use std::{collections::HashMap, fmt::write, time::SystemTime};
use std::{
    collections::HashSet,
    path::{Path, PathBuf},
};

use super::{ErlangAppCodegenConfiguration, TargetCodegenConfiguration};

#[cfg(not(target_os = "windows"))]
const ELIXIR_EXECUTABLE: &str = "elixir";
#[cfg(target_os = "windows")]
const ELIXIR_EXECUTABLE: &str = "elixir.bat";

#[derive(Debug)]
pub struct PackageCompiler<'a, IO> {
    pub io: IO,
    pub out: &'a Path,
    pub lib: &'a Path,
    pub root: &'a Path,
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
}

impl<'a, IO> PackageCompiler<'a, IO>
where
    IO: FileSystemIO + CommandExecutor + Clone,
{
    pub fn new(
        config: &'a PackageConfig,
        mode: Mode,
        root: &'a Path,
        out: &'a Path,
        lib: &'a Path,
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
        }
    }

    /// Compile the package.
    /// Returns a list of modules that were compiled. Any modules that were read
    /// from the cache will not be returned.
    // TODO: return the cached modules.
    pub fn compile(
        mut self,
        warnings: &mut Vec<Warning>,
        existing_modules: &mut im::HashMap<SmolStr, type_::Module>,
        already_defined_modules: &mut im::HashMap<SmolStr, PathBuf>,
    ) -> Result<Vec<Module>, Error> {
        let span = tracing::info_span!("compile", package = %self.config.name.as_str());
        let _enter = span.enter();

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
            codegen_required,
            &artefact_directory,
            self.target.target(),
            &self.config.name,
            already_defined_modules,
        )
        .run()?;

        // Load the cached modules that have previously been compiled
        for module in loaded.cached.into_iter() {
            _ = existing_modules.insert(module.name.clone(), module.clone());
        }

        // Type check the modules that are new or have changed
        tracing::info!(count=%loaded.to_compile.len(), "type_checking_modules");
        let modules = type_check(
            &self.config.name,
            self.target.target(),
            &self.ids,
            loaded.to_compile,
            existing_modules,
            warnings,
        )?;

        tracing::info!("performing_code_generation");
        self.perform_codegen(&modules)?;
        self.encode_and_write_metadata(&modules)?;

        Ok(modules)
    }

    fn compile_erlang_to_beam(&mut self, modules: &HashSet<PathBuf>) -> Result<(), Error> {
        if modules.is_empty() {
            tracing::info!("no_erlang_to_compile");
            return Ok(());
        }

        tracing::info!("compiling_erlang");

        let escript_path = self
            .out
            .join(paths::ARTEFACT_DIRECTORY_NAME)
            .join("gleam@@compile.erl");
        if !escript_path.exists() {
            let escript_source = std::include_str!("../../templates/gleam@@compile.erl");
            self.io.write(&escript_path, escript_source)?;
        }

        let mut args = vec![
            escript_path.to_string_lossy().to_string(),
            // Tell the compiler where to find other libraries
            "--lib".into(),
            self.lib.to_string_lossy().to_string(),
            // Write compiled .beam to ./ebin
            "--out".into(),
            self.out.join("ebin").to_string_lossy().to_string(),
        ];
        // Add the list of modules to compile
        for module in modules {
            let path = self.out.join(paths::ARTEFACT_DIRECTORY_NAME).join(module);
            args.push(path.to_string_lossy().to_string());
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
        destination_dir: &Path,
        to_compile_modules: &mut HashSet<PathBuf>,
    ) -> Result<(), Error> {
        tracing::info!("copying_native_source_files");

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
            maybe_link_elixir_libs(&self.io, &self.lib.to_path_buf(), self.subprocess_stdio)?;
        }

        Ok(())
    }

    fn encode_and_write_metadata(&mut self, modules: &[Module]) -> Result<()> {
        if !self.write_metadata {
            tracing::info!("package_metadata_writing_disabled");
            return Ok(());
        }
        if modules.is_empty() {
            return Ok(());
        }

        let artefact_dir = self.out.join(paths::ARTEFACT_DIRECTORY_NAME);

        tracing::info!("writing_module_caches");
        for module in modules {
            let module_name = module.name.replace('/', "@");

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
                digest: SourceDigest::new(&module.code),
            };
            self.io.write_bytes(&path, &info.to_binary())?;
        }
        Ok(())
    }

    fn perform_codegen(&mut self, modules: &[Module]) -> Result<()> {
        if !self.perform_codegen {
            tracing::info!("skipping_codegen");
            return Ok(());
        }

        match self.target {
            TargetCodegenConfiguration::JavaScript {
                emit_typescript_definitions,
            } => self.perform_javascript_codegen(modules, *emit_typescript_definitions),
            TargetCodegenConfiguration::Erlang { app_file } => {
                self.perform_erlang_codegen(modules, app_file.as_ref())
            }
        }
    }

    fn perform_erlang_codegen(
        &mut self,
        modules: &[Module],
        app_file: Option<&ErlangAppCodegenConfiguration>,
    ) -> Result<(), Error> {
        let mut written = HashSet::new();
        let build_dir = self.out.join(paths::ARTEFACT_DIRECTORY_NAME);
        let include_dir = self.out.join("include");
        let io = self.io.clone();

        io.mkdir(&build_dir)?;

        if self.copy_native_files {
            self.copy_project_native_files(&build_dir, &mut written)?;
        } else {
            tracing::info!("skipping_native_file_copying");
        }

        if let Some(config) = app_file {
            ErlangApp::new(&self.out.join("ebin"), config.include_dev_deps).render(
                io.clone(),
                &self.config,
                modules,
            )?;
        }

        if self.compile_beam_bytecode && self.write_entrypoint {
            self.render_erlang_entrypoint_module(&build_dir, &mut written)?;
        } else {
            tracing::info!("skipping_entrypoint_generation");
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
            tracing::info!("skipping_erlang_bytecode_compilation");
        }
        Ok(())
    }

    fn perform_javascript_codegen(
        &mut self,
        modules: &[Module],
        typescript: bool,
    ) -> Result<(), Error> {
        let mut written = HashSet::new();
        let typescript = if typescript {
            TypeScriptDeclarations::Emit
        } else {
            TypeScriptDeclarations::None
        };

        JavaScript::new(&self.out, typescript).render(&self.io, modules)?;

        if self.copy_native_files {
            self.copy_project_native_files(&self.out, &mut written)?;
        } else {
            tracing::info!("skipping_native_file_copying");
        }

        Ok(())
    }

    fn render_erlang_entrypoint_module(
        &mut self,
        out: &Path,
        modules_to_compile: &mut HashSet<PathBuf>,
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

fn type_check(
    package_name: &SmolStr,
    target: Target,
    ids: &UniqueIdGenerator,
    mut parsed_modules: Vec<UncompiledModule>,
    module_types: &mut im::HashMap<SmolStr, type_::Module>,
    warnings: &mut Vec<Warning>,
) -> Result<Vec<Module>, Error> {
    let mut modules = Vec::with_capacity(parsed_modules.len() + 1);

    // Insert the prelude
    // DUPE: preludeinsertion
    // TODO: Currently we do this here and also in the tests. It would be better
    // to have one place where we create all this required state for use in each
    // place.
    let _ = module_types.insert("gleam".into(), type_::build_prelude(ids));

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
        let mut type_warnings = Vec::new();
        let ast = crate::analyse::infer_module(
            target,
            ids,
            ast,
            origin,
            package_name,
            module_types,
            &mut type_warnings,
        )
        .map_err(|error| Error::Type {
            path: path.clone(),
            src: code.clone(),
            error,
        })?;

        // Register any warnings emitted as type warnings
        let type_warnings = type_warnings
            .into_iter()
            .map(|w| w.into_warning(path.clone(), code.clone()));
        warnings.extend(type_warnings);

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

pub fn maybe_link_elixir_libs<IO: CommandExecutor + FileSystemIO + Clone>(
    io: &IO,
    build_dir: &PathBuf,
    subprocess_stdio: Stdio,
) -> Result<(), Error> {
    // These Elixir core libs will be loaded with the current project
    // Each should be linked into build/{target}/erlang if:
    // - It isn't already
    // - Its ebin dir doesn't exist (e.g. Elixir's path changed)
    let elixir_libs = ["eex", "elixir", "logger", "mix"];

    // The pathfinder is a file in build/{target}/erlang
    // It contains the full path for each Elixir core lib we need, new-line delimited
    // The pathfinder saves us from repeatedly loading Elixir to get this info
    let mut update_links = false;
    let pathfinder_name = "gleam_elixir_paths";
    let pathfinder = build_dir.join(pathfinder_name);
    if !io.is_file(&pathfinder) {
        // The pathfinder must be written
        // Any existing core lib links will get updated
        update_links = true;
        // TODO: test
        let env = [("TERM", "dumb".into())];
        // Prepare the libs for Erlang's code:lib_dir function
        let elixir_atoms: Vec<String> = elixir_libs.iter().map(|lib| format!(":{}", lib)).collect();
        // Use Elixir to find its core lib paths and write the pathfinder file
        let args = [
            "--eval".into(),
            format!(
                ":ok = File.write(~s({}), [{}] |> Stream.map(fn(lib) -> lib |> :code.lib_dir |> Path.expand end) |> Enum.join(~s(\\n)))",
                pathfinder_name,
                elixir_atoms.join(", "),
            )
            .into(),
        ];
        tracing::debug!("writing_elixir_paths_to_build");
        let status = io.exec(
            ELIXIR_EXECUTABLE,
            &args,
            &env,
            Some(&build_dir),
            subprocess_stdio,
        )?;
        if status != 0 {
            return Err(Error::ShellCommand {
                program: "elixir".into(),
                err: None,
            });
        }
    }

    // Each pathfinder line is a system path for an Elixir core library
    let read_pathfinder = io.read(&pathfinder)?;
    for lib_path in read_pathfinder.split('\n') {
        let source = PathBuf::from(lib_path);
        let name = source
            .as_path()
            .file_name()
            .expect(&format!("Unexpanded path in {}", pathfinder_name));
        let dest = build_dir.join(name);
        let ebin = dest.join("ebin");
        if !update_links || io.is_directory(&ebin) {
            // Either links don't need updating
            // Or this library is already linked
            continue;
        }
        // TODO: unit test
        if io.is_directory(&dest) {
            // Delete the existing link
            io.delete(&dest)?;
        }
        tracing::debug!(
            "linking_{}_to_build",
            name.to_str().unwrap_or("elixir_core_lib"),
        );
        io.symlink_dir(&source, &dest)?;
    }

    Ok(())
}

pub(crate) fn module_name(package_path: &Path, full_module_path: &Path) -> SmolStr {
    // /path/to/project/_build/default/lib/the_package/src/my/module.gleam

    // my/module.gleam
    let mut module_path = full_module_path
        .strip_prefix(package_path)
        .expect("Stripping package prefix from module path")
        .to_path_buf();

    // my/module
    let _ = module_path.set_extension("");

    // Stringify
    let name = module_path
        .to_str()
        .expect("Module name path to str")
        .to_string();

    // normalise windows paths
    name.replace("\\", "/").into()
}

#[derive(Debug)]
pub(crate) enum Input {
    New(UncompiledModule),
    Cached(CachedModule),
}

impl Input {
    pub fn name(&self) -> &SmolStr {
        match self {
            Input::New(m) => &m.name,
            Input::Cached(m) => &m.name,
        }
    }

    pub fn source_path(&self) -> &Path {
        match self {
            Input::New(m) => &m.path,
            Input::Cached(m) => &m.source_path,
        }
    }

    pub fn dependencies(&self) -> Vec<SmolStr> {
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
    pub name: SmolStr,
    pub origin: Origin,
    pub dependencies: Vec<SmolStr>,
    pub source_path: PathBuf,
}

#[derive(Debug, serde::Serialize, serde::Deserialize)]
pub(crate) struct CacheMetadata {
    pub mtime: SystemTime,
    pub codegen_performed: bool,
    pub dependencies: Vec<SmolStr>,
    pub digest: SourceDigest,
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
    pub cached: Vec<type_::Module>,
}

#[derive(Debug, PartialEq, Eq)]
pub(crate) struct UncompiledModule {
    pub path: PathBuf,
    pub name: SmolStr,
    pub code: SmolStr,
    pub mtime: SystemTime,
    pub origin: Origin,
    pub package: SmolStr,
    pub dependencies: Vec<(SmolStr, SrcSpan)>,
    pub ast: UntypedModule,
    pub extra: ModuleExtra,
}

#[derive(Template)]
#[template(path = "gleam@@main.erl", escape = "none")]
struct ErlangEntrypointModule<'a> {
    application: &'a str,
}
