use std::{
    collections::{HashMap, HashSet},
    path::{Path, PathBuf},
    time::{Duration, SystemTime},
};

use itertools::Itertools;

use crate::{
    build::{dep_tree, package_compiler::module_name, seconds_since_unix_epoch, Module, Origin},
    config::PackageConfig,
    error::{FileIoAction, FileKind},
    io::{CommandExecutor, FileSystemIO},
    Error, Result,
};

use super::{
    package_compiler::{CacheMetadata, CachedModule, Input, Loaded, UncompiledModule},
    Mode, Target,
};

#[derive(Debug)]
pub struct PackageLoader<'a, IO> {
    io: IO,
    mode: Mode,
    root: &'a Path,
    artefact_directory: &'a Path,
    package_name: &'a str,
    target: Target,
    already_defined_modules: &'a mut im::HashMap<String, PathBuf>,
}

impl<'a, IO> PackageLoader<'a, IO>
where
    IO: FileSystemIO + CommandExecutor + Clone,
{
    pub(crate) fn new(
        io: IO,
        mode: Mode,
        root: &'a Path,
        artefact_directory: &'a Path,
        target: Target,
        package_name: &'a str,
        already_defined_modules: &'a mut im::HashMap<String, PathBuf>,
    ) -> Self {
        Self {
            io,
            mode,
            root,
            target,
            package_name,
            artefact_directory,
            already_defined_modules,
        }
    }

    pub(crate) fn run(mut self) -> Result<Loaded> {
        let mut inputs = self.read_source_files()?;

        // Determine order in which modules are to be processed
        let deps = inputs
            .values()
            .map(|m| (m.name().to_string(), m.dependencies()))
            .collect();
        let sequence = dep_tree::toposort_deps(deps).map_err(convert_deps_tree_error)?;

        let mut loaded = Loaded::default();
        let mut stale = HashSet::new();
        for name in sequence {
            let input = inputs
                .remove(&name)
                .expect("Getting parsed module for name");

            match input {
                // A new uncached module is to be compiled
                // TODO: test
                Input::New(module) => {
                    _ = stale.insert(module.name.clone());
                    loaded.to_compile.push(module);
                }

                // A cached module with dependencies that are stale must be
                // recompiled as the changes in the dependencies may have affect
                // the output, making the cache invalid.
                // TODO: implement
                // TODO: test
                // Input::Cached(cached) if any_stale(stale, cached.dependencies) => {
                //     _ = stale.insert(cached.name.as_str());
                //     let module = self.load_and_parse(cached)?;
                //     loaded.to_compile.push(module);
                // }

                // A cached module with no stale dependencies can be used as-is
                // and does not need to be recompiled.
                // TODO: test
                Input::Cached(cached) => {
                    // TODO: read metadata
                    loaded.cached.push(());
                }
            }
        }

        Ok(loaded)
    }

    fn read_source_files(&self) -> Result<HashMap<String, Input>> {
        let span = tracing::info_span!("load", package = %self.package_name);
        let _enter = span.enter();
        tracing::info!("Reading source files");

        let mut inputs = Inputs::new(self.already_defined_modules);

        let src = self.root.join("src");
        let mut loader = ModuleLoader {
            io: self.io.clone(),
            mode: self.mode,
            target: self.target,
            package_name: self.package_name,
            artefact_directory: self.artefact_directory,
            source_directory: &src,
            origin: Origin::Src,
        };

        // Src
        for path in self.io.gleam_source_files(&src) {
            let input = loader.load(path)?;
            inputs.insert(input)?;
        }

        // Test
        if self.mode.is_dev() {
            let test = self.root.join("test");
            loader.origin = Origin::Test;
            loader.source_directory = &test;

            for path in self.io.gleam_source_files(&test) {
                let input = loader.load(path)?;
                inputs.insert(input)?;
            }
        }

        Ok(inputs.collection)
    }
}

fn convert_deps_tree_error(e: dep_tree::Error) -> Error {
    match e {
        dep_tree::Error::Cycle(modules) => Error::ImportCycle { modules },
    }
}

#[derive(Debug)]
struct ModuleLoader<'a, IO> {
    io: IO,
    mode: Mode,
    target: Target,
    package_name: &'a str,
    source_directory: &'a Path,
    artefact_directory: &'a Path,
    origin: Origin,
}

impl<'a, IO> ModuleLoader<'a, IO>
where
    IO: FileSystemIO + CommandExecutor + Clone,
{
    /// Load a module from the given path.
    ///
    /// If the module has been compiled before and the source file has not been
    /// changed since then, load the precompiled data instead.
    ///
    /// Whether the module has changed or not is determined by comparing the
    /// modification time of the source file with the value recorded in the
    /// `.timestamp` file in the artefact directory.
    fn load(&self, path: PathBuf) -> Result<Input> {
        let name = module_name(self.source_directory, &path);
        let artefact = name.replace("/", "@");
        let source_mtime = self.io.modification_time(&path)?;

        Ok(match self.read_cache_metadata(&artefact)? {
            // If there's a timestamp and it's newer than the source file
            // modification time then we read the cached data.
            Some(meta) if meta.mtime <= source_mtime => Input::Cached(self.cached(name, meta)),

            // Otherwise we read the source file to compile it.
            _ => Input::New(self.read_source(path, name, source_mtime)?),
        })
    }

    /// Read the timestamp file from the artefact directory for the given
    /// artefact slug. If the file does not exist, return `None`.
    fn read_cache_metadata(&self, artefact: &str) -> Result<Option<CacheMetadata>> {
        let meta_path = self
            .artefact_directory
            .join(artefact)
            .with_extension("cache_meta");

        if !self.io.is_file(&meta_path) {
            return Ok(None);
        }

        let binary = self.io.read_bytes(&meta_path)?;
        let cache_metadata = CacheMetadata::from_binary(&binary).map_err(|e| -> Error {
            Error::FileIo {
                action: FileIoAction::Parse,
                kind: FileKind::File,
                path: meta_path,
                err: Some(e),
            }
        })?;
        Ok(Some(cache_metadata))
    }

    fn read_source(
        &self,
        path: PathBuf,
        name: String,
        mtime: SystemTime,
    ) -> Result<UncompiledModule, Error> {
        let code = self.io.read(&path)?;

        let (mut ast, extra) = crate::parse::parse_module(&code).map_err(|error| Error::Parse {
            path: path.clone(),
            src: code.clone(),
            error,
        })?;

        let dependencies = ast.dependencies(self.target);

        // TODO: store the name on the AST as a string.
        ast.name = name.split("/").map(String::from).collect();
        let module = UncompiledModule {
            package: self.package_name.to_string(),
            origin: self.origin,
            dependencies,
            extra,
            mtime,
            path,
            name: name.clone(),
            code,
            ast,
        };
        Ok(module)
    }

    fn cached(&self, name: String, meta: CacheMetadata) -> CachedModule {
        CachedModule {
            dependencies: meta.dependencies,
            source_path: self.source_directory.join(format!("{}.gleam", name)),
            name,
        }
    }
}

#[derive(Debug)]
struct Inputs<'a> {
    collection: HashMap<String, Input>,
    already_defined_modules: &'a im::HashMap<String, PathBuf>,
}

impl<'a> Inputs<'a> {
    fn new(already_defined_modules: &'a im::HashMap<String, PathBuf>) -> Self {
        Self {
            collection: Default::default(),
            already_defined_modules,
        }
    }

    /// Insert a module into the hashmap. If there is already a module with the
    /// same name then an error is returned.
    fn insert(&mut self, input: Input) -> Result<()> {
        let name = input.name().to_string();

        if let Some(first) = self.already_defined_modules.get(&name) {
            return Err(Error::DuplicateModule {
                module: name.clone(),
                first: first.to_path_buf(),
                second: input.source_path().to_path_buf(),
            });
        }

        let second = input.source_path().to_path_buf();
        if let Some(first) = self.collection.insert(name.clone(), input) {
            return Err(Error::DuplicateModule {
                module: name.clone(),
                first: first.source_path().to_path_buf(),
                second,
            });
        }

        Ok(())
    }
}
