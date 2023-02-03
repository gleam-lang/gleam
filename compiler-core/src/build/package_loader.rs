#[cfg(test)]
mod tests;

use std::{
    collections::{HashMap, HashSet},
    path::{Path, PathBuf},
    time::{Duration, SystemTime},
};

// TODO: emit warnings for cached modules even if they are not compiled again.

use itertools::Itertools;
use smol_str::SmolStr;

use crate::{
    build::{dep_tree, module_loader::ModuleLoader, package_compiler::module_name, Module, Origin},
    config::PackageConfig,
    error::{FileIoAction, FileKind},
    io::{CommandExecutor, FileSystemIO},
    metadata, type_,
    uid::UniqueIdGenerator,
    Error, Result,
};

use super::{
    module_loader::read_source,
    package_compiler::{CacheMetadata, CachedModule, Input, Loaded, UncompiledModule},
    Mode, Target,
};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum CodegenRequired {
    Yes,
    No,
}

impl CodegenRequired {
    /// Returns `true` if the codegen required is [`Yes`].
    ///
    /// [`Yes`]: CodegenRequired::Yes
    #[must_use]
    pub fn is_required(&self) -> bool {
        matches!(self, Self::Yes)
    }
}

#[derive(Debug)]
pub struct PackageLoader<'a, IO> {
    io: IO,
    ids: UniqueIdGenerator,
    mode: Mode,
    root: &'a Path,
    codegen: CodegenRequired,
    artefact_directory: &'a Path,
    package_name: &'a SmolStr,
    target: Target,
    already_defined_modules: &'a mut im::HashMap<SmolStr, PathBuf>,
}

impl<'a, IO> PackageLoader<'a, IO>
where
    IO: FileSystemIO + CommandExecutor + Clone,
{
    pub(crate) fn new(
        io: IO,
        ids: UniqueIdGenerator,
        mode: Mode,
        root: &'a Path,
        codegen: CodegenRequired,
        artefact_directory: &'a Path,
        target: Target,
        package_name: &'a SmolStr,
        already_defined_modules: &'a mut im::HashMap<SmolStr, PathBuf>,
    ) -> Self {
        Self {
            io,
            ids,
            mode,
            root,
            codegen,
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
            .map(|m| (m.name().clone(), m.dependencies()))
            .collect();
        let sequence = dep_tree::toposort_deps(deps).map_err(convert_deps_tree_error)?;

        let mut loaded = Loaded::default();
        let mut stale = StaleTracker::default();
        for name in sequence {
            let input = inputs
                .remove(&name)
                .expect("Getting parsed module for name");

            match input {
                // A new uncached module is to be compiled
                Input::New(module) => {
                    tracing::debug!(module = %module.name, "module_to_be_compiled");
                    stale.add(module.name.clone());
                    loaded.to_compile.push(module);
                }

                // A cached module with dependencies that are stale must be
                // recompiled as the changes in the dependencies may have affect
                // the output, making the cache invalid.
                Input::Cached(info) if stale.includes_any(&info.dependencies) => {
                    tracing::debug!(module = %info.name, "module_to_be_compiled");
                    stale.add(info.name.clone());
                    let module = self.load_and_parse(info)?;
                    loaded.to_compile.push(module);
                }

                // A cached module with no stale dependencies can be used as-is
                // and does not need to be recompiled.
                Input::Cached(info) => {
                    tracing::debug!(module = %info.name, "module_to_load_from_cache");
                    let module = self.load_cached_module(info)?;
                    loaded.cached.push(module);
                }
            }
        }

        Ok(loaded)
    }

    fn load_cached_module(&self, info: CachedModule) -> Result<type_::Module, Error> {
        let path = self
            .artefact_directory
            .join(info.name.replace('/', "@"))
            .with_extension("cache");
        let bytes = self.io.read_bytes(&path)?;
        metadata::ModuleDecoder::new(self.ids.clone()).read(bytes.as_slice())
    }

    fn read_source_files(&self) -> Result<HashMap<SmolStr, Input>> {
        let span = tracing::info_span!("load", package = %self.package_name);
        let _enter = span.enter();
        tracing::info!("Reading source files");

        let mut inputs = Inputs::new(self.already_defined_modules);

        let src = self.root.join("src");
        let mut loader = ModuleLoader {
            io: self.io.clone(),
            mode: self.mode,
            target: self.target,
            codegen: self.codegen,
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
        if self.mode.includes_tests() {
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

    fn load_and_parse(&self, cached: CachedModule) -> Result<UncompiledModule> {
        let mtime = self.io.modification_time(&cached.source_path)?;
        read_source(
            self.io.clone(),
            self.target,
            cached.origin,
            cached.source_path,
            cached.name,
            self.package_name.clone(),
            mtime,
        )
    }
}

fn convert_deps_tree_error(e: dep_tree::Error) -> Error {
    match e {
        dep_tree::Error::Cycle(modules) => Error::ImportCycle { modules },
    }
}

#[derive(Debug, Default)]
struct StaleTracker(HashSet<SmolStr>);

impl StaleTracker {
    fn add(&mut self, name: SmolStr) {
        _ = self.0.insert(name);
    }

    fn includes_any(&self, names: &[SmolStr]) -> bool {
        names.iter().any(|n| self.0.contains(n.as_str()))
    }
}

#[derive(Debug)]
pub struct Inputs<'a> {
    collection: HashMap<SmolStr, Input>,
    already_defined_modules: &'a im::HashMap<SmolStr, PathBuf>,
}

impl<'a> Inputs<'a> {
    fn new(already_defined_modules: &'a im::HashMap<SmolStr, PathBuf>) -> Self {
        Self {
            collection: Default::default(),
            already_defined_modules,
        }
    }

    /// Insert a module into the hashmap. If there is already a module with the
    /// same name then an error is returned.
    fn insert(&mut self, input: Input) -> Result<()> {
        let name = input.name().clone();

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
                module: name,
                first: first.source_path().to_path_buf(),
                second,
            });
        }

        Ok(())
    }
}
