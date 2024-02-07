#[cfg(test)]
mod tests;

use std::{
    collections::{HashMap, HashSet},
    time::{Duration, SystemTime},
};

use camino::{Utf8Path, Utf8PathBuf};

// TODO: emit warnings for cached modules even if they are not compiled again.

use ecow::EcoString;
use itertools::Itertools;

use crate::{
    build::{module_loader::ModuleLoader, package_compiler::module_name, Module, Origin},
    config::PackageConfig,
    dep_tree,
    error::{FileIoAction, FileKind},
    io::{CommandExecutor, FileSystemReader, FileSystemWriter},
    metadata, type_,
    uid::UniqueIdGenerator,
    warning::WarningEmitter,
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
    root: &'a Utf8Path,
    warnings: &'a WarningEmitter,
    codegen: CodegenRequired,
    artefact_directory: &'a Utf8Path,
    package_name: &'a EcoString,
    target: Target,
    stale_modules: &'a mut StaleTracker,
    already_defined_modules: &'a mut im::HashMap<EcoString, Utf8PathBuf>,
}

impl<'a, IO> PackageLoader<'a, IO>
where
    IO: FileSystemWriter + FileSystemReader + CommandExecutor + Clone,
{
    pub(crate) fn new(
        io: IO,
        ids: UniqueIdGenerator,
        mode: Mode,
        root: &'a Utf8Path,
        warnings: &'a WarningEmitter,
        codegen: CodegenRequired,
        artefact_directory: &'a Utf8Path,
        target: Target,
        package_name: &'a EcoString,
        stale_modules: &'a mut StaleTracker,
        already_defined_modules: &'a mut im::HashMap<EcoString, Utf8PathBuf>,
    ) -> Self {
        Self {
            io,
            ids,
            mode,
            root,
            warnings,
            codegen,
            target,
            package_name,
            artefact_directory,
            stale_modules,
            already_defined_modules,
        }
    }

    pub(crate) fn run(mut self) -> Result<Loaded> {
        // First read the source files. This will use the `ModuleLoader`, which
        // will check the mtimes and hashes of sources and caches to determine
        // which should be loaded.
        let mut inputs = self.read_sources_and_caches()?;

        // Determine order in which modules are to be processed
        let deps = inputs
            .values()
            .map(|m| (m.name().clone(), m.dependencies()))
            // Making sure that the module order is deterministic, to prevent different
            // compilations of the same project compiling in different orders. This could impact
            // any bugged outcomes, though not any where the compiler is working correctly, so it's
            // mostly to aid debugging.
            .sorted_by(|(a, _), (b, _)| a.cmp(b))
            .collect();
        let sequence = dep_tree::toposort_deps(deps).map_err(convert_deps_tree_error)?;

        // Now that we have loaded sources and caches we check to see if any of
        // the caches need to be invalidated because their dependencies have
        // changed.
        let mut loaded = Loaded::default();
        for name in sequence {
            let input = inputs
                .remove(&name)
                .expect("Getting parsed module for name");

            match input {
                // A new uncached module is to be compiled
                Input::New(module) => {
                    tracing::debug!(module = %module.name, "module_to_be_compiled");
                    self.stale_modules.add(module.name.clone());
                    loaded.to_compile.push(module);
                }

                // A cached module with dependencies that are stale must be
                // recompiled as the changes in the dependencies may have affect
                // the output, making the cache invalid.
                Input::Cached(info) if self.stale_modules.includes_any(&info.dependencies) => {
                    tracing::debug!(module = %info.name, "module_to_be_compiled");
                    self.stale_modules.add(info.name.clone());
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

    fn load_cached_module(&self, info: CachedModule) -> Result<type_::ModuleInterface, Error> {
        let path = self
            .artefact_directory
            .join(info.name.replace("/", "@").as_ref())
            .with_extension("cache");
        let bytes = self.io.read_bytes(&path)?;
        metadata::ModuleDecoder::new(self.ids.clone()).read(bytes.as_slice())
    }

    pub fn is_gleam_path(&self, path: &Utf8Path, dir: &Utf8Path) -> bool {
        use regex::Regex;
        use std::cell::OnceCell;
        const RE: OnceCell<Regex> = OnceCell::new();

        RE.get_or_init(|| {
            Regex::new(&format!(
                "^({module}{slash})*{module}\\.gleam$",
                module = "[a-z][_a-z0-9]*",
                slash = "(/|\\\\)",
            ))
            .expect("is_gleam_path() RE regex")
        })
        .is_match(
            path.strip_prefix(dir)
                .expect("is_gleam_path(): strip_prefix")
                .as_str(),
        )
    }

    fn read_sources_and_caches(&self) -> Result<HashMap<EcoString, Input>> {
        let span = tracing::info_span!("load");
        let _enter = span.enter();

        let mut inputs = Inputs::new(self.already_defined_modules);

        let src = self.root.join("src");
        let mut loader = ModuleLoader {
            io: self.io.clone(),
            warnings: self.warnings,
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
            if !self.is_gleam_path(&path, &src) {
                self.warnings.emit(crate::Warning::InvalidSource { path });
                continue;
            }
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
pub struct StaleTracker(HashSet<EcoString>);

impl StaleTracker {
    fn add(&mut self, name: EcoString) {
        _ = self.0.insert(name);
    }

    fn includes_any(&self, names: &[EcoString]) -> bool {
        names.iter().any(|n| self.0.contains(n.as_str()))
    }

    pub fn empty(&mut self) {
        let _ = self.0.drain(); // Clears the set but retains allocated memory
    }
}

#[derive(Debug)]
pub struct Inputs<'a> {
    collection: HashMap<EcoString, Input>,
    already_defined_modules: &'a im::HashMap<EcoString, Utf8PathBuf>,
}

impl<'a> Inputs<'a> {
    fn new(already_defined_modules: &'a im::HashMap<EcoString, Utf8PathBuf>) -> Self {
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
