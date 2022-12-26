use std::{
    collections::HashMap,
    path::{Path, PathBuf},
};

use crate::{
    build::{dep_tree, package_compiler::module_name, Module, Origin},
    config::PackageConfig,
    io::{CommandExecutor, FileSystemIO},
    Error, Result,
};

use super::{
    package_compiler::{Source, UncompiledModule},
    Mode, Target,
};

#[derive(Debug)]
pub struct PackageLoader<'a, IO> {
    io: IO,
    mode: Mode,
    root: &'a Path,
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
            already_defined_modules,
        }
    }

    pub(crate) fn run(mut self) -> Result<Vec<UncompiledModule>> {
        let sources = self.read_source_files()?;

        tracing::info!("Parsing source code");
        let mut sources_map = self.parse_sources(sources)?;

        // Determine order in which modules are to be processed
        let sequence = dep_tree::toposort_deps(
            sources_map
                .values()
                .map(|m| module_deps_for_graph(self.target, m))
                .collect(),
        )
        .map_err(convert_deps_tree_error)?;

        let mut modules = Vec::with_capacity(sequence.len());
        for name in sequence {
            let module = sources_map
                .remove(&name)
                .expect("Getting parsed module for name");
            modules.push(module);
        }

        Ok(modules)
    }

    fn read_source_files(&self) -> Result<Vec<Source>> {
        let span = tracing::info_span!("load", package = %self.package_name);
        let _enter = span.enter();
        tracing::info!("Reading source files");
        let mut sources = Vec::new();

        let src = self.root.join("src");
        let mut loader = ModuleLoader {
            io: self.io.clone(),
            mode: self.mode,
            source_directory: &src,
            origin: Origin::Src,
        };

        // Src
        for path in self.io.gleam_source_files(&src) {
            sources.push(loader.load(path)?);
        }

        // Test
        if self.mode.is_dev() {
            let test = self.root.join("test");
            loader.origin = Origin::Test;
            loader.source_directory = &test;

            for path in self.io.gleam_source_files(&test) {
                sources.push(loader.load(path)?);
            }
        }
        Ok(sources)
    }

    fn parse_sources(&mut self, sources: Vec<Source>) -> Result<HashMap<String, UncompiledModule>> {
        let mut parsed_modules = HashMap::with_capacity(sources.len());
        for Source {
            name,
            code,
            path,
            origin,
            mtime,
        } in sources
        {
            // Ensure there are no modules defined that already have this name
            if let Some(first) = self
                .already_defined_modules
                .insert(name.clone(), path.clone())
            {
                return Err(Error::DuplicateModule {
                    module: name.clone(),
                    first,
                    second: path.clone(),
                });
            }

            let (mut ast, extra) =
                crate::parse::parse_module(&code).map_err(|error| Error::Parse {
                    path: path.clone(),
                    src: code.clone(),
                    error,
                })?;

            // Store the name
            // TODO: store the module name as a string
            ast.name = name.split("/").map(String::from).collect();

            let module = UncompiledModule {
                package: self.package_name.to_string(),
                origin,
                extra,
                mtime,
                path,
                name,
                code,
                ast,
            };

            // Register the parsed module
            let _ = parsed_modules.insert(module.name.clone(), module);
        }
        Ok(parsed_modules)
    }
}

fn module_deps_for_graph(target: Target, module: &UncompiledModule) -> (String, Vec<String>) {
    let name = module.name.clone();
    let deps: Vec<_> = module
        .ast
        .dependencies(target)
        .into_iter()
        .map(|(dep, _span)| dep)
        .collect();
    (name, deps)
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
    source_directory: &'a Path,
    origin: Origin,
}

impl<'a, IO> ModuleLoader<'a, IO>
where
    IO: FileSystemIO + CommandExecutor + Clone,
{
    fn new(io: IO, mode: Mode, source_directory: &'a Path, origin: Origin) -> Self {
        Self {
            io,
            mode,
            source_directory,
            origin,
        }
    }

    fn load(&self, path: PathBuf) -> Result<Source> {
        dbg!(&path);
        let name = module_name(self.source_directory, &path);
        let code = self.io.read(&path)?;
        let mtime = self.io.modification_time(&path)?;
        Ok(Source {
            name,
            path,
            code,
            mtime,
            origin: self.origin,
        })
    }
}
