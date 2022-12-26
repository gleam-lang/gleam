use std::{
    collections::HashMap,
    path::{Path, PathBuf},
};

use crate::{
    build::{dep_tree, package_compiler::module_name, Origin},
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
        let src = self.root.join("src");
        let test = self.root.join("test");
        let mut sources = Vec::new();

        let mut add_module = |path: PathBuf, dir: &Path, origin: Origin| -> Result<()> {
            let name = module_name(&dir, &path);
            let code = self.io.read(&path)?;
            let mtime = self.io.modification_time(&path)?;
            sources.push(Source {
                name,
                path,
                code,
                mtime,
                origin,
            });
            Ok(())
        };

        // Src
        for path in self.io.gleam_source_files(&src) {
            add_module(path, &src, Origin::Src)?;
        }

        // Test
        if self.mode.is_dev() {
            for path in self.io.gleam_source_files(&test) {
                add_module(path, &test, Origin::Test)?;
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
