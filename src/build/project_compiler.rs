use codegen::ErlangApp;

use crate::{
    build::{
        dep_tree, package_compiler::PackageCompiler, project_root::ProjectRoot, Origin, Package,
    },
    codegen,
    config::PackageConfig,
    fs::FileSystemAccessor,
    typ, Error, GleamExpect,
};
use std::{collections::HashMap, path::PathBuf};

use super::package_compiler;

#[derive(Debug)]
pub struct ProjectCompiler<'a> {
    root: &'a ProjectRoot,
    root_config: PackageConfig,
    configs: HashMap<String, PackageConfig>,
    packages: HashMap<String, Package>,
    type_manifests: HashMap<String, (Origin, typ::Module)>,
    defined_modules: HashMap<String, PathBuf>,
}

// TODO: test top level package has test modules compiled
// TODO: test that tests cannot be imported into src
// TODO: test that dep cycles are not allowed between packages

impl<'a> ProjectCompiler<'a> {
    pub fn new(
        root: &'a ProjectRoot,
        root_config: PackageConfig,
        configs: HashMap<String, PackageConfig>,
    ) -> Self {
        let estimated_number_of_modules = configs.len() * 5;
        Self {
            packages: HashMap::with_capacity(configs.len()),
            type_manifests: HashMap::with_capacity(estimated_number_of_modules),
            defined_modules: HashMap::with_capacity(estimated_number_of_modules),
            root_config,
            configs,
            root,
        }
    }

    pub fn compile(mut self) -> Result<HashMap<String, Package>, Error> {
        // Determine package processing order
        let sequence = order_packages(&self.configs)?;

        // Read and type check deps packages
        for name in sequence.into_iter() {
            let config = self
                .configs
                .remove(name.as_str())
                .gleam_expect("Missing package config");
            self.compile_package(name, config, SourceLocations::Src)?;
        }

        // Read and type check top level package
        let root_config = std::mem::replace(&mut self.root_config, Default::default());
        let name = root_config.name.clone();
        self.compile_package(name, root_config, SourceLocations::SrcAndTest)?;

        Ok(self.packages)
    }

    fn compile_package(
        &mut self,
        name: String,
        config: PackageConfig,
        locations: SourceLocations,
    ) -> Result<(), Error> {
        crate::cli::print_compiling(name.as_str());
        let test_path = match locations {
            SourceLocations::SrcAndTest => {
                Some(self.root.default_build_lib_package_test_path(&name))
            }
            _ => None,
        };

        // TODO: this isn't the right location. We may want multiple output locations.
        let out_path = self.root.default_build_lib_package_src_path(&name);
        let options = package_compiler::Options {
            src_path: self.root.default_build_lib_package_src_path(&name),
            out_path: out_path.clone(),
            test_path,
            name: name.clone(),
        };

        let mut compiler = options.into_compiler(FileSystemAccessor::new())?;

        // Compile project
        let compiled = compiler.compile(&mut self.type_manifests, &mut self.defined_modules)?;
        ErlangApp::new(out_path.as_path()).render(
            &FileSystemAccessor::new(),
            &config,
            compiled.modules.as_slice(),
        )?;

        self.packages.insert(name, compiled);
        Ok(())
    }
}

#[derive(Debug, PartialEq)]
enum SourceLocations {
    Src,
    SrcAndTest,
}

fn order_packages(configs: &HashMap<String, PackageConfig>) -> Result<Vec<String>, Error> {
    dep_tree::toposort_deps(configs.values().map(package_deps_for_graph).collect())
        .map_err(convert_deps_tree_error)
}

fn convert_deps_tree_error(e: dep_tree::Error) -> Error {
    match e {
        dep_tree::Error::Cycle(packages) => Error::PackageCycle { packages },
    }
}

fn package_deps_for_graph(config: &PackageConfig) -> (String, Vec<String>) {
    let name = config.name.to_string();
    let deps: Vec<_> = config
        .dependencies
        .iter()
        .map(|(dep, _)| dep.to_string())
        .collect();
    (name, deps)
}
