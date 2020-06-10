use crate::{
    build::{
        dep_tree, package_analyser::PackageAnalyser, project_root::ProjectRoot, Origin, Package,
    },
    config::PackageConfig,
    error::{Error, GleamExpect},
    typ,
};
use std::{collections::HashMap, path::PathBuf};

#[derive(Debug)]
pub struct ProjectAnalyser<'a> {
    root: &'a ProjectRoot,
    root_config: PackageConfig,
    configs: HashMap<String, PackageConfig>,
    packages: HashMap<String, Package>,
    type_manifests: HashMap<String, typ::Module>,
    defined_modules: HashMap<String, PathBuf>,
}

// TODO: test top level package has test modules analysed
// TODO: test that tests cannot be imported into src

impl<'a> ProjectAnalyser<'a> {
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

    pub fn analyse(mut self) -> Result<HashMap<String, Package>, Error> {
        // Determine package processing order
        let sequence = order_packges(&self.configs)?;

        // Read and type check deps packages
        for name in sequence.into_iter() {
            let config = self
                .configs
                .remove(name.as_str())
                .gleam_expect("Missing package config");
            self.analyse_package(name, config, SourceLocations::Src)?;
        }

        // Read and type check top level package
        let root_config = std::mem::replace(&mut self.root_config, Default::default());
        let name = root_config.name.clone();
        self.analyse_package(name, root_config, SourceLocations::SrcAndTest)?;

        Ok(self.packages)
    }

    fn analyse_package(
        &mut self,
        name: String,
        config: PackageConfig,
        locations: SourceLocations,
    ) -> Result<(), Error> {
        let mut analyser = PackageAnalyser::new(self.root, config);

        // Read source files
        analyser.read_package_source_files(Origin::Src)?;
        if locations == SourceLocations::SrcAndTest {
            analyser.read_package_source_files(Origin::Test)?;
        }

        // Parse and type check
        let analysed = analyser.analyse(&mut self.type_manifests, &mut self.defined_modules)?;
        self.packages.insert(name, analysed);
        Ok(())
    }
}

#[derive(Debug, PartialEq)]
enum SourceLocations {
    Src,
    SrcAndTest,
}

fn order_packges(configs: &HashMap<String, PackageConfig>) -> Result<Vec<String>, Error> {
    dep_tree::toposort_deps(configs.values().map(package_deps_for_graph).collect())
        .map_err(convert_deps_tree_error)
}

fn convert_deps_tree_error(e: dep_tree::Error) -> Error {
    todo!()
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
