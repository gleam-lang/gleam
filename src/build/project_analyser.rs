use crate::{
    build::{dep_tree, package_analyser::PackageAnalyser, project_root::ProjectRoot, Package},
    config::PackageConfig,
    error::{Error, GleamExpect},
};
use std::collections::HashMap;

#[derive(Debug)]
pub struct ProjectAnalyser<'a> {
    root: &'a ProjectRoot,
    configs: HashMap<String, PackageConfig>,
}

impl<'a> ProjectAnalyser<'a> {
    pub fn new(root: &'a ProjectRoot, configs: HashMap<String, PackageConfig>) -> Self {
        Self { root, configs }
    }

    pub fn analyse(mut self) -> Result<HashMap<String, Package>, Error> {
        let mut packages = HashMap::with_capacity(self.configs.len());
        let mut module_type_manifests = HashMap::with_capacity(self.configs.len() * 5);

        // Determine package processing order
        let sequence = order_packges(&self.configs)?;

        // Read and type check packages
        for name in sequence.into_iter() {
            let config = self
                .configs
                .remove(name.as_str())
                .gleam_expect("Missing package config");
            let mut analyser = PackageAnalyser::new(self.root, config);
            analyser.read_package_source_files()?;
            let analysed = analyser.analyse(&mut module_type_manifests)?;
            packages.insert(name, analysed);
        }

        Ok(packages)
    }
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
