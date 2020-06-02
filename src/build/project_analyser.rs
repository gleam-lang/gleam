use crate::{
    build::{dep_tree, package_analyser::PackageAnalyser, project_root::ProjectRoot, Package},
    config::PackageConfig,
    error::{Error, GleamExpect},
};
use std::collections::HashMap;

#[derive(Debug)]
pub struct ProjectAnalyser<'a> {
    root: &'a ProjectRoot,
    configs: &'a HashMap<String, PackageConfig>,
}

impl<'a> ProjectAnalyser<'a> {
    pub fn new(root: &'a ProjectRoot, configs: &'a HashMap<String, PackageConfig>) -> Self {
        Self { root, configs }
    }

    pub fn analyse(self) -> Result<HashMap<String, Package<'a>>, Error> {
        let mut packages = HashMap::with_capacity(self.configs.len());
        let mut module_type_manifests = HashMap::with_capacity(self.configs.len() * 5);

        // Determine package processing order
        let deps: Vec<_> = self.configs.values().map(package_deps_for_graph).collect();
        let sequence = dep_tree::toposort_deps(deps.as_slice()).map_err(convert_deps_tree_error)?;

        // Read and type check packages
        for name in sequence.into_iter() {
            let config = self
                .configs
                .get(name)
                .gleam_expect("Missing package config");
            let mut analyser = PackageAnalyser::new(self.root, config);
            analyser.read_package_source_files()?;
            let analysed = analyser.analyse(&mut module_type_manifests)?;
            packages.insert(name.to_string(), analysed);
        }

        Ok(packages)
    }
}

fn convert_deps_tree_error(e: dep_tree::Error) -> Error {
    todo!()
}

fn package_deps_for_graph(config: &PackageConfig) -> (&str, Vec<&str>) {
    let name = config.name.as_str();
    let deps: Vec<_> = config
        .dependencies
        .iter()
        .map(|(dep, _)| dep.as_str())
        .collect();
    (name, deps)
}
