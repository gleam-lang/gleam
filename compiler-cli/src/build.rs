use std::collections::HashMap;

use gleam_core::{
    build::{Package, ProjectCompiler},
    Result,
};

use crate::{cli, fs};

pub fn main() -> Result<HashMap<String, Package>> {
    let root_config = crate::config::root_config()?;
    let telemetry = Box::new(cli::Reporter::new());
    let io = fs::FileSystemAccessor::new();

    tracing::info!("Copying root package to _build");
    crate::copy_root_package_to_build(&root_config)?;

    tracing::info!("Reading package configs from .build");
    let configs = crate::config::package_configs(&root_config.name)?;

    tracing::info!("Compiling packages");
    let packages = ProjectCompiler::new(root_config, configs, telemetry, io).compile()?;

    Ok(packages)
}
