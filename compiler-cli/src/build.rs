use std::time::Instant;

use gleam_core::{
    build::{Package, ProjectCompiler},
    Result,
};

use crate::{cli, fs};

pub fn main() -> Result<Package> {
    let manifest = crate::dependencies::download()?;

    let root_config = crate::config::root_config()?;
    let telemetry = Box::new(cli::Reporter::new());
    let io = fs::FileSystemAccessor::new();
    let start = Instant::now();

    tracing::info!("Reading package configs from build");
    let configs = crate::config::package_configs(&root_config.name, &manifest)?;

    tracing::info!("Compiling packages");
    let compiled = ProjectCompiler::new(root_config, configs, telemetry, io).compile()?;

    cli::print_compiled(start.elapsed());
    Ok(compiled)
}
