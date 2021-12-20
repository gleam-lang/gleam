use std::time::Instant;

use gleam_core::{
    build::{Mode, Package, ProjectCompiler, Target},
    Result,
};

use crate::{cli, fs};

pub fn main(mode: Mode, target: Option<Target>) -> Result<Package> {
    let manifest = crate::dependencies::download(None)?;

    let root_config = crate::config::root_config()?;
    let telemetry = Box::new(cli::Reporter::new());
    let io = fs::ProjectIO::new();
    let start = Instant::now();
    let target = target.unwrap_or(root_config.target);

    tracing::info!("Compiling packages");
    let compiled =
        ProjectCompiler::new(root_config, mode, target, &manifest.packages, telemetry, io)
            .compile()?;

    cli::print_compiled(start.elapsed());
    Ok(compiled)
}
