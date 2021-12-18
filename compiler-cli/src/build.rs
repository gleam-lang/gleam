use std::time::Instant;

use gleam_core::{
    build::{Package, ProjectCompiler, Target},
    Result,
};

use crate::{cli, fs};

pub fn main() -> Result<Package> {
    let manifest = crate::dependencies::download(None)?;

    let root_config = crate::config::root_config()?;
    let telemetry = Box::new(cli::Reporter::new());
    let io = fs::ProjectIO::new();
    let start = Instant::now();
    let target = Target::Erlang;

    tracing::info!("Compiling packages");
    let compiled =
        ProjectCompiler::new(root_config, target, &manifest.packages, telemetry, io).compile()?;

    cli::print_compiled(start.elapsed());
    Ok(compiled)
}
