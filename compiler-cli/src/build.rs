use std::time::Instant;

use gleam_core::{Result, build::{Package, ProjectCompiler, Target}};

use crate::{cli, fs};

pub fn main() -> Result<Package> {
    let manifest = crate::dependencies::download(None)?;

    let root_config = crate::config::root_config()?;
    let telemetry = Box::new(cli::Reporter::new());
    let io = fs::ProjectIO::new();
    let start = Instant::now();

    tracing::info!("Compiling packages");
    let compiled = ProjectCompiler::new(root_config, &manifest.packages, telemetry, io)
        .compile(Target::JavaScript)?;

    cli::print_compiled(start.elapsed());
    Ok(compiled)
}
