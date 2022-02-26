use std::time::Instant;

use gleam_core::{
    build::{Options, Package, ProjectCompiler},
    Result,
};

use crate::{cli, fs};

pub fn main(options: Options) -> Result<Package> {
    let manifest = crate::dependencies::download(None)?;

    let perform_codegen = options.perform_codegen;
    let root_config = crate::config::root_config()?;
    let telemetry = Box::new(cli::Reporter::new());
    let io = fs::ProjectIO::new();
    let start = Instant::now();

    tracing::info!("Compiling packages");
    let compiled =
        ProjectCompiler::new(root_config, options, manifest.packages, telemetry, io).compile()?;

    if perform_codegen {
        cli::print_compiled(start.elapsed());
    } else {
        cli::print_checked(start.elapsed());
    }
    Ok(compiled)
}
