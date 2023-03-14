use std::{sync::Arc, time::Instant};

use gleam_core::{
    build::{Codegen, Options, Package, ProjectCompiler},
    paths::ProjectPaths,
    Result,
};

use crate::{
    build_lock::BuildLock,
    cli,
    dependencies::UseManifest,
    fs::{self, ConsoleWarningEmitter},
};

pub fn main(options: Options) -> Result<Package> {
    let paths = crate::project_paths_at_current_directory();
    let manifest =
        crate::dependencies::download(&paths, cli::Reporter::new(), None, UseManifest::Yes)?;

    let perform_codegen = options.codegen;
    let root_config = crate::config::root_config()?;
    let telemetry = Box::new(cli::Reporter::new());
    let io = fs::ProjectIO::new();
    let start = Instant::now();
    let lock = BuildLock::new_target(
        &paths,
        options.mode,
        options.target.unwrap_or(root_config.target),
    )?;
    let current_dir = std::env::current_dir().expect("Failed to get current directory");

    tracing::info!("Compiling packages");
    let compiled = {
        let _guard = lock.lock(telemetry.as_ref());
        ProjectCompiler::new(
            root_config,
            options,
            manifest.packages,
            telemetry,
            Arc::new(ConsoleWarningEmitter),
            ProjectPaths::new(current_dir),
            io,
        )
        .compile()?
    };

    match perform_codegen {
        Codegen::All | Codegen::DepsOnly => cli::print_compiled(start.elapsed()),
        Codegen::None => cli::print_checked(start.elapsed()),
    };
    Ok(compiled)
}
