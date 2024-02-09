use std::{sync::Arc, time::Instant};

use gleam_core::{
    build::{Built, Codegen, Options, ProjectCompiler},
    manifest::Manifest,
    paths::ProjectPaths,
    Result,
};

use crate::{
    build_lock::BuildLock,
    cli,
    dependencies::UseManifest,
    fs::{self, get_current_directory, get_project_root, ConsoleWarningEmitter},
};

pub fn download_dependencies() -> Result<Manifest> {
    let paths = crate::find_project_paths()?;
    crate::dependencies::download(&paths, cli::Reporter::new(), None, UseManifest::Yes)
}

pub fn main(options: Options, manifest: Manifest) -> Result<Built> {
    let paths = crate::find_project_paths()?;
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
    let current_dir = get_project_root(get_current_directory()?)?;

    tracing::info!("Compiling packages");
    let result = {
        let _guard = lock.lock(telemetry.as_ref());
        let compiler = ProjectCompiler::new(
            root_config,
            options,
            manifest.packages,
            telemetry,
            Arc::new(ConsoleWarningEmitter),
            ProjectPaths::new(current_dir),
            io,
        );
        compiler.compile()?
    };

    match perform_codegen {
        Codegen::All | Codegen::DepsOnly => cli::print_compiled(start.elapsed()),
        Codegen::None => cli::print_checked(start.elapsed()),
    };

    Ok(result)
}
