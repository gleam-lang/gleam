use std::{sync::Arc, time::Instant};

use gleam_core::{
    build::{Built, Codegen, Options, ProjectCompiler},
    manifest::Manifest,
    paths::ProjectPaths,
    Result,
};
use gleam_core::build::{NullTelemetry, Telemetry};

use crate::{
    build_lock::BuildLock,
    cli,
    dependencies::UseManifest,
    fs::{self, get_current_directory, ConsoleWarningEmitter},
};

pub fn download_dependencies(no_print_progress: bool) -> Result<Manifest> {
    let paths = crate::project_paths_at_current_directory();
    if no_print_progress {
        crate::dependencies::download(&paths, NullTelemetry, None, UseManifest::Yes)
    } else {
        crate::dependencies::download(&paths, cli::Reporter::new(), None, UseManifest::Yes)
    }
}

pub fn main(options: Options, manifest: Manifest) -> Result<Built> {
    let paths = crate::project_paths_at_current_directory();
    let perform_codegen = options.codegen;
    let root_config = crate::config::root_config()?;
    let no_print_progress = options.no_print_progress;
    let telemetry: Box<dyn Telemetry> = if no_print_progress { Box::new(NullTelemetry) } else { Box::new(cli::Reporter::new()) };
    let io = fs::ProjectIO::new();
    let start = Instant::now();
    let lock = BuildLock::new_target(
        &paths,
        options.mode,
        options.target.unwrap_or(root_config.target),
    )?;
    let current_dir = get_current_directory().expect("Failed to get current directory");

    tracing::info!("Compiling packages");
    let compiled = {
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

    if !no_print_progress {
        match perform_codegen {
            Codegen::All | Codegen::DepsOnly => cli::print_compiled(start.elapsed()),
            Codegen::None => cli::print_checked(start.elapsed()),
        };
    }

    Ok(compiled)
}
