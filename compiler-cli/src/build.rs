use std::{rc::Rc, time::Instant};

use gleam_core::{
    build::{Built, Codegen, NullTelemetry, Options, ProjectCompiler, Telemetry},
    manifest::Manifest,
    paths::ProjectPaths,
    warning::WarningEmitterIO,
    Result,
};

use crate::{
    build_lock::BuildLock,
    cli,
    dependencies::UseManifest,
    fs::{self, get_current_directory, get_project_root, ConsoleWarningEmitter},
};

pub fn download_dependencies(telemetry: impl Telemetry) -> Result<Manifest> {
    let paths = crate::find_project_paths()?;
    crate::dependencies::download(&paths, telemetry, None, Vec::new(), UseManifest::Yes)
}

pub fn main(options: Options, manifest: Manifest) -> Result<Built> {
    main_with_warnings(options, manifest, Rc::new(ConsoleWarningEmitter))
}

pub(crate) fn main_with_warnings(
    options: Options,
    manifest: Manifest,
    warnings: Rc<dyn WarningEmitterIO>,
) -> Result<Built> {
    let paths = crate::find_project_paths()?;
    let perform_codegen = options.codegen;
    let root_config = crate::config::root_config()?;
    let telemetry: &'static dyn Telemetry = if options.no_print_progress {
        &NullTelemetry
    } else {
        &cli::Reporter
    };
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
        let _guard = lock.lock(telemetry);
        let compiler = ProjectCompiler::new(
            root_config,
            options,
            manifest.packages,
            telemetry,
            warnings,
            ProjectPaths::new(current_dir),
            io,
        );
        compiler.compile()?
    };

    match perform_codegen {
        Codegen::All | Codegen::DepsOnly => telemetry.compiled_package(start.elapsed()),
        Codegen::None => telemetry.checked_package(start.elapsed()),
    };

    Ok(result)
}
