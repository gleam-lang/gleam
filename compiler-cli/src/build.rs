use std::{rc::Rc, time::Instant};

use gleam_core::{
    Result,
    build::{Built, Codegen, NullTelemetry, Options, ProjectCompiler, Telemetry},
    manifest::Manifest,
    paths::ProjectPaths,
    warning::WarningEmitterIO,
};

use crate::{
    build_lock::BuildLock,
    cli, dependencies,
    fs::{self, ConsoleWarningEmitter},
};

pub fn download_dependencies(paths: &ProjectPaths, telemetry: impl Telemetry) -> Result<Manifest> {
    dependencies::download(
        paths,
        telemetry,
        None,
        Vec::new(),
        dependencies::DependencyManagerConfig {
            use_manifest: dependencies::UseManifest::Yes,
            check_major_versions: dependencies::CheckMajorVersions::No,
        },
    )
}

pub fn main(paths: &ProjectPaths, options: Options, manifest: Manifest) -> Result<Built> {
    main_with_warnings(paths, options, manifest, Rc::new(ConsoleWarningEmitter))
}

pub(crate) fn main_with_warnings(
    paths: &ProjectPaths,
    options: Options,
    manifest: Manifest,
    warnings: Rc<dyn WarningEmitterIO>,
) -> Result<Built> {
    let perform_codegen = options.codegen;
    let root_config = crate::config::root_config(paths)?;
    let telemetry: &'static dyn Telemetry = if options.no_print_progress {
        &NullTelemetry
    } else {
        &cli::Reporter
    };
    let io = fs::ProjectIO::new();
    let start = Instant::now();
    let lock = BuildLock::new_target(
        paths,
        options.mode,
        options.target.unwrap_or(root_config.target),
    )?;

    tracing::info!("Compiling packages");
    let result = {
        let _guard = lock.lock(telemetry);
        let compiler = ProjectCompiler::new(
            root_config,
            options,
            manifest.packages,
            telemetry,
            warnings,
            paths.clone(),
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
