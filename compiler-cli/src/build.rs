use std::{sync::Arc, time::Instant};

use gleam_core::{
    build::{Codegen, Options, Package, ProjectCompiler},
    Result,
};

use crate::{
    build_lock::BuildLock,
    cli,
    dependencies::UseManifest,
    fs::{self, ConsoleWarningEmitter, ProjectIO},
};

pub fn main(options: Options) -> Result<(ProjectCompiler<ProjectIO>, Package)> {
    let manifest = crate::dependencies::download(cli::Reporter::new(), None, UseManifest::Yes)?;

    let perform_codegen = options.codegen;
    let root_config = crate::config::root_config()?;
    let telemetry = Box::new(cli::Reporter::new());
    let io = fs::ProjectIO::new();
    let start = Instant::now();
    let lock = BuildLock::new_target(options.mode, options.target.unwrap_or(root_config.target))?;

    tracing::info!("Compiling packages");
    let compiled = {
        let _guard = lock.lock(telemetry.as_ref());
        let mut compiler = ProjectCompiler::new(
            root_config,
            options,
            manifest.packages,
            telemetry,
            Arc::new(ConsoleWarningEmitter),
            io,
        );
        let compiled_package = compiler.compile()?;
        (compiler, compiled_package)
    };

    match perform_codegen {
        Codegen::All | Codegen::DepsOnly => cli::print_compiled(start.elapsed()),
        Codegen::None => cli::print_checked(start.elapsed()),
    };
    Ok(compiled)
}
