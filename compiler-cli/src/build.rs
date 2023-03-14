use std::{sync::Arc, time::Instant};

use gleam_core::{
    build::{Codegen, Options, Package, ProjectCompiler},
    type_, Result,
};
use smol_str::SmolStr;

use crate::{
    build_lock::BuildLock,
    cli,
    dependencies::UseManifest,
    fs::{self, ConsoleWarningEmitter},
};

pub struct Built {
    pub root_package: Package,
    pub module_interfaces: im::HashMap<SmolStr, type_::Module>,
}

pub fn main(options: Options) -> Result<Built> {
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
        let root_package = compiler.compile()?;
        Built {
            root_package: root_package,
            module_interfaces: compiler.move_importable_modules(),
        }
    };

    match perform_codegen {
        Codegen::All | Codegen::DepsOnly => cli::print_compiled(start.elapsed()),
        Codegen::None => cli::print_checked(start.elapsed()),
    };
    Ok(compiled)
}
