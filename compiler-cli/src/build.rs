use std::{collections::HashSet, rc::Rc, time::Instant};

use gleam_core::{
    Result,
    build::{Built, Codegen, Mode, NullTelemetry, Options, ProjectCompiler, Telemetry},
    config::PackageConfig,
    manifest::{Manifest, ManifestPackage},
    paths::ProjectPaths,
    warning::WarningEmitterIO,
};

use crate::{
    build_lock::BuildLock,
    cli, dependencies,
    fs::{self, ConsoleWarningEmitter},
};

pub fn download_dependencies(paths: &ProjectPaths, telemetry: impl Telemetry) -> Result<Manifest> {
    dependencies::resolve_and_download(
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

    let packages = match options.mode {
        Mode::Prod => prod_only_packages(&root_config, manifest.packages),
        Mode::Dev | Mode::Lsp => manifest.packages,
    };

    tracing::info!("Compiling packages");
    let result = {
        let _guard = lock.lock(telemetry);
        let compiler = ProjectCompiler::new(
            root_config,
            options,
            packages,
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

/// Filter manifest packages to only those transitively reachable from
/// production dependencies, excluding dev-only dependencies.
fn prod_only_packages(
    config: &PackageConfig,
    packages: Vec<ManifestPackage>,
) -> Vec<ManifestPackage> {
    let index: std::collections::HashMap<&str, &ManifestPackage> =
        packages.iter().map(|p| (p.name.as_str(), p)).collect();

    let mut needed: HashSet<String> = HashSet::new();
    let mut queue: Vec<String> = config.dependencies.keys().map(|s| s.to_string()).collect();

    while let Some(name) = queue.pop() {
        if needed.insert(name.clone()) {
            if let Some(pkg) = index.get(name.as_str()) {
                for req in &pkg.requirements {
                    queue.push(req.to_string());
                }
            }
        }
    }

    packages
        .into_iter()
        .filter(|p| needed.contains(p.name.as_str()))
        .collect()
}
