use crate::{
    config,
    fs::{self, ConsoleWarningEmitter, ProjectIO},
    CompilePackage,
};
use camino::Utf8Path;
use gleam_core::{
    build::{Mode, PackageCompiler, StaleTracker, Target, TargetCodegenConfiguration},
    metadata,
    paths::{self, ProjectPaths},
    type_::ModuleInterface,
    uid::UniqueIdGenerator,
    warning::WarningEmitter,
    Result,
};
use smol_str::SmolStr;
use std::sync::Arc;

pub fn command(options: CompilePackage) -> Result<()> {
    let ids = UniqueIdGenerator::new();
    let mut type_manifests = load_libraries(&ids, &options.libraries_directory)?;
    let mut defined_modules = im::HashMap::new();
    let warnings = WarningEmitter::new(Arc::new(ConsoleWarningEmitter));
    let paths = ProjectPaths::new(options.package_directory.clone());
    let config = config::read(paths.root_config())?;

    let target = match options.target {
        Target::Erlang => TargetCodegenConfiguration::Erlang { app_file: None },
        Target::JavaScript => TargetCodegenConfiguration::JavaScript {
            emit_typescript_definitions: false,
            // TODO: prelude error
            prelude_location: options.javascript_prelude.ok_or_else(|| {
                let x: gleam_core::Error = todo!();
                x
            })?,
        },
    };

    tracing::info!("Compiling package");

    let mut compiler = PackageCompiler::new(
        &config,
        Mode::Dev,
        &options.package_directory,
        &options.output_directory,
        &options.libraries_directory,
        &target,
        ids,
        ProjectIO::new(),
    );
    compiler.write_entrypoint = false;
    compiler.write_metadata = true;
    compiler.compile_beam_bytecode = !options.skip_beam_compilation;
    let _ = compiler.compile(
        &warnings,
        &mut type_manifests,
        &mut defined_modules,
        &mut StaleTracker::default(),
    )?;

    Ok(())
}

fn load_libraries(
    ids: &UniqueIdGenerator,
    lib: &Utf8Path,
) -> Result<im::HashMap<SmolStr, ModuleInterface>> {
    tracing::info!("Reading precompiled module metadata files");
    let mut manifests = im::HashMap::new();
    for lib in fs::read_dir(lib)?.filter_map(Result::ok) {
        let path = lib.path().join(paths::ARTEFACT_DIRECTORY_NAME);
        if !path.is_dir() {
            continue;
        }
        for module in fs::module_caches_paths(path)? {
            let reader = fs::buffered_reader(module)?;
            let module = metadata::ModuleDecoder::new(ids.clone()).read(reader)?;
            let _ = manifests.insert(module.name.clone(), module);
        }
    }

    Ok(manifests)
}
