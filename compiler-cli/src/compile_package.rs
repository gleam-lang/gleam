use crate::{
    CompilePackage, config,
    fs::{self, ConsoleWarningEmitter, ProjectIO},
};
use camino::Utf8Path;
use ecow::EcoString;
use gleam_core::{
    Error, Result,
    build::{
        Mode, NullTelemetry, PackageCompiler, StaleTracker, Target, TargetCodegenConfiguration,
    },
    config::PackageKind,
    metadata,
    paths::{self, ProjectPaths},
    type_::ModuleInterface,
    uid::UniqueIdGenerator,
    warning::WarningEmitter,
};
use std::{collections::HashSet, rc::Rc};

pub fn command(options: CompilePackage) -> Result<()> {
    let ids = UniqueIdGenerator::new();
    let mut type_manifests = load_libraries(&ids, &options.libraries_directory)?;
    let mut defined_modules = im::HashMap::new();
    let warnings = WarningEmitter::new(Rc::new(ConsoleWarningEmitter));
    let paths = ProjectPaths::new(options.package_directory.clone());
    let config = config::read(paths.root_config())?;

    let target = match options.target {
        Target::Erlang => TargetCodegenConfiguration::Erlang { app_file: None },
        Target::JavaScript => TargetCodegenConfiguration::JavaScript {
            emit_typescript_definitions: false,
            prelude_location: options
                .javascript_prelude
                .ok_or_else(|| Error::JavaScriptPreludeRequired)?,
        },
    };

    tracing::info!("Compiling package");

    let mut compiler = PackageCompiler::new(
        PackageKind::Root,
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
    compiler
        .compile(
            &warnings,
            &mut type_manifests,
            &mut defined_modules,
            &mut StaleTracker::default(),
            &mut HashSet::new(),
            &NullTelemetry,
        )
        .into_result()
        .map(|_| ())
}

fn load_libraries(
    ids: &UniqueIdGenerator,
    lib: &Utf8Path,
) -> Result<im::HashMap<EcoString, ModuleInterface>> {
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
