use crate::{
    config,
    fs::{self, ProjectIO},
    CompilePackage,
};
use gleam_core::{
    build::{Mode, PackageCompiler},
    metadata,
    type_::Module,
    uid::UniqueIdGenerator,
    Result,
};
use std::path::Path;

pub fn command(options: CompilePackage) -> Result<()> {
    let ids = UniqueIdGenerator::new();
    let mut type_manifests = load_libraries(&ids, &options.libraries_directory)?;
    let mut defined_modules = im::HashMap::new();
    let mut warnings = Vec::new();
    let config = config::read(options.package_directory.join("gleam.toml"))?;

    tracing::info!("Compiling package");

    let mut compiler = PackageCompiler::new(
        &config,
        &options.package_directory,
        &options.output_directory,
        &options.libraries_directory,
        options.target,
        ids,
        ProjectIO::new(),
        None,
    );
    compiler.write_entrypoint = false;
    compiler.write_metadata = true;
    compiler.compile_beam_bytecode = !options.skip_beam_compilation;
    compiler.read_source_files(Mode::Dev)?;
    let _ = compiler.compile(&mut warnings, &mut type_manifests, &mut defined_modules)?;

    // Print warnings
    for warning in warnings {
        crate::print_warning(&warning);
    }

    // TODO: Support --warnings-as-errors
    // TODO: Create a Warnings struct to wrap up this functionality

    Ok(())
}

fn load_libraries(ids: &UniqueIdGenerator, lib: &Path) -> Result<im::HashMap<String, Module>> {
    tracing::info!("Reading precompiled module metadata files");
    let mut manifests = im::HashMap::new();
    for lib in fs::read_dir(lib)?.filter_map(Result::ok) {
        let path = lib.path().join("build");
        if !path.is_dir() {
            continue;
        }
        for module in fs::gleam_modules_metadata_paths(path)? {
            let reader = fs::buffered_reader(module)?;
            let module = metadata::ModuleDecoder::new(ids.clone()).read(reader)?;
            let _ = manifests.insert(module.name.join("/"), module);
        }
    }
    Ok(manifests)
}
