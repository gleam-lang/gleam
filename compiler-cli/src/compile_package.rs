use gleam_core::{
    build::{Mode, PackageCompiler},
    metadata,
    type_::Module,
    Result,
};
use std::{collections::HashMap, path::Path};

use crate::{
    config,
    fs::{self, ProjectIO},
    CompilePackage,
};

pub fn command(options: CompilePackage) -> Result<()> {
    let mut type_manifests = load_libraries(&options.libraries_directory)?;
    let mut defined_modules = HashMap::new();
    let mut warnings = Vec::new();
    let config = config::read(options.package_directory.join("gleam.toml"))?;

    tracing::info!("Compiling package");

    let mut compiler = PackageCompiler::new(
        &config,
        &options.package_directory,
        &options.libraries_directory,
        &options.output_directory,
        options.target,
        ProjectIO::new(),
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

fn load_libraries(lib: &Path) -> Result<HashMap<String, Module>> {
    tracing::info!("Reading precompiled module metadata files");
    let mut manifests = HashMap::new();
    for lib in fs::read_dir(lib)?.filter_map(Result::ok) {
        let path = lib.path().join("build");
        if !path.is_dir() {
            continue;
        }
        for module in fs::gleam_modules_metadata_paths(path)? {
            let reader = fs::buffered_reader(module)?;
            let module = metadata::ModuleDecoder::new().read(reader)?;
            let _ = manifests.insert(module.name.join("/"), module);
        }
    }
    Ok(manifests)
}
