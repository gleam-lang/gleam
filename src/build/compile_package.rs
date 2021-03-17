use crate::{
    build::{Origin, PackageCompiler},
    fs::{FileSystemAccessor, FileSystemWriter},
    metadata, CompilePackage, Result,
};
use std::{collections::HashMap, path::PathBuf};

pub fn command(options: CompilePackage) -> Result<()> {
    // TODO: Load precompiled libraries
    tracing::info!("Reading precompiled module metadata files");
    let mut type_manifests = HashMap::new();
    let mut defined_modules = HashMap::new();
    let mut warnings = Vec::new();

    let package = options
        .into_package_compiler_options()
        .into_compiler(FileSystemAccessor::new())?
        .write_metadata(true)
        .compile(&mut warnings, &mut type_manifests, &mut defined_modules)?;

    // TODO: Print warnings
    // TODO: Support --warnings-as-errors

    Ok(())
}
