use crate::{
    build::{Origin, PackageCompiler},
    fs::FileSystemAccessor,
    CompilePackage, Result,
};
use std::collections::HashMap;

pub fn command(options: CompilePackage) -> Result<()> {
    // TODO: Load precompiled libraries
    tracing::info!("Reading precompiled module metadata files");
    let mut type_manifests = HashMap::new();
    let mut defined_modules = HashMap::new();

    let mut compiler = options
        .into_package_compiler_options()
        .into_compiler(FileSystemAccessor::new())?
        .compile(&mut type_manifests, &mut defined_modules)?;

    Ok(())
}
