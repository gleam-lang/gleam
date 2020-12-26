use crate::{
    build::{Origin, PackageCompiler},
    codegen::{ErlangModules, ErlangRecordHeaders},
    fs::FileSystemAccessor,
    CompilePackage, Result,
};
use std::collections::HashMap;

pub fn command(options: CompilePackage) -> Result<()> {
    // TODO: Load precompiled libraries
    tracing::info!("Reading precompiled module metadata files");
    let mut type_manifests = HashMap::new();
    let mut defined_modules = HashMap::new();

    // Prepare package compile
    let codegen_records = ErlangRecordHeaders::new(options.out_path.clone());
    let codegen_modules = ErlangModules::new(options.out_path.clone());
    let mut compiler = PackageCompiler::new(options.name.clone(), FileSystemAccessor::boxed())
        .with_code_generator(Box::new(codegen_records))
        .with_code_generator(Box::new(codegen_modules));

    // Read source files
    tracing::info!("Reading source files");
    compiler.read_package_source_files(&options.src_path, Origin::Src)?;
    if let Some(path) = &options.test_path {
        compiler.read_package_source_files(path, Origin::Test)?;
    }

    // TODO: Parse and type check
    let compiled = compiler.compile(&mut type_manifests, &mut defined_modules)?;

    Ok(())
}
