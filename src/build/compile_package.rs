use crate::{CompilePackage, Result};
// build::PackageCompiler,
// codegen::{ErlangModules, ErlangRecordHeaders},
// fs::FileSystemAccessor,

pub fn command(options: CompilePackage) -> Result<()> {
    // let codegen_records = ErlangRecordHeaders::new(options.out_path);
    // let codegen_modules = ErlangModules::new(options.out_path);
    // let mut compiler = PackageCompiler::new(config, FileSystemAccessor::boxed())
    //     .with_code_generator(Box::new(codegen_records))
    //     .with_code_generator(Box::new(codegen_modules));

    // // Read source files
    // compiler.read_package_source_files(
    //     &self.root.default_build_lib_package_src_path(&name),
    //     Origin::Src,
    // )?;
    // if locations == SourceLocations::SrcAndTest {
    //     compiler.read_package_source_files(
    //         &self.root.default_build_lib_package_test_path(&name),
    //         Origin::Test,
    //     )?;
    // }

    // // Parse and type check
    // let compiled = compiler.compile(&mut self.type_manifests, &mut self.defined_modules)?;
    // self.packages.insert(name, compiled);
    Ok(())
}
