/// `prost_build` generates files in the output directory, which means that if we want
/// to use it, we would need the protoc compiler as a build dependency.
/// To get around this, we need run the build script and manually copy the generated files
/// into the `src` folder. To do so, uncomment the below lines, then copy the files from the
/// output directory into the `src/proto` folder. The path to the output directory
/// will be printed in the terminal.
///
fn main() {
    // prost_build::compile_protos(
    //     &[
    //         "proto/signed.proto",
    //         "proto/package.proto",
    //         "proto/versions.proto",
    //     ],
    //     &["proto/"],
    // )
    // .expect("Failed to generate prost code from .proto files");

    // println!(
    //     "cargo::warning=Regenerated proto files, which must be manually copied into the `src` directory. Generated files can be found in {}",
    //     std::env::var("OUT_DIR").unwrap_or_default()
    // );
}
