fn main() {
    prost_build::compile_protos(
        &[
            "proto/signed.proto",
            "proto/package.proto",
            "proto/versions.proto",
        ],
        &["proto/"],
    )
    .expect("Failed to generate prost code from .proto files.");
}
