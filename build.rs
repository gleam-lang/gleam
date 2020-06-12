fn main() {
    protobuf_codegen_pure::Codegen::new()
        .out_dir("src/proto")
        .inputs(&[
            "proto/names.proto",
            "proto/package.proto",
            "proto/signed.proto",
            "proto/versions.proto",
        ])
        .include("proto")
        .run()
        .expect("Protobuf codegen failed.");
}
