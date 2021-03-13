fn main() {
    capnpc::CompilerCommand::new()
        .file("schema.capnp")
        .output_path("generated/")
        .run()
        .expect("compiling schema.capnp");
}
