fn main() {
    lalrpop::process_root().unwrap();

    // capnpc::CompilerCommand::new()
    //     .file("src/bindata.capnp")
    //     .output_path("./")
    //     .run()
    //     .expect("compiling src/bindata.capnp");
}
