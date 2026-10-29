pub type Argv {
  Argv(
    /// The path to the runtime executable.
    /// For example, `"/usr/bin/erl"`.
    runtime: String,
    /// The path to the program being run.
    /// This may either be the entrypoint script for JavaScript, path of the
    /// escript for Erlang, or the path of the project if none of those could 
    /// be determined.
    program: String,
    /// The command line arguments passed to the program.
    arguments: List(String),
  )
}

pub fn load() -> Argv {
  let #(runtime, program, arguments) = do()
  Argv(runtime: runtime, program: program, arguments: arguments)
}

@external(erlang, "argv_ffi", "load")
@external(javascript, "./argv_ffi.mjs", "load")
fn do() -> #(String, String, List(String))
