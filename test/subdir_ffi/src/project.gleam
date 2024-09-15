import nested/submodule

pub fn main() {
  println("Hello from subdir_ffi!")
  submodule.submodule_main()
  println(subdir_message())
  println(subdir_elixir_message())
}

@external(erlang, "erlang", "display")
@external(javascript, "./project_ffi.mjs", "log")
fn println(a: String) -> Nil

@external(erlang, "submodule_ffi", "main2")
@external(javascript, "./nested/submodule_ffi.mjs", "main2")
fn subdir_message() -> String

@external(erlang, "Elixir.ElixirFileAgain", "main")
@external(javascript, "./nested/submodule_ffi.mjs", "main2")
fn subdir_elixir_message() -> String
