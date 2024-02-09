if erlang {
  import gleam/erlang/os

  pub fn get_env(name: String) -> Result(String, Nil) {
    os.get_env(name)
  }

  pub fn set_env(name: String, value: String) -> Nil {
    os.set_env(name, value)
  }

  pub fn unset_env(name: String) -> Nil {
    os.unset_env(name)
  }
}

if javascript {
  pub external fn get_env(name: String) -> Result(String, Nil) =
    "../../glenvy_ffi.mjs" "get_env"

  pub external fn set_env(name: String, value: String) -> Nil =
    "../../glenvy_ffi.mjs" "set_env"

  pub external fn unset_env(name: String) -> Nil =
    "../../glenvy_ffi.mjs" "unset_env"
}
