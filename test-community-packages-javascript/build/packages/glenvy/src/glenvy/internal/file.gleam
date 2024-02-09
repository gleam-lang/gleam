if erlang {
  import gleam/erlang/file
  import gleam/result
  import gleam/string

  pub fn read(from path: String) -> Result(String, String) {
    file.read(from: path)
    |> result.map_error(string.inspect)
  }
}

if javascript {
  pub external fn read(from: String) -> Result(String, String) =
    "../../glenvy_ffi.mjs" "read_file"
}
