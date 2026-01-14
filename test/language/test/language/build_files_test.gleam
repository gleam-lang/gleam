import ffi

@target(javascript)
pub fn typescript_file_included_test() {
  let path = "./build/dev/javascript/language/ffi_typescript.ts"
  assert ffi.file_exists(path)
}

@target(erlang)
pub fn typescript_file_included_test() {
  let path = "./build/dev/erlang/language/_gleam_artefacts/ffi_typescript.ts"
  assert ffi.file_exists(path)
}
