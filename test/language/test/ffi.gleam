pub type Dynamic

@external(erlang, "ffi_erlang", "print")
@external(javascript, "./ffi_javascript.mjs", "print")
pub fn print(a: String) -> Nil

@external(erlang, "ffi_erlang", "append")
@external(javascript, "./ffi_javascript.mjs", "append")
pub fn append(a: String, b: String) -> String

@external(erlang, "ffi_erlang", "to_string")
@external(javascript, "./ffi_javascript.mjs", "toString")
pub fn to_string(a: anything) -> String

@external(erlang, "ffi_erlang", "file_exists")
@external(javascript, "./ffi_javascript.mjs", "fileExists")
pub fn file_exists(a: String) -> Bool

@external(erlang, "ffi_erlang", "halt")
@external(javascript, "./ffi_javascript.mjs", "halt")
pub fn halt(a: Int) -> Nil

@external(erlang, "ffi_erlang", "to_dynamic")
@external(javascript, "./ffi_javascript.mjs", "toDynamic")
pub fn to_dynamic(a: x) -> Dynamic
