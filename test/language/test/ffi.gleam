pub type Dynamic

@target(javascript)
pub external fn print(String) -> Nil =
  "./ffi_javascript.mjs" "print"

@target(javascript)
pub external fn append(String, String) -> String =
  "./ffi_javascript.mjs" "append"

@target(javascript)
pub external fn to_string(anything) -> String =
  "./ffi_javascript.mjs" "toString"

@target(javascript)
pub external fn ansi_green(String) -> String =
  "./ffi_javascript.mjs" "ansiGreen"

@target(javascript)
pub external fn file_exists(String) -> Bool =
  "./ffi_javascript.mjs" "fileExists"

@target(javascript)
pub external fn halt(Int) -> Nil =
  "./ffi_javascript.mjs" "halt"

@target(javascript)
pub external fn to_dynamic(x) -> Dynamic =
  "./ffi_javascript.mjs" "toDynamic"

@target(erlang)
pub external fn print(String) -> Nil =
  "ffi_erlang" "print"

@target(erlang)
pub external fn append(String, String) -> String =
  "ffi_erlang" "append"

@target(erlang)
pub external fn to_string(anything) -> String =
  "ffi_erlang" "to_string"

@target(erlang)
pub external fn ansi_green(String) -> String =
  "ffi_erlang" "ansi_green"

@target(erlang)
pub external fn file_exists(String) -> Bool =
  "ffi_erlang" "file_exists"

@target(erlang)
pub external fn halt(Int) -> Nil =
  "ffi_erlang" "halt"

@target(erlang)
pub external fn to_dynamic(x) -> Dynamic =
  "ffi_erlang" "to_dynamic"
