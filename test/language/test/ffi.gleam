pub external type Dynamic

if javascript {
  pub external fn print(String) -> Nil =
    "./ffi_javascript.mjs" "print"

  pub external fn append(String, String) -> String =
    "./ffi_javascript.mjs" "append"

  pub external fn to_string(anything) -> String =
    "./ffi_javascript.mjs" "toString"

  pub external fn ansi_green(String) -> String =
    "./ffi_javascript.mjs" "ansiGreen"

  pub external fn file_exists(String) -> Bool =
    "./ffi_javascript.mjs" "fileExists"

  pub external fn halt(Int) -> Nil =
    "./ffi_javascript.mjs" "halt"

  pub external fn to_dynamic(x) -> Dynamic =
    "./ffi_javascript.mjs" "toDynamic"
}

if erlang {
  pub external fn print(String) -> Nil =
    "ffi_erlang" "print"

  pub external fn append(String, String) -> String =
    "ffi_erlang" "append"

  pub external fn to_string(anything) -> String =
    "ffi_erlang" "to_string"

  pub external fn ansi_green(String) -> String =
    "ffi_erlang" "ansi_green"

  pub external fn file_exists(String) -> Bool =
    "ffi_erlang" "file_exists"

  pub external fn halt(Int) -> Nil =
    "ffi_erlang" "halt"

  pub external fn to_dynamic(x) -> Dynamic =
    "ffi_erlang" "to_dynamic"
}
