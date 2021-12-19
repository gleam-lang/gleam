if javascript {
  pub external fn print(String) -> Nil =
    "./ffi_javascript.mjs" "print"

  pub external fn append(String, String) -> String =
    "./ffi_javascript.mjs" "append"

  pub external fn to_string(anything) -> String =
    "./ffi_javascript.mjs" "toString"

  pub external fn ansi_green(String) -> String =
    "./ffi_javascript.mjs" "ansi_green"
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
}
