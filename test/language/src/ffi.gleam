if javascript {
  pub external fn print(String) -> Nil =
    "./ffi_javascript.js" "print"

  pub external fn append(String, String) -> String =
    "./ffi_javascript.js" "append"

  pub external fn to_string(anything) -> String =
    "./ffi_javascript.js" "toString"
}

if erlang {
  pub external fn print(String) -> Nil =
    "ffi_erlang" "print"

  pub external fn append(String, String) -> String =
    "ffi_erlang" "append"

  pub external fn to_string(anything) -> String =
    "ffi_erlang" "to_string"
}
