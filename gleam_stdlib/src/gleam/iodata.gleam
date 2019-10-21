pub external type Iodata;

pub external fn prepend(to: Iodata, prefix: String) -> Iodata =
  "gleam_stdlib" "iodata_prepend";

pub external fn append(to: Iodata, suffix: String) -> Iodata =
  "gleam_stdlib" "iodata_append";

pub external fn prepend_iodata(to: Iodata, prefix: Iodata) -> Iodata =
  "gleam_stdlib" "iodata_prepend";

pub external fn append_iodata(to: Iodata, suffix: Iodata) -> Iodata =
  "gleam_stdlib" "iodata_append";

pub external fn from_strings(List(String)) -> Iodata =
  "gleam_stdlib" "identity";

pub external fn concat(List(Iodata)) -> Iodata =
  "gleam_stdlib" "identity";

pub external fn new(String) -> Iodata =
  "gleam_stdlib" "identity";

pub external fn to_string(Iodata) -> String =
  "erlang" "iolist_to_binary";

pub external fn byte_size(Iodata) -> Int =
  "erlang" "iolist_size";

pub external fn from_float(Float) -> Iodata =
  "io_lib_format" "fwrite_g";

pub external fn lowercase(Iodata) -> Iodata = "string" "lowercase"

pub external fn uppercase(Iodata) -> Iodata = "string" "uppercase"

pub external fn reverse(Iodata) -> Iodata = "string" "reverse"

enum Direction =
  | All

external fn erl_split(Iodata, String, Direction) -> List(Iodata) =
  "string" "split"

pub fn split(iodata, on pattern) {
  erl_split(iodata, pattern, All)
}

external fn erl_replace(Iodata, String, String, Direction) -> Iodata =
  "string" "replace"

pub fn replace(in iodata, all pattern, with substitute) {
  erl_replace(iodata, pattern, substitute, All)
}

pub external fn is_equal(Iodata, Iodata) -> Bool = "string" "equal"

pub external fn is_empty(Iodata) -> Bool = "string" "is_empty"
