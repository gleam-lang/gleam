import expect

// concat should work on List(Iodata)
// need a name for the string version

pub external type Iodata;

pub external fn prepend(Iodata, String) -> Iodata =
  "gleam__stdlib" "iodata_prepend";

pub external fn append(Iodata, String) -> Iodata =
  "gleam__stdlib" "iodata_append";

pub external fn prepend_iodata(Iodata, Iodata) -> Iodata =
  "gleam__stdlib" "iodata_prepend";

pub external fn append_iodata(Iodata, Iodata) -> Iodata =
  "gleam__stdlib" "iodata_append";

pub external fn from_strings(List(String)) -> Iodata =
  "gleam__stdlib" "identity";

pub external fn concat(List(Iodata)) -> Iodata =
  "gleam__stdlib" "identity";

pub external fn new(String) -> Iodata =
  "gleam__stdlib" "identity";

pub external fn to_string(Iodata) -> String =
  "erlang" "iolist_to_binary";

pub external fn byte_size(Iodata) -> Int =
  "erlang" "iolist_size";

pub external fn from_float(Float) -> Iodata =
  "io_lib_format" "fwrite_g";

test iodata {
  let iodata = new("ello")
    |> append(_, ",")
    |> append(_, " world!")
    |> prepend(_, "H")

  iodata
  |> to_string
  |> expect:equal(_, "Hello, world!")

  iodata
  |> byte_size
  |> expect:equal(_, 13)

  let iodata = new("ello")
    |> append_iodata(_, new(","))
    |> append_iodata(_, concat([new(" wo"), new("rld!")]))
    |> prepend_iodata(_, new("H"))

  iodata
  |> to_string
  |> expect:equal(_, "Hello, world!")

  iodata
  |> byte_size
  |> expect:equal(_, 13)
}

pub external fn lowercase(Iodata) -> Iodata = "string" "lowercase"

test lowercase {
  ["Gleam", "Gleam"]
  |> from_strings
  |> lowercase
  |> to_string
  |> expect:equal(_, "gleamgleam")
}

pub external fn uppercase(Iodata) -> Iodata = "string" "uppercase"

test uppercase {
  ["Gleam", "Gleam"]
  |> from_strings
  |> uppercase
  |> to_string
  |> expect:equal(_, "GLEAMGLEAM")
}

pub external fn reverse(Iodata) -> Iodata = "string" "reverse"

enum Direction =
  | All

external fn erl_split(Iodata, String, Direction) -> List(Iodata) =
  "string" "split"

pub fn split(iodata, on) {
  erl_split(iodata, on, All)
}

test split {
  "Gleam,Erlang,Elixir"
  |> new
  |> split(_, ",")
  |> expect:equal(_, [new("Gleam"), new("Erlang"), new("Elixir")])

  ["Gleam, Erl", "ang,Elixir"]
  |> from_strings
  |> split(_, ", ")
  |> expect:equal(_, [new("Gleam"), from_strings(["Erl", "ang,Elixir"])])
}

external fn erl_replace(Iodata, String, String, Direction) -> Iodata =
  "string" "replace"

pub fn replace(iodata, pattern, replacement) {
  erl_replace(iodata, pattern, replacement, All)
}

pub external fn is_equal(Iodata, Iodata) -> Bool = "string" "equal"

test is_equal {
  new("12")
  |> is_equal(_, from_strings(["1", "2"]))
  |> expect:true

  new("12")
  |> is_equal(_, new("12"))
  |> expect:true

  new("12")
  |> is_equal(_, new("2"))
  |> expect:false
}

pub external fn is_empty(Iodata) -> Bool = "string" "is_empty"

test is_empty {
  new("")
  |> is_empty
  |> expect:true

  new("12")
  |> is_empty
  |> expect:false

  from_strings([])
  |> is_empty
  |> expect:true

  from_strings(["", ""])
  |> is_empty
  |> expect:true
}
