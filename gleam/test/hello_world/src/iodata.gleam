pub external type Iodata

pub external fn prepend(Iodata, String) -> Iodata =
  "gleam__stdlib" "iodata_prepend"

pub external fn append(Iodata, String) -> Iodata =
  "gleam__stdlib" "iodata_append"

pub external fn from(List(String)) -> Iodata =
  "gleam__stdlib" "identity"

pub external fn to_string(Iodata) -> String =
  "erlang" "iolist_to_binary"

pub external fn byte_size(Iodata) -> Int =
  "erlang" "iolist_size"
