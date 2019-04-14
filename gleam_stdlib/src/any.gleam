import list
import atom
import result

fn list_module() {
  list
}

// `Any` data is data that we don"t know the type of yet.
// We likely get data like this from interop with Erlang, or from
// IO with the outside world.
//
pub external type Any;

// Convert any Gleam data into `Any` data.
//
pub external fn from(a) -> Any = "gleam__stdlib" "identity";

// Unsafely cast any type into any other type.
//
// This is an escape hatch for the type system that may be useful when wrapping
// native Erlang APIs. It is to be used as a last measure only.
//
pub external fn unsafe_coerce(a) -> b = "gleam__stdlib" "identity";

pub external fn string(Any) -> Result(String, String)
  = "gleam__stdlib" "decode_string"

pub external fn int(Any) -> Result(Int, String)
  = "gleam__stdlib" "decode_int"

pub external fn float(Any) -> Result(Float, String)
  = "gleam__stdlib" "decode_float"

pub external fn atom(Any) -> Result(atom:Atom, String)
  = "gleam__stdlib" "decode_atom"

pub external fn bool(Any) -> Result(Bool, String)
  = "gleam__stdlib" "decode_bool"

pub external fn thunk(Any) -> Result(fn() -> Any, String)
  = "gleam__stdlib" "decode_thunk"

external fn list_any(Any) -> Result(List(Any), String) =
  "gleam__stdlib" "decode_list"

pub fn list(any, decode) {
  any
  |> list_any
  |> result:then(_, list_module():traverse(_, decode))
}

pub external fn tuple(Any) -> Result({Any, Any}, String)
  = "gleam__stdlib" "decode_tuple"

pub external fn field(Any, a) -> Result(Any, String)
  = "gleam__stdlib" "decode_field"
