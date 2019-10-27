import gleam/list as list_mod
import gleam/atom
import gleam/result
import gleam/pair.{Pair}

// `Any` data is data that we don"t know the type of yet.
// We likely get data like this from interop with Erlang, or from
// IO with the outside world.
//
pub external type Any;

// Convert any Gleam data into `Any` data.
//
pub external fn from(a) -> Any = "gleam_stdlib" "identity";

// Unsafely cast any type into any other type.
//
// This is an escape hatch for the type system that may be useful when wrapping
// native Erlang APIs. It is to be used as a last measure only.
//
pub external fn unsafe_coerce(a) -> b = "gleam_stdlib" "identity";

pub external fn string(from: Any) -> Result(String, String)
  = "gleam_stdlib" "decode_string"

pub external fn int(from: Any) -> Result(Int, String)
  = "gleam_stdlib" "decode_int"

pub external fn float(from: Any) -> Result(Float, String)
  = "gleam_stdlib" "decode_float"

pub external fn atom(from: Any) -> Result(atom.Atom, String)
  = "gleam_stdlib" "decode_atom"

pub external fn bool(from: Any) -> Result(Bool, String)
  = "gleam_stdlib" "decode_bool"

pub external fn thunk(from: Any) -> Result(fn() -> Any, String)
  = "gleam_stdlib" "decode_thunk"

external fn list_any(from: Any) -> Result(List(Any), String)
  = "gleam_stdlib" "decode_list"

pub fn list(from any, containing decoder_type) {
  any
  |> list_any
  |> result.then(_, list_mod.traverse(_, decoder_type))
}

pub external fn pair(from: Any) -> Result(Pair(Any, Any), String)
  = "gleam_stdlib" "decode_pair"

pub external fn field(from: Any, named: a) -> Result(Any, String)
  = "gleam_stdlib" "decode_field"
