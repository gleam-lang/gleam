import list
import atom
import tuple
import result
import expect

fn list_module() {
  list
}

fn tuple_module() {
  tuple
}

// `Any` data is data that we don"t know the type of yet.
// We likely get data like this from interop with Erlang, or from
// IO with the outside world.
//
pub external type Any;

// Convert any Gleam data into `Any` data.
//
pub external fn from(a) -> Any = "gleam__stdlib" "identity";

// Unsafely cast any type into any other type.o
//
// This is an escape hatch for the type system that may be useful when wrapping
// native Erlang APIs. It is to be used as a last measure only.
//
pub external fn unsafeCoerce(a) -> b = "gleam__stdlib" "identity";

pub external fn string(Any) -> Result(String, String)
  = "gleam__stdlib" "decode_string"

test string {
  ""
  |> from
  |> string
  |> expect:equal(_, Ok(""))

  "Hello"
  |> from
  |> string
  |> expect:equal(_, Ok("Hello"))

  1
  |> from
  |> string
  |> expect:equal(_, Error("Expected a String, got `1`"))

  []
  |> from
  |> string
  |> expect:equal(_, Error("Expected a String, got `[]`"))
}

pub external fn int(Any) -> Result(Int, String)
  = "gleam__stdlib" "decode_int"

test int {
  1
  |> from
  |> int
  |> expect:equal(_, Ok(1))

  2
  |> from
  |> int
  |> expect:equal(_, Ok(2))

  1.0
  |> from
  |> int
  |> expect:equal(_, Error("Expected an Int, got `1.0`"))

  []
  |> from
  |> int
  |> expect:equal(_, Error("Expected an Int, got `[]`"))
}

pub external fn float(Any) -> Result(Float, String)
  = "gleam__stdlib" "decode_float"

test float {
  1.0
  |> from
  |> float
  |> expect:equal(_, Ok(1.0))

  2.2
  |> from
  |> float
  |> expect:equal(_, Ok(2.2))

  1
  |> from
  |> float
  |> expect:equal(_, Error("Expected a Float, got `1`"))

  []
  |> from
  |> float
  |> expect:equal(_, Error("Expected a Float, got `[]`"))
}

// TODO
// pub external fn atom(Any) -> Result(Atom, String)
//   = "gleam__stdlib" "decode_atom"

// test atom {
//   make an atom here
//     |> from
//     |> atom
//     |> expect:equal(_, Ok(""))

//   make an atom here
//     |> from
//     |> atom
//     |> expect:equal(_, Ok("ok"))

//   1
//     |> from
//     |> atom
//     |> expect:is_error

//   []
//     |> from
//     |> atom
//     |> expect:is_error
// }

pub external fn bool(Any) -> Result(Bool, String)
  = "gleam__stdlib" "decode_bool"

test bool {
  True
  |> from
  |> bool
  |> expect:equal(_, Ok(True))

  False
  |> from
  |> bool
  |> expect:equal(_, Ok(False))

  1
  |> from
  |> bool
  |> expect:equal(_, Error("Expected a Bool, got `1`"))

  []
  |> from
  |> bool
  |> expect:equal(_, Error("Expected a Bool, got `[]`"))
}

pub external fn thunk(Any) -> Result(fn() -> Any, String)
  = "gleam__stdlib" "decode_thunk"

test thunk {
  fn() { 1 }
  |> from
  |> thunk
  |> expect:is_ok

  fn() { 1 }
  |> from
  |> thunk
  |> result:map(_, fn(f) { f() })
  |> expect:equal(_, Ok(from(1)))

  fn(x) { x }
  |> from
  |> thunk
  |> expect:is_error

  1
  |> from
  |> thunk
  |> expect:is_error

  []
  |> from
  |> thunk
  |> expect:is_error
}

external fn list_any(Any) -> Result(List(Any), String) =
  "gleam__stdlib" "decode_list"

pub fn list(any, decode) {
  any
  |> list_any
  |> result:then(_, list_module():traverse(_, decode))
}

test list {
  []
  |> from
  |> list(_, string)
  |> expect:equal(_, Ok([]))

  []
  |> from
  |> list(_, int)
  |> expect:equal(_, Ok([]))

  [1, 2, 3]
  |> from
  |> list(_, int)
  |> expect:equal(_, Ok([1, 2, 3]))

  [[1], [2], [3]]
  |> from
  |> list(_, list(_, int))
  |> expect:equal(_, Ok([[1], [2], [3]]))

  1
  |> from
  |> list(_, string)
  |> expect:is_error

  1.0
  |> from
  |> list(_, int)
  |> expect:is_error

  [""]
  |> from
  |> list(_, int)
  |> expect:is_error

  [from(1), from("not an int")]
  |> from
  |> list(_, int)
  |> expect:is_error
}

pub external fn tuple(Any) -> Result({Any, Any}, String)
  = "gleam__stdlib" "decode_tuple"

test tuple {
  {1, []}
  |> from
  |> tuple
  |> expect:equal(_, Ok({from(1), from([])}))

  {"ok", "ok"}
  |> from
  |> tuple
  |> expect:equal(_, Ok({from("ok"), from("ok")}))

  {1}
  |> from
  |> tuple
  |> expect:is_error

  {1, 2, 3}
  |> from
  |> tuple
  |> expect:is_error

  {1, 2.0}
  |> from
  |> tuple
  |> result:then(_, fn(x) {
    x
    |> tuple_module():first
    |> int
    |> result:map(_, fn(f) { {f, tuple_module():second(x)} })
  })
  |> result:then(_, fn(x) {
    x
    |> tuple_module():second
    |> float
    |> result:map(_, fn(f) { {tuple_module():first(x), f} })
  })
  |> expect:equal(_, Ok({1, 2.0}))
}

pub external fn field(Any, a) -> Result(Any, String)
  = "gleam__stdlib" "decode_field"

test field {
  let Ok(ok_atom) = atom:from_string("ok")

  {ok = 1}
  |> from
  |> field(_, ok_atom)
  |> expect:equal(_, Ok(from(1)))

  {earlier = 2, ok = 3}
  |> from
  |> field(_, ok_atom)
  |> expect:equal(_, Ok(from(3)))

  {}
  |> from
  |> field(_, ok_atom)
  |> expect:is_error

  1
  |> from
  |> field(_, ok_atom)
  |> expect:is_error

  []
  |> from
  |> field(_, [])
  |> expect:is_error
}
