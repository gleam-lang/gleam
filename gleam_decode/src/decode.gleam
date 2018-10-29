// Decoding of `Any` data into typed data

import any:Any
import result:[Result, Ok, Error]
import list
import expect

type DecodeError =
  String;

pub external fn string(Any) -> Result(DecodeError, String) = 'gleam__decode_erl' 'string'

test string {
  ""
    |> any:from
    |> string
    |> expect:equal(Ok(""))

  "Hello"
    |> any:from
    |> string
    |> expect:equal(Ok("Hello"))

  1
    |> any:from
    |> string
    |> expect:is_error

  []
    |> any:from
    |> string
    |> expect:is_error
}

pub external fn int(Any) -> Result(DecodeError, Int) = 'gleam__decode_erl' 'int'

test int {
  1
    |> any:from
    |> int
    |> expect:equal(Ok(1))

  2
    |> any:from
    |> int
    |> expect:equal(Ok(2))

  1.0
    |> any:from
    |> int
    |> expect:is_error

  []
    |> any:from
    |> int
    |> expect:is_error
}

pub external fn float(Any) -> Result(DecodeError, Float) = 'gleam__decode_erl' 'float'

test float {
  1.0
    |> any:from
    |> float
    |> expect:equal(Ok(1.0))

  2.2
    |> any:from
    |> float
    |> expect:equal(Ok(2.2))

  1
    |> any:from
    |> float
    |> expect:is_error

  []
    |> any:from
    |> float
    |> expect:is_error
}

pub external fn atom(Any) -> Result(DecodeError, Atom) = 'gleam__decode_erl' 'atom'

test atom {
  ''
    |> any:from
    |> atom
    |> expect:equal(Ok(''))

  'ok'
    |> any:from
    |> atom
    |> expect:equal(Ok('ok'))

  1
    |> any:from
    |> atom
    |> expect:is_error

  []
    |> any:from
    |> atom
    |> expect:is_error
}

external fn list_any(Any) -> Result(DecodeError, List(Any)) = 'gleam__decode_erl' 'list'

pub fn list(any, decoder) {
  any
    |> list_any
    |> result:flat_map(_, list:traverse(_, decoder))
}

test list {
  []
    |> any:from
    |> list(string)
    |> expect:equal(Ok([]))

  []
    |> any:from
    |> list(atom)
    |> expect:equal(Ok([]))

  [1, 2, 3]
    |> any:from
    |> list(int)
    |> expect:equal(Ok([1, 2, 3]))

  [[1], [2], [3]]
    |> any:from
    |> list(list(int))
    |> expect:equal(Ok([1, 2, 3]))

  1
    |> any:from
    |> list(string)
    |> expect:is_error

  1.0
    |> any:from()
    |> list(int)
    |> expect:is_error

  [""]
    |> any:from()
    |> list(int)
    |> expect:is_error

  [any:from(1), any:from('not an int')]
    |> any:from
    |> list(int)
    |> expect:is_error
}
