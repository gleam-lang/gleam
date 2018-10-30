// Decoding of `Any` data into typed data

import process:[Pid, Reference]
import any:Any
import result:[Result, Ok, Error]
import list
import expect

pub external fn string(Any) -> Result(String, String)
  = 'gleam__decode_erl' 'string'

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

pub external fn int(Any) -> Result(DecodeError, Int)
  = 'gleam__decode_erl' 'int'

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

pub external fn float(Any) -> Result(String, Float)
  = 'gleam__decode_erl' 'float'

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

pub external fn atom(Any) -> Result(String, Atom)
  = 'gleam__decode_erl' 'atom'

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

pub external fn bool(Any) -> Result(String, Bool)
  = 'gleam__decode_erl' 'bool'

test bool {
  True
    |> any:from
    |> bool
    |> expect:equal(Ok(True))

  False
    |> any:from
    |> bool
    |> expect:equal(Ok(False))

  1
    |> any:from
    |> bool
    |> expect:is_error

  []
    |> any:from
    |> bool
    |> expect:is_error
}

pub external fn pid(Any) -> Result(String, Pid)
  = 'gleam__decode_erl' 'pid'

test pid {
  process:self()
    |> any:from
    |> pid
    |> expect:equal(Ok(process:self()))

  1
    |> any:from
    |> pid
    |> expect:is_error

  []
    |> any:from
    |> pid
    |> expect:is_error
}

pub external fn reference(Any) -> Result(String, Reference)
  = 'gleam__decode_erl' 'reference'

test reference {
  ref = process:make_reference()

  ref
    |> any:from
    |> reference
    |> expect:equal(Ok(ref))

  1
    |> any:from
    |> reference
    |> expect:is_error

  []
    |> any:from
    |> reference
    |> expect:is_error
}

pub external fn thunk(Any) -> Result(String, fn() { Any })
  = 'gleam__decode_erl' 'thunk'

test thunk {
  fn() { 1 }
    |> any:from
    |> thunk
    |> expect:is_ok

  fn(x) { x }
    |> any:from
    |> thunk
    |> expect:is_error

  1
    |> any:from
    |> thunk
    |> expect:is_error

  []
    |> any:from
    |> thunk
    |> expect:is_error
}

external fn list_any(Any) -> Result(String, List(Any)) = 'gleam__decode_erl' 'list'

pub fn list(any, decode) {
  any
    |> list_any
    |> result:flat_map(_, list:traverse(_, decode))
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

@doc("Attempt to decode a 2 element tuple.

The the elements of the tuple can be decoded like so:

    Ok({raw_elem1, raw_elem2}) <- decode:tuple(raw_data)
    Ok(elem1) <- decode:atom(raw_elem1)
    Ok(elem2) <- decode:int(raw_elem2)
    Ok({elem1, elem2})

See `tuple3`, `tuple4`, and `tuple5` for decoding of tuples of other lengths.
")
pub external fn tuple(Any) -> Result(String, Tuple(Any, Any))
  = 'gleam__decode_erl' 'tuple'

test tuple {
  {1, []}
    |> any:from
    |> tuple
    |> expect:equal({any:from(1), any:from([])})

  {'ok', "ok"}
    |> any:from
    |> tuple
    |> expect:equal({any:from('ok'), any:from("ok")})

  {1}
    |> any:from
    |> tuple
    |> expect:is_error

  {1, 2, 3}
    |> any:from
    |> tuple
    |> expect:is_error
}

pub external fn tuple3(Any) -> Result(String, Tuple(Any, Any, Any))
  = 'gleam__decode_erl' 'tuple3'

test tuple3 {
  {1, [], 3}
    |> any:from
    |> tuple3
    |> expect:equal({any:from(1), any:from([]), any:from(3)})

  {'ok', "ok", 3}
    |> any:from
    |> tuple3
    |> expect:equal({any:from('ok'), any:from("ok"), any:from(3)})

  {1}
    |> any:from
    |> tuple3
    |> expect:is_error

  {1, 2}
    |> any:from
    |> tuple3
    |> expect:is_error

  {1, 2, 3, 4}
    |> any:from
    |> tuple3
    |> expect:is_error
}


pub external fn tuple4(Any) -> Result(String, Tuple(Any, Any, Any, Any))
  = 'gleam__decode_erl' 'tuple4'

test tuple4 {
  {1, [], 3, 4}
    |> any:from
    |> tuple4
    |> expect:equal({any:from(1), any:from([]), any:from(3), any:from(4)})

  {'ok', "ok", 3, 4}
    |> any:from
    |> tuple4
    |> expect:equal({any:from('ok'), any:from("ok"), any:from(3), any:from(4)})

  {1}
    |> any:from
    |> tuple4
    |> expect:is_error

  {1, 2}
    |> any:from
    |> tuple4
    |> expect:is_error

  {1, 2, 3}
    |> any:from
    |> tuple4
    |> expect:is_error

  {1, 2, 3, 4, 5}
    |> any:from
    |> tuple4
    |> expect:is_error
}

pub external fn tuple5(Any) -> Result(String, Tuple(Any, Any, Any, Any, Any))
  = 'gleam__decode_erl' 'tuple5'

test tuple5 {
  {1, [], 3, 4, 5}
    |> any:from
    |> tuple5
    |> expect:equal(
        {any:from(1), any:from([]), any:from(3), any:from(4), any:from(5)}
      )

  {'ok', "ok", 3, 4, 5}
    |> any:from
    |> tuple5
    |> expect:equal(
        {any:from('ok'), any:from("ok"), any:from(3), any:from(4), any:from(5)}
      )

  {1}
    |> any:from
    |> tuple5
    |> expect:is_error

  {1, 2}
    |> any:from
    |> tuple5
    |> expect:is_error

  {1, 2, 3}
    |> any:from
    |> tuple5
    |> expect:is_error

  {1, 2, 3, 4}
    |> any:from
    |> tuple5
    |> expect:is_error

  {1, 2, 3, 4, 5, 6}
    |> any:from
    |> tuple5
    |> expect:is_error
}
