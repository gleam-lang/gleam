// Decoding of `Any` data into typed data

import process:[Pid, Reference]
import any:Any
import result:[Result, Ok, Error]
import list
import expect

type DecodeError =
  String;

pub external fn string(Any) -> Result(DecodeError, String)
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

pub external fn float(Any) -> Result(DecodeError, Float)
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

pub external fn atom(Any) -> Result(DecodeError, Atom)
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

pub external fn bool(Any) -> Result(DecodeError, Bool)
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

pub external fn pid(Any) -> Result(DecodeError, Pid)
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

pub external fn reference(Any) -> Result(DecodeError, Reference)
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

pub external fn thunk(Any) -> Result(DecodeError, fn() { Any })
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

external fn list_any(Any) -> Result(DecodeError, List(Any)) = 'gleam__decode_erl' 'list'

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

external fn t2(Any) -> Result(DecodeError, Tuple(Any, Any))
  = 'gleam__decode_erl' 'tuple'

pub fn tuple(any, elem1, elem2) {
  Ok({e1, e2}) <- t2(any)
  Ok(ok1) <- elem1(e1)
  Ok(ok2) <- elem2(e2)
  {ok1, ok2}
}

// TODO: tuple test

external fn t3(Any) -> Result(DecodeError, Tuple(Any, Any, Any))
  = 'gleam__decode_erl' 'tuple3'

pub fn tuple3(any, decode1, decode2, decode3) {
  Ok({e1, e2, e3}) <- t3(any)
  Ok(ok1) <- decode1(e1)
  Ok(ok2) <- decode2(e2)
  Ok(ok3) <- decode3(e3)
  {ok1, ok2, ok3}
}

// TODO: tuple3 test

external fn t4(Any) -> Result(DecodeError, Tuple(Any, Any, Any, Any))
  = 'gleam__decode_erl' 'tuple4'

pub fn tuple4(any, decode1, decode2, decode3, decode4) {
  Ok({e1, e2, e3, e4}) <- t4(any)
  Ok(ok1) <- decode1(e1)
  Ok(ok2) <- decode2(e2)
  Ok(ok3) <- decode3(e3)
  Ok(ok4) <- decode4(e4)
  {ok1, ok2, ok3, ok4}
}

// TODO: tuple4 test

external fn t5(Any) -> Result(DecodeError, Tuple(Any, Any, Any, Any, Any))
  = 'gleam__decode_erl' 'tuple5'

pub fn tuple5(any, decode1, decode2, decode3, decode4, decode5) {
  Ok({e1, e2, e3, e4, e5}) <- t5(any)
  Ok(ok1) <- decode1(e1)
  Ok(ok2) <- decode2(e2)
  Ok(ok3) <- decode3(e3)
  Ok(ok4) <- decode4(e4)
  Ok(ok5) <- decode5(e5)
  {ok1, ok2, ok3, ok4, ok5}
}

// TODO: tuple5 test
