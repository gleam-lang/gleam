// Decoding of `Any` data into typed data

pub external type Any;

// import process:[Pid, Reference, Port]
// import any:Any
// import list
// import expect

pub enum Tagged =
  | Int(Int)
  // | Pid(Pid)
  // | Port(Port)
  | Atom(String)
  | Float(Float)
  | String(String)
  | List(List(Tagged))
  | Tuple(List(Tagged))
  // | Reference(Reference)
  | Map(List({Tagged, Tagged}))

pub external fn string(Any) -> Result(String, String)
  = "gleam__decode_erl" "string"

// test string {
//   let _ = ""
//     |> any:from
//     |> string
//     |> expect:equal(Ok(""))

//   let _ = "Hello"
//     |> any:from
//     |> string
//     |> expect:equal(Ok("Hello"))

//   let _ = 1
//     |> any:from
//     |> string
//     |> expect:is_error

//   []
//     |> any:from
//     |> string
//     |> expect:is_error
// }

pub external fn int(Any) -> Result(String, Int)
  = "gleam__decode_erl" "int"

// test int {
//   let _ = 1
//     |> any:from
//     |> int
//     |> expect:equal(Ok(1))

//   let _ = 2
//     |> any:from
//     |> int
//     |> expect:equal(Ok(2))

//   let _ = 1.0
//     |> any:from
//     |> int
//     |> expect:is_error

//   []
//     |> any:from
//     |> int
//     |> expect:is_error
// }

pub external fn float(Any) -> Result(String, Float)
  = "gleam__decode_erl" "float"

// test float {
//   let _ = 1.0
//     |> any:from
//     |> float
//     |> expect:equal(Ok(1.0))

//   let _ = 2.2
//     |> any:from
//     |> float
//     |> expect:equal(Ok(2.2))

//   let _ = 1
//     |> any:from
//     |> float
//     |> expect:is_error

//   []
//     |> any:from
//     |> float
//     |> expect:is_error
// }

pub external fn atom(Any) -> Result(String, String)
  = "gleam__decode_erl" "atom"

// test atom {
//   // TODO
//   // make an atom here
//   //   |> any:from
//   //   |> atom
//   //   |> expect:equal(Ok(""))

//   // TODO
//   // make an atom here
//   //   |> any:from
//   //   |> atom
//   //   |> expect:equal(Ok("ok"))

//   let _ = 1
//     |> any:from
//     |> atom
//     |> expect:is_error

//   []
//     |> any:from
//     |> atom
//     |> expect:is_error
// }

pub external fn bool(Any) -> Result(String, Bool)
  = "gleam__decode_erl" "bool"

// test bool {
//   let _ = True
//     |> any:from
//     |> bool
//     |> expect:equal(Ok(True))

//   let _ = False
//     |> any:from
//     |> bool
//     |> expect:equal(Ok(False))

//   let _ = 1
//     |> any:from
//     |> bool
//     |> expect:is_error

//   []
//     |> any:from
//     |> bool
//     |> expect:is_error
// }

// pub external fn pid(Any) -> Result(String, Pid)
//   = "gleam__decode_erl" "pid"

// test pid {
//   let _ = process:self()
//     |> any:from
//     |> pid
//     |> expect:equal(Ok(process:self()))

//   let _ = 1
//     |> any:from
//     |> pid
//     |> expect:is_error

//   []
//     |> any:from
//     |> pid
//     |> expect:is_error
// }

// pub external fn reference(Any) -> Result(String, Reference)
//   = "gleam__decode_erl" "reference"

// test reference {
//   let ref = process:make_reference()

//   let _ = ref
//     |> any:from
//     |> reference
//     |> expect:equal(Ok(ref))

//   let _ = 1
//     |> any:from
//     |> reference
//     |> expect:is_error

//   []
//     |> any:from
//     |> reference
//     |> expect:is_error
// }

pub external fn thunk(Any) -> Result(String, fn() -> Any)
  = "gleam__decode_erl" "thunk"

// test thunk {
//   let _ = fn() { 1 }
//     |> any:from
//     |> thunk
//     |> expect:is_ok

//   let _ = fn(x) { x }
//     |> any:from
//     |> thunk
//     |> expect:is_error

//   let _ = 1
//     |> any:from
//     |> thunk
//     |> expect:is_error

//   []
//     |> any:from
//     |> thunk
//     |> expect:is_error
// }

// external fn list_any(Any) -> Result(String, List(Any)) = "gleam__decode_erl" "list"

// pub fn list(any, decode) {
//   any
//     |> list_any
//     |> result:flat_map(_, list:traverse(_, decode))
// }

// test list {
//   let _ = []
//     |> any:from
//     |> list(string)
//     |> expect:equal(Ok([]))

//   let _ = []
//     |> any:from
//     |> list(atom)
//     |> expect:equal(Ok([]))

//   let _ = [1, 2, 3]
//     |> any:from
//     |> list(int)
//     |> expect:equal(Ok([1, 2, 3]))

//   let _ = [[1], [2], [3]]
//     |> any:from
//     |> list(list(int))
//     |> expect:equal(Ok([1, 2, 3]))

//   let _ = 1
//     |> any:from
//     |> list(string)
//     |> expect:is_error

//   let _ = 1.0
//     |> any:from()
//     |> list(int)
//     |> expect:is_error

//   let _ = [""]
//     |> any:from()
//     |> list(int)
//     |> expect:is_error

//   [any:from(1), any:from("not an int")]
//     |> any:from
//     |> list(int)
//     |> expect:is_error
// }

//// Attempt to decode a 2 element tuple.
////
//// The the elements of the tuple can be decoded like so:
////
////     Ok({raw_elem1, raw_elem2}) <- decode:tuple(raw_data)
////     Ok(elem1) <- decode:atom(raw_elem1)
////     Ok(elem2) <- decode:int(raw_elem2)
////     Ok({elem1, elem2})
////
//// See `tuple3`, `tuple4`, and `tuple5` for decoding of tuples of other lengths.
////
//pub external fn tuple(Any) -> Result(String, Tuple(Any, Any))
//  = "gleam__decode_erl" "tuple"

//test tuple {
//  let _ = {1, []}
//    |> any:from
//    |> tuple
//    |> expect:equal({any:from(1), any:from([])})

//  let _ = {"ok", "ok"}
//    |> any:from
//    |> tuple
//    |> expect:equal({any:from("ok"), any:from("ok")})

//  let _ = {1}
//    |> any:from
//    |> tuple
//    |> expect:is_error

//  {1, 2, 3}
//    |> any:from
//    |> tuple
//    |> expect:is_error
//}

//pub external fn tuple3(Any) -> Result(String, Tuple(Any, Any, Any))
//  = "gleam__decode_erl" "tuple3"

//test tuple3 {
//  let _ = {1, [], 3}
//    |> any:from
//    |> tuple3
//    |> expect:equal({any:from(1), any:from([]), any:from(3)})

//  let _ = {"ok", "ok", 3}
//    |> any:from
//    |> tuple3
//    |> expect:equal({any:from("ok"), any:from("ok"), any:from(3)})

//  let _ = {1}
//    |> any:from
//    |> tuple3
//    |> expect:is_error

//  let _ = {1, 2}
//    |> any:from
//    |> tuple3
//    |> expect:is_error

//  {1, 2, 3, 4}
//    |> any:from
//    |> tuple3
//    |> expect:is_error
//}


//pub external fn tuple4(Any) -> Result(String, Tuple(Any, Any, Any, Any))
//  = "gleam__decode_erl" "tuple4"

//test tuple4 {
//  let _ = {1, [], 3, 4}
//    |> any:from
//    |> tuple4
//    |> expect:equal({any:from(1), any:from([]), any:from(3), any:from(4)})

//  let _ = {"ok", "ok", 3, 4}
//    |> any:from
//    |> tuple4
//    |> expect:equal({any:from("ok"), any:from("ok"), any:from(3), any:from(4)})

//  let _ = {1}
//    |> any:from
//    |> tuple4
//    |> expect:is_error

//  let _ = {1, 2}
//    |> any:from
//    |> tuple4
//    |> expect:is_error

//  let _ = {1, 2, 3}
//    |> any:from
//    |> tuple4
//    |> expect:is_error

//  {1, 2, 3, 4, 5}
//    |> any:from
//    |> tuple4
//    |> expect:is_error
//}

//pub external fn tuple5(Any) -> Result(String, Tuple(Any, Any, Any, Any, Any))
//  = "gleam__decode_erl" "tuple5"

//test tuple5 {
//  let _ = {1, [], 3, 4, 5}
//    |> any:from
//    |> tuple5
//    |> expect:equal(
//        {any:from(1), any:from([]), any:from(3), any:from(4), any:from(5)}
//      )

//  let _ = {"ok", "ok", 3, 4, 5}
//    |> any:from
//    |> tuple5
//    |> expect:equal(
//        {any:from("ok"), any:from("ok"), any:from(3), any:from(4), any:from(5)}
//      )

//  let _ = {1}
//    |> any:from
//    |> tuple5
//    |> expect:is_error

//  let _ = {1, 2}
//    |> any:from
//    |> tuple5
//    |> expect:is_error

//  let _ = {1, 2, 3}
//    |> any:from
//    |> tuple5
//    |> expect:is_error

//  let _ = {1, 2, 3, 4}
//    |> any:from
//    |> tuple5
//    |> expect:is_error

//  {1, 2, 3, 4, 5, 6}
//    |> any:from
//    |> tuple5
//    |> expect:is_error
//}

//// TODO: FIXME: This doesn't work anymore because atoms are no longer a thing.
//// "Decode a field from a map, extracting a specified field.
////
//// Multiple fields can be extracted and stored in a new record like so:
////
////     Ok(name) <- decode:field(raw_data, \"name\") |> result:flat_map(_, decode:string)
////     Ok(size) <- decode:field(raw_data, \"size\") |> result:flat_map(_, decode:int)
////     Ok({ name = name, size = size })
////
//pub external fn field(Any, a) -> Result(String, Any)
//  = "gleam__decode_erl" "field"

//test field {
//  let _ = {ok = 1}
//    |> any:from
//    |> field("ok")
//    |> expect:equal(any:from(1))

//  let _ = {earlier = 2, ok = 3}
//    |> any:from
//    |> field("ok")
//    |> expect:equal(any:from(3))

//  let _ = {}
//    |> any:from
//    |> field("ok")
//    |> expect:is_error

//  let _ = 1
//    |> any:from
//    |> field("ok")
//    |> expect:is_error

//  []
//    |> any:from
//    |> field("ok")
//    |> expect:is_error
//}
