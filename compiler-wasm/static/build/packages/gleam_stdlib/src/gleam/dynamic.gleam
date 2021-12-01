import gleam/bit_string
import gleam/list
import gleam/map
import gleam/int
import gleam/option
import gleam/result
import gleam/string_builder
import gleam/map.{Map}
import gleam/option.{Option}

/// `Dynamic` data is data that we don't know the type of yet.
/// We likely get data like this from interop with Erlang, or from
/// IO with the outside world.
pub external type Dynamic

/// Error returned when unexpected data is encountered
pub type DecodeError {
  DecodeError(expected: String, found: String)
}

pub type Decoder(t) =
  fn(Dynamic) -> Result(t, DecodeError)

/// Converts any Gleam data into `Dynamic` data.
///
pub fn from(a) -> Dynamic {
  do_from(a)
}

if erlang {
  external fn do_from(anything) -> Dynamic =
    "gleam_stdlib" "identity"
}

if javascript {
  external fn do_from(anything) -> Dynamic =
    "../gleam_stdlib.js" "identity"
}

/// Unsafely casts a Dynamic value into any other type.
///
/// This is an escape hatch for the type system that may be useful when wrapping
/// native Erlang APIs. It is to be used as a last measure only!
///
/// If you can avoid using this function, do!
///
pub fn unsafe_coerce(a: Dynamic) -> anything {
  do_unsafe_coerce(a)
}

if erlang {
  external fn do_unsafe_coerce(Dynamic) -> a =
    "gleam_stdlib" "identity"
}

if javascript {
  external fn do_unsafe_coerce(Dynamic) -> a =
    "../gleam_stdlib.js" "identity"
}

/// Checks to see whether a Dynamic value is a bit_string, and return the bit_string if
/// it is.
///
/// ## Examples
///
///    > bit_string(from("Hello")) == bit_string.from_string("Hello")
///    True
///
///    > bit_string(from(123))
///    Error(DecodeError(expected: "BitString", found: "Int"))
///
pub fn bit_string(from data: Dynamic) -> Result(BitString, DecodeError) {
  decode_bit_string(data)
}

if erlang {
  external fn decode_bit_string(Dynamic) -> Result(BitString, DecodeError) =
    "gleam_stdlib" "decode_bit_string"
}

if javascript {
  external fn decode_bit_string(Dynamic) -> Result(BitString, DecodeError) =
    "../gleam_stdlib.js" "decode_bit_string"
}

/// Checks to see whether a Dynamic value is a string, and return the string if
/// it is.
///
/// ## Examples
///
///    > string(from("Hello"))
///    Ok("Hello")
///
///    > string(from(123))
///    Error(DecodeError(expected: "String", found: "Int"))
///
pub fn string(from data: Dynamic) -> Result(String, DecodeError) {
  decode_string(data)
}

if erlang {
  fn decode_string(data: Dynamic) -> Result(String, DecodeError) {
    bit_string(data)
    |> result.map_error(fn(error) { DecodeError(..error, expected: "String") })
    |> result.then(fn(raw) {
      case bit_string.to_string(raw) {
        Ok(string) -> Ok(string)
        Error(Nil) -> Error(DecodeError(expected: "String", found: "BitString"))
      }
    })
  }
}

if javascript {
  external fn decode_string(Dynamic) -> Result(String, DecodeError) =
    "../gleam_stdlib.js" "decode_string"
}

/// Return a string indicating the type of the dynamic value.
///
/// ```
/// > classify(from("Hello"))
/// "String"
/// ```
///
pub fn classify(data: Dynamic) -> String {
  do_classify(data)
}

if erlang {
  external fn do_classify(Dynamic) -> String =
    "gleam_stdlib" "classify_dynamic"
}

if javascript {
  external fn do_classify(Dynamic) -> String =
    "../gleam_stdlib.js" "classify_dynamic"
}

/// Checks to see whether a Dynamic value is an int, and return the int if it
/// is.
///
/// ## Examples
///
///    > int(from(123))
///    Ok(123)
///
///    > int(from("Hello"))
///    Error(DecodeError(expected: "Int", found: "String"))
///
pub fn int(from data: Dynamic) -> Result(Int, DecodeError) {
  decode_int(data)
}

if erlang {
  external fn decode_int(Dynamic) -> Result(Int, DecodeError) =
    "gleam_stdlib" "decode_int"
}

if javascript {
  external fn decode_int(Dynamic) -> Result(Int, DecodeError) =
    "../gleam_stdlib.js" "decode_int"
}

/// Checks to see whether a Dynamic value is an float, and return the float if
/// it is.
///
/// ## Examples
///
///    > float(from(2.0))
///    Ok(2.0)
///
///    > float(from(123))
///    Error(DecodeError(expected: "Float", found: "Int"))
///
pub fn float(from data: Dynamic) -> Result(Float, DecodeError) {
  decode_float(data)
}

if erlang {
  external fn decode_float(Dynamic) -> Result(Float, DecodeError) =
    "gleam_stdlib" "decode_float"
}

if javascript {
  external fn decode_float(Dynamic) -> Result(Float, DecodeError) =
    "../gleam_stdlib.js" "decode_float"
}

/// Checks to see whether a Dynamic value is an bool, and return the bool if
/// it is.
///
/// ## Examples
///
///    > bool(from(True))
///    Ok(True)
///
///    > bool(from(123))
///    Error(DecodeError(expected: "bool", found: "Int"))
///
pub fn bool(from data: Dynamic) -> Result(Bool, DecodeError) {
  decode_bool(data)
}

if erlang {
  external fn decode_bool(Dynamic) -> Result(Bool, DecodeError) =
    "gleam_stdlib" "decode_bool"
}

if javascript {
  external fn decode_bool(Dynamic) -> Result(Bool, DecodeError) =
    "../gleam_stdlib.js" "decode_bool"
}

/// Checks to see whether a Dynamic value is a list, and return the list if it
/// is.
///
/// If you wish to decode all the elements in the list use the `typed_list`
/// instead.
///
/// ## Examples
///
///    > list(from(["a", "b", "c"]))
///    Ok([from("a"), from("b"), from("c")])
///
///    > list(1)
///    Error(DecodeError(expected: "Int", found: "Int"))
///
pub fn list(from value: Dynamic) -> Result(List(Dynamic), DecodeError) {
  decode_list(value)
}

if erlang {
  external fn decode_list(Dynamic) -> Result(List(Dynamic), DecodeError) =
    "gleam_stdlib" "decode_list"
}

if javascript {
  external fn decode_list(Dynamic) -> Result(List(Dynamic), DecodeError) =
    "../gleam_stdlib.js" "decode_list"
}

/// Checks to see whether a Dynamic value is a result, and return the result if
/// it is.
///
/// ## Examples
///
///    > result(from(Ok(1)))
///    Ok(Ok(from(1)))
///
///    > result(from(Error("boom")))
///    Ok(Error(from("boom")))
///
///    > result(from(123))
///    Error(DecodeError(expected: "2 element tuple", found: "Int"))
///
pub fn result(
  from value: Dynamic,
) -> Result(Result(Dynamic, Dynamic), DecodeError) {
  decode_result(value)
}

if erlang {
  external fn decode_result(Dynamic) -> Result(Result(a, e), DecodeError) =
    "gleam_stdlib" "decode_result"
}

if javascript {
  external fn decode_result(Dynamic) -> Result(Result(a, e), DecodeError) =
    "../gleam_stdlib.js" "decode_result"
}

/// Checks to see whether a Dynamic value is a result of a particular type, and
/// return the result if it is
///
/// The `ok` and `error` arguments are decoders for decoding the `Ok` and
/// `Error` values of the result.
///
/// ## Examples
///
///    > typed_result(of: from(Ok(1)), ok: int, error: string)
///    Ok(Ok(1))
///
///    > typed_result(of: from(Error("boom")), ok: int, error: string)
///    Ok(Error("boom"))
///
///    > typed_result(of: from(123), ok: int, error: string)
///    Error(DecodeError(expected: "2 element tuple", found: "Int"))
///
pub fn typed_result(
  of dynamic: Dynamic,
  ok decode_ok: Decoder(a),
  error decode_error: Decoder(e),
) -> Result(Result(a, e), DecodeError) {
  try inner_result = result(dynamic)

  case inner_result {
    Ok(raw) ->
      raw
      |> decode_ok
      |> result.map(Ok)
    Error(raw) ->
      raw
      |> decode_error
      |> result.map(Error)
  }
}

/// Checks to see whether a Dynamic value is a list of a particular type, and
/// return the list if it is.
///
/// The second argument is a decoder function used to decode the elements of
/// the list. The list is only decoded if all elements in the list can be
/// successfully decoded using this function.
///
/// If you do not wish to decode all the elements in the list use the `list`
/// function instead.
///
/// ## Examples
///
///    > typed_list(from(["a", "b", "c"]), of: string)
///    Ok(["a", "b", "c"])
///
///    > typed_list(from([1, 2, 3]), of: string)
///    Error(DecodeError(expected: "String", found: "Int"))
///
///    > typed_list(from("ok"), of: string)
///    Error(DecodeError(expected: "List", found: "String"))
///
pub fn typed_list(
  from dynamic: Dynamic,
  of decoder_type: fn(Dynamic) -> Result(inner, DecodeError),
) -> Result(List(inner), DecodeError) {
  dynamic
  |> list
  |> result.then(list.try_map(_, decoder_type))
}

/// Checks to see if a Dynamic value is a nullable version of a particular
/// type, and return the Option if it is.
///
/// ## Examples
///
///    > option(from("Hello"), string)
///    Ok(Some("Hello"))
///
///    > option(from("Hello"), string)
///    Ok(Some("Hello"))
///
///    > option(from(atom.from_string("null")), string)
///    Ok(None)
///
///    > option(from(atom.from_string("nil")), string)
///    Ok(None)
///
///    > option(from(atom.from_string("undefined")), string)
///    Ok(None)
///
///    > option(from(123), string)
///    Error(DecodeError(expected: "BitString", found: "Int"))
///
pub fn optional(
  from value: Dynamic,
  of decode: Decoder(inner),
) -> Result(Option(inner), DecodeError) {
  decode_optional(value, decode)
}

if erlang {
  external fn decode_optional(
    Dynamic,
    Decoder(a),
  ) -> Result(Option(a), DecodeError) =
    "gleam_stdlib" "decode_option"
}

if javascript {
  external fn decode_optional(
    Dynamic,
    Decoder(a),
  ) -> Result(Option(a), DecodeError) =
    "../gleam_stdlib.js" "decode_option"
}

/// Checks to see if a Dynamic value is a map with a specific field, and return
/// the value of the field if it is.
///
/// This will not succeed on a record.
///
/// ## Examples
///
///    > import gleam/map
///    > field(from(map.new("Hello", "World")), "Hello")
///    Ok(Dynamic)
///
///    > field(from(123), "Hello")
///    Error(DecodeError(expected: "Map", found: "Int"))
///
pub fn field(from value: Dynamic, named name: a) -> Result(Dynamic, DecodeError) {
  decode_field(value, name)
}

if erlang {
  external fn decode_field(Dynamic, name) -> Result(Dynamic, DecodeError) =
    "gleam_stdlib" "decode_field"
}

if javascript {
  external fn decode_field(Dynamic, name) -> Result(Dynamic, DecodeError) =
    "../gleam_stdlib.js" "decode_field"
}

/// Checks to see if the Dynamic value is a tuple large enough to have a certain
/// index, and return the value of that index if it is.
///
/// ## Examples
///
///    > element(from(#(1, 2)), 0)
///    Ok(from(1))
///
///    > element(from(#(1, 2)), 2)
///    Error(DecodeError(expected: "3 element tuple", found: "2 element tuple"))
///
///    > element(from(""), 2)
///    Error(DecodeError(expected: "Tuple", found: "String"))
///
pub fn element(
  from data: Dynamic,
  get index: Int,
) -> Result(Dynamic, DecodeError) {
  try tuple = decode_tuple(data)
  let size = tuple_size(tuple)
  case index >= 0 {
    True ->
      case index < size {
        True -> tuple_get(tuple, index)
        False -> at_least_decode_tuple_error(index + 1, data)
      }
    False ->
      case int.absolute_value(index) <= size {
        True -> tuple_get(tuple, size + index)
        False -> at_least_decode_tuple_error(int.absolute_value(index), data)
      }
  }
}

fn exact_decode_tuple_error(size: Int, data: Dynamic) -> Result(a, DecodeError) {
  let s = case size {
    0 -> ""
    _ -> "s"
  }
  ["Tuple of ", int.to_string(size), " element", s]
  |> string_builder.from_strings
  |> string_builder.to_string
  |> DecodeError(found: classify(data))
  |> Error
}

fn at_least_decode_tuple_error(
  size: Int,
  data: Dynamic,
) -> Result(a, DecodeError) {
  let s = case size {
    0 -> ""
    _ -> "s"
  }
  ["Tuple of at least ", int.to_string(size), " element", s]
  |> string_builder.from_strings
  |> string_builder.to_string
  |> DecodeError(found: classify(data))
  |> Error
}

// A tuple of unknown size
external type UnknownTuple

if erlang {
  external fn decode_tuple(Dynamic) -> Result(UnknownTuple, DecodeError) =
    "gleam_stdlib" "decode_tuple"

  external fn tuple_get(UnknownTuple, Int) -> Result(Dynamic, DecodeError) =
    "gleam_stdlib" "tuple_get"

  external fn tuple_size(UnknownTuple) -> Int =
    "gleam_stdlib" "size_of_tuple"
}

if javascript {
  external fn decode_tuple(Dynamic) -> Result(UnknownTuple, DecodeError) =
    "../gleam_stdlib.js" "decode_tuple"

  external fn tuple_get(UnknownTuple, Int) -> Result(Dynamic, DecodeError) =
    "../gleam_stdlib.js" "tuple_get"

  external fn tuple_size(UnknownTuple) -> Int =
    "../gleam_stdlib.js" "length"
}

/// Checks to see if the Dynamic value is a 2 element tuple.
///
/// If you do not wish to decode all the elements in the tuple use the
/// `typed_tuple2` function instead.
///
/// ## Examples
///
///    > tuple2(from(#(1, 2)))
///    Ok(#(from(1), from(2)))
///
///    > tuple2(from(#(1, 2, 3)))
///    Error(DecodeError(expected: "2 element tuple", found: "3 element tuple"))
///
///    > tuple2(from(""))
///    Error(DecodeError(expected: "2 element tuple", found: "String"))
///
pub fn tuple2(from value: Dynamic) -> Result(#(Dynamic, Dynamic), DecodeError) {
  try _ = assert_is_tuple(value, 2)
  Ok(unsafe_coerce(value))
}

fn assert_is_tuple(
  value: Dynamic,
  desired_size: Int,
) -> Result(Nil, DecodeError) {
  let expected =
    string_builder.to_string(string_builder.from_strings([
      "Tuple of ",
      int.to_string(desired_size),
      " elements",
    ]))
  try tuple = put_expected(decode_tuple(value), expected)
  case tuple_size(tuple) {
    size if size == desired_size -> Ok(Nil)
    _ -> exact_decode_tuple_error(desired_size, value)
  }
}

fn put_expected(
  result: Result(a, DecodeError),
  expected: String,
) -> Result(a, DecodeError) {
  case result {
    Ok(_) -> result
    Error(e) -> Error(DecodeError(..e, expected: expected))
  }
}

/// Checks to see if the Dynamic value is a 2 element tuple containing two
/// specifically typed elements.
///
/// If you wish to decode all the elements in the list use the `typed_tuple2`
/// instead.
///
/// ## Examples
///
///    > typed_tuple2(from(#(1, 2)), int, int)
///    Ok(#(1, 2))
///
///    > typed_tuple2(from(#(1, 2.0)), int, float)
///    Ok(#(1, 2.0))
///
///    > typed_tuple2(from(#(1, 2, 3)), int, float)
///    Error(DecodeError(expected: "2 element tuple", found: "3 element tuple"))
///
///    > typed_tuple2(from(""), int, float)
///    Error(DecodeError(expected: "2 element tuple", found: "String"))
///
pub fn typed_tuple2(
  from tup: Dynamic,
  first decode_first: Decoder(a),
  second decode_second: Decoder(b),
) -> Result(#(a, b), DecodeError) {
  try #(first, second) = tuple2(tup)
  try a = decode_first(first)
  try b = decode_second(second)
  Ok(#(a, b))
}

/// Checks to see if the Dynamic value is a 3 element tuple.
///
/// If you do not wish to decode all the elements in the tuple use the
/// `typed_tuple3` function instead.
///
/// ## Examples
///
///    > tuple3(from(#(1, 2, 3)))
///    Ok(#(from(1), from(2), from(3)))
///
///    > tuple3(from(#(1, 2)))
///    Error(DecodeError(expected: "3 element tuple", found: "3 element tuple"))
///
///    > tuple3(from(""))
///    Error(DecodeError(expected: "3 element tuple", found: "String"))
///
pub fn tuple3(
  from value: Dynamic,
) -> Result(#(Dynamic, Dynamic, Dynamic), DecodeError) {
  try _ = assert_is_tuple(value, 3)
  Ok(unsafe_coerce(value))
}

/// Checks to see if the Dynamic value is a 3 element tuple containing two
/// specifically typed elements.
///
/// If you wish to decode all the elements in the list use the `typed_tuple3`
/// instead.
///
/// ## Examples
///
///    > typed_tuple3(from(#(1, 2, 3)), int, int, int)
///    Ok(#(1, 2, 3))
///
///    > typed_tuple3(from(#(1, 2.0, "3")), int, float, string)
///    Ok(#(1, 2.0, "3"))
///
///    > typed_tuple3(from(#(1, 2)), int, float, string)
///    Error(DecodeError(expected: "3 element tuple", found: "2 element tuple"))
///
///    > typed_tuple3(from(""), int, float, string)
///    Error(DecodeError(expected: "3 element tuple", found: "String"))
///
pub fn typed_tuple3(
  from tup: Dynamic,
  first decode_first: Decoder(a),
  second decode_second: Decoder(b),
  third decode_third: Decoder(c),
) -> Result(#(a, b, c), DecodeError) {
  try #(first, second, third) = tuple3(tup)
  try a = decode_first(first)
  try b = decode_second(second)
  try c = decode_third(third)
  Ok(#(a, b, c))
}

/// Checks to see if the Dynamic value is a 4 element tuple.
///
/// If you do not wish to decode all the elements in the tuple use the
/// `typed_tuple4` function instead.
///
/// ## Examples
///
///    > tuple4(from(#(1, 2, 3, 4)))
///    Ok(#(from(1), from(2), from(3), from(4)))
///
///    > tuple4(from(#(1, 2)))
///    Error(DecodeError(expected: "4 element tuple", found: "2 element tuple"))
///
///    > tuple4(from(""))
///    Error(DecodeError(expected: "4 element tuple", found: "String"))
///
pub fn tuple4(
  from value: Dynamic,
) -> Result(#(Dynamic, Dynamic, Dynamic, Dynamic), DecodeError) {
  try _ = assert_is_tuple(value, 4)
  Ok(unsafe_coerce(value))
}

/// Checks to see if the Dynamic value is a 4 element tuple containing two
/// specifically typed elements.
///
/// If you wish to decode all the elements in the list use the `typed_tuple4`
/// instead.
///
/// ## Examples
///
///    > typed_tuple4(from(#(1, 2, 3, 4)), int, int, int, int)
///    Ok(#(1, 2, 3, 4))
///
///    > typed_tuple4(from(#(1, 2.0, "3", 4)), int, float, string, int)
///    Ok(#(1, 2.0, "3", 4))
///
///    > typed_tuple4(from(#(1, 2)), int, float, string, int)
///    Error("Expected a 4 element tuple, found a 2 element tuple")
///    Error(DecodeError(expected: "4 element tuple", found: "2 element tuple"))
///
///    > typed_tuple4(from(""), int, float, string, int)
///    Error(DecodeError(expected: "4 element tuple", found: "String"))
///
pub fn typed_tuple4(
  from tup: Dynamic,
  first decode_first: Decoder(a),
  second decode_second: Decoder(b),
  third decode_third: Decoder(c),
  fourth decode_fourth: Decoder(d),
) -> Result(#(a, b, c, d), DecodeError) {
  try #(first, second, third, fourth) = tuple4(tup)
  try a = decode_first(first)
  try b = decode_second(second)
  try c = decode_third(third)
  try d = decode_fourth(fourth)
  Ok(#(a, b, c, d))
}

/// Checks to see if the Dynamic value is a 5 element tuple.
///
/// If you do not wish to decode all the elements in the tuple use the
/// `typed_tuple5` function instead.
///
/// ## Examples
///
///    > tuple5(from(#(1, 2, 3, 4, 5)))
///    Ok(#(from(1), from(2), from(3), from(4), from(5)))
///
///    > tuple5(from(#(1, 2)))
///    Error(DecodeError(expected: "5 element tuple", found: "2 element tuple"))
///
///    > tuple5(from(""))
///    Error(DecodeError(expected: "5 element tuple", found: "String"))
///
pub fn tuple5(
  from value: Dynamic,
) -> Result(#(Dynamic, Dynamic, Dynamic, Dynamic, Dynamic), DecodeError) {
  try _ = assert_is_tuple(value, 5)
  Ok(unsafe_coerce(value))
}

/// Checks to see if the Dynamic value is a 5 element tuple containing two
/// specifically typed elements.
///
/// If you wish to decode all the elements in the list use the `typed_tuple5`
/// instead.
///
/// ## Examples
///
///    > typed_tuple5(from(#(1, 2, 3, 4, 5)), int, int, int, int, int)
///    Ok(#(1, 2, 3, 4, 5))
///
///    > typed_tuple5(from(#(1, 2.0, "3", 4, 5)), int, float, string, int, int)
///    Ok(#(1, 2.0, "3", 4, 5))
///
///    > typed_tuple5(from(#(1, 2)), int, float, string, int, int)
///    Error(DecodeError(expected: "5 element tuple", found: "2 element tuple"))
///
///    > typed_tuple5(from(""), int, float, string, int, int)
///    Error(DecodeError(expected: "5 element tuple", found: "String"))
///
pub fn typed_tuple5(
  from tup: Dynamic,
  first decode_first: Decoder(a),
  second decode_second: Decoder(b),
  third decode_third: Decoder(c),
  fourth decode_fourth: Decoder(d),
  fifth decode_fifth: Decoder(e),
) -> Result(#(a, b, c, d, e), DecodeError) {
  try #(first, second, third, fourth, fifth) = tuple5(tup)
  try a = decode_first(first)
  try b = decode_second(second)
  try c = decode_third(third)
  try d = decode_fourth(fourth)
  try e = decode_fifth(fifth)
  Ok(#(a, b, c, d, e))
}

/// Checks to see if the Dynamic value is a 6 element tuple.
///
/// If you do not wish to decode all the elements in the tuple use the
/// `typed_tuple6` function instead.
///
/// ## Examples
///
///    > tuple6(from(#(1, 2, 3, 4, 5, 6)))
///    Ok(#(from(1), from(2), from(3), from(4), from(5), from(6)))
///
///    > tuple6(from(#(1, 2)))
///    Error(DecodeError(expected: "6 element tuple", found: "2 element tuple"))
///
///    > tuple6(from(""))
///    Error(DecodeError(expected: "6 element tuple", found: "String"))
///
pub fn tuple6(
  from value: Dynamic,
) -> Result(
  #(Dynamic, Dynamic, Dynamic, Dynamic, Dynamic, Dynamic),
  DecodeError,
) {
  try _ = assert_is_tuple(value, 6)
  Ok(unsafe_coerce(value))
}

/// Checks to see if the Dynamic value is a 6 element tuple containing two
/// specifically typed elements.
///
/// If you wish to decode all the elements in the list use the `typed_tuple6`
/// instead.
///
/// ## Examples
///
///    > typed_tuple6(from(#(1, 2, 3, 4, 5, 6)), int, int, int, int, int, int)
///    Ok(#(1, 2, 3, 4, 5, 6))
///
///    > typed_tuple6(from(#(1, 2.0, "3", 4, 5, 6)), int, float, string, int, int)
///    Ok(#(1, 2.0, "3", 4, 5, 6))
///
///    > typed_tuple6(from(#(1, 2)), int, float, string, int, int, int)
///    Error(DecodeError(expected: "6 element tuple", found: "2 element tuple"))
///
///    > typed_tuple6(from(""), int, float, string, int, int, int)
///    Error(DecodeError(expected: "6 element tuple", found: "String"))
///
pub fn typed_tuple6(
  from tup: Dynamic,
  first decode_first: Decoder(a),
  second decode_second: Decoder(b),
  third decode_third: Decoder(c),
  fourth decode_fourth: Decoder(d),
  fifth decode_fifth: Decoder(e),
  sixth decode_sixth: Decoder(f),
) -> Result(#(a, b, c, d, e, f), DecodeError) {
  try #(first, second, third, fourth, fifth, sixth) = tuple6(tup)
  try a = decode_first(first)
  try b = decode_second(second)
  try c = decode_third(third)
  try d = decode_fourth(fourth)
  try e = decode_fifth(fifth)
  try f = decode_sixth(sixth)
  Ok(#(a, b, c, d, e, f))
}

/// Checks to see if the Dynamic value is map.
///
/// ## Examples
///
///    > import gleam/map
///    > map(from(map.new()))
///    Ok(map.new())
///
///    > map(from(1))
///    Error(DecodeError(expected: "Map", found: "Int"))
///
///    > map(from(""))
///    Error(DecodeError(expected: "Map", found: "String"))
///
pub fn map(from value: Dynamic) -> Result(Map(Dynamic, Dynamic), DecodeError) {
  decode_map(value)
}

if erlang {
  external fn decode_map(Dynamic) -> Result(Map(Dynamic, Dynamic), DecodeError) =
    "gleam_stdlib" "decode_map"
}

if javascript {
  external fn decode_map(Dynamic) -> Result(Map(Dynamic, Dynamic), DecodeError) =
    "../gleam_stdlib.js" "decode_map"
}

if erlang {
  /// Joins multiple decoders into one. When run they will each be tried in turn
  /// until one succeeds, or they all fail.
  ///
  /// ## Examples
  ///
  ///    > import gleam/result
  ///    > let bool_or_string = any(_, of: [
  ///    >   string,
  ///    >   fn(x) { result.map(bool(x), fn(_) { "a bool" }) }
  ///    > ])
  ///    > bool_or_string(from("ok"))
  ///    Ok("ok")
  ///
  ///    > bool_or_string(from(True))
  ///    Ok("a bool")
  ///
  ///    > bool_or_string(from(1))
  ///    Error(DecodeError(expected: "unknown", found: "unknown"))
  ///
  pub fn any(
    from data: Dynamic,
    of decoders: List(Decoder(t)),
  ) -> Result(t, DecodeError) {
    decoders
    |> list.find_map(fn(decoder) { decoder(data) })
    |> result.map_error(fn(_) {
      DecodeError(expected: "unknown", found: "unknown")
    })
  }
}
