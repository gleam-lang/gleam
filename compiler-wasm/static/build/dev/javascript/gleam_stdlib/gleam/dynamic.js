import { Ok, Error, toList, CustomType, throwError } from "../gleam.js";
import * as $bit_string from "../gleam/bit_string.js";
import * as $int from "../gleam/int.js";
import * as $list from "../gleam/list.js";
import * as $map from "../gleam/map.js";
import * as $option from "../gleam/option.js";
import * as $result from "../gleam/result.js";
import * as $string_builder from "../gleam/string_builder.js";
import {
  identity as do_from,
  identity as do_unsafe_coerce,
  decode_bit_string,
  decode_string,
  classify_dynamic as do_classify,
  decode_int,
  decode_float,
  decode_bool,
  decode_list,
  decode_result,
  decode_option as decode_optional,
  decode_field,
  decode_tuple,
  tuple_get,
  length as tuple_size,
  decode_map,
} from "../gleam_stdlib.js";

export class DecodeError extends CustomType {
  constructor(expected, found) {
    super();
    this.expected = expected;
    this.found = found;
  }
}

export function from(a) {
  return do_from(a);
}

export function unsafe_coerce(a) {
  return do_unsafe_coerce(a);
}

export function bit_string(data) {
  return decode_bit_string(data);
}

export function string(data) {
  return decode_string(data);
}

export function classify(data) {
  return do_classify(data);
}

export function int(data) {
  return decode_int(data);
}

export function float(data) {
  return decode_float(data);
}

export function bool(data) {
  return decode_bool(data);
}

export function list(value) {
  return decode_list(value);
}

export function result(value) {
  return decode_result(value);
}

export function typed_result(dynamic, decode_ok, decode_error) {
  let $ = result(dynamic);
  if (!$.isOk()) return $;
  let inner_result = $[0];

  if (inner_result.isOk()) {
    let raw = inner_result[0];
    let _pipe = raw;
    let _pipe$1 = decode_ok(_pipe);
    return $result.map(_pipe$1, (var0) => { return new Ok(var0); });
  } else if (!inner_result.isOk()) {
    let raw = inner_result[0];
    let _pipe = raw;
    let _pipe$1 = decode_error(_pipe);
    return $result.map(_pipe$1, (var0) => { return new Error(var0); });
  } else {
    throwError(
      "case_no_match",
      "gleam/dynamic",
      297,
      "typed_result",
      "No case clause matched",
      { values: [inner_result] }
    );
  }
}

export function typed_list(dynamic, decoder_type) {
  let _pipe = dynamic;
  let _pipe$1 = list(_pipe);
  return $result.then(
    _pipe$1,
    (_capture) => { return $list.try_map(_capture, decoder_type); },
  );
}

export function optional(value, decode) {
  return decode_optional(value, decode);
}

export function field(value, name) {
  return decode_field(value, name);
}

export function element(data, index) {
  let $ = decode_tuple(data);
  if (!$.isOk()) return $;
  let tuple = $[0];

  let size = tuple_size(tuple);
  let $1 = index >= 0;
  if ($1) {
    let $2 = index < size;
    if ($2) {
      return tuple_get(tuple, index);
    } else if (!$2) {
      return at_least_decode_tuple_error(index + 1, data);
    } else {
      throwError(
        "case_no_match",
        "gleam/dynamic",
        435,
        "element",
        "No case clause matched",
        { values: [$2] }
      );
    }
  } else if (!$1) {
    let $2 = $int.absolute_value(index) <= size;
    if ($2) {
      return tuple_get(tuple, size + index);
    } else if (!$2) {
      return at_least_decode_tuple_error($int.absolute_value(index), data);
    } else {
      throwError(
        "case_no_match",
        "gleam/dynamic",
        440,
        "element",
        "No case clause matched",
        { values: [$2] }
      );
    }
  } else {
    throwError(
      "case_no_match",
      "gleam/dynamic",
      433,
      "element",
      "No case clause matched",
      { values: [$1] }
    );
  }
}

function exact_decode_tuple_error(size, data) {
  let s = (() => {
    if (size === 0) {
      return "";
    } else {
      return "s";
    }
  })();
  let _pipe = toList(["Tuple of ", $int.to_string(size), " element", s]);
  let _pipe$1 = $string_builder.from_strings(_pipe);
  let _pipe$2 = $string_builder.to_string(_pipe$1);
  let _pipe$3 = new DecodeError(_pipe$2, classify(data));
  return new Error(_pipe$3);
}

function at_least_decode_tuple_error(size, data) {
  let s = (() => {
    if (size === 0) {
      return "";
    } else {
      return "s";
    }
  })();
  let _pipe = toList(["Tuple of at least ", $int.to_string(size), " element", s]);
  let _pipe$1 = $string_builder.from_strings(_pipe);
  let _pipe$2 = $string_builder.to_string(_pipe$1);
  let _pipe$3 = new DecodeError(_pipe$2, classify(data));
  return new Error(_pipe$3);
}

export function tuple2(value) {
  let $ = assert_is_tuple(value, 2);
  if (!$.isOk()) return $;
  return new Ok(unsafe_coerce(value));
}

function assert_is_tuple(value, desired_size) {
  let expected = $string_builder.to_string(
    $string_builder.from_strings(
      toList(["Tuple of ", $int.to_string(desired_size), " elements"]),
    ),
  );
  let $ = put_expected(decode_tuple(value), expected);
  if (!$.isOk()) return $;
  let tuple = $[0];

  let $1 = tuple_size(tuple);
  if ($1 === desired_size) {
    let size = $1;
    return new Ok(undefined);
  } else {
    return exact_decode_tuple_error(desired_size, value);
  }
}

function put_expected(result, expected) {
  if (result.isOk()) {
    return result;
  } else if (!result.isOk()) {
    let e = result[0];
    return new Error(e.withFields({ expected: expected }));
  } else {
    throwError(
      "case_no_match",
      "gleam/dynamic",
      541,
      "put_expected",
      "No case clause matched",
      { values: [result] }
    );
  }
}

export function typed_tuple2(tup, decode_first, decode_second) {
  let $ = tuple2(tup);
  if (!$.isOk()) return $;
  let first = $[0][0];
  let second = $[0][1];

  let $1 = decode_first(first);
  if (!$1.isOk()) return $1;
  let a = $1[0];

  let $2 = decode_second(second);
  if (!$2.isOk()) return $2;
  let b = $2[0];

  return new Ok([a, b]);
}

export function tuple3(value) {
  let $ = assert_is_tuple(value, 3);
  if (!$.isOk()) return $;
  return new Ok(unsafe_coerce(value));
}

export function typed_tuple3(tup, decode_first, decode_second, decode_third) {
  let $ = tuple3(tup);
  if (!$.isOk()) return $;
  let first = $[0][0];
  let second = $[0][1];
  let third = $[0][2];

  let $1 = decode_first(first);
  if (!$1.isOk()) return $1;
  let a = $1[0];

  let $2 = decode_second(second);
  if (!$2.isOk()) return $2;
  let b = $2[0];

  let $3 = decode_third(third);
  if (!$3.isOk()) return $3;
  let c = $3[0];

  return new Ok([a, b, c]);
}

export function tuple4(value) {
  let $ = assert_is_tuple(value, 4);
  if (!$.isOk()) return $;
  return new Ok(unsafe_coerce(value));
}

export function typed_tuple4(
  tup,
  decode_first,
  decode_second,
  decode_third,
  decode_fourth
) {
  let $ = tuple4(tup);
  if (!$.isOk()) return $;
  let first = $[0][0];
  let second = $[0][1];
  let third = $[0][2];
  let fourth = $[0][3];

  let $1 = decode_first(first);
  if (!$1.isOk()) return $1;
  let a = $1[0];

  let $2 = decode_second(second);
  if (!$2.isOk()) return $2;
  let b = $2[0];

  let $3 = decode_third(third);
  if (!$3.isOk()) return $3;
  let c = $3[0];

  let $4 = decode_fourth(fourth);
  if (!$4.isOk()) return $4;
  let d = $4[0];

  return new Ok([a, b, c, d]);
}

export function tuple5(value) {
  let $ = assert_is_tuple(value, 5);
  if (!$.isOk()) return $;
  return new Ok(unsafe_coerce(value));
}

export function typed_tuple5(
  tup,
  decode_first,
  decode_second,
  decode_third,
  decode_fourth,
  decode_fifth
) {
  let $ = tuple5(tup);
  if (!$.isOk()) return $;
  let first = $[0][0];
  let second = $[0][1];
  let third = $[0][2];
  let fourth = $[0][3];
  let fifth = $[0][4];

  let $1 = decode_first(first);
  if (!$1.isOk()) return $1;
  let a = $1[0];

  let $2 = decode_second(second);
  if (!$2.isOk()) return $2;
  let b = $2[0];

  let $3 = decode_third(third);
  if (!$3.isOk()) return $3;
  let c = $3[0];

  let $4 = decode_fourth(fourth);
  if (!$4.isOk()) return $4;
  let d = $4[0];

  let $5 = decode_fifth(fifth);
  if (!$5.isOk()) return $5;
  let e = $5[0];

  return new Ok([a, b, c, d, e]);
}

export function tuple6(value) {
  let $ = assert_is_tuple(value, 6);
  if (!$.isOk()) return $;
  return new Ok(unsafe_coerce(value));
}

export function typed_tuple6(
  tup,
  decode_first,
  decode_second,
  decode_third,
  decode_fourth,
  decode_fifth,
  decode_sixth
) {
  let $ = tuple6(tup);
  if (!$.isOk()) return $;
  let first = $[0][0];
  let second = $[0][1];
  let third = $[0][2];
  let fourth = $[0][3];
  let fifth = $[0][4];
  let sixth = $[0][5];

  let $1 = decode_first(first);
  if (!$1.isOk()) return $1;
  let a = $1[0];

  let $2 = decode_second(second);
  if (!$2.isOk()) return $2;
  let b = $2[0];

  let $3 = decode_third(third);
  if (!$3.isOk()) return $3;
  let c = $3[0];

  let $4 = decode_fourth(fourth);
  if (!$4.isOk()) return $4;
  let d = $4[0];

  let $5 = decode_fifth(fifth);
  if (!$5.isOk()) return $5;
  let e = $5[0];

  let $6 = decode_sixth(sixth);
  if (!$6.isOk()) return $6;
  let f = $6[0];

  return new Ok([a, b, c, d, e, f]);
}

export function map(value) {
  return decode_map(value);
}
