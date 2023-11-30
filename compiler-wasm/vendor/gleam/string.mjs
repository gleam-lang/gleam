import { Ok, Error, toList, makeError, remainderInt, divideInt } from "../gleam.mjs";
import * as $iterator from "../gleam/iterator.mjs";
import * as $list from "../gleam/list.mjs";
import * as $option from "../gleam/option.mjs";
import { None, Some } from "../gleam/option.mjs";
import * as $order from "../gleam/order.mjs";
import * as $string_builder from "../gleam/string_builder.mjs";
import {
  string_length as do_length,
  lowercase as do_lowercase,
  uppercase as do_uppercase,
  less_than,
  crop_string as crop,
  contains_string as contains,
  starts_with as do_starts_with,
  ends_with as do_ends_with,
  split_once as do_split_once,
  join as do_join,
  trim as do_trim,
  trim_left as do_trim_left,
  trim_right as do_trim_right,
  pop_grapheme as do_pop_grapheme,
  graphemes as to_graphemes,
  codepoint as unsafe_int_to_utf_codepoint,
  string_to_codepoint_integer_list,
  utf_codepoint_list_to_string as from_utf_codepoints,
  utf_codepoint_to_int as do_utf_codepoint_to_int,
  inspect as do_inspect,
  byte_size,
} from "../gleam_stdlib.mjs";

export { byte_size, contains, crop, from_utf_codepoints, to_graphemes };

export function is_empty(str) {
  return str === "";
}

export function length(string) {
  return do_length(string);
}

export function replace(string, pattern, substitute) {
  let _pipe = string;
  let _pipe$1 = $string_builder.from_string(_pipe);
  let _pipe$2 = $string_builder.replace(_pipe$1, pattern, substitute);
  return $string_builder.to_string(_pipe$2);
}

export function lowercase(string) {
  return do_lowercase(string);
}

export function uppercase(string) {
  return do_uppercase(string);
}

export function compare(a, b) {
  let $ = a === b;
  if ($) {
    return new $order.Eq();
  } else {
    let $1 = less_than(a, b);
    if ($1) {
      return new $order.Lt();
    } else {
      return new $order.Gt();
    }
  }
}

export function starts_with(string, prefix) {
  return do_starts_with(string, prefix);
}

export function ends_with(string, suffix) {
  return do_ends_with(string, suffix);
}

export function split_once(x, substring) {
  return do_split_once(x, substring);
}

export function append(first, second) {
  let _pipe = first;
  let _pipe$1 = $string_builder.from_string(_pipe);
  let _pipe$2 = $string_builder.append(_pipe$1, second);
  return $string_builder.to_string(_pipe$2);
}

export function concat(strings) {
  let _pipe = strings;
  let _pipe$1 = $string_builder.from_strings(_pipe);
  return $string_builder.to_string(_pipe$1);
}

export function repeat(string, times) {
  let _pipe = $iterator.repeat(string);
  let _pipe$1 = $iterator.take(_pipe, times);
  let _pipe$2 = $iterator.to_list(_pipe$1);
  return concat(_pipe$2);
}

export function join(strings, separator) {
  return do_join(strings, separator);
}

export function trim(string) {
  return do_trim(string);
}

export function trim_left(string) {
  return do_trim_left(string);
}

export function trim_right(string) {
  return do_trim_right(string);
}

export function pop_grapheme(string) {
  return do_pop_grapheme(string);
}

function do_to_graphemes(loop$string, loop$acc) {
  while (true) {
    let string = loop$string;
    let acc = loop$acc;
    let $ = pop_grapheme(string);
    if ($.isOk()) {
      let grapheme = $[0][0];
      let rest = $[0][1];
      loop$string = rest;
      loop$acc = toList([grapheme], acc);
    } else {
      return acc;
    }
  }
}

function do_reverse(string) {
  let _pipe = string;
  let _pipe$1 = to_graphemes(_pipe);
  let _pipe$2 = $list.reverse(_pipe$1);
  return concat(_pipe$2);
}

export function reverse(string) {
  return do_reverse(string);
}

function do_slice(string, idx, len) {
  let _pipe = string;
  let _pipe$1 = to_graphemes(_pipe);
  let _pipe$2 = $list.drop(_pipe$1, idx);
  let _pipe$3 = $list.take(_pipe$2, len);
  return concat(_pipe$3);
}

export function slice(string, idx, len) {
  let $ = len < 0;
  if ($) {
    return "";
  } else if (!$) {
    let $1 = idx < 0;
    if ($1) {
      let translated_idx = length(string) + idx;
      let $2 = translated_idx < 0;
      if ($2) {
        return "";
      } else if (!$2) {
        return do_slice(string, translated_idx, len);
      } else {
        throw makeError(
          "case_no_match",
          "gleam/string",
          223,
          "slice",
          "No case clause matched",
          { values: [$2] }
        )
      }
    } else if (!$1) {
      return do_slice(string, idx, len);
    } else {
      throw makeError(
        "case_no_match",
        "gleam/string",
        220,
        "slice",
        "No case clause matched",
        { values: [$1] }
      )
    }
  } else {
    throw makeError(
      "case_no_match",
      "gleam/string",
      217,
      "slice",
      "No case clause matched",
      { values: [$] }
    )
  }
}

export function drop_left(string, num_graphemes) {
  let $ = num_graphemes < 0;
  if ($) {
    return string;
  } else if (!$) {
    return slice(string, num_graphemes, length(string) - num_graphemes);
  } else {
    throw makeError(
      "case_no_match",
      "gleam/string",
      270,
      "drop_left",
      "No case clause matched",
      { values: [$] }
    )
  }
}

export function drop_right(string, num_graphemes) {
  let $ = num_graphemes < 0;
  if ($) {
    return string;
  } else if (!$) {
    return slice(string, 0, length(string) - num_graphemes);
  } else {
    throw makeError(
      "case_no_match",
      "gleam/string",
      286,
      "drop_right",
      "No case clause matched",
      { values: [$] }
    )
  }
}

export function split(x, substring) {
  if (substring === "") {
    return to_graphemes(x);
  } else {
    let _pipe = x;
    let _pipe$1 = $string_builder.from_string(_pipe);
    let _pipe$2 = $string_builder.split(_pipe$1, substring);
    return $list.map(_pipe$2, $string_builder.to_string);
  }
}

function padding(size, pad_string) {
  let pad_length = length(pad_string);
  let num_pads = divideInt(size, pad_length);
  let extra = remainderInt(size, pad_length);
  let _pipe = $iterator.repeat(pad_string);
  let _pipe$1 = $iterator.take(_pipe, num_pads);
  return $iterator.append(
    _pipe$1,
    $iterator.single(slice(pad_string, 0, extra)),
  );
}

export function pad_left(string, desired_length, pad_string) {
  let current_length = length(string);
  let to_pad_length = desired_length - current_length;
  let _pipe = padding(to_pad_length, pad_string);
  let _pipe$1 = $iterator.append(_pipe, $iterator.single(string));
  let _pipe$2 = $iterator.to_list(_pipe$1);
  return concat(_pipe$2);
}

export function pad_right(string, desired_length, pad_string) {
  let current_length = length(string);
  let to_pad_length = desired_length - current_length;
  let _pipe = $iterator.single(string);
  let _pipe$1 = $iterator.append(_pipe, padding(to_pad_length, pad_string));
  let _pipe$2 = $iterator.to_list(_pipe$1);
  return concat(_pipe$2);
}

function do_to_utf_codepoints(string) {
  let _pipe = string;
  let _pipe$1 = string_to_codepoint_integer_list(_pipe);
  return $list.map(_pipe$1, unsafe_int_to_utf_codepoint);
}

export function to_utf_codepoints(string) {
  return do_to_utf_codepoints(string);
}

export function utf_codepoint(value) {
  if (value > 1_114_111) {
    let i = value;
    return new Error(undefined);
  } else if (value === 65_534) {
    return new Error(undefined);
  } else if (value === 65_535) {
    return new Error(undefined);
  } else if ((value >= 55_296) && (value <= 57_343)) {
    let i = value;
    return new Error(undefined);
  } else {
    let i = value;
    return new Ok(unsafe_int_to_utf_codepoint(i));
  }
}

export function utf_codepoint_to_int(cp) {
  return do_utf_codepoint_to_int(cp);
}

export function to_option(s) {
  if (s === "") {
    return new None();
  } else {
    return new Some(s);
  }
}

export function first(s) {
  let $ = pop_grapheme(s);
  if ($.isOk()) {
    let first$1 = $[0][0];
    return new Ok(first$1);
  } else if (!$.isOk()) {
    let e = $[0];
    return new Error(e);
  } else {
    throw makeError(
      "case_no_match",
      "gleam/string",
      841,
      "first",
      "No case clause matched",
      { values: [$] }
    )
  }
}

export function last(s) {
  let $ = pop_grapheme(s);
  if ($.isOk() && $[0][1] === "") {
    let first$1 = $[0][0];
    return new Ok(first$1);
  } else if ($.isOk()) {
    let rest = $[0][1];
    return new Ok(slice(rest, -1, 1));
  } else if (!$.isOk()) {
    let e = $[0];
    return new Error(e);
  } else {
    throw makeError(
      "case_no_match",
      "gleam/string",
      864,
      "last",
      "No case clause matched",
      { values: [$] }
    )
  }
}

export function capitalise(s) {
  let $ = pop_grapheme(s);
  if ($.isOk()) {
    let first$1 = $[0][0];
    let rest = $[0][1];
    return append(uppercase(first$1), lowercase(rest));
  } else {
    return "";
  }
}

export function inspect(term) {
  let _pipe = do_inspect(term);
  return $string_builder.to_string(_pipe);
}
