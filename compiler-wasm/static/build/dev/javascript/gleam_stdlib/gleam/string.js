import { Ok, Error, toList, throwError, divideInt } from "../gleam.js";
import * as $iterator from "../gleam/iterator.js";
import * as $list from "../gleam/list.js";
import * as $option from "../gleam/option.js";
import { None, Some } from "../gleam/option.js";
import * as $order from "../gleam/order.js";
import * as $result from "../gleam/result.js";
import * as $string_builder from "../gleam/string_builder.js";
import {
  string_length as do_length,
  lowercase as do_lowercase,
  uppercase as do_uppercase,
  less_than,
  slice_string as do_slice,
  crop_string as do_crop,
  index_of,
  starts_with as do_starts_with,
  ends_with as do_ends_with,
  split_once as do_split_once,
  trim as do_trim,
  trim_left as do_trim_left,
  trim_right as do_trim_right,
  pop_grapheme as do_pop_grapheme,
  codepoint as unsafe_int_to_utf_codepoint,
} from "../gleam_stdlib.js";

export function is_empty(str) {
  return str === "";
}

export function length(string) {
  return do_length(string);
}

export function reverse(string) {
  let _pipe = string;
  let _pipe$1 = $string_builder.from_string(_pipe);
  let _pipe$2 = $string_builder.reverse(_pipe$1);
  return $string_builder.to_string(_pipe$2);
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
        throwError(
          "case_no_match",
          "gleam/string",
          205,
          "slice",
          "No case clause matched",
          { values: [$2] }
        );
      }
    } else if (!$1) {
      return do_slice(string, idx, len);
    } else {
      throwError(
        "case_no_match",
        "gleam/string",
        202,
        "slice",
        "No case clause matched",
        { values: [$1] }
      );
    }
  } else {
    throwError(
      "case_no_match",
      "gleam/string",
      199,
      "slice",
      "No case clause matched",
      { values: [$] }
    );
  }
}

export function crop(string, substring) {
  return do_crop(string, substring);
}

export function drop_left(string, num_graphemes) {
  let $ = num_graphemes < 0;
  if ($) {
    return string;
  } else if (!$) {
    return slice(string, num_graphemes, length(string) - num_graphemes);
  } else {
    throwError(
      "case_no_match",
      "gleam/string",
      260,
      "drop_left",
      "No case clause matched",
      { values: [$] }
    );
  }
}

export function drop_right(string, num_graphemes) {
  let $ = num_graphemes < 0;
  if ($) {
    return string;
  } else if (!$) {
    return slice(string, 0, length(string) - num_graphemes);
  } else {
    throwError(
      "case_no_match",
      "gleam/string",
      273,
      "drop_right",
      "No case clause matched",
      { values: [$] }
    );
  }
}

export function contains(haystack, needle) {
  return do_contains(haystack, needle);
}

function do_contains(haystack, needle) {
  return index_of(haystack, needle) !== -1;
}

export function starts_with(string, prefix) {
  return do_starts_with(string, prefix);
}

export function ends_with(string, suffix) {
  return do_ends_with(string, suffix);
}

export function split(x, substring) {
  let _pipe = x;
  let _pipe$1 = $string_builder.from_string(_pipe);
  let _pipe$2 = $string_builder.split(_pipe$1, substring);
  return $list.map(_pipe$2, $string_builder.to_string);
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
  let _pipe = strings;
  let _pipe$1 = $list.intersperse(_pipe, separator);
  return concat(_pipe$1);
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

function padding(size, pad_string) {
  let pad_length = length(pad_string);
  let num_pads = divideInt(size, pad_length);
  let extra = size % pad_length;
  let _pipe = $iterator.repeat(pad_string);
  let _pipe$1 = $iterator.take(_pipe, num_pads);
  return $iterator.append(
    _pipe$1,
    $iterator.single(slice(pad_string, 0, extra)),
  );
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

export function to_graphemes(string) {
  let $ = pop_grapheme(string);
  if ($.isOk()) {
    let grapheme = $[0][0];
    let rest = $[0][1];
    return toList([grapheme], to_graphemes(rest));
  } else {
    return toList([]);
  }
}

export function utf_codepoint(value) {
  if (value > 1114111) {
    let i = value;
    return new Error(undefined);
  } else if (value === 65534) {
    return new Error(undefined);
  } else if (value === 65535) {
    return new Error(undefined);
  } else if ((value >= 55296) && (value <= 57343)) {
    let i = value;
    return new Error(undefined);
  } else {
    let i = value;
    return new Ok(unsafe_int_to_utf_codepoint(i));
  }
}

export function to_option(s) {
  if (s === "") {
    return new None();
  } else {
    return new Some(s);
  }
}
