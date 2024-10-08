import {
  Ok,
  Error,
  prepend as listPrepend,
  CustomType as $CustomType,
  remainderInt,
  divideInt,
} from "../gleam.mjs";
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
  string_slice as do_slice,
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

class Leading extends $CustomType {}

class Trailing extends $CustomType {}

class Both extends $CustomType {}

export function is_empty(str) {
  return str === "";
}

export function length(string) {
  return do_length(string);
}

function do_reverse(string) {
  let _pipe = string;
  let _pipe$1 = $string_builder.from_string(_pipe);
  let _pipe$2 = $string_builder.reverse(_pipe$1);
  return $string_builder.to_string(_pipe$2);
}

export function reverse(string) {
  return do_reverse(string);
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
  } else {
    let $1 = idx < 0;
    if ($1) {
      let translated_idx = length(string) + idx;
      let $2 = translated_idx < 0;
      if ($2) {
        return "";
      } else {
        return do_slice(string, translated_idx, len);
      }
    } else {
      return do_slice(string, idx, len);
    }
  }
}

export function drop_left(string, num_graphemes) {
  let $ = num_graphemes < 0;
  if ($) {
    return string;
  } else {
    return slice(string, num_graphemes, length(string) - num_graphemes);
  }
}

export function drop_right(string, num_graphemes) {
  let $ = num_graphemes < 0;
  if ($) {
    return string;
  } else {
    return slice(string, 0, length(string) - num_graphemes);
  }
}

export function starts_with(string, prefix) {
  return do_starts_with(string, prefix);
}

export function ends_with(string, suffix) {
  return do_ends_with(string, suffix);
}

export function split_once(string, substring) {
  return do_split_once(string, substring);
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

function do_repeat(loop$string, loop$times, loop$acc) {
  while (true) {
    let string = loop$string;
    let times = loop$times;
    let acc = loop$acc;
    let $ = times <= 0;
    if ($) {
      return acc;
    } else {
      loop$string = string;
      loop$times = times - 1;
      loop$acc = acc + string;
    }
  }
}

export function repeat(string, times) {
  return do_repeat(string, times, "");
}

export function join(strings, separator) {
  return do_join(strings, separator);
}

function padding(size, pad_string) {
  let pad_string_length = length(pad_string);
  let num_pads = divideInt(size, pad_string_length);
  let extra = remainderInt(size, pad_string_length);
  return repeat(pad_string, num_pads) + slice(pad_string, 0, extra);
}

export function pad_left(string, desired_length, pad_string) {
  let current_length = length(string);
  let to_pad_length = desired_length - current_length;
  let $ = to_pad_length <= 0;
  if ($) {
    return string;
  } else {
    return padding(to_pad_length, pad_string) + string;
  }
}

export function pad_right(string, desired_length, pad_string) {
  let current_length = length(string);
  let to_pad_length = desired_length - current_length;
  let $ = to_pad_length <= 0;
  if ($) {
    return string;
  } else {
    return string + padding(to_pad_length, pad_string);
  }
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
      loop$acc = listPrepend(grapheme, acc);
    } else {
      return acc;
    }
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

export function to_option(string) {
  if (string === "") {
    return new None();
  } else {
    return new Some(string);
  }
}

export function first(string) {
  let $ = pop_grapheme(string);
  if ($.isOk()) {
    let first$1 = $[0][0];
    return new Ok(first$1);
  } else {
    let e = $[0];
    return new Error(e);
  }
}

export function last(string) {
  let $ = pop_grapheme(string);
  if ($.isOk() && $[0][1] === "") {
    let first$1 = $[0][0];
    return new Ok(first$1);
  } else if ($.isOk()) {
    let rest = $[0][1];
    return new Ok(slice(rest, -1, 1));
  } else {
    let e = $[0];
    return new Error(e);
  }
}

export function capitalise(string) {
  let $ = pop_grapheme(string);
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
