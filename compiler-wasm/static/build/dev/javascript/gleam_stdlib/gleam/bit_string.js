import { toList } from "../gleam.js";
import {
  bit_string_from_string as do_from_string,
  length as do_byte_size,
  bit_string_slice as do_slice,
  bit_string_to_string as do_to_string,
  bit_string_concat as do_concat,
} from "../gleam_stdlib.js";

export function from_string(x) {
  return do_from_string(x);
}

export function byte_size(x) {
  return do_byte_size(x);
}

export function append(first, second) {
  return concat(toList([first, second]));
}

export function slice(string, position, length) {
  return do_slice(string, position, length);
}

export function is_utf8(bits) {
  return do_is_utf8(bits);
}

function do_is_utf8(bits) {
  let $ = to_string(bits);
  if ($.isOk()) {
    return true;
  } else {
    return false;
  }
}

export function to_string(bits) {
  return do_to_string(bits);
}

export function concat(bit_strings) {
  return do_concat(bit_strings);
}
