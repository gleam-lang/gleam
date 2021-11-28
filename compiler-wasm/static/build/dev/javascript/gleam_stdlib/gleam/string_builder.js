import { isEqual } from "../gleam.js";
import {
  add as do_append,
  join as do_from_strings,
  join as do_concat,
  identity as do_from_string,
  identity as do_to_string,
  length as do_byte_size,
  float_to_string as do_from_float,
  lowercase as do_lowercase,
  uppercase as do_uppercase,
  string_reverse as do_reverse,
  split as do_split,
  string_replace as do_replace,
  equal as do_is_equal,
} from "../gleam_stdlib.js";

export function prepend(builder, prefix) {
  return append_builder(from_string(prefix), builder);
}

export function append(builder, second) {
  return append_builder(builder, from_string(second));
}

export function prepend_builder(builder, prefix) {
  return do_append(prefix, builder);
}

export function append_builder(builder, suffix) {
  return do_append(builder, suffix);
}

export function from_strings(strings) {
  return do_from_strings(strings);
}

export function concat(builders) {
  return do_concat(builders);
}

export function from_string(string) {
  return do_from_string(string);
}

export function to_string(builder) {
  return do_to_string(builder);
}

export function byte_size(builder) {
  return do_byte_size(builder);
}

export function from_float(f) {
  return do_from_float(f);
}

export function lowercase(builder) {
  return do_lowercase(builder);
}

export function uppercase(builder) {
  return do_uppercase(builder);
}

export function reverse(builder) {
  return do_reverse(builder);
}

export function split(iodata, pattern) {
  return do_split(iodata, pattern);
}

export function replace(builder, pattern, substitute) {
  return do_replace(builder, pattern, substitute);
}

export function is_equal(a, b) {
  return do_is_equal(a, b);
}

export function is_empty(builder) {
  return do_is_empty(builder);
}

function do_is_empty(builder) {
  return isEqual(from_string(""), builder);
}
