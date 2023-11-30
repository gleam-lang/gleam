import { toList, isEqual } from "../gleam.mjs";
import * as $list from "../gleam/list.mjs";
import {
  add as do_append,
  concat as do_from_strings,
  concat as do_concat,
  identity as do_from_string,
  identity as do_to_string,
  length as do_byte_size,
  lowercase as do_lowercase,
  uppercase as do_uppercase,
  graphemes as do_to_graphemes,
  split as do_split,
  string_replace as do_replace,
  equal as do_is_equal,
} from "../gleam_stdlib.mjs";

export function prepend_builder(builder, prefix) {
  return do_append(prefix, builder);
}

export function append_builder(builder, suffix) {
  return do_append(builder, suffix);
}

export function new$() {
  return do_from_strings(toList([]));
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

export function prepend(builder, prefix) {
  return append_builder(from_string(prefix), builder);
}

export function append(builder, second) {
  return append_builder(builder, from_string(second));
}

export function to_string(builder) {
  return do_to_string(builder);
}

export function byte_size(builder) {
  return do_byte_size(builder);
}

export function join(builders, sep) {
  let _pipe = builders;
  let _pipe$1 = $list.intersperse(_pipe, from_string(sep));
  return concat(_pipe$1);
}

export function lowercase(builder) {
  return do_lowercase(builder);
}

export function uppercase(builder) {
  return do_uppercase(builder);
}

function do_reverse(builder) {
  let _pipe = builder;
  let _pipe$1 = to_string(_pipe);
  let _pipe$2 = do_to_graphemes(_pipe$1);
  let _pipe$3 = $list.reverse(_pipe$2);
  return from_strings(_pipe$3);
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

function do_is_empty(builder) {
  return isEqual(from_string(""), builder);
}

export function is_empty(builder) {
  return do_is_empty(builder);
}
