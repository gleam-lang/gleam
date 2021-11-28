import { toList, CustomType, throwError } from "../gleam.js";
import * as $bit_string from "../gleam/bit_string.js";
import * as $list from "../gleam/list.js";
import * as $string_builder from "../gleam/string_builder.js";

class Bits extends CustomType {
  constructor(x0) {
    super();
    this[0] = x0;
  }
}

class Text extends CustomType {
  constructor(x0) {
    super();
    this[0] = x0;
  }
}

class Many extends CustomType {
  constructor(x0) {
    super();
    this[0] = x0;
  }
}

export function prepend(to, prefix) {
  return append_builder(from_bit_string(prefix), to);
}

export function append(to, suffix) {
  return append_builder(to, from_bit_string(suffix));
}

export function prepend_builder(to, prefix) {
  return append_builder(prefix, to);
}

export function append_builder(first, second) {
  return do_append_builder(first, second);
}

function do_append_builder(first, second) {
  if (second instanceof Many) {
    let builders = second[0];
    return new Many(toList([first], builders));
  } else {
    return new Many(toList([first, second]));
  }
}

export function prepend_string(to, prefix) {
  return append_builder(from_string(prefix), to);
}

export function append_string(to, suffix) {
  return append_builder(to, from_string(suffix));
}

export function concat(builders) {
  return do_concat(builders);
}

function do_concat(builders) {
  return new Many(builders);
}

export function from_string(string) {
  return do_from_string(string);
}

function do_from_string(string) {
  return new Text($string_builder.from_string(string));
}

export function from_string_builder(builder) {
  return do_from_string_builder(builder);
}

function do_from_string_builder(builder) {
  return new Text(builder);
}

export function from_bit_string(bits) {
  return do_from_bit_string(bits);
}

function do_from_bit_string(bits) {
  return new Bits(bits);
}

export function to_bit_string(builder) {
  return do_to_bit_string(builder);
}

function do_to_bit_string(builder) {
  let _pipe = toList([toList([builder])]);
  let _pipe$1 = to_list(_pipe, toList([]));
  let _pipe$2 = $list.reverse(_pipe$1);
  return $bit_string.concat(_pipe$2);
}

function to_list(loop$stack, loop$acc) {
  let stack = loop$stack;
  let acc = loop$acc;
  while (true) {
    if (stack.hasLength(0)) {
      return acc;
    } else if (stack.atLeastLength(1) && stack.head.hasLength(0)) {
      let remaining_stack = stack.tail;
      stack = remaining_stack;
      acc = acc;
    } else if (stack.atLeastLength(1) && stack.head.atLeastLength(1) && stack.head.head instanceof Bits) {
      let bits = stack.head.head[0];
      let rest = stack.head.tail;
      let remaining_stack = stack.tail;
      stack = toList([rest], remaining_stack);
      acc = toList([bits], acc);
    } else if (stack.atLeastLength(1) && stack.head.atLeastLength(1) && stack.head.head instanceof Text) {
      let builder = stack.head.head[0];
      let rest = stack.head.tail;
      let remaining_stack = stack.tail;
      let bits = $bit_string.from_string($string_builder.to_string(builder));
      stack = toList([rest], remaining_stack);
      acc = toList([bits], acc);
    } else if (stack.atLeastLength(1) && stack.head.atLeastLength(1) && stack.head.head instanceof Many) {
      let builders = stack.head.head[0];
      let rest = stack.head.tail;
      let remaining_stack = stack.tail;
      stack = toList([builders, rest], remaining_stack);
      acc = acc;
    } else {
      throwError(
        "case_no_match",
        "gleam/bit_builder",
        221,
        "to_list",
        "No case clause matched",
        { values: [stack] }
      );
    }
  }
}

export function byte_size(builder) {
  return do_byte_size(builder);
}

function do_byte_size(builder) {
  let _pipe = toList([toList([builder])]);
  let _pipe$1 = to_list(_pipe, toList([]));
  return $list.fold(
    _pipe$1,
    0,
    (acc, builder) => { return $bit_string.byte_size(builder) + acc; },
  );
}
