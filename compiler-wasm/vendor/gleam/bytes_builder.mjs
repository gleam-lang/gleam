import { toList, CustomType as $CustomType, makeError } from "../gleam.mjs";
import * as $bit_array from "../gleam/bit_array.mjs";
import * as $list from "../gleam/list.mjs";
import * as $string_builder from "../gleam/string_builder.mjs";

class Bytes extends $CustomType {
  constructor(x0) {
    super();
    this[0] = x0;
  }
}

class Text extends $CustomType {
  constructor(x0) {
    super();
    this[0] = x0;
  }
}

class Many extends $CustomType {
  constructor(x0) {
    super();
    this[0] = x0;
  }
}

export function append_builder(first, second) {
  if (second instanceof Many) {
    let builders = second[0];
    return new Many(toList([first], builders));
  } else {
    return new Many(toList([first, second]));
  }
}

export function prepend_builder(second, first) {
  return append_builder(first, second);
}

export function concat(builders) {
  return new Many(builders);
}

export function new$() {
  return concat(toList([]));
}

export function from_string(string) {
  return new Text($string_builder.from_string(string));
}

export function prepend_string(second, first) {
  return append_builder(from_string(first), second);
}

export function append_string(first, second) {
  return append_builder(first, from_string(second));
}

export function from_string_builder(builder) {
  return new Text(builder);
}

export function from_bit_array(bits) {
  return new Bytes(bits);
}

export function prepend(second, first) {
  return append_builder(from_bit_array(first), second);
}

export function append(first, second) {
  return append_builder(first, from_bit_array(second));
}

export function concat_bit_arrays(bits) {
  let _pipe = bits;
  let _pipe$1 = $list.map(_pipe, (b) => { return from_bit_array(b); });
  return concat(_pipe$1);
}

function to_list(loop$stack, loop$acc) {
  while (true) {
    let stack = loop$stack;
    let acc = loop$acc;
    if (stack.hasLength(0)) {
      return acc;
    } else if (stack.atLeastLength(1) && stack.head.hasLength(0)) {
      let remaining_stack = stack.tail;
      loop$stack = remaining_stack;
      loop$acc = acc;
    } else if (stack.atLeastLength(1) &&
    stack.head.atLeastLength(1) &&
    stack.head.head instanceof Bytes) {
      let bits = stack.head.head[0];
      let rest = stack.head.tail;
      let remaining_stack = stack.tail;
      loop$stack = toList([rest], remaining_stack);
      loop$acc = toList([bits], acc);
    } else if (stack.atLeastLength(1) &&
    stack.head.atLeastLength(1) &&
    stack.head.head instanceof Text) {
      let builder = stack.head.head[0];
      let rest = stack.head.tail;
      let remaining_stack = stack.tail;
      let bits = $bit_array.from_string($string_builder.to_string(builder));
      loop$stack = toList([rest], remaining_stack);
      loop$acc = toList([bits], acc);
    } else if (stack.atLeastLength(1) &&
    stack.head.atLeastLength(1) &&
    stack.head.head instanceof Many) {
      let builders = stack.head.head[0];
      let rest = stack.head.tail;
      let remaining_stack = stack.tail;
      loop$stack = toList([builders, rest], remaining_stack);
      loop$acc = acc;
    } else {
      throw makeError(
        "case_no_match",
        "gleam/bytes_builder",
        170,
        "to_list",
        "No case clause matched",
        { values: [stack] }
      )
    }
  }
}

export function to_bit_array(builder) {
  let _pipe = toList([toList([builder])]);
  let _pipe$1 = to_list(_pipe, toList([]));
  let _pipe$2 = $list.reverse(_pipe$1);
  return $bit_array.concat(_pipe$2);
}

export function byte_size(builder) {
  let _pipe = toList([toList([builder])]);
  let _pipe$1 = to_list(_pipe, toList([]));
  return $list.fold(
    _pipe$1,
    0,
    (acc, builder) => { return $bit_array.byte_size(builder) + acc; },
  );
}
