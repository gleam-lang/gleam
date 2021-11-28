import { Ok, Error, CustomType, throwError } from "../gleam.js";
import * as $order from "../gleam/order.js";
import {
  parse_int as do_parse,
  to_string as do_to_string,
  int_to_base_string as do_to_base_string,
  identity as do_to_float,
} from "../gleam_stdlib.js";

export function absolute_value(num) {
  let $ = num >= 0;
  if ($) {
    return num;
  } else if (!$) {
    return Math.imul(num, -1);
  } else {
    throwError(
      "case_no_match",
      "gleam/int",
      14,
      "absolute_value",
      "No case clause matched",
      { values: [$] }
    );
  }
}

export function parse(string) {
  return do_parse(string);
}

export function to_string(int) {
  return do_to_string(int);
}

export class InvalidBase extends CustomType {}

export function to_base_string(int, base) {
  let $ = (base >= 2) && (base <= 36);
  if ($) {
    return new Ok(do_to_base_string(int, base));
  } else if (!$) {
    return new Error(new InvalidBase());
  } else {
    throwError(
      "case_no_match",
      "gleam/int",
      92,
      "to_base_string",
      "No case clause matched",
      { values: [$] }
    );
  }
}

export function to_base2(int) {
  return do_to_base_string(int, 2);
}

export function to_base8(int) {
  return do_to_base_string(int, 8);
}

export function to_base16(int) {
  return do_to_base_string(int, 16);
}

export function to_base36(int) {
  return do_to_base_string(int, 36);
}

export function to_float(int) {
  return do_to_float(int);
}

export function clamp(n, min_bound, max_bound) {
  let _pipe = n;
  let _pipe$1 = min(_pipe, max_bound);
  return max(_pipe$1, min_bound);
}

export function compare(a, b) {
  let $ = a === b;
  if ($) {
    return new $order.Eq();
  } else if (!$) {
    let $1 = a < b;
    if ($1) {
      return new $order.Lt();
    } else if (!$1) {
      return new $order.Gt();
    } else {
      throwError(
        "case_no_match",
        "gleam/int",
        211,
        "compare",
        "No case clause matched",
        { values: [$1] }
      );
    }
  } else {
    throwError(
      "case_no_match",
      "gleam/int",
      208,
      "compare",
      "No case clause matched",
      { values: [$] }
    );
  }
}

export function min(a, b) {
  let $ = a < b;
  if ($) {
    return a;
  } else if (!$) {
    return b;
  } else {
    throwError(
      "case_no_match",
      "gleam/int",
      226,
      "min",
      "No case clause matched",
      { values: [$] }
    );
  }
}

export function max(a, b) {
  let $ = a > b;
  if ($) {
    return a;
  } else if (!$) {
    return b;
  } else {
    throwError(
      "case_no_match",
      "gleam/int",
      240,
      "max",
      "No case clause matched",
      { values: [$] }
    );
  }
}

export function is_even(x) {
  return (x % 2) === 0;
}

export function is_odd(x) {
  return (x % 2) !== 0;
}

export function negate(x) {
  return Math.imul(-1, x);
}

export function sum(numbers) {
  let _pipe = numbers;
  return do_sum(_pipe, 0);
}

function do_sum(loop$numbers, loop$initial) {
  let numbers = loop$numbers;
  let initial = loop$initial;
  while (true) {
    if (numbers.hasLength(0)) {
      return initial;
    } else if (numbers.atLeastLength(1)) {
      let x = numbers.head;
      let rest = numbers.tail;
      numbers = rest;
      initial = x + initial;
    } else {
      throwError(
        "case_no_match",
        "gleam/int",
        298,
        "do_sum",
        "No case clause matched",
        { values: [numbers] }
      );
    }
  }
}

export function product(numbers) {
  if (numbers.hasLength(0)) {
    return 0;
  } else {
    return do_product(numbers, 1);
  }
}

function do_product(loop$numbers, loop$initial) {
  let numbers = loop$numbers;
  let initial = loop$initial;
  while (true) {
    if (numbers.hasLength(0)) {
      return initial;
    } else if (numbers.atLeastLength(1)) {
      let x = numbers.head;
      let rest = numbers.tail;
      numbers = rest;
      initial = Math.imul(x, initial);
    } else {
      throwError(
        "case_no_match",
        "gleam/int",
        319,
        "do_product",
        "No case clause matched",
        { values: [numbers] }
      );
    }
  }
}
