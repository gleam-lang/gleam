import {
  Ok,
  Error,
  toList,
  CustomType as $CustomType,
  makeError,
  remainderInt,
  divideInt,
} from "../gleam.mjs";
import * as $float from "../gleam/float.mjs";
import * as $order from "../gleam/order.mjs";
import {
  parse_int as do_parse,
  int_from_base_string as do_base_parse,
  to_string as do_to_string,
  int_to_base_string as do_to_base_string,
  identity as do_to_float,
  bitwise_and,
  bitwise_not,
  bitwise_or,
  bitwise_exclusive_or,
  bitwise_shift_left,
  bitwise_shift_right,
} from "../gleam_stdlib.mjs";

export {
  bitwise_and,
  bitwise_exclusive_or,
  bitwise_not,
  bitwise_or,
  bitwise_shift_left,
  bitwise_shift_right,
};

export class InvalidBase extends $CustomType {}

export function absolute_value(x) {
  let $ = x >= 0;
  if ($) {
    return x;
  } else if (!$) {
    return x * -1;
  } else {
    throw makeError(
      "case_no_match",
      "gleam/int",
      31,
      "absolute_value",
      "No case clause matched",
      { values: [$] }
    )
  }
}

export function parse(string) {
  return do_parse(string);
}

export function base_parse(string, base) {
  let $ = (base >= 2) && (base <= 36);
  if ($) {
    return do_base_parse(string, base);
  } else if (!$) {
    return new Error(undefined);
  } else {
    throw makeError(
      "case_no_match",
      "gleam/int",
      138,
      "base_parse",
      "No case clause matched",
      { values: [$] }
    )
  }
}

export function to_string(x) {
  return do_to_string(x);
}

export function to_base_string(x, base) {
  let $ = (base >= 2) && (base <= 36);
  if ($) {
    return new Ok(do_to_base_string(x, base));
  } else if (!$) {
    return new Error(new InvalidBase());
  } else {
    throw makeError(
      "case_no_match",
      "gleam/int",
      203,
      "to_base_string",
      "No case clause matched",
      { values: [$] }
    )
  }
}

export function to_base2(x) {
  return do_to_base_string(x, 2);
}

export function to_base8(x) {
  return do_to_base_string(x, 8);
}

export function to_base16(x) {
  return do_to_base_string(x, 16);
}

export function to_base36(x) {
  return do_to_base_string(x, 36);
}

export function to_float(x) {
  return do_to_float(x);
}

export function power(base, exponent) {
  let _pipe = base;
  let _pipe$1 = to_float(_pipe);
  return $float.power(_pipe$1, exponent);
}

export function square_root(x) {
  let _pipe = x;
  let _pipe$1 = to_float(_pipe);
  return $float.square_root(_pipe$1);
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
      throw makeError(
        "case_no_match",
        "gleam/int",
        330,
        "compare",
        "No case clause matched",
        { values: [$1] }
      )
    }
  } else {
    throw makeError(
      "case_no_match",
      "gleam/int",
      327,
      "compare",
      "No case clause matched",
      { values: [$] }
    )
  }
}

export function min(a, b) {
  let $ = a < b;
  if ($) {
    return a;
  } else if (!$) {
    return b;
  } else {
    throw makeError(
      "case_no_match",
      "gleam/int",
      347,
      "min",
      "No case clause matched",
      { values: [$] }
    )
  }
}

export function max(a, b) {
  let $ = a > b;
  if ($) {
    return a;
  } else if (!$) {
    return b;
  } else {
    throw makeError(
      "case_no_match",
      "gleam/int",
      363,
      "max",
      "No case clause matched",
      { values: [$] }
    )
  }
}

export function clamp(x, min_bound, max_bound) {
  let _pipe = x;
  let _pipe$1 = min(_pipe, max_bound);
  return max(_pipe$1, min_bound);
}

export function is_even(x) {
  return (remainderInt(x, 2)) === 0;
}

export function is_odd(x) {
  return (remainderInt(x, 2)) !== 0;
}

export function negate(x) {
  return -1 * x;
}

function do_sum(loop$numbers, loop$initial) {
  while (true) {
    let numbers = loop$numbers;
    let initial = loop$initial;
    if (numbers.hasLength(0)) {
      return initial;
    } else if (numbers.atLeastLength(1)) {
      let x = numbers.head;
      let rest = numbers.tail;
      loop$numbers = rest;
      loop$initial = x + initial;
    } else {
      throw makeError(
        "case_no_match",
        "gleam/int",
        433,
        "do_sum",
        "No case clause matched",
        { values: [numbers] }
      )
    }
  }
}

export function sum(numbers) {
  let _pipe = numbers;
  return do_sum(_pipe, 0);
}

function do_product(loop$numbers, loop$initial) {
  while (true) {
    let numbers = loop$numbers;
    let initial = loop$initial;
    if (numbers.hasLength(0)) {
      return initial;
    } else if (numbers.atLeastLength(1)) {
      let x = numbers.head;
      let rest = numbers.tail;
      loop$numbers = rest;
      loop$initial = x * initial;
    } else {
      throw makeError(
        "case_no_match",
        "gleam/int",
        456,
        "do_product",
        "No case clause matched",
        { values: [numbers] }
      )
    }
  }
}

export function product(numbers) {
  if (numbers.hasLength(0)) {
    return 1;
  } else {
    return do_product(numbers, 1);
  }
}

function do_digits(loop$x, loop$base, loop$acc) {
  while (true) {
    let x = loop$x;
    let base = loop$base;
    let acc = loop$acc;
    let $ = absolute_value(x) < base;
    if ($) {
      return toList([x], acc);
    } else if (!$) {
      loop$x = divideInt(x, base);
      loop$base = base;
      loop$acc = toList([remainderInt(x, base)], acc);
    } else {
      throw makeError(
        "case_no_match",
        "gleam/int",
        484,
        "do_digits",
        "No case clause matched",
        { values: [$] }
      )
    }
  }
}

export function digits(x, base) {
  let $ = base < 2;
  if ($) {
    return new Error(new InvalidBase());
  } else if (!$) {
    return new Ok(do_digits(x, base, toList([])));
  } else {
    throw makeError(
      "case_no_match",
      "gleam/int",
      477,
      "digits",
      "No case clause matched",
      { values: [$] }
    )
  }
}

function do_undigits(loop$numbers, loop$base, loop$acc) {
  while (true) {
    let numbers = loop$numbers;
    let base = loop$base;
    let acc = loop$acc;
    if (numbers.hasLength(0)) {
      return new Ok(acc);
    } else if (numbers.atLeastLength(1) && numbers.head >= base) {
      let digit = numbers.head;
      return new Error(new InvalidBase());
    } else if (numbers.atLeastLength(1)) {
      let digit = numbers.head;
      let rest = numbers.tail;
      loop$numbers = rest;
      loop$base = base;
      loop$acc = acc * base + digit;
    } else {
      throw makeError(
        "case_no_match",
        "gleam/int",
        522,
        "do_undigits",
        "No case clause matched",
        { values: [numbers] }
      )
    }
  }
}

export function undigits(numbers, base) {
  let $ = base < 2;
  if ($) {
    return new Error(new InvalidBase());
  } else if (!$) {
    return do_undigits(numbers, base, 0);
  } else {
    throw makeError(
      "case_no_match",
      "gleam/int",
      511,
      "undigits",
      "No case clause matched",
      { values: [$] }
    )
  }
}

export function random(min, max) {
  let _pipe = $float.random(to_float(min), to_float(max));
  let _pipe$1 = $float.floor(_pipe);
  return $float.round(_pipe$1);
}

export function divide(dividend, divisor) {
  if (divisor === 0) {
    return new Error(undefined);
  } else {
    let divisor$1 = divisor;
    return new Ok(divideInt(dividend, divisor$1));
  }
}

export function remainder(dividend, divisor) {
  if (divisor === 0) {
    return new Error(undefined);
  } else {
    let divisor$1 = divisor;
    return new Ok(remainderInt(dividend, divisor$1));
  }
}

export function modulo(dividend, divisor) {
  if (divisor === 0) {
    return new Error(undefined);
  } else {
    let remainder$1 = remainderInt(dividend, divisor);
    let $ = remainder$1 * divisor < 0;
    if ($) {
      return new Ok(remainder$1 + divisor);
    } else if (!$) {
      return new Ok(remainder$1);
    } else {
      throw makeError(
        "case_no_match",
        "gleam/int",
        680,
        "modulo",
        "No case clause matched",
        { values: [$] }
      )
    }
  }
}

export function floor_divide(dividend, divisor) {
  if (divisor === 0) {
    return new Error(undefined);
  } else {
    let divisor$1 = divisor;
    let $ = (dividend * divisor$1 < 0) && ((remainderInt(dividend, divisor$1)) !== 0);
    if ($) {
      return new Ok((divideInt(dividend, divisor$1)) - 1);
    } else if (!$) {
      return new Ok(divideInt(dividend, divisor$1));
    } else {
      throw makeError(
        "case_no_match",
        "gleam/int",
        723,
        "floor_divide",
        "No case clause matched",
        { values: [$] }
      )
    }
  }
}

export function add(a, b) {
  return a + b;
}

export function multiply(a, b) {
  return a * b;
}

export function subtract(a, b) {
  return a - b;
}
