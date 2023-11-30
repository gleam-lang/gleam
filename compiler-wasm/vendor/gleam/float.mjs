import { Ok, Error, makeError, divideFloat } from "../gleam.mjs";
import * as $order from "../gleam/order.mjs";
import {
  parse_float as do_parse,
  float_to_string as do_to_string,
  ceiling as do_ceiling,
  floor as do_floor,
  round as js_round,
  truncate as do_truncate,
  power as do_power,
  random_uniform as do_random_uniform,
} from "../gleam_stdlib.mjs";

export function parse(string) {
  return do_parse(string);
}

export function to_string(x) {
  return do_to_string(x);
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
        "gleam/float",
        90,
        "compare",
        "No case clause matched",
        { values: [$1] }
      )
    }
  } else {
    throw makeError(
      "case_no_match",
      "gleam/float",
      87,
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
      "gleam/float",
      168,
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
      "gleam/float",
      184,
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

export function ceiling(x) {
  return do_ceiling(x);
}

export function floor(x) {
  return do_floor(x);
}

export function truncate(x) {
  return do_truncate(x);
}

export function absolute_value(x) {
  let $ = x >= 0.0;
  if ($) {
    return x;
  } else {
    return 0.0 - x;
  }
}

export function loosely_compare(a, b, tolerance) {
  let difference = absolute_value(a - b);
  let $ = difference <= tolerance;
  if ($) {
    return new $order.Eq();
  } else if (!$) {
    return compare(a, b);
  } else {
    throw makeError(
      "case_no_match",
      "gleam/float",
      122,
      "loosely_compare",
      "No case clause matched",
      { values: [$] }
    )
  }
}

export function loosely_equals(a, b, tolerance) {
  let difference = absolute_value(a - b);
  return difference <= tolerance;
}

export function power(base, exponent) {
  let fractional = (ceiling(exponent) - exponent) > 0.0;
  let $ = ((base < 0.0) && fractional) || ((base === 0.0) && (exponent < 0.0));
  if ($) {
    return new Error(undefined);
  } else if (!$) {
    return new Ok(do_power(base, exponent));
  } else {
    throw makeError(
      "case_no_match",
      "gleam/float",
      334,
      "power",
      "No case clause matched",
      { values: [$] }
    )
  }
}

export function square_root(x) {
  return power(x, 0.5);
}

export function negate(x) {
  return -1.0 * x;
}

function do_round(x) {
  let $ = x >= 0.0;
  if ($) {
    return js_round(x);
  } else {
    return 0 - js_round(negate(x));
  }
}

export function round(x) {
  return do_round(x);
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
        "gleam/float",
        390,
        "do_sum",
        "No case clause matched",
        { values: [numbers] }
      )
    }
  }
}

export function sum(numbers) {
  let _pipe = numbers;
  return do_sum(_pipe, 0.0);
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
        "gleam/float",
        413,
        "do_product",
        "No case clause matched",
        { values: [numbers] }
      )
    }
  }
}

export function product(numbers) {
  if (numbers.hasLength(0)) {
    return 1.0;
  } else {
    return do_product(numbers, 1.0);
  }
}

export function random(min, max) {
  return (do_random_uniform() * (max - min)) + min;
}

export function divide(a, b) {
  if (b === 0.0) {
    return new Error(undefined);
  } else {
    let b$1 = b;
    return new Ok(divideFloat(a, b$1));
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
