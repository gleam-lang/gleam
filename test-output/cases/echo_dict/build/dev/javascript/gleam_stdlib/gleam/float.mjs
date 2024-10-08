import { Ok, Error, divideFloat } from "../gleam.mjs";
import * as $order from "../gleam/order.mjs";
import {
  parse_float as do_parse,
  float_to_string as do_to_string,
  ceiling as do_ceiling,
  floor as do_floor,
  round as js_round,
  truncate as do_truncate,
  identity as do_to_float,
  power as do_power,
  random_uniform as random,
} from "../gleam_stdlib.mjs";

export { random };

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
  } else {
    let $1 = a < b;
    if ($1) {
      return new $order.Lt();
    } else {
      return new $order.Gt();
    }
  }
}

export function min(a, b) {
  let $ = a < b;
  if ($) {
    return a;
  } else {
    return b;
  }
}

export function max(a, b) {
  let $ = a > b;
  if ($) {
    return a;
  } else {
    return b;
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
  } else {
    return compare(a, b);
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
  } else {
    return new Ok(do_power(base, exponent));
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

export function to_precision(x, precision) {
  let factor = do_power(10.0, do_to_float(- precision));
  return do_to_float(round(divideFloat(x, factor))) * factor;
}

function do_sum(loop$numbers, loop$initial) {
  while (true) {
    let numbers = loop$numbers;
    let initial = loop$initial;
    if (numbers.hasLength(0)) {
      return initial;
    } else {
      let x = numbers.head;
      let rest = numbers.tail;
      loop$numbers = rest;
      loop$initial = x + initial;
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
    } else {
      let x = numbers.head;
      let rest = numbers.tail;
      loop$numbers = rest;
      loop$initial = x * initial;
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

export function modulo(dividend, divisor) {
  if (divisor === 0.0) {
    return new Error(undefined);
  } else {
    return new Ok(dividend - (floor(divideFloat(dividend, divisor)) * divisor));
  }
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
