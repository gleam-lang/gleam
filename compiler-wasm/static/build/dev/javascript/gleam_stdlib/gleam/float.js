import { Ok, Error, throwError } from "../gleam.js";
import * as $order from "../gleam/order.js";
import * as $string_builder from "../gleam/string_builder.js";
import {
  parse_float as do_parse,
  ceiling as do_ceiling,
  floor as do_floor,
  round as js_round,
  truncate as do_truncate,
  power as do_power,
} from "../gleam_stdlib.js";

export function parse(string) {
  return do_parse(string);
}

export function to_string(f) {
  let _pipe = f;
  let _pipe$1 = $string_builder.from_float(_pipe);
  return $string_builder.to_string(_pipe$1);
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
        "gleam/float",
        65,
        "compare",
        "No case clause matched",
        { values: [$1] }
      );
    }
  } else {
    throwError(
      "case_no_match",
      "gleam/float",
      62,
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
      "gleam/float",
      80,
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
      "gleam/float",
      94,
      "max",
      "No case clause matched",
      { values: [$] }
    );
  }
}

export function ceiling(float) {
  return do_ceiling(float);
}

export function floor(float) {
  return do_floor(float);
}

export function round(float) {
  return do_round(float);
}

function do_round(float) {
  let $ = float >= 0.0;
  if ($) {
    return js_round(float);
  } else {
    return 0 - js_round(negate(float));
  }
}

export function truncate(float) {
  return do_truncate(float);
}

export function absolute_value(float) {
  let $ = float >= 0.;
  if ($) {
    return float;
  } else {
    return 0. - float;
  }
}

export function power(base, exponent) {
  return do_power(base, exponent);
}

export function square_root(number) {
  let $ = number < 0.0;
  if ($) {
    return new Error(undefined);
  } else if (!$) {
    return new Ok(power(number, 0.5));
  } else {
    throwError(
      "case_no_match",
      "gleam/float",
      247,
      "square_root",
      "No case clause matched",
      { values: [$] }
    );
  }
}

export function negate(x) {
  return -1. * x;
}

export function sum(numbers) {
  let _pipe = numbers;
  return do_sum(_pipe, 0.0);
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
        "gleam/float",
        277,
        "do_sum",
        "No case clause matched",
        { values: [numbers] }
      );
    }
  }
}

export function product(numbers) {
  if (numbers.hasLength(0)) {
    return 0.;
  } else {
    return do_product(numbers, 1.);
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
      initial = x * initial;
    } else {
      throwError(
        "case_no_match",
        "gleam/float",
        298,
        "do_product",
        "No case clause matched",
        { values: [numbers] }
      );
    }
  }
}
