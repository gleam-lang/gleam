import { Ok, Error, toList, makeError } from "../gleam.mjs";
import * as $list from "../gleam/list.mjs";

export function is_ok(result) {
  if (!result.isOk()) {
    return false;
  } else if (result.isOk()) {
    return true;
  } else {
    throw makeError(
      "case_no_match",
      "gleam/result",
      21,
      "is_ok",
      "No case clause matched",
      { values: [result] }
    )
  }
}

export function is_error(result) {
  if (result.isOk()) {
    return false;
  } else if (!result.isOk()) {
    return true;
  } else {
    throw makeError(
      "case_no_match",
      "gleam/result",
      42,
      "is_error",
      "No case clause matched",
      { values: [result] }
    )
  }
}

export function map(result, fun) {
  if (result.isOk()) {
    let x = result[0];
    return new Ok(fun(x));
  } else if (!result.isOk()) {
    let e = result[0];
    return new Error(e);
  } else {
    throw makeError(
      "case_no_match",
      "gleam/result",
      67,
      "map",
      "No case clause matched",
      { values: [result] }
    )
  }
}

export function map_error(result, fun) {
  if (result.isOk()) {
    let x = result[0];
    return new Ok(x);
  } else if (!result.isOk()) {
    let error = result[0];
    return new Error(fun(error));
  } else {
    throw makeError(
      "case_no_match",
      "gleam/result",
      95,
      "map_error",
      "No case clause matched",
      { values: [result] }
    )
  }
}

export function flatten(result) {
  if (result.isOk()) {
    let x = result[0];
    return x;
  } else if (!result.isOk()) {
    let error = result[0];
    return new Error(error);
  } else {
    throw makeError(
      "case_no_match",
      "gleam/result",
      121,
      "flatten",
      "No case clause matched",
      { values: [result] }
    )
  }
}

export function try$(result, fun) {
  if (result.isOk()) {
    let x = result[0];
    return fun(x);
  } else if (!result.isOk()) {
    let e = result[0];
    return new Error(e);
  } else {
    throw makeError(
      "case_no_match",
      "gleam/result",
      162,
      "try",
      "No case clause matched",
      { values: [result] }
    )
  }
}

export function then$(result, fun) {
  return try$(result, fun);
}

export function unwrap(result, default$) {
  if (result.isOk()) {
    let v = result[0];
    return v;
  } else if (!result.isOk()) {
    return default$;
  } else {
    throw makeError(
      "case_no_match",
      "gleam/result",
      193,
      "unwrap",
      "No case clause matched",
      { values: [result] }
    )
  }
}

export function lazy_unwrap(result, default$) {
  if (result.isOk()) {
    let v = result[0];
    return v;
  } else if (!result.isOk()) {
    return default$();
  } else {
    throw makeError(
      "case_no_match",
      "gleam/result",
      215,
      "lazy_unwrap",
      "No case clause matched",
      { values: [result] }
    )
  }
}

export function unwrap_error(result, default$) {
  if (result.isOk()) {
    return default$;
  } else if (!result.isOk()) {
    let e = result[0];
    return e;
  } else {
    throw makeError(
      "case_no_match",
      "gleam/result",
      237,
      "unwrap_error",
      "No case clause matched",
      { values: [result] }
    )
  }
}

export function unwrap_both(result) {
  if (result.isOk()) {
    let a = result[0];
    return a;
  } else if (!result.isOk()) {
    let a = result[0];
    return a;
  } else {
    throw makeError(
      "case_no_match",
      "gleam/result",
      259,
      "unwrap_both",
      "No case clause matched",
      { values: [result] }
    )
  }
}

export function nil_error(result) {
  return map_error(result, (_) => { return undefined; });
}

export function or(first, second) {
  if (first.isOk()) {
    return first;
  } else if (!first.isOk()) {
    return second;
  } else {
    throw makeError(
      "case_no_match",
      "gleam/result",
      308,
      "or",
      "No case clause matched",
      { values: [first] }
    )
  }
}

export function lazy_or(first, second) {
  if (first.isOk()) {
    return first;
  } else if (!first.isOk()) {
    return second();
  } else {
    throw makeError(
      "case_no_match",
      "gleam/result",
      342,
      "lazy_or",
      "No case clause matched",
      { values: [first] }
    )
  }
}

export function all(results) {
  return $list.try_map(results, (x) => { return x; });
}

function do_partition(loop$results, loop$oks, loop$errors) {
  while (true) {
    let results = loop$results;
    let oks = loop$oks;
    let errors = loop$errors;
    if (results.hasLength(0)) {
      return [oks, errors];
    } else if (results.atLeastLength(1) && results.head.isOk()) {
      let a = results.head[0];
      let rest = results.tail;
      loop$results = rest;
      loop$oks = toList([a], oks);
      loop$errors = errors;
    } else if (results.atLeastLength(1) && !results.head.isOk()) {
      let e = results.head[0];
      let rest = results.tail;
      loop$results = rest;
      loop$oks = oks;
      loop$errors = toList([e], errors);
    } else {
      throw makeError(
        "case_no_match",
        "gleam/result",
        385,
        "do_partition",
        "No case clause matched",
        { values: [results] }
      )
    }
  }
}

export function partition(results) {
  return do_partition(results, toList([]), toList([]));
}

export function replace(result, value) {
  if (result.isOk()) {
    return new Ok(value);
  } else if (!result.isOk()) {
    let error = result[0];
    return new Error(error);
  } else {
    throw makeError(
      "case_no_match",
      "gleam/result",
      407,
      "replace",
      "No case clause matched",
      { values: [result] }
    )
  }
}

export function replace_error(result, error) {
  if (result.isOk()) {
    let x = result[0];
    return new Ok(x);
  } else if (!result.isOk()) {
    return new Error(error);
  } else {
    throw makeError(
      "case_no_match",
      "gleam/result",
      428,
      "replace_error",
      "No case clause matched",
      { values: [result] }
    )
  }
}

export function values(results) {
  return $list.filter_map(results, (r) => { return r; });
}

export function try_recover(result, fun) {
  if (result.isOk()) {
    let value = result[0];
    return new Ok(value);
  } else if (!result.isOk()) {
    let error = result[0];
    return fun(error);
  } else {
    throw makeError(
      "case_no_match",
      "gleam/result",
      478,
      "try_recover",
      "No case clause matched",
      { values: [result] }
    )
  }
}
