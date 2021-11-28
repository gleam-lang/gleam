import { Ok, Error, throwError } from "../gleam.js";
import * as $list from "../gleam/list.js";

export function is_ok(result) {
  if (!result.isOk()) {
    return false;
  } else if (result.isOk()) {
    return true;
  } else {
    throwError(
      "case_no_match",
      "gleam/result",
      17,
      "is_ok",
      "No case clause matched",
      { values: [result] }
    );
  }
}

export function is_error(result) {
  if (result.isOk()) {
    return false;
  } else if (!result.isOk()) {
    return true;
  } else {
    throwError(
      "case_no_match",
      "gleam/result",
      34,
      "is_error",
      "No case clause matched",
      { values: [result] }
    );
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
    throwError(
      "case_no_match",
      "gleam/result",
      55,
      "map",
      "No case clause matched",
      { values: [result] }
    );
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
    throwError(
      "case_no_match",
      "gleam/result",
      79,
      "map_error",
      "No case clause matched",
      { values: [result] }
    );
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
    throwError(
      "case_no_match",
      "gleam/result",
      99,
      "flatten",
      "No case clause matched",
      { values: [result] }
    );
  }
}

export function then(result, fun) {
  if (result.isOk()) {
    let x = result[0];
    return fun(x);
  } else if (!result.isOk()) {
    let e = result[0];
    return new Error(e);
  } else {
    throwError(
      "case_no_match",
      "gleam/result",
      133,
      "then",
      "No case clause matched",
      { values: [result] }
    );
  }
}

export function unwrap(result, default$) {
  if (result.isOk()) {
    let v = result[0];
    return v;
  } else if (!result.isOk()) {
    return default$;
  } else {
    throwError(
      "case_no_match",
      "gleam/result",
      151,
      "unwrap",
      "No case clause matched",
      { values: [result] }
    );
  }
}

export function lazy_unwrap(result, default$) {
  if (result.isOk()) {
    let v = result[0];
    return v;
  } else if (!result.isOk()) {
    return default$();
  } else {
    throwError(
      "case_no_match",
      "gleam/result",
      169,
      "lazy_unwrap",
      "No case clause matched",
      { values: [result] }
    );
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
    throwError(
      "case_no_match",
      "gleam/result",
      206,
      "or",
      "No case clause matched",
      { values: [first] }
    );
  }
}

export function lazy_or(first, second) {
  if (first.isOk()) {
    return first;
  } else if (!first.isOk()) {
    return second();
  } else {
    throwError(
      "case_no_match",
      "gleam/result",
      232,
      "lazy_or",
      "No case clause matched",
      { values: [first] }
    );
  }
}

export function all(results) {
  return $list.try_map(results, (x) => { return x; });
}

export function replace_error(result, error) {
  let _pipe = result;
  return map_error(_pipe, (_) => { return error; });
}

export function values(results) {
  return $list.filter_map(results, (r) => { return r; });
}
