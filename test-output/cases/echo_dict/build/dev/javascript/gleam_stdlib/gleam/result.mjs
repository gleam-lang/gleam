import { Ok, Error, toList, prepend as listPrepend } from "../gleam.mjs";
import * as $list from "../gleam/list.mjs";

export function is_ok(result) {
  if (!result.isOk()) {
    return false;
  } else {
    return true;
  }
}

export function is_error(result) {
  if (result.isOk()) {
    return false;
  } else {
    return true;
  }
}

export function map(result, fun) {
  if (result.isOk()) {
    let x = result[0];
    return new Ok(fun(x));
  } else {
    let e = result[0];
    return new Error(e);
  }
}

export function map_error(result, fun) {
  if (result.isOk()) {
    let x = result[0];
    return new Ok(x);
  } else {
    let error = result[0];
    return new Error(fun(error));
  }
}

export function flatten(result) {
  if (result.isOk()) {
    let x = result[0];
    return x;
  } else {
    let error = result[0];
    return new Error(error);
  }
}

export function try$(result, fun) {
  if (result.isOk()) {
    let x = result[0];
    return fun(x);
  } else {
    let e = result[0];
    return new Error(e);
  }
}

export function then$(result, fun) {
  return try$(result, fun);
}

export function unwrap(result, default$) {
  if (result.isOk()) {
    let v = result[0];
    return v;
  } else {
    return default$;
  }
}

export function lazy_unwrap(result, default$) {
  if (result.isOk()) {
    let v = result[0];
    return v;
  } else {
    return default$();
  }
}

export function unwrap_error(result, default$) {
  if (result.isOk()) {
    return default$;
  } else {
    let e = result[0];
    return e;
  }
}

export function unwrap_both(result) {
  if (result.isOk()) {
    let a = result[0];
    return a;
  } else {
    let a = result[0];
    return a;
  }
}

export function nil_error(result) {
  return map_error(result, (_) => { return undefined; });
}

export function or(first, second) {
  if (first.isOk()) {
    return first;
  } else {
    return second;
  }
}

export function lazy_or(first, second) {
  if (first.isOk()) {
    return first;
  } else {
    return second();
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
      loop$oks = listPrepend(a, oks);
      loop$errors = errors;
    } else {
      let e = results.head[0];
      let rest = results.tail;
      loop$results = rest;
      loop$oks = oks;
      loop$errors = listPrepend(e, errors);
    }
  }
}

export function partition(results) {
  return do_partition(results, toList([]), toList([]));
}

export function replace(result, value) {
  if (result.isOk()) {
    return new Ok(value);
  } else {
    let error = result[0];
    return new Error(error);
  }
}

export function replace_error(result, error) {
  if (result.isOk()) {
    let x = result[0];
    return new Ok(x);
  } else {
    return new Error(error);
  }
}

export function values(results) {
  return $list.filter_map(results, (r) => { return r; });
}

export function try_recover(result, fun) {
  if (result.isOk()) {
    let value = result[0];
    return new Ok(value);
  } else {
    let error = result[0];
    return fun(error);
  }
}
