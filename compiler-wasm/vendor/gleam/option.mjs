import { Ok, Error, toList, CustomType as $CustomType, makeError, isEqual } from "../gleam.mjs";

export class Some extends $CustomType {
  constructor(x0) {
    super();
    this[0] = x0;
  }
}

export class None extends $CustomType {}

function do_all(list, acc) {
  if (list.hasLength(0)) {
    return new Some(acc);
  } else if (list.atLeastLength(1)) {
    let x = list.head;
    let rest = list.tail;
    let accumulate = (acc, item) => {
      if (acc instanceof Some && item instanceof Some) {
        let values$1 = acc[0];
        let value = item[0];
        return new Some(toList([value], values$1));
      } else {
        return new None();
      }
    };
    return accumulate(do_all(rest, acc), x);
  } else {
    throw makeError(
      "case_no_match",
      "gleam/option",
      13,
      "do_all",
      "No case clause matched",
      { values: [list] }
    )
  }
}

export function all(list) {
  return do_all(list, toList([]));
}

export function is_some(option) {
  return !isEqual(option, new None());
}

export function is_none(option) {
  return isEqual(option, new None());
}

export function to_result(option, e) {
  if (option instanceof Some) {
    let a = option[0];
    return new Ok(a);
  } else {
    return new Error(e);
  }
}

export function from_result(result) {
  if (result.isOk()) {
    let a = result[0];
    return new Some(a);
  } else {
    return new None();
  }
}

export function unwrap(option, default$) {
  if (option instanceof Some) {
    let x = option[0];
    return x;
  } else if (option instanceof None) {
    return default$;
  } else {
    throw makeError(
      "case_no_match",
      "gleam/option",
      140,
      "unwrap",
      "No case clause matched",
      { values: [option] }
    )
  }
}

export function lazy_unwrap(option, default$) {
  if (option instanceof Some) {
    let x = option[0];
    return x;
  } else if (option instanceof None) {
    return default$();
  } else {
    throw makeError(
      "case_no_match",
      "gleam/option",
      161,
      "lazy_unwrap",
      "No case clause matched",
      { values: [option] }
    )
  }
}

export function map(option, fun) {
  if (option instanceof Some) {
    let x = option[0];
    return new Some(fun(x));
  } else if (option instanceof None) {
    return new None();
  } else {
    throw makeError(
      "case_no_match",
      "gleam/option",
      186,
      "map",
      "No case clause matched",
      { values: [option] }
    )
  }
}

export function flatten(option) {
  if (option instanceof Some) {
    let x = option[0];
    return x;
  } else if (option instanceof None) {
    return new None();
  } else {
    throw makeError(
      "case_no_match",
      "gleam/option",
      212,
      "flatten",
      "No case clause matched",
      { values: [option] }
    )
  }
}

export function then$(option, fun) {
  if (option instanceof Some) {
    let x = option[0];
    return fun(x);
  } else if (option instanceof None) {
    return new None();
  } else {
    throw makeError(
      "case_no_match",
      "gleam/option",
      251,
      "then",
      "No case clause matched",
      { values: [option] }
    )
  }
}

export function or(first, second) {
  if (first instanceof Some) {
    return first;
  } else if (first instanceof None) {
    return second;
  } else {
    throw makeError(
      "case_no_match",
      "gleam/option",
      282,
      "or",
      "No case clause matched",
      { values: [first] }
    )
  }
}

export function lazy_or(first, second) {
  if (first instanceof Some) {
    return first;
  } else if (first instanceof None) {
    return second();
  } else {
    throw makeError(
      "case_no_match",
      "gleam/option",
      313,
      "lazy_or",
      "No case clause matched",
      { values: [first] }
    )
  }
}

function do_values(list, acc) {
  if (list.hasLength(0)) {
    return acc;
  } else if (list.atLeastLength(1)) {
    let x = list.head;
    let xs = list.tail;
    let accumulate = (acc, item) => {
      if (item instanceof Some) {
        let value = item[0];
        return toList([value], acc);
      } else if (item instanceof None) {
        return acc;
      } else {
        throw makeError(
          "case_no_match",
          "gleam/option",
          324,
          "",
          "No case clause matched",
          { values: [item] }
        )
      }
    };
    return accumulate(do_values(xs, acc), x);
  } else {
    throw makeError(
      "case_no_match",
      "gleam/option",
      320,
      "do_values",
      "No case clause matched",
      { values: [list] }
    )
  }
}

export function values(options) {
  return do_values(options, toList([]));
}
