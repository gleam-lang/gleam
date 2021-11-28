import { Ok, Error, toList, CustomType, throwError, isEqual } from "../gleam.js";
import * as $list from "../gleam/list.js";

export class Some extends CustomType {
  constructor(x0) {
    super();
    this[0] = x0;
  }
}

export class None extends CustomType {}

export function all(list) {
  return $list.fold_right(
    list,
    new Some(toList([])),
    (acc, item) => {
      if (acc instanceof Some &&
      item instanceof Some) {
        let values = acc[0];
        let value = item[0];
        return new Some(toList([value], values));
      } else {
        return new None();
      }
    },
  );
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
    throwError(
      "case_no_match",
      "gleam/option",
      112,
      "unwrap",
      "No case clause matched",
      { values: [option] }
    );
  }
}

export function map(option, fun) {
  if (option instanceof Some) {
    let x = option[0];
    return new Some(fun(x));
  } else if (option instanceof None) {
    return new None();
  } else {
    throwError(
      "case_no_match",
      "gleam/option",
      133,
      "map",
      "No case clause matched",
      { values: [option] }
    );
  }
}

export function flatten(option) {
  if (option instanceof Some) {
    let x = option[0];
    return x;
  } else if (option instanceof None) {
    return new None();
  } else {
    throwError(
      "case_no_match",
      "gleam/option",
      153,
      "flatten",
      "No case clause matched",
      { values: [option] }
    );
  }
}

export function then(option, fun) {
  if (option instanceof Some) {
    let x = option[0];
    return fun(x);
  } else if (option instanceof None) {
    return new None();
  } else {
    throwError(
      "case_no_match",
      "gleam/option",
      184,
      "then",
      "No case clause matched",
      { values: [option] }
    );
  }
}

export function or(first, second) {
  if (first instanceof Some) {
    return first;
  } else if (first instanceof None) {
    return second;
  } else {
    throwError(
      "case_no_match",
      "gleam/option",
      207,
      "or",
      "No case clause matched",
      { values: [first] }
    );
  }
}

export function values(options) {
  return $list.filter_map(options, (op) => { return to_result(op, ""); });
}
