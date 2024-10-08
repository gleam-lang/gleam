import {
  Ok,
  Error,
  toList,
  prepend as listPrepend,
  CustomType as $CustomType,
  isEqual,
} from "../gleam.mjs";

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
  } else {
    let x = list.head;
    let rest = list.tail;
    let accumulate = (acc, item) => {
      if (acc instanceof Some && item instanceof Some) {
        let values$1 = acc[0];
        let value = item[0];
        return new Some(listPrepend(value, values$1));
      } else {
        return new None();
      }
    };
    return accumulate(do_all(rest, acc), x);
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
  } else {
    return default$;
  }
}

export function lazy_unwrap(option, default$) {
  if (option instanceof Some) {
    let x = option[0];
    return x;
  } else {
    return default$();
  }
}

export function map(option, fun) {
  if (option instanceof Some) {
    let x = option[0];
    return new Some(fun(x));
  } else {
    return new None();
  }
}

export function flatten(option) {
  if (option instanceof Some) {
    let x = option[0];
    return x;
  } else {
    return new None();
  }
}

export function then$(option, fun) {
  if (option instanceof Some) {
    let x = option[0];
    return fun(x);
  } else {
    return new None();
  }
}

export function or(first, second) {
  if (first instanceof Some) {
    return first;
  } else {
    return second;
  }
}

export function lazy_or(first, second) {
  if (first instanceof Some) {
    return first;
  } else {
    return second();
  }
}

function do_values(list, acc) {
  if (list.hasLength(0)) {
    return acc;
  } else {
    let first = list.head;
    let rest = list.tail;
    let accumulate = (acc, item) => {
      if (item instanceof Some) {
        let value = item[0];
        return listPrepend(value, acc);
      } else {
        return acc;
      }
    };
    return accumulate(do_values(rest, acc), first);
  }
}

export function values(options) {
  return do_values(options, toList([]));
}
