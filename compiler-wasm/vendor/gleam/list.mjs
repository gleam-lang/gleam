import {
  Ok,
  Error,
  toList,
  CustomType as $CustomType,
  makeError,
  divideInt,
  isEqual,
} from "../gleam.mjs";
import * as $dict from "../gleam/dict.mjs";
import * as $float from "../gleam/float.mjs";
import * as $int from "../gleam/int.mjs";
import * as $order from "../gleam/order.mjs";
import * as $pair from "../gleam/pair.mjs";

export class LengthMismatch extends $CustomType {}

export class Continue extends $CustomType {
  constructor(x0) {
    super();
    this[0] = x0;
  }
}

export class Stop extends $CustomType {
  constructor(x0) {
    super();
    this[0] = x0;
  }
}

function do_length_acc(loop$list, loop$count) {
  while (true) {
    let list = loop$list;
    let count = loop$count;
    if (list.atLeastLength(1)) {
      let list$1 = list.tail;
      loop$list = list$1;
      loop$count = count + 1;
    } else {
      return count;
    }
  }
}

function do_length(list) {
  return do_length_acc(list, 0);
}

export function length(list) {
  return do_length(list);
}

function do_reverse_acc(loop$remaining, loop$accumulator) {
  while (true) {
    let remaining = loop$remaining;
    let accumulator = loop$accumulator;
    if (remaining.hasLength(0)) {
      return accumulator;
    } else if (remaining.atLeastLength(1)) {
      let item = remaining.head;
      let rest$1 = remaining.tail;
      loop$remaining = rest$1;
      loop$accumulator = toList([item], accumulator);
    } else {
      throw makeError(
        "case_no_match",
        "gleam/list",
        124,
        "do_reverse_acc",
        "No case clause matched",
        { values: [remaining] }
      )
    }
  }
}

function do_reverse(list) {
  return do_reverse_acc(list, toList([]));
}

export function reverse(xs) {
  return do_reverse(xs);
}

export function is_empty(list) {
  return isEqual(list, toList([]));
}

export function contains(loop$list, loop$elem) {
  while (true) {
    let list = loop$list;
    let elem = loop$elem;
    if (list.hasLength(0)) {
      return false;
    } else if (list.atLeastLength(1) && isEqual(list.head, elem)) {
      let first$1 = list.head;
      return true;
    } else if (list.atLeastLength(1)) {
      let rest$1 = list.tail;
      loop$list = rest$1;
      loop$elem = elem;
    } else {
      throw makeError(
        "case_no_match",
        "gleam/list",
        188,
        "contains",
        "No case clause matched",
        { values: [list] }
      )
    }
  }
}

export function first(list) {
  if (list.hasLength(0)) {
    return new Error(undefined);
  } else if (list.atLeastLength(1)) {
    let x = list.head;
    return new Ok(x);
  } else {
    throw makeError(
      "case_no_match",
      "gleam/list",
      215,
      "first",
      "No case clause matched",
      { values: [list] }
    )
  }
}

export function rest(list) {
  if (list.hasLength(0)) {
    return new Error(undefined);
  } else if (list.atLeastLength(1)) {
    let xs = list.tail;
    return new Ok(xs);
  } else {
    throw makeError(
      "case_no_match",
      "gleam/list",
      244,
      "rest",
      "No case clause matched",
      { values: [list] }
    )
  }
}

function update_group(f) {
  return (groups, elem) => {
    let $ = $dict.get(groups, f(elem));
    if ($.isOk()) {
      let existing = $[0];
      return $dict.insert(groups, f(elem), toList([elem], existing));
    } else if (!$.isOk()) {
      return $dict.insert(groups, f(elem), toList([elem]));
    } else {
      throw makeError(
        "case_no_match",
        "gleam/list",
        254,
        "",
        "No case clause matched",
        { values: [$] }
      )
    }
  };
}

function do_filter(loop$list, loop$fun, loop$acc) {
  while (true) {
    let list = loop$list;
    let fun = loop$fun;
    let acc = loop$acc;
    if (list.hasLength(0)) {
      return reverse(acc);
    } else if (list.atLeastLength(1)) {
      let x = list.head;
      let xs = list.tail;
      let new_acc = (() => {
        let $ = fun(x);
        if ($) {
          return toList([x], acc);
        } else if (!$) {
          return acc;
        } else {
          throw makeError(
            "case_no_match",
            "gleam/list",
            296,
            "do_filter",
            "No case clause matched",
            { values: [$] }
          )
        }
      })();
      loop$list = xs;
      loop$fun = fun;
      loop$acc = new_acc;
    } else {
      throw makeError(
        "case_no_match",
        "gleam/list",
        293,
        "do_filter",
        "No case clause matched",
        { values: [list] }
      )
    }
  }
}

export function filter(list, predicate) {
  return do_filter(list, predicate, toList([]));
}

function do_filter_map(loop$list, loop$fun, loop$acc) {
  while (true) {
    let list = loop$list;
    let fun = loop$fun;
    let acc = loop$acc;
    if (list.hasLength(0)) {
      return reverse(acc);
    } else if (list.atLeastLength(1)) {
      let x = list.head;
      let xs = list.tail;
      let new_acc = (() => {
        let $ = fun(x);
        if ($.isOk()) {
          let x$1 = $[0];
          return toList([x$1], acc);
        } else if (!$.isOk()) {
          return acc;
        } else {
          throw makeError(
            "case_no_match",
            "gleam/list",
            332,
            "do_filter_map",
            "No case clause matched",
            { values: [$] }
          )
        }
      })();
      loop$list = xs;
      loop$fun = fun;
      loop$acc = new_acc;
    } else {
      throw makeError(
        "case_no_match",
        "gleam/list",
        329,
        "do_filter_map",
        "No case clause matched",
        { values: [list] }
      )
    }
  }
}

export function filter_map(list, fun) {
  return do_filter_map(list, fun, toList([]));
}

function do_map(loop$list, loop$fun, loop$acc) {
  while (true) {
    let list = loop$list;
    let fun = loop$fun;
    let acc = loop$acc;
    if (list.hasLength(0)) {
      return reverse(acc);
    } else if (list.atLeastLength(1)) {
      let x = list.head;
      let xs = list.tail;
      loop$list = xs;
      loop$fun = fun;
      loop$acc = toList([fun(x)], acc);
    } else {
      throw makeError(
        "case_no_match",
        "gleam/list",
        361,
        "do_map",
        "No case clause matched",
        { values: [list] }
      )
    }
  }
}

export function map(list, fun) {
  return do_map(list, fun, toList([]));
}

function do_map2(loop$list1, loop$list2, loop$fun, loop$acc) {
  while (true) {
    let list1 = loop$list1;
    let list2 = loop$list2;
    let fun = loop$fun;
    let acc = loop$acc;
    if (list1.hasLength(0)) {
      return reverse(acc);
    } else if (list2.hasLength(0)) {
      return reverse(acc);
    } else if (list1.atLeastLength(1) && list2.atLeastLength(1)) {
      let a = list1.head;
      let as_ = list1.tail;
      let b = list2.head;
      let bs = list2.tail;
      loop$list1 = as_;
      loop$list2 = bs;
      loop$fun = fun;
      loop$acc = toList([fun(a, b)], acc);
    } else {
      throw makeError(
        "case_no_match",
        "gleam/list",
        407,
        "do_map2",
        "No case clause matched",
        { values: [list1, list2] }
      )
    }
  }
}

export function map2(list1, list2, fun) {
  return do_map2(list1, list2, fun, toList([]));
}

function do_index_map(loop$list, loop$fun, loop$index, loop$acc) {
  while (true) {
    let list = loop$list;
    let fun = loop$fun;
    let index = loop$index;
    let acc = loop$acc;
    if (list.hasLength(0)) {
      return reverse(acc);
    } else if (list.atLeastLength(1)) {
      let x = list.head;
      let xs = list.tail;
      let acc$1 = toList([fun(index, x)], acc);
      loop$list = xs;
      loop$fun = fun;
      loop$index = index + 1;
      loop$acc = acc$1;
    } else {
      throw makeError(
        "case_no_match",
        "gleam/list",
        445,
        "do_index_map",
        "No case clause matched",
        { values: [list] }
      )
    }
  }
}

export function index_map(list, fun) {
  return do_index_map(list, fun, 0, toList([]));
}

function do_try_map(loop$list, loop$fun, loop$acc) {
  while (true) {
    let list = loop$list;
    let fun = loop$fun;
    let acc = loop$acc;
    if (list.hasLength(0)) {
      return new Ok(reverse(acc));
    } else if (list.atLeastLength(1)) {
      let x = list.head;
      let xs = list.tail;
      let $ = fun(x);
      if ($.isOk()) {
        let y = $[0];
        loop$list = xs;
        loop$fun = fun;
        loop$acc = toList([y], acc);
      } else if (!$.isOk()) {
        let error = $[0];
        return new Error(error);
      } else {
        throw makeError(
          "case_no_match",
          "gleam/list",
          479,
          "do_try_map",
          "No case clause matched",
          { values: [$] }
        )
      }
    } else {
      throw makeError(
        "case_no_match",
        "gleam/list",
        476,
        "do_try_map",
        "No case clause matched",
        { values: [list] }
      )
    }
  }
}

export function try_map(list, fun) {
  return do_try_map(list, fun, toList([]));
}

export function drop(loop$list, loop$n) {
  while (true) {
    let list = loop$list;
    let n = loop$n;
    let $ = n <= 0;
    if ($) {
      return list;
    } else if (!$) {
      if (list.hasLength(0)) {
        return toList([]);
      } else if (list.atLeastLength(1)) {
        let xs = list.tail;
        loop$list = xs;
        loop$n = n - 1;
      } else {
        throw makeError(
          "case_no_match",
          "gleam/list",
          549,
          "drop",
          "No case clause matched",
          { values: [list] }
        )
      }
    } else {
      throw makeError(
        "case_no_match",
        "gleam/list",
        546,
        "drop",
        "No case clause matched",
        { values: [$] }
      )
    }
  }
}

function do_take(loop$list, loop$n, loop$acc) {
  while (true) {
    let list = loop$list;
    let n = loop$n;
    let acc = loop$acc;
    let $ = n <= 0;
    if ($) {
      return reverse(acc);
    } else if (!$) {
      if (list.hasLength(0)) {
        return reverse(acc);
      } else if (list.atLeastLength(1)) {
        let x = list.head;
        let xs = list.tail;
        loop$list = xs;
        loop$n = n - 1;
        loop$acc = toList([x], acc);
      } else {
        throw makeError(
          "case_no_match",
          "gleam/list",
          560,
          "do_take",
          "No case clause matched",
          { values: [list] }
        )
      }
    } else {
      throw makeError(
        "case_no_match",
        "gleam/list",
        557,
        "do_take",
        "No case clause matched",
        { values: [$] }
      )
    }
  }
}

export function take(list, n) {
  return do_take(list, n, toList([]));
}

export function new$() {
  return toList([]);
}

function do_append_acc(loop$first, loop$second) {
  while (true) {
    let first = loop$first;
    let second = loop$second;
    if (first.hasLength(0)) {
      return second;
    } else if (first.atLeastLength(1)) {
      let item = first.head;
      let rest$1 = first.tail;
      loop$first = rest$1;
      loop$second = toList([item], second);
    } else {
      throw makeError(
        "case_no_match",
        "gleam/list",
        631,
        "do_append_acc",
        "No case clause matched",
        { values: [first] }
      )
    }
  }
}

function do_append(first, second) {
  return do_append_acc(reverse(first), second);
}

export function append(first, second) {
  return do_append(first, second);
}

export function prepend(list, item) {
  return toList([item], list);
}

function reverse_and_prepend(loop$prefix, loop$suffix) {
  while (true) {
    let prefix = loop$prefix;
    let suffix = loop$suffix;
    if (prefix.hasLength(0)) {
      return suffix;
    } else if (prefix.atLeastLength(1)) {
      let first$1 = prefix.head;
      let rest$1 = prefix.tail;
      loop$prefix = rest$1;
      loop$suffix = toList([first$1], suffix);
    } else {
      throw makeError(
        "case_no_match",
        "gleam/list",
        650,
        "reverse_and_prepend",
        "No case clause matched",
        { values: [prefix] }
      )
    }
  }
}

function do_concat(loop$lists, loop$acc) {
  while (true) {
    let lists = loop$lists;
    let acc = loop$acc;
    if (lists.hasLength(0)) {
      return reverse(acc);
    } else if (lists.atLeastLength(1)) {
      let list = lists.head;
      let further_lists = lists.tail;
      loop$lists = further_lists;
      loop$acc = reverse_and_prepend(list, acc);
    } else {
      throw makeError(
        "case_no_match",
        "gleam/list",
        657,
        "do_concat",
        "No case clause matched",
        { values: [lists] }
      )
    }
  }
}

export function concat(lists) {
  return do_concat(lists, toList([]));
}

export function flatten(lists) {
  return do_concat(lists, toList([]));
}

export function flat_map(list, fun) {
  let _pipe = map(list, fun);
  return concat(_pipe);
}

export function fold(loop$list, loop$initial, loop$fun) {
  while (true) {
    let list = loop$list;
    let initial = loop$initial;
    let fun = loop$fun;
    if (list.hasLength(0)) {
      return initial;
    } else if (list.atLeastLength(1)) {
      let x = list.head;
      let rest$1 = list.tail;
      loop$list = rest$1;
      loop$initial = fun(initial, x);
      loop$fun = fun;
    } else {
      throw makeError(
        "case_no_match",
        "gleam/list",
        722,
        "fold",
        "No case clause matched",
        { values: [list] }
      )
    }
  }
}

export function group(list, key) {
  return fold(list, $dict.new$(), update_group(key));
}

export function map_fold(list, acc, fun) {
  let _pipe = fold(
    list,
    [acc, toList([])],
    (acc, item) => {
      let current_acc = acc[0];
      let items = acc[1];
      let $ = fun(current_acc, item);
      let next_acc = $[0];
      let next_item = $[1];
      return [next_acc, toList([next_item], items)];
    },
  );
  return $pair.map_second(_pipe, reverse);
}

export function fold_right(list, initial, fun) {
  if (list.hasLength(0)) {
    return initial;
  } else if (list.atLeastLength(1)) {
    let x = list.head;
    let rest$1 = list.tail;
    return fun(fold_right(rest$1, initial, fun), x);
  } else {
    throw makeError(
      "case_no_match",
      "gleam/list",
      744,
      "fold_right",
      "No case clause matched",
      { values: [list] }
    )
  }
}

function do_index_fold(loop$over, loop$acc, loop$with, loop$index) {
  while (true) {
    let over = loop$over;
    let acc = loop$acc;
    let with$ = loop$with;
    let index = loop$index;
    if (over.hasLength(0)) {
      return acc;
    } else if (over.atLeastLength(1)) {
      let first$1 = over.head;
      let rest$1 = over.tail;
      loop$over = rest$1;
      loop$acc = with$(acc, first$1, index);
      loop$with = with$;
      loop$index = index + 1;
    } else {
      throw makeError(
        "case_no_match",
        "gleam/list",
        756,
        "do_index_fold",
        "No case clause matched",
        { values: [over] }
      )
    }
  }
}

export function index_fold(over, initial, fun) {
  return do_index_fold(over, initial, fun, 0);
}

export function try_fold(loop$collection, loop$accumulator, loop$fun) {
  while (true) {
    let collection = loop$collection;
    let accumulator = loop$accumulator;
    let fun = loop$fun;
    if (collection.hasLength(0)) {
      return new Ok(accumulator);
    } else if (collection.atLeastLength(1)) {
      let first$1 = collection.head;
      let rest$1 = collection.tail;
      let $ = fun(accumulator, first$1);
      if ($.isOk()) {
        let result = $[0];
        loop$collection = rest$1;
        loop$accumulator = result;
        loop$fun = fun;
      } else if (!$.isOk()) {
        let error = $;
        return error;
      } else {
        throw makeError(
          "case_no_match",
          "gleam/list",
          806,
          "try_fold",
          "No case clause matched",
          { values: [$] }
        )
      }
    } else {
      throw makeError(
        "case_no_match",
        "gleam/list",
        803,
        "try_fold",
        "No case clause matched",
        { values: [collection] }
      )
    }
  }
}

export function fold_until(loop$collection, loop$accumulator, loop$fun) {
  while (true) {
    let collection = loop$collection;
    let accumulator = loop$accumulator;
    let fun = loop$fun;
    if (collection.hasLength(0)) {
      return accumulator;
    } else if (collection.atLeastLength(1)) {
      let first$1 = collection.head;
      let rest$1 = collection.tail;
      let $ = fun(accumulator, first$1);
      if ($ instanceof Continue) {
        let next_accumulator = $[0];
        loop$collection = rest$1;
        loop$accumulator = next_accumulator;
        loop$fun = fun;
      } else if ($ instanceof Stop) {
        let b = $[0];
        return b;
      } else {
        throw makeError(
          "case_no_match",
          "gleam/list",
          844,
          "fold_until",
          "No case clause matched",
          { values: [$] }
        )
      }
    } else {
      throw makeError(
        "case_no_match",
        "gleam/list",
        841,
        "fold_until",
        "No case clause matched",
        { values: [collection] }
      )
    }
  }
}

export function find(loop$haystack, loop$is_desired) {
  while (true) {
    let haystack = loop$haystack;
    let is_desired = loop$is_desired;
    if (haystack.hasLength(0)) {
      return new Error(undefined);
    } else if (haystack.atLeastLength(1)) {
      let x = haystack.head;
      let rest$1 = haystack.tail;
      let $ = is_desired(x);
      if ($) {
        return new Ok(x);
      } else {
        loop$haystack = rest$1;
        loop$is_desired = is_desired;
      }
    } else {
      throw makeError(
        "case_no_match",
        "gleam/list",
        877,
        "find",
        "No case clause matched",
        { values: [haystack] }
      )
    }
  }
}

export function find_map(loop$haystack, loop$fun) {
  while (true) {
    let haystack = loop$haystack;
    let fun = loop$fun;
    if (haystack.hasLength(0)) {
      return new Error(undefined);
    } else if (haystack.atLeastLength(1)) {
      let x = haystack.head;
      let rest$1 = haystack.tail;
      let $ = fun(x);
      if ($.isOk()) {
        let x$1 = $[0];
        return new Ok(x$1);
      } else {
        loop$haystack = rest$1;
        loop$fun = fun;
      }
    } else {
      throw makeError(
        "case_no_match",
        "gleam/list",
        913,
        "find_map",
        "No case clause matched",
        { values: [haystack] }
      )
    }
  }
}

export function all(loop$list, loop$predicate) {
  while (true) {
    let list = loop$list;
    let predicate = loop$predicate;
    if (list.hasLength(0)) {
      return true;
    } else if (list.atLeastLength(1)) {
      let first$1 = list.head;
      let rest$1 = list.tail;
      let $ = predicate(first$1);
      if ($) {
        loop$list = rest$1;
        loop$predicate = predicate;
      } else if (!$) {
        return false;
      } else {
        throw makeError(
          "case_no_match",
          "gleam/list",
          948,
          "all",
          "No case clause matched",
          { values: [$] }
        )
      }
    } else {
      throw makeError(
        "case_no_match",
        "gleam/list",
        945,
        "all",
        "No case clause matched",
        { values: [list] }
      )
    }
  }
}

export function any(loop$list, loop$predicate) {
  while (true) {
    let list = loop$list;
    let predicate = loop$predicate;
    if (list.hasLength(0)) {
      return false;
    } else if (list.atLeastLength(1)) {
      let first$1 = list.head;
      let rest$1 = list.tail;
      let $ = predicate(first$1);
      if ($) {
        return true;
      } else if (!$) {
        loop$list = rest$1;
        loop$predicate = predicate;
      } else {
        throw makeError(
          "case_no_match",
          "gleam/list",
          985,
          "any",
          "No case clause matched",
          { values: [$] }
        )
      }
    } else {
      throw makeError(
        "case_no_match",
        "gleam/list",
        982,
        "any",
        "No case clause matched",
        { values: [list] }
      )
    }
  }
}

function do_zip(loop$xs, loop$ys, loop$acc) {
  while (true) {
    let xs = loop$xs;
    let ys = loop$ys;
    let acc = loop$acc;
    if (xs.atLeastLength(1) && ys.atLeastLength(1)) {
      let x = xs.head;
      let xs$1 = xs.tail;
      let y = ys.head;
      let ys$1 = ys.tail;
      loop$xs = xs$1;
      loop$ys = ys$1;
      loop$acc = toList([[x, y]], acc);
    } else {
      return reverse(acc);
    }
  }
}

export function zip(list, other) {
  return do_zip(list, other, toList([]));
}

export function strict_zip(list, other) {
  let $ = length(list) === length(other);
  if ($) {
    return new Ok(zip(list, other));
  } else if (!$) {
    return new Error(new LengthMismatch());
  } else {
    throw makeError(
      "case_no_match",
      "gleam/list",
      1060,
      "strict_zip",
      "No case clause matched",
      { values: [$] }
    )
  }
}

function do_unzip(loop$input, loop$xs, loop$ys) {
  while (true) {
    let input = loop$input;
    let xs = loop$xs;
    let ys = loop$ys;
    if (input.hasLength(0)) {
      return [reverse(xs), reverse(ys)];
    } else if (input.atLeastLength(1)) {
      let x = input.head[0];
      let y = input.head[1];
      let rest$1 = input.tail;
      loop$input = rest$1;
      loop$xs = toList([x], xs);
      loop$ys = toList([y], ys);
    } else {
      throw makeError(
        "case_no_match",
        "gleam/list",
        1067,
        "do_unzip",
        "No case clause matched",
        { values: [input] }
      )
    }
  }
}

export function unzip(input) {
  return do_unzip(input, toList([]), toList([]));
}

function do_intersperse(loop$list, loop$separator, loop$acc) {
  while (true) {
    let list = loop$list;
    let separator = loop$separator;
    let acc = loop$acc;
    if (list.hasLength(0)) {
      return reverse(acc);
    } else if (list.atLeastLength(1)) {
      let x = list.head;
      let rest$1 = list.tail;
      loop$list = rest$1;
      loop$separator = separator;
      loop$acc = toList([x, separator], acc);
    } else {
      throw makeError(
        "case_no_match",
        "gleam/list",
        1092,
        "do_intersperse",
        "No case clause matched",
        { values: [list] }
      )
    }
  }
}

export function intersperse(list, elem) {
  if (list.hasLength(0)) {
    return list;
  } else if (list.hasLength(1)) {
    return list;
  } else if (list.atLeastLength(1)) {
    let x = list.head;
    let rest$1 = list.tail;
    return do_intersperse(rest$1, elem, toList([x]));
  } else {
    throw makeError(
      "case_no_match",
      "gleam/list",
      1115,
      "intersperse",
      "No case clause matched",
      { values: [list] }
    )
  }
}

export function at(list, index) {
  let $ = index >= 0;
  if ($) {
    let _pipe = list;
    let _pipe$1 = drop(_pipe, index);
    return first(_pipe$1);
  } else if (!$) {
    return new Error(undefined);
  } else {
    throw makeError(
      "case_no_match",
      "gleam/list",
      1140,
      "at",
      "No case clause matched",
      { values: [$] }
    )
  }
}

export function unique(list) {
  if (list.hasLength(0)) {
    return toList([]);
  } else if (list.atLeastLength(1)) {
    let x = list.head;
    let rest$1 = list.tail;
    return toList(
      [x],
      unique(filter(rest$1, (y) => { return !isEqual(y, x); })),
    );
  } else {
    throw makeError(
      "case_no_match",
      "gleam/list",
      1161,
      "unique",
      "No case clause matched",
      { values: [list] }
    )
  }
}

function merge_up(loop$na, loop$nb, loop$a, loop$b, loop$acc, loop$compare) {
  while (true) {
    let na = loop$na;
    let nb = loop$nb;
    let a = loop$a;
    let b = loop$b;
    let acc = loop$acc;
    let compare = loop$compare;
    if (na === 0 && nb === 0) {
      return acc;
    } else if (nb === 0 && a.atLeastLength(1)) {
      let ax = a.head;
      let ar = a.tail;
      loop$na = na - 1;
      loop$nb = nb;
      loop$a = ar;
      loop$b = b;
      loop$acc = toList([ax], acc);
      loop$compare = compare;
    } else if (na === 0 && b.atLeastLength(1)) {
      let bx = b.head;
      let br = b.tail;
      loop$na = na;
      loop$nb = nb - 1;
      loop$a = a;
      loop$b = br;
      loop$acc = toList([bx], acc);
      loop$compare = compare;
    } else if (a.atLeastLength(1) && b.atLeastLength(1)) {
      let ax = a.head;
      let ar = a.tail;
      let bx = b.head;
      let br = b.tail;
      let $ = compare(ax, bx);
      if ($ instanceof $order.Gt) {
        loop$na = na;
        loop$nb = nb - 1;
        loop$a = a;
        loop$b = br;
        loop$acc = toList([bx], acc);
        loop$compare = compare;
      } else {
        loop$na = na - 1;
        loop$nb = nb;
        loop$a = ar;
        loop$b = b;
        loop$acc = toList([ax], acc);
        loop$compare = compare;
      }
    } else {
      return acc;
    }
  }
}

function merge_down(loop$na, loop$nb, loop$a, loop$b, loop$acc, loop$compare) {
  while (true) {
    let na = loop$na;
    let nb = loop$nb;
    let a = loop$a;
    let b = loop$b;
    let acc = loop$acc;
    let compare = loop$compare;
    if (na === 0 && nb === 0) {
      return acc;
    } else if (nb === 0 && a.atLeastLength(1)) {
      let ax = a.head;
      let ar = a.tail;
      loop$na = na - 1;
      loop$nb = nb;
      loop$a = ar;
      loop$b = b;
      loop$acc = toList([ax], acc);
      loop$compare = compare;
    } else if (na === 0 && b.atLeastLength(1)) {
      let bx = b.head;
      let br = b.tail;
      loop$na = na;
      loop$nb = nb - 1;
      loop$a = a;
      loop$b = br;
      loop$acc = toList([bx], acc);
      loop$compare = compare;
    } else if (a.atLeastLength(1) && b.atLeastLength(1)) {
      let ax = a.head;
      let ar = a.tail;
      let bx = b.head;
      let br = b.tail;
      let $ = compare(bx, ax);
      if ($ instanceof $order.Lt) {
        loop$na = na - 1;
        loop$nb = nb;
        loop$a = ar;
        loop$b = b;
        loop$acc = toList([ax], acc);
        loop$compare = compare;
      } else {
        loop$na = na;
        loop$nb = nb - 1;
        loop$a = a;
        loop$b = br;
        loop$acc = toList([bx], acc);
        loop$compare = compare;
      }
    } else {
      return acc;
    }
  }
}

function merge_sort(l, ln, compare, down) {
  let n = divideInt(ln, 2);
  let a = l;
  let b = drop(l, n);
  let $ = ln < 3;
  if ($) {
    if (down) {
      return merge_down(n, ln - n, a, b, toList([]), compare);
    } else if (!down) {
      return merge_up(n, ln - n, a, b, toList([]), compare);
    } else {
      throw makeError(
        "case_no_match",
        "gleam/list",
        1232,
        "merge_sort",
        "No case clause matched",
        { values: [down] }
      )
    }
  } else if (!$) {
    if (down) {
      return merge_down(
        n,
        ln - n,
        merge_sort(a, n, compare, false),
        merge_sort(b, ln - n, compare, false),
        toList([]),
        compare,
      );
    } else if (!down) {
      return merge_up(
        n,
        ln - n,
        merge_sort(a, n, compare, true),
        merge_sort(b, ln - n, compare, true),
        toList([]),
        compare,
      );
    } else {
      throw makeError(
        "case_no_match",
        "gleam/list",
        1237,
        "merge_sort",
        "No case clause matched",
        { values: [down] }
      )
    }
  } else {
    throw makeError(
      "case_no_match",
      "gleam/list",
      1230,
      "merge_sort",
      "No case clause matched",
      { values: [$] }
    )
  }
}

export function sort(list, compare) {
  return merge_sort(list, length(list), compare, true);
}

function tail_recursive_range(loop$start, loop$stop, loop$acc) {
  while (true) {
    let start = loop$start;
    let stop = loop$stop;
    let acc = loop$acc;
    let $ = $int.compare(start, stop);
    if ($ instanceof $order.Eq) {
      return toList([stop], acc);
    } else if ($ instanceof $order.Gt) {
      loop$start = start;
      loop$stop = stop + 1;
      loop$acc = toList([stop], acc);
    } else if ($ instanceof $order.Lt) {
      loop$start = start;
      loop$stop = stop - 1;
      loop$acc = toList([stop], acc);
    } else {
      throw makeError(
        "case_no_match",
        "gleam/list",
        1299,
        "tail_recursive_range",
        "No case clause matched",
        { values: [$] }
      )
    }
  }
}

export function range(start, stop) {
  return tail_recursive_range(start, stop, toList([]));
}

function do_repeat(loop$a, loop$times, loop$acc) {
  while (true) {
    let a = loop$a;
    let times = loop$times;
    let acc = loop$acc;
    let $ = times <= 0;
    if ($) {
      return acc;
    } else if (!$) {
      loop$a = a;
      loop$times = times - 1;
      loop$acc = toList([a], acc);
    } else {
      throw makeError(
        "case_no_match",
        "gleam/list",
        1307,
        "do_repeat",
        "No case clause matched",
        { values: [$] }
      )
    }
  }
}

export function repeat(a, times) {
  return do_repeat(a, times, toList([]));
}

function do_split(loop$list, loop$n, loop$taken) {
  while (true) {
    let list = loop$list;
    let n = loop$n;
    let taken = loop$taken;
    let $ = n <= 0;
    if ($) {
      return [reverse(taken), list];
    } else if (!$) {
      if (list.hasLength(0)) {
        return [reverse(taken), toList([])];
      } else if (list.atLeastLength(1)) {
        let x = list.head;
        let xs = list.tail;
        loop$list = xs;
        loop$n = n - 1;
        loop$taken = toList([x], taken);
      } else {
        throw makeError(
          "case_no_match",
          "gleam/list",
          1335,
          "do_split",
          "No case clause matched",
          { values: [list] }
        )
      }
    } else {
      throw makeError(
        "case_no_match",
        "gleam/list",
        1332,
        "do_split",
        "No case clause matched",
        { values: [$] }
      )
    }
  }
}

export function split(list, index) {
  return do_split(list, index, toList([]));
}

function do_split_while(loop$list, loop$f, loop$acc) {
  while (true) {
    let list = loop$list;
    let f = loop$f;
    let acc = loop$acc;
    if (list.hasLength(0)) {
      return [reverse(acc), toList([])];
    } else if (list.atLeastLength(1)) {
      let x = list.head;
      let xs = list.tail;
      let $ = f(x);
      if (!$) {
        return [reverse(acc), list];
      } else {
        loop$list = xs;
        loop$f = f;
        loop$acc = toList([x], acc);
      }
    } else {
      throw makeError(
        "case_no_match",
        "gleam/list",
        1373,
        "do_split_while",
        "No case clause matched",
        { values: [list] }
      )
    }
  }
}

export function split_while(list, predicate) {
  return do_split_while(list, predicate, toList([]));
}

export function key_find(keyword_list, desired_key) {
  return find_map(
    keyword_list,
    (keyword) => {
      let key = keyword[0];
      let value = keyword[1];
      let $ = isEqual(key, desired_key);
      if ($) {
        return new Ok(value);
      } else if (!$) {
        return new Error(undefined);
      } else {
        throw makeError(
          "case_no_match",
          "gleam/list",
          1439,
          "",
          "No case clause matched",
          { values: [$] }
        )
      }
    },
  );
}

export function key_filter(keyword_list, desired_key) {
  return filter_map(
    keyword_list,
    (keyword) => {
      let key = keyword[0];
      let value = keyword[1];
      let $ = isEqual(key, desired_key);
      if ($) {
        return new Ok(value);
      } else if (!$) {
        return new Error(undefined);
      } else {
        throw makeError(
          "case_no_match",
          "gleam/list",
          1470,
          "",
          "No case clause matched",
          { values: [$] }
        )
      }
    },
  );
}

function do_pop(loop$haystack, loop$predicate, loop$checked) {
  while (true) {
    let haystack = loop$haystack;
    let predicate = loop$predicate;
    let checked = loop$checked;
    if (haystack.hasLength(0)) {
      return new Error(undefined);
    } else if (haystack.atLeastLength(1)) {
      let x = haystack.head;
      let rest$1 = haystack.tail;
      let $ = predicate(x);
      if ($) {
        return new Ok([x, append(reverse(checked), rest$1)]);
      } else if (!$) {
        loop$haystack = rest$1;
        loop$predicate = predicate;
        loop$checked = toList([x], checked);
      } else {
        throw makeError(
          "case_no_match",
          "gleam/list",
          1481,
          "do_pop",
          "No case clause matched",
          { values: [$] }
        )
      }
    } else {
      throw makeError(
        "case_no_match",
        "gleam/list",
        1478,
        "do_pop",
        "No case clause matched",
        { values: [haystack] }
      )
    }
  }
}

export function pop(haystack, is_desired) {
  return do_pop(haystack, is_desired, toList([]));
}

function do_pop_map(loop$haystack, loop$mapper, loop$checked) {
  while (true) {
    let haystack = loop$haystack;
    let mapper = loop$mapper;
    let checked = loop$checked;
    if (haystack.hasLength(0)) {
      return new Error(undefined);
    } else if (haystack.atLeastLength(1)) {
      let x = haystack.head;
      let rest$1 = haystack.tail;
      let $ = mapper(x);
      if ($.isOk()) {
        let y = $[0];
        return new Ok([y, append(reverse(checked), rest$1)]);
      } else if (!$.isOk()) {
        loop$haystack = rest$1;
        loop$mapper = mapper;
        loop$checked = toList([x], checked);
      } else {
        throw makeError(
          "case_no_match",
          "gleam/list",
          1520,
          "do_pop_map",
          "No case clause matched",
          { values: [$] }
        )
      }
    } else {
      throw makeError(
        "case_no_match",
        "gleam/list",
        1517,
        "do_pop_map",
        "No case clause matched",
        { values: [haystack] }
      )
    }
  }
}

export function pop_map(haystack, is_desired) {
  return do_pop_map(haystack, is_desired, toList([]));
}

export function key_pop(haystack, key) {
  return pop_map(
    haystack,
    (entry) => {
      let k = entry[0];
      let v = entry[1];
      if (isEqual(k, key)) {
        let k$1 = k;
        return new Ok(v);
      } else {
        return new Error(undefined);
      }
    },
  );
}

export function key_set(list, key, value) {
  if (list.hasLength(0)) {
    return toList([[key, value]]);
  } else if (list.atLeastLength(1) && isEqual(list.head[0], key)) {
    let k = list.head[0];
    let rest$1 = list.tail;
    return toList([[key, value]], rest$1);
  } else if (list.atLeastLength(1)) {
    let first$1 = list.head;
    let rest$1 = list.tail;
    return toList([first$1], key_set(rest$1, key, value));
  } else {
    throw makeError(
      "case_no_match",
      "gleam/list",
      1610,
      "key_set",
      "No case clause matched",
      { values: [list] }
    )
  }
}

export function each(loop$list, loop$f) {
  while (true) {
    let list = loop$list;
    let f = loop$f;
    if (list.hasLength(0)) {
      return undefined;
    } else if (list.atLeastLength(1)) {
      let x = list.head;
      let xs = list.tail;
      f(x);
      loop$list = xs;
      loop$f = f;
    } else {
      throw makeError(
        "case_no_match",
        "gleam/list",
        1627,
        "each",
        "No case clause matched",
        { values: [list] }
      )
    }
  }
}

export function try_each(loop$list, loop$fun) {
  while (true) {
    let list = loop$list;
    let fun = loop$fun;
    if (list.hasLength(0)) {
      return new Ok(undefined);
    } else if (list.atLeastLength(1)) {
      let x = list.head;
      let xs = list.tail;
      let $ = fun(x);
      if ($.isOk()) {
        loop$list = xs;
        loop$fun = fun;
      } else if (!$.isOk()) {
        let e = $[0];
        return new Error(e);
      } else {
        throw makeError(
          "case_no_match",
          "gleam/list",
          1659,
          "try_each",
          "No case clause matched",
          { values: [$] }
        )
      }
    } else {
      throw makeError(
        "case_no_match",
        "gleam/list",
        1656,
        "try_each",
        "No case clause matched",
        { values: [list] }
      )
    }
  }
}

function do_partition(loop$list, loop$categorise, loop$trues, loop$falses) {
  while (true) {
    let list = loop$list;
    let categorise = loop$categorise;
    let trues = loop$trues;
    let falses = loop$falses;
    if (list.hasLength(0)) {
      return [reverse(trues), reverse(falses)];
    } else if (list.atLeastLength(1)) {
      let x = list.head;
      let xs = list.tail;
      let $ = categorise(x);
      if ($) {
        loop$list = xs;
        loop$categorise = categorise;
        loop$trues = toList([x], trues);
        loop$falses = falses;
      } else if (!$) {
        loop$list = xs;
        loop$categorise = categorise;
        loop$trues = trues;
        loop$falses = toList([x], falses);
      } else {
        throw makeError(
          "case_no_match",
          "gleam/list",
          1670,
          "do_partition",
          "No case clause matched",
          { values: [$] }
        )
      }
    } else {
      throw makeError(
        "case_no_match",
        "gleam/list",
        1667,
        "do_partition",
        "No case clause matched",
        { values: [list] }
      )
    }
  }
}

export function partition(list, categorise) {
  return do_partition(list, categorise, toList([]), toList([]));
}

export function permutations(l) {
  if (l.hasLength(0)) {
    return toList([toList([])]);
  } else {
    let _pipe = l;
    let _pipe$1 = index_map(
      _pipe,
      (i_idx, i) => {
        let _pipe$1 = l;
        let _pipe$2 = index_fold(
          _pipe$1,
          toList([]),
          (acc, j, j_idx) => {
            let $ = i_idx === j_idx;
            if ($) {
              return acc;
            } else if (!$) {
              return toList([j], acc);
            } else {
              throw makeError(
                "case_no_match",
                "gleam/list",
                1711,
                "",
                "No case clause matched",
                { values: [$] }
              )
            }
          },
        );
        let _pipe$3 = reverse(_pipe$2);
        let _pipe$4 = permutations(_pipe$3);
        return map(
          _pipe$4,
          (permutation) => { return toList([i], permutation); },
        );
      },
    );
    return concat(_pipe$1);
  }
}

function do_window(loop$acc, loop$l, loop$n) {
  while (true) {
    let acc = loop$acc;
    let l = loop$l;
    let n = loop$n;
    let window$1 = take(l, n);
    let $ = length(window$1) === n;
    if ($) {
      loop$acc = toList([window$1], acc);
      loop$l = drop(l, 1);
      loop$n = n;
    } else if (!$) {
      return acc;
    } else {
      throw makeError(
        "case_no_match",
        "gleam/list",
        1727,
        "do_window",
        "No case clause matched",
        { values: [$] }
      )
    }
  }
}

export function window(l, n) {
  let _pipe = do_window(toList([]), l, n);
  return reverse(_pipe);
}

export function window_by_2(l) {
  return zip(l, drop(l, 1));
}

export function drop_while(loop$list, loop$predicate) {
  while (true) {
    let list = loop$list;
    let predicate = loop$predicate;
    if (list.hasLength(0)) {
      return toList([]);
    } else if (list.atLeastLength(1)) {
      let x = list.head;
      let xs = list.tail;
      let $ = predicate(x);
      if ($) {
        loop$list = xs;
        loop$predicate = predicate;
      } else if (!$) {
        return toList([x], xs);
      } else {
        throw makeError(
          "case_no_match",
          "gleam/list",
          1786,
          "drop_while",
          "No case clause matched",
          { values: [$] }
        )
      }
    } else {
      throw makeError(
        "case_no_match",
        "gleam/list",
        1783,
        "drop_while",
        "No case clause matched",
        { values: [list] }
      )
    }
  }
}

function do_take_while(loop$list, loop$predicate, loop$acc) {
  while (true) {
    let list = loop$list;
    let predicate = loop$predicate;
    let acc = loop$acc;
    if (list.hasLength(0)) {
      return reverse(acc);
    } else if (list.atLeastLength(1)) {
      let first$1 = list.head;
      let rest$1 = list.tail;
      let $ = predicate(first$1);
      if ($) {
        loop$list = rest$1;
        loop$predicate = predicate;
        loop$acc = toList([first$1], acc);
      } else if (!$) {
        return reverse(acc);
      } else {
        throw makeError(
          "case_no_match",
          "gleam/list",
          1801,
          "do_take_while",
          "No case clause matched",
          { values: [$] }
        )
      }
    } else {
      throw makeError(
        "case_no_match",
        "gleam/list",
        1798,
        "do_take_while",
        "No case clause matched",
        { values: [list] }
      )
    }
  }
}

export function take_while(list, predicate) {
  return do_take_while(list, predicate, toList([]));
}

function do_chunk(
  loop$list,
  loop$f,
  loop$previous_key,
  loop$current_chunk,
  loop$acc
) {
  while (true) {
    let list = loop$list;
    let f = loop$f;
    let previous_key = loop$previous_key;
    let current_chunk = loop$current_chunk;
    let acc = loop$acc;
    if (list.atLeastLength(1)) {
      let first$1 = list.head;
      let rest$1 = list.tail;
      let key = f(first$1);
      let $ = isEqual(key, previous_key);
      if (!$) {
        let new_acc = toList([reverse(current_chunk)], acc);
        loop$list = rest$1;
        loop$f = f;
        loop$previous_key = key;
        loop$current_chunk = toList([first$1]);
        loop$acc = new_acc;
      } else {
        loop$list = rest$1;
        loop$f = f;
        loop$previous_key = key;
        loop$current_chunk = toList([first$1], current_chunk);
        loop$acc = acc;
      }
    } else {
      return reverse(toList([reverse(current_chunk)], acc));
    }
  }
}

export function chunk(list, f) {
  if (list.hasLength(0)) {
    return toList([]);
  } else if (list.atLeastLength(1)) {
    let first$1 = list.head;
    let rest$1 = list.tail;
    return do_chunk(rest$1, f, f(first$1), toList([first$1]), toList([]));
  } else {
    throw makeError(
      "case_no_match",
      "gleam/list",
      1857,
      "chunk",
      "No case clause matched",
      { values: [list] }
    )
  }
}

function do_sized_chunk(
  loop$list,
  loop$count,
  loop$left,
  loop$current_chunk,
  loop$acc
) {
  while (true) {
    let list = loop$list;
    let count = loop$count;
    let left = loop$left;
    let current_chunk = loop$current_chunk;
    let acc = loop$acc;
    if (list.hasLength(0)) {
      if (current_chunk.hasLength(0)) {
        return reverse(acc);
      } else {
        let remaining = current_chunk;
        return reverse(toList([reverse(remaining)], acc));
      }
    } else if (list.atLeastLength(1)) {
      let first$1 = list.head;
      let rest$1 = list.tail;
      let chunk$1 = toList([first$1], current_chunk);
      let $ = left > 1;
      if (!$) {
        loop$list = rest$1;
        loop$count = count;
        loop$left = count;
        loop$current_chunk = toList([]);
        loop$acc = toList([reverse(chunk$1)], acc);
      } else if ($) {
        loop$list = rest$1;
        loop$count = count;
        loop$left = left - 1;
        loop$current_chunk = chunk$1;
        loop$acc = acc;
      } else {
        throw makeError(
          "case_no_match",
          "gleam/list",
          1878,
          "do_sized_chunk",
          "No case clause matched",
          { values: [$] }
        )
      }
    } else {
      throw makeError(
        "case_no_match",
        "gleam/list",
        1870,
        "do_sized_chunk",
        "No case clause matched",
        { values: [list] }
      )
    }
  }
}

export function sized_chunk(list, count) {
  return do_sized_chunk(list, count, count, toList([]), toList([]));
}

export function reduce(list, fun) {
  if (list.hasLength(0)) {
    return new Error(undefined);
  } else if (list.atLeastLength(1)) {
    let first$1 = list.head;
    let rest$1 = list.tail;
    return new Ok(fold(rest$1, first$1, fun));
  } else {
    throw makeError(
      "case_no_match",
      "gleam/list",
      1930,
      "reduce",
      "No case clause matched",
      { values: [list] }
    )
  }
}

function do_scan(loop$list, loop$accumulator, loop$accumulated, loop$fun) {
  while (true) {
    let list = loop$list;
    let accumulator = loop$accumulator;
    let accumulated = loop$accumulated;
    let fun = loop$fun;
    if (list.hasLength(0)) {
      return reverse(accumulated);
    } else if (list.atLeastLength(1)) {
      let x = list.head;
      let xs = list.tail;
      let next = fun(accumulator, x);
      loop$list = xs;
      loop$accumulator = next;
      loop$accumulated = toList([next], accumulated);
      loop$fun = fun;
    } else {
      throw makeError(
        "case_no_match",
        "gleam/list",
        1942,
        "do_scan",
        "No case clause matched",
        { values: [list] }
      )
    }
  }
}

export function scan(list, initial, fun) {
  return do_scan(list, initial, toList([]), fun);
}

export function last(list) {
  let _pipe = list;
  return reduce(_pipe, (_, elem) => { return elem; });
}

export function combinations(items, n) {
  if (n === 0) {
    return toList([toList([])]);
  } else {
    if (items.hasLength(0)) {
      return toList([]);
    } else if (items.atLeastLength(1)) {
      let x = items.head;
      let xs = items.tail;
      let first_combinations = (() => {
        let _pipe = map(
          combinations(xs, n - 1),
          (com) => { return toList([x], com); },
        );
        return reverse(_pipe);
      })();
      return fold(
        first_combinations,
        combinations(xs, n),
        (acc, c) => { return toList([c], acc); },
      );
    } else {
      throw makeError(
        "case_no_match",
        "gleam/list",
        2011,
        "combinations",
        "No case clause matched",
        { values: [items] }
      )
    }
  }
}

function do_combination_pairs(items) {
  if (items.hasLength(0)) {
    return toList([]);
  } else if (items.atLeastLength(1)) {
    let x = items.head;
    let xs = items.tail;
    let first_combinations = map(xs, (other) => { return [x, other]; });
    return toList([first_combinations], do_combination_pairs(xs));
  } else {
    throw makeError(
      "case_no_match",
      "gleam/list",
      2026,
      "do_combination_pairs",
      "No case clause matched",
      { values: [items] }
    )
  }
}

export function combination_pairs(items) {
  let _pipe = do_combination_pairs(items);
  return concat(_pipe);
}

export function transpose(loop$list_of_list) {
  while (true) {
    let list_of_list = loop$list_of_list;
    let take_first = (list) => {
      if (list.hasLength(0)) {
        return toList([]);
      } else if (list.hasLength(1)) {
        let f = list.head;
        return toList([f]);
      } else if (list.atLeastLength(1)) {
        let f = list.head;
        return toList([f]);
      } else {
        throw makeError(
          "case_no_match",
          "gleam/list",
          2078,
          "",
          "No case clause matched",
          { values: [list] }
        )
      }
    };
    if (list_of_list.hasLength(0)) {
      return toList([]);
    } else if (list_of_list.atLeastLength(1) && list_of_list.head.hasLength(0)) {
      let xss = list_of_list.tail;
      loop$list_of_list = xss;
    } else {
      let rows = list_of_list;
      let firsts = (() => {
        let _pipe = rows;
        let _pipe$1 = map(_pipe, take_first);
        return concat(_pipe$1);
      })();
      let rest$1 = transpose(
        map(rows, (_capture) => { return drop(_capture, 1); }),
      );
      return toList([firsts], rest$1);
    }
  }
}

export function interleave(list) {
  let _pipe = transpose(list);
  return concat(_pipe);
}

function do_shuffle_pair_unwrap(loop$list, loop$acc) {
  while (true) {
    let list = loop$list;
    let acc = loop$acc;
    if (list.hasLength(0)) {
      return acc;
    } else if (list.atLeastLength(1)) {
      let elem_pair = list.head;
      let enumerable = list.tail;
      loop$list = enumerable;
      loop$acc = toList([elem_pair[1]], acc);
    } else {
      throw makeError(
        "case_no_match",
        "gleam/list",
        2100,
        "do_shuffle_pair_unwrap",
        "No case clause matched",
        { values: [list] }
      )
    }
  }
}

function do_shuffle_by_pair_indexes(list_of_pairs) {
  return sort(
    list_of_pairs,
    (a_pair, b_pair) => { return $float.compare(a_pair[0], b_pair[0]); },
  );
}

export function shuffle(list) {
  let _pipe = list;
  let _pipe$1 = fold(
    _pipe,
    toList([]),
    (acc, a) => { return toList([[$float.random(0.0, 1.0), a]], acc); },
  );
  let _pipe$2 = do_shuffle_by_pair_indexes(_pipe$1);
  return do_shuffle_pair_unwrap(_pipe$2, toList([]));
}
