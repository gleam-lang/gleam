import { Ok, Error, toList, CustomType, throwError, divideInt, isEqual } from "../gleam.js";
import * as $int from "../gleam/int.js";
import * as $order from "../gleam/order.js";
import * as $pair from "../gleam/pair.js";

export class LengthMismatch extends CustomType {}

export function length(list) {
  return do_length(list);
}

function do_length(list) {
  return do_length_acc(list, 0);
}

function do_length_acc(loop$list, loop$count) {
  let list = loop$list;
  let count = loop$count;
  while (true) {
    if (list.atLeastLength(1)) {
      let list$1 = list.tail;
      list = list$1;
      count = count + 1;
    } else {
      return count;
    }
  }
}

export function reverse(xs) {
  return do_reverse(xs);
}

function do_reverse(list) {
  return do_reverse_acc(list, toList([]));
}

function do_reverse_acc(loop$remaining, loop$accumulator) {
  let remaining = loop$remaining;
  let accumulator = loop$accumulator;
  while (true) {
    if (remaining.hasLength(0)) {
      return accumulator;
    } else if (remaining.atLeastLength(1)) {
      let item = remaining.head;
      let rest = remaining.tail;
      remaining = rest;
      accumulator = toList([item], accumulator);
    } else {
      throwError(
        "case_no_match",
        "gleam/list",
        107,
        "do_reverse_acc",
        "No case clause matched",
        { values: [remaining] }
      );
    }
  }
}

export function is_empty(list) {
  return isEqual(list, toList([]));
}

export function contains(list, elem) {
  if (list.hasLength(0)) {
    return false;
  } else if (list.atLeastLength(1)) {
    let head = list.head;
    let rest = list.tail;
    return (isEqual(head, elem)) || contains(rest, elem);
  } else {
    throwError(
      "case_no_match",
      "gleam/list",
      156,
      "contains",
      "No case clause matched",
      { values: [list] }
    );
  }
}

export function first(list) {
  if (list.hasLength(0)) {
    return new Error(undefined);
  } else if (list.atLeastLength(1)) {
    let x = list.head;
    return new Ok(x);
  } else {
    throwError(
      "case_no_match",
      "gleam/list",
      176,
      "first",
      "No case clause matched",
      { values: [list] }
    );
  }
}

export function rest(list) {
  if (list.hasLength(0)) {
    return new Error(undefined);
  } else if (list.atLeastLength(1)) {
    let xs = list.tail;
    return new Ok(xs);
  } else {
    throwError(
      "case_no_match",
      "gleam/list",
      199,
      "rest",
      "No case clause matched",
      { values: [list] }
    );
  }
}

function do_filter(loop$list, loop$fun, loop$acc) {
  let list = loop$list;
  let fun = loop$fun;
  let acc = loop$acc;
  while (true) {
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
          throwError(
            "case_no_match",
            "gleam/list",
            209,
            "do_filter",
            "No case clause matched",
            { values: [$] }
          );
        }
      })();
      list = xs;
      fun = fun;
      acc = new_acc;
    } else {
      throwError(
        "case_no_match",
        "gleam/list",
        206,
        "do_filter",
        "No case clause matched",
        { values: [list] }
      );
    }
  }
}

export function filter(list, predicate) {
  return do_filter(list, predicate, toList([]));
}

function do_filter_map(loop$list, loop$fun, loop$acc) {
  let list = loop$list;
  let fun = loop$fun;
  let acc = loop$acc;
  while (true) {
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
          throwError(
            "case_no_match",
            "gleam/list",
            241,
            "do_filter_map",
            "No case clause matched",
            { values: [$] }
          );
        }
      })();
      list = xs;
      fun = fun;
      acc = new_acc;
    } else {
      throwError(
        "case_no_match",
        "gleam/list",
        238,
        "do_filter_map",
        "No case clause matched",
        { values: [list] }
      );
    }
  }
}

export function filter_map(list, fun) {
  return do_filter_map(list, fun, toList([]));
}

function do_map(loop$list, loop$fun, loop$acc) {
  let list = loop$list;
  let fun = loop$fun;
  let acc = loop$acc;
  while (true) {
    if (list.hasLength(0)) {
      return reverse(acc);
    } else if (list.atLeastLength(1)) {
      let x = list.head;
      let xs = list.tail;
      list = xs;
      fun = fun;
      acc = toList([fun(x)], acc);
    } else {
      throwError(
        "case_no_match",
        "gleam/list",
        266,
        "do_map",
        "No case clause matched",
        { values: [list] }
      );
    }
  }
}

export function map(list, fun) {
  return do_map(list, fun, toList([]));
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

function do_index_map(loop$list, loop$fun, loop$index, loop$acc) {
  let list = loop$list;
  let fun = loop$fun;
  let index = loop$index;
  let acc = loop$acc;
  while (true) {
    if (list.hasLength(0)) {
      return reverse(acc);
    } else if (list.atLeastLength(1)) {
      let x = list.head;
      let xs = list.tail;
      let acc$1 = toList([fun(index, x)], acc);
      list = xs;
      fun = fun;
      index = index + 1;
      acc = acc$1;
    } else {
      throwError(
        "case_no_match",
        "gleam/list",
        320,
        "do_index_map",
        "No case clause matched",
        { values: [list] }
      );
    }
  }
}

export function index_map(list, fun) {
  return do_index_map(list, fun, 0, toList([]));
}

function do_try_map(loop$list, loop$fun, loop$acc) {
  let list = loop$list;
  let fun = loop$fun;
  let acc = loop$acc;
  while (true) {
    if (list.hasLength(0)) {
      return new Ok(reverse(acc));
    } else if (list.atLeastLength(1)) {
      let x = list.head;
      let xs = list.tail;
      let $ = fun(x);
      if ($.isOk()) {
        let y = $[0];
        list = xs;
        fun = fun;
        acc = toList([y], acc);
      } else if (!$.isOk()) {
        let error = $[0];
        return new Error(error);
      } else {
        throwError(
          "case_no_match",
          "gleam/list",
          352,
          "do_try_map",
          "No case clause matched",
          { values: [$] }
        );
      }
    } else {
      throwError(
        "case_no_match",
        "gleam/list",
        349,
        "do_try_map",
        "No case clause matched",
        { values: [list] }
      );
    }
  }
}

export function try_map(list, fun) {
  return do_try_map(list, fun, toList([]));
}

export function drop(loop$list, loop$n) {
  let list = loop$list;
  let n = loop$n;
  while (true) {
    let $ = n <= 0;
    if ($) {
      return list;
    } else if (!$) {
      if (list.hasLength(0)) {
        return toList([]);
      } else if (list.atLeastLength(1)) {
        let xs = list.tail;
        list = xs;
        n = n - 1;
      } else {
        throwError(
          "case_no_match",
          "gleam/list",
          410,
          "drop",
          "No case clause matched",
          { values: [list] }
        );
      }
    } else {
      throwError(
        "case_no_match",
        "gleam/list",
        407,
        "drop",
        "No case clause matched",
        { values: [$] }
      );
    }
  }
}

function do_take(loop$list, loop$n, loop$acc) {
  let list = loop$list;
  let n = loop$n;
  let acc = loop$acc;
  while (true) {
    let $ = n <= 0;
    if ($) {
      return reverse(acc);
    } else if (!$) {
      if (list.hasLength(0)) {
        return reverse(acc);
      } else if (list.atLeastLength(1)) {
        let x = list.head;
        let xs = list.tail;
        list = xs;
        n = n - 1;
        acc = toList([x], acc);
      } else {
        throwError(
          "case_no_match",
          "gleam/list",
          421,
          "do_take",
          "No case clause matched",
          { values: [list] }
        );
      }
    } else {
      throwError(
        "case_no_match",
        "gleam/list",
        418,
        "do_take",
        "No case clause matched",
        { values: [$] }
      );
    }
  }
}

export function take(list, n) {
  return do_take(list, n, toList([]));
}

export function new$() {
  return toList([]);
}

export function append(first, second) {
  return do_append(first, second);
}

function do_append(first, second) {
  return do_append_acc(reverse(first), second);
}

function do_append_acc(loop$first, loop$second) {
  let first = loop$first;
  let second = loop$second;
  while (true) {
    if (first.hasLength(0)) {
      return second;
    } else if (first.atLeastLength(1)) {
      let item = first.head;
      let rest$1 = first.tail;
      first = rest$1;
      second = toList([item], second);
    } else {
      throwError(
        "case_no_match",
        "gleam/list",
        484,
        "do_append_acc",
        "No case clause matched",
        { values: [first] }
      );
    }
  }
}

function do_flatten(loop$lists, loop$acc) {
  let lists = loop$lists;
  let acc = loop$acc;
  while (true) {
    if (lists.hasLength(0)) {
      return acc;
    } else if (lists.atLeastLength(1)) {
      let l = lists.head;
      let rest$1 = lists.tail;
      lists = rest$1;
      acc = append(acc, l);
    } else {
      throwError(
        "case_no_match",
        "gleam/list",
        492,
        "do_flatten",
        "No case clause matched",
        { values: [lists] }
      );
    }
  }
}

export function flatten(lists) {
  return do_flatten(lists, toList([]));
}

export function flat_map(list, fun) {
  let _pipe = map(list, fun);
  return flatten(_pipe);
}

export function fold(loop$list, loop$initial, loop$fun) {
  let list = loop$list;
  let initial = loop$initial;
  let fun = loop$fun;
  while (true) {
    if (list.hasLength(0)) {
      return initial;
    } else if (list.atLeastLength(1)) {
      let x = list.head;
      let rest$1 = list.tail;
      list = rest$1;
      initial = fun(initial, x);
      fun = fun;
    } else {
      throwError(
        "case_no_match",
        "gleam/list",
        538,
        "fold",
        "No case clause matched",
        { values: [list] }
      );
    }
  }
}

export function fold_right(list, initial, fun) {
  if (list.hasLength(0)) {
    return initial;
  } else if (list.atLeastLength(1)) {
    let x = list.head;
    let rest$1 = list.tail;
    return fun(fold_right(rest$1, initial, fun), x);
  } else {
    throwError(
      "case_no_match",
      "gleam/list",
      560,
      "fold_right",
      "No case clause matched",
      { values: [list] }
    );
  }
}

function do_index_fold(loop$over, loop$acc, loop$with, loop$index) {
  let over = loop$over;
  let acc = loop$acc;
  let with$ = loop$with;
  let index = loop$index;
  while (true) {
    if (over.hasLength(0)) {
      return acc;
    } else if (over.atLeastLength(1)) {
      let first$1 = over.head;
      let rest$1 = over.tail;
      over = rest$1;
      acc = with$(acc, first$1, index);
      with$ = with$;
      index = index + 1;
    } else {
      throwError(
        "case_no_match",
        "gleam/list",
        572,
        "do_index_fold",
        "No case clause matched",
        { values: [over] }
      );
    }
  }
}

export function index_fold(over, initial, fun) {
  return do_index_fold(over, initial, fun, 0);
}

export function try_fold(loop$collection, loop$accumulator, loop$fun) {
  let collection = loop$collection;
  let accumulator = loop$accumulator;
  let fun = loop$fun;
  while (true) {
    if (collection.hasLength(0)) {
      return new Ok(accumulator);
    } else if (collection.atLeastLength(1)) {
      let first$1 = collection.head;
      let rest$1 = collection.tail;
      let $ = fun(accumulator, first$1);
      if (!$.isOk()) return $;
      let accumulator$1 = $[0];

      collection = rest$1;
      accumulator = accumulator$1;
      fun = fun;
    } else {
      throwError(
        "case_no_match",
        "gleam/list",
        619,
        "try_fold",
        "No case clause matched",
        { values: [collection] }
      );
    }
  }
}

export class Continue extends CustomType {
  constructor(x0) {
    super();
    this[0] = x0;
  }
}

export class Stop extends CustomType {
  constructor(x0) {
    super();
    this[0] = x0;
  }
}

export function fold_until(loop$collection, loop$accumulator, loop$fun) {
  let collection = loop$collection;
  let accumulator = loop$accumulator;
  let fun = loop$fun;
  while (true) {
    if (collection.hasLength(0)) {
      return accumulator;
    } else if (collection.atLeastLength(1)) {
      let first$1 = collection.head;
      let rest$1 = collection.tail;
      let $ = fun(accumulator, first$1);
      if ($ instanceof Continue) {
        let next_accumulator = $[0];
        collection = rest$1;
        accumulator = next_accumulator;
        fun = fun;
      } else if ($ instanceof Stop) {
        let b = $[0];
        return b;
      } else {
        throwError(
          "case_no_match",
          "gleam/list",
          659,
          "fold_until",
          "No case clause matched",
          { values: [$] }
        );
      }
    } else {
      throwError(
        "case_no_match",
        "gleam/list",
        656,
        "fold_until",
        "No case clause matched",
        { values: [collection] }
      );
    }
  }
}

export function find(loop$haystack, loop$is_desired) {
  let haystack = loop$haystack;
  let is_desired = loop$is_desired;
  while (true) {
    if (haystack.hasLength(0)) {
      return new Error(undefined);
    } else if (haystack.atLeastLength(1)) {
      let x = haystack.head;
      let rest$1 = haystack.tail;
      let $ = is_desired(x);
      if ($) {
        return new Ok(x);
      } else {
        haystack = rest$1;
        is_desired = is_desired;
      }
    } else {
      throwError(
        "case_no_match",
        "gleam/list",
        687,
        "find",
        "No case clause matched",
        { values: [haystack] }
      );
    }
  }
}

export function find_map(loop$haystack, loop$fun) {
  let haystack = loop$haystack;
  let fun = loop$fun;
  while (true) {
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
        haystack = rest$1;
        fun = fun;
      }
    } else {
      throwError(
        "case_no_match",
        "gleam/list",
        718,
        "find_map",
        "No case clause matched",
        { values: [haystack] }
      );
    }
  }
}

export function all(list, predicate) {
  if (list.hasLength(0)) {
    return true;
  } else if (list.atLeastLength(1)) {
    let x = list.head;
    let rest$1 = list.tail;
    return predicate(x) && all(rest$1, predicate);
  } else {
    throwError(
      "case_no_match",
      "gleam/list",
      744,
      "all",
      "No case clause matched",
      { values: [list] }
    );
  }
}

export function any(list, predicate) {
  if (list.hasLength(0)) {
    return false;
  } else if (list.atLeastLength(1)) {
    let x = list.head;
    let rest$1 = list.tail;
    return predicate(x) || any(rest$1, predicate);
  } else {
    throwError(
      "case_no_match",
      "gleam/list",
      769,
      "any",
      "No case clause matched",
      { values: [list] }
    );
  }
}

function do_zip(loop$xs, loop$ys, loop$acc) {
  let xs = loop$xs;
  let ys = loop$ys;
  let acc = loop$acc;
  while (true) {
    if (xs.atLeastLength(1) && ys.atLeastLength(1)) {
      let x = xs.head;
      let xs$1 = xs.tail;
      let y = ys.head;
      let ys$1 = ys.tail;
      xs = xs$1;
      ys = ys$1;
      acc = toList([[x, y]], acc);
    } else {
      return reverse(acc);
    }
  }
}

export function zip(xs, ys) {
  return do_zip(xs, ys, toList([]));
}

export function strict_zip(l1, l2) {
  let $ = length(l1) === length(l2);
  if ($) {
    return new Ok(zip(l1, l2));
  } else if (!$) {
    return new Error(new LengthMismatch());
  } else {
    throwError(
      "case_no_match",
      "gleam/list",
      827,
      "strict_zip",
      "No case clause matched",
      { values: [$] }
    );
  }
}

function do_unzip(loop$input, loop$xs, loop$ys) {
  let input = loop$input;
  let xs = loop$xs;
  let ys = loop$ys;
  while (true) {
    if (input.hasLength(0)) {
      return [reverse(xs), reverse(ys)];
    } else if (input.atLeastLength(1)) {
      let x = input.head[0];
      let y = input.head[1];
      let rest$1 = input.tail;
      input = rest$1;
      xs = toList([x], xs);
      ys = toList([y], ys);
    } else {
      throwError(
        "case_no_match",
        "gleam/list",
        834,
        "do_unzip",
        "No case clause matched",
        { values: [input] }
      );
    }
  }
}

export function unzip(input) {
  return do_unzip(input, toList([]), toList([]));
}

function do_intersperse(loop$list, loop$separator, loop$acc) {
  let list = loop$list;
  let separator = loop$separator;
  let acc = loop$acc;
  while (true) {
    if (list.hasLength(0)) {
      return reverse(acc);
    } else if (list.atLeastLength(1)) {
      let x = list.head;
      let rest$1 = list.tail;
      list = rest$1;
      separator = separator;
      acc = toList([x, separator], acc);
    } else {
      throwError(
        "case_no_match",
        "gleam/list",
        855,
        "do_intersperse",
        "No case clause matched",
        { values: [list] }
      );
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
    throwError(
      "case_no_match",
      "gleam/list",
      874,
      "intersperse",
      "No case clause matched",
      { values: [list] }
    );
  }
}

export function at(list, index) {
  let _pipe = list;
  let _pipe$1 = drop(_pipe, index);
  return first(_pipe$1);
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
    throwError(
      "case_no_match",
      "gleam/list",
      911,
      "unique",
      "No case clause matched",
      { values: [list] }
    );
  }
}

function merge_sort(a, b, compare) {
  if (a.hasLength(0)) {
    return b;
  } else if (b.hasLength(0)) {
    return a;
  } else if (a.atLeastLength(1) && b.atLeastLength(1)) {
    let ax = a.head;
    let ar = a.tail;
    let bx = b.head;
    let br = b.tail;
    let $ = compare(ax, bx);
    if ($ instanceof $order.Lt) {
      return toList([ax], merge_sort(ar, b, compare));
    } else {
      return toList([bx], merge_sort(a, br, compare));
    }
  } else {
    throwError(
      "case_no_match",
      "gleam/list",
      918,
      "merge_sort",
      "No case clause matched",
      { values: [a, b] }
    );
  }
}

function do_sort(list, compare, list_length) {
  let $ = list_length < 2;
  if ($) {
    return list;
  } else if (!$) {
    let split_length = divideInt(list_length, 2);
    let a_list = take(list, split_length);
    let b_list = drop(list, split_length);
    return merge_sort(
      do_sort(a_list, compare, split_length),
      do_sort(b_list, compare, list_length - split_length),
      compare,
    );
  } else {
    throwError(
      "case_no_match",
      "gleam/list",
      934,
      "do_sort",
      "No case clause matched",
      { values: [$] }
    );
  }
}

export function sort(list, compare) {
  return do_sort(list, compare, length(list));
}

export function range(start, stop) {
  let $ = $int.compare(start, stop);
  if ($ instanceof $order.Eq) {
    return toList([]);
  } else if ($ instanceof $order.Gt) {
    return toList([start], range(start - 1, stop));
  } else if ($ instanceof $order.Lt) {
    return toList([start], range(start + 1, stop));
  } else {
    throwError(
      "case_no_match",
      "gleam/list",
      976,
      "range",
      "No case clause matched",
      { values: [$] }
    );
  }
}

function do_repeat(loop$a, loop$times, loop$acc) {
  let a = loop$a;
  let times = loop$times;
  let acc = loop$acc;
  while (true) {
    let $ = times <= 0;
    if ($) {
      return acc;
    } else if (!$) {
      a = a;
      times = times - 1;
      acc = toList([a], acc);
    } else {
      throwError(
        "case_no_match",
        "gleam/list",
        984,
        "do_repeat",
        "No case clause matched",
        { values: [$] }
      );
    }
  }
}

export function repeat(a, times) {
  return do_repeat(a, times, toList([]));
}

function do_split(loop$list, loop$n, loop$taken) {
  let list = loop$list;
  let n = loop$n;
  let taken = loop$taken;
  while (true) {
    let $ = n <= 0;
    if ($) {
      return [reverse(taken), list];
    } else if (!$) {
      if (list.hasLength(0)) {
        return [reverse(taken), toList([])];
      } else if (list.atLeastLength(1)) {
        let x = list.head;
        let xs = list.tail;
        list = xs;
        n = n - 1;
        taken = toList([x], taken);
      } else {
        throwError(
          "case_no_match",
          "gleam/list",
          1008,
          "do_split",
          "No case clause matched",
          { values: [list] }
        );
      }
    } else {
      throwError(
        "case_no_match",
        "gleam/list",
        1005,
        "do_split",
        "No case clause matched",
        { values: [$] }
      );
    }
  }
}

export function split(list, index) {
  return do_split(list, index, toList([]));
}

function do_split_while(loop$list, loop$f, loop$acc) {
  let list = loop$list;
  let f = loop$f;
  let acc = loop$acc;
  while (true) {
    if (list.hasLength(0)) {
      return [reverse(acc), toList([])];
    } else if (list.atLeastLength(1)) {
      let x = list.head;
      let xs = list.tail;
      let $ = f(x);
      if (!$) {
        return [reverse(acc), list];
      } else {
        list = xs;
        f = f;
        acc = toList([x], acc);
      }
    } else {
      throwError(
        "case_no_match",
        "gleam/list",
        1040,
        "do_split_while",
        "No case clause matched",
        { values: [list] }
      );
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
        throwError(
          "case_no_match",
          "gleam/list",
          1098,
          "",
          "No case clause matched",
          { values: [$] }
        );
      }
    },
  );
}

function do_pop(loop$haystack, loop$predicate, loop$checked) {
  let haystack = loop$haystack;
  let predicate = loop$predicate;
  let checked = loop$checked;
  while (true) {
    if (haystack.hasLength(0)) {
      return new Error(undefined);
    } else if (haystack.atLeastLength(1)) {
      let x = haystack.head;
      let rest$1 = haystack.tail;
      let $ = predicate(x);
      if ($) {
        return new Ok([x, append(reverse(checked), rest$1)]);
      } else if (!$) {
        haystack = rest$1;
        predicate = predicate;
        checked = toList([x], checked);
      } else {
        throwError(
          "case_no_match",
          "gleam/list",
          1110,
          "do_pop",
          "No case clause matched",
          { values: [$] }
        );
      }
    } else {
      throwError(
        "case_no_match",
        "gleam/list",
        1107,
        "do_pop",
        "No case clause matched",
        { values: [haystack] }
      );
    }
  }
}

export function pop(haystack, is_desired) {
  return do_pop(haystack, is_desired, toList([]));
}

function do_pop_map(loop$haystack, loop$mapper, loop$checked) {
  let haystack = loop$haystack;
  let mapper = loop$mapper;
  let checked = loop$checked;
  while (true) {
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
        haystack = rest$1;
        mapper = mapper;
        checked = toList([x], checked);
      } else {
        throwError(
          "case_no_match",
          "gleam/list",
          1144,
          "do_pop_map",
          "No case clause matched",
          { values: [$] }
        );
      }
    } else {
      throwError(
        "case_no_match",
        "gleam/list",
        1141,
        "do_pop_map",
        "No case clause matched",
        { values: [haystack] }
      );
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
    throwError(
      "case_no_match",
      "gleam/list",
      1223,
      "key_set",
      "No case clause matched",
      { values: [list] }
    );
  }
}

export function each(loop$list, loop$f) {
  let list = loop$list;
  let f = loop$f;
  while (true) {
    if (list.hasLength(0)) {
      return undefined;
    } else if (list.atLeastLength(1)) {
      let x = list.head;
      let xs = list.tail;
      f(x);
      list = xs;
      f = f;
    } else {
      throwError(
        "case_no_match",
        "gleam/list",
        1233,
        "each",
        "No case clause matched",
        { values: [list] }
      );
    }
  }
}

function do_partition(loop$list, loop$categorise, loop$trues, loop$falses) {
  let list = loop$list;
  let categorise = loop$categorise;
  let trues = loop$trues;
  let falses = loop$falses;
  while (true) {
    if (list.hasLength(0)) {
      return [reverse(trues), reverse(falses)];
    } else if (list.atLeastLength(1)) {
      let x = list.head;
      let xs = list.tail;
      let $ = categorise(x);
      if ($) {
        list = xs;
        categorise = categorise;
        trues = toList([x], trues);
        falses = falses;
      } else if (!$) {
        list = xs;
        categorise = categorise;
        trues = trues;
        falses = toList([x], falses);
      } else {
        throwError(
          "case_no_match",
          "gleam/list",
          1246,
          "do_partition",
          "No case clause matched",
          { values: [$] }
        );
      }
    } else {
      throwError(
        "case_no_match",
        "gleam/list",
        1243,
        "do_partition",
        "No case clause matched",
        { values: [list] }
      );
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
    let _pipe = map(
      l,
      (x) => {
        let _pipe = filter(l, (y) => { return !isEqual(y, x); });
        let _pipe$1 = permutations(_pipe);
        return map(
          _pipe$1,
          (_capture) => { return append(toList([x]), _capture); },
        );
      },
    );
    return flatten(_pipe);
  }
}

function do_window(loop$acc, loop$l, loop$n) {
  let acc = loop$acc;
  let l = loop$l;
  let n = loop$n;
  while (true) {
    let window = take(l, n);
    let $ = length(window) === n;
    if ($) {
      acc = toList([window], acc);
      l = drop(l, 1);
      n = n;
    } else if (!$) {
      return acc;
    } else {
      throwError(
        "case_no_match",
        "gleam/list",
        1287,
        "do_window",
        "No case clause matched",
        { values: [$] }
      );
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
  let list = loop$list;
  let predicate = loop$predicate;
  while (true) {
    if (list.hasLength(0)) {
      return toList([]);
    } else if (list.atLeastLength(1)) {
      let x = list.head;
      let xs = list.tail;
      let $ = predicate(x);
      if ($) {
        list = xs;
        predicate = predicate;
      } else if (!$) {
        return toList([x], xs);
      } else {
        throwError(
          "case_no_match",
          "gleam/list",
          1340,
          "drop_while",
          "No case clause matched",
          { values: [$] }
        );
      }
    } else {
      throwError(
        "case_no_match",
        "gleam/list",
        1337,
        "drop_while",
        "No case clause matched",
        { values: [list] }
      );
    }
  }
}

function do_take_while(loop$list, loop$predicate, loop$acc) {
  let list = loop$list;
  let predicate = loop$predicate;
  let acc = loop$acc;
  while (true) {
    if (list.hasLength(0)) {
      return reverse(acc);
    } else if (list.atLeastLength(1)) {
      let head = list.head;
      let tail = list.tail;
      let $ = predicate(head);
      if ($) {
        list = tail;
        predicate = predicate;
        acc = toList([head], acc);
      } else if (!$) {
        return reverse(acc);
      } else {
        throwError(
          "case_no_match",
          "gleam/list",
          1355,
          "do_take_while",
          "No case clause matched",
          { values: [$] }
        );
      }
    } else {
      throwError(
        "case_no_match",
        "gleam/list",
        1352,
        "do_take_while",
        "No case clause matched",
        { values: [list] }
      );
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
  let list = loop$list;
  let f = loop$f;
  let previous_key = loop$previous_key;
  let current_chunk = loop$current_chunk;
  let acc = loop$acc;
  while (true) {
    if (list.atLeastLength(1)) {
      let head = list.head;
      let tail = list.tail;
      let key = f(head);
      let $ = isEqual(key, previous_key);
      if (!$) {
        let new_acc = toList([reverse(current_chunk)], acc);
        list = tail;
        f = f;
        previous_key = key;
        current_chunk = toList([head]);
        acc = new_acc;
      } else {
        list = tail;
        f = f;
        previous_key = key;
        current_chunk = toList([head], current_chunk);
        acc = acc;
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
    let head = list.head;
    let tail = list.tail;
    return do_chunk(tail, f, f(head), toList([head]), toList([]));
  } else {
    throwError(
      "case_no_match",
      "gleam/list",
      1407,
      "chunk",
      "No case clause matched",
      { values: [list] }
    );
  }
}

function do_sized_chunk(
  loop$list,
  loop$count,
  loop$left,
  loop$current_chunk,
  loop$acc
) {
  let list = loop$list;
  let count = loop$count;
  let left = loop$left;
  let current_chunk = loop$current_chunk;
  let acc = loop$acc;
  while (true) {
    if (list.hasLength(0)) {
      if (current_chunk.hasLength(0)) {
        return reverse(acc);
      } else {
        let remaining = current_chunk;
        return reverse(toList([reverse(remaining)], acc));
      }
    } else if (list.atLeastLength(1)) {
      let head = list.head;
      let tail = list.tail;
      let chunk$1 = toList([head], current_chunk);
      let $ = left > 1;
      if (!$) {
        list = tail;
        count = count;
        left = count;
        current_chunk = toList([]);
        acc = toList([reverse(chunk$1)], acc);
      } else if ($) {
        list = tail;
        count = count;
        left = left - 1;
        current_chunk = chunk$1;
        acc = acc;
      } else {
        throwError(
          "case_no_match",
          "gleam/list",
          1428,
          "do_sized_chunk",
          "No case clause matched",
          { values: [$] }
        );
      }
    } else {
      throwError(
        "case_no_match",
        "gleam/list",
        1420,
        "do_sized_chunk",
        "No case clause matched",
        { values: [list] }
      );
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
    let head = list.head;
    let tail = list.tail;
    return new Ok(fold(tail, head, fun));
  } else {
    throwError(
      "case_no_match",
      "gleam/list",
      1471,
      "reduce",
      "No case clause matched",
      { values: [list] }
    );
  }
}

function do_scan(loop$list, loop$accumulator, loop$accumulated, loop$fun) {
  let list = loop$list;
  let accumulator = loop$accumulator;
  let accumulated = loop$accumulated;
  let fun = loop$fun;
  while (true) {
    if (list.hasLength(0)) {
      return reverse(accumulated);
    } else if (list.atLeastLength(1)) {
      let x = list.head;
      let xs = list.tail;
      let next = fun(accumulator, x);
      list = xs;
      accumulator = next;
      accumulated = toList([next], accumulated);
      fun = fun;
    } else {
      throwError(
        "case_no_match",
        "gleam/list",
        1483,
        "do_scan",
        "No case clause matched",
        { values: [list] }
      );
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
      throwError(
        "case_no_match",
        "gleam/list",
        1544,
        "combinations",
        "No case clause matched",
        { values: [items] }
      );
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
    throwError(
      "case_no_match",
      "gleam/list",
      1561,
      "do_combination_pairs",
      "No case clause matched",
      { values: [items] }
    );
  }
}

export function combination_pairs(items) {
  let _pipe = do_combination_pairs(items);
  return flatten(_pipe);
}

export function interleave(list) {
  let _pipe = transpose(list);
  return flatten(_pipe);
}

export function transpose(loop$list_of_list) {
  let list_of_list = loop$list_of_list;
  while (true) {
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
        throwError(
          "case_no_match",
          "gleam/list",
          1608,
          "",
          "No case clause matched",
          { values: [list] }
        );
      }
    };
    if (list_of_list.hasLength(0)) {
      return toList([]);
    } else if (list_of_list.atLeastLength(1) && list_of_list.head.hasLength(0)) {
      let xss = list_of_list.tail;
      list_of_list = xss;
    } else {
      let rows = list_of_list;
      let firsts = (() => {
        let _pipe = rows;
        let _pipe$1 = map(_pipe, take_first);
        return flatten(_pipe$1);
      })();
      let rest$1 = transpose(
        map(rows, (_capture) => { return drop(_capture, 1); }),
      );
      return toList([firsts], rest$1);
    }
  }
}
