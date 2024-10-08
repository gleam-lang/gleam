import {
  Ok,
  Error,
  toList,
  prepend as listPrepend,
  CustomType as $CustomType,
  isEqual,
} from "../gleam.mjs";
import * as $dict from "../gleam/dict.mjs";
import * as $float from "../gleam/float.mjs";
import * as $int from "../gleam/int.mjs";
import * as $order from "../gleam/order.mjs";
import * as $pair from "../gleam/pair.mjs";

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

class Ascending extends $CustomType {}

class Descending extends $CustomType {}

function count_length(loop$list, loop$count) {
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

export function length(list) {
  return count_length(list, 0);
}

function do_reverse(loop$remaining, loop$accumulator) {
  while (true) {
    let remaining = loop$remaining;
    let accumulator = loop$accumulator;
    if (remaining.hasLength(0)) {
      return accumulator;
    } else {
      let item = remaining.head;
      let rest$1 = remaining.tail;
      loop$remaining = rest$1;
      loop$accumulator = listPrepend(item, accumulator);
    }
  }
}

export function reverse(list) {
  return do_reverse(list, toList([]));
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
    } else if (list.atLeastLength(1) && (isEqual(list.head, elem))) {
      let first$1 = list.head;
      return true;
    } else {
      let rest$1 = list.tail;
      loop$list = rest$1;
      loop$elem = elem;
    }
  }
}

export function first(list) {
  if (list.hasLength(0)) {
    return new Error(undefined);
  } else {
    let x = list.head;
    return new Ok(x);
  }
}

export function rest(list) {
  if (list.hasLength(0)) {
    return new Error(undefined);
  } else {
    let rest$1 = list.tail;
    return new Ok(rest$1);
  }
}

function update_group(f) {
  return (groups, elem) => {
    let $ = $dict.get(groups, f(elem));
    if ($.isOk()) {
      let existing = $[0];
      return $dict.insert(groups, f(elem), listPrepend(elem, existing));
    } else {
      return $dict.insert(groups, f(elem), toList([elem]));
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
    } else {
      let first$1 = list.head;
      let rest$1 = list.tail;
      let new_acc = (() => {
        let $ = fun(first$1);
        if ($) {
          return listPrepend(first$1, acc);
        } else {
          return acc;
        }
      })();
      loop$list = rest$1;
      loop$fun = fun;
      loop$acc = new_acc;
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
    } else {
      let first$1 = list.head;
      let rest$1 = list.tail;
      let new_acc = (() => {
        let $ = fun(first$1);
        if ($.isOk()) {
          let first$2 = $[0];
          return listPrepend(first$2, acc);
        } else {
          return acc;
        }
      })();
      loop$list = rest$1;
      loop$fun = fun;
      loop$acc = new_acc;
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
    } else {
      let first$1 = list.head;
      let rest$1 = list.tail;
      loop$list = rest$1;
      loop$fun = fun;
      loop$acc = listPrepend(fun(first$1), acc);
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
    } else {
      let a = list1.head;
      let as_ = list1.tail;
      let b = list2.head;
      let bs = list2.tail;
      loop$list1 = as_;
      loop$list2 = bs;
      loop$fun = fun;
      loop$acc = listPrepend(fun(a, b), acc);
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
    } else {
      let first$1 = list.head;
      let rest$1 = list.tail;
      let acc$1 = listPrepend(fun(first$1, index), acc);
      loop$list = rest$1;
      loop$fun = fun;
      loop$index = index + 1;
      loop$acc = acc$1;
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
    } else {
      let first$1 = list.head;
      let rest$1 = list.tail;
      let $ = fun(first$1);
      if ($.isOk()) {
        let first$2 = $[0];
        loop$list = rest$1;
        loop$fun = fun;
        loop$acc = listPrepend(first$2, acc);
      } else {
        let error = $[0];
        return new Error(error);
      }
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
    } else {
      if (list.hasLength(0)) {
        return toList([]);
      } else {
        let rest$1 = list.tail;
        loop$list = rest$1;
        loop$n = n - 1;
      }
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
    } else {
      if (list.hasLength(0)) {
        return reverse(acc);
      } else {
        let first$1 = list.head;
        let rest$1 = list.tail;
        loop$list = rest$1;
        loop$n = n - 1;
        loop$acc = listPrepend(first$1, acc);
      }
    }
  }
}

export function take(list, n) {
  return do_take(list, n, toList([]));
}

export function new$() {
  return toList([]);
}

export function wrap(item) {
  return toList([item]);
}

function do_append(loop$first, loop$second) {
  while (true) {
    let first = loop$first;
    let second = loop$second;
    if (first.hasLength(0)) {
      return second;
    } else {
      let item = first.head;
      let rest$1 = first.tail;
      loop$first = rest$1;
      loop$second = listPrepend(item, second);
    }
  }
}

export function append(first, second) {
  return do_append(reverse(first), second);
}

export function prepend(list, item) {
  return listPrepend(item, list);
}

function reverse_and_prepend(loop$prefix, loop$suffix) {
  while (true) {
    let prefix = loop$prefix;
    let suffix = loop$suffix;
    if (prefix.hasLength(0)) {
      return suffix;
    } else {
      let first$1 = prefix.head;
      let rest$1 = prefix.tail;
      loop$prefix = rest$1;
      loop$suffix = listPrepend(first$1, suffix);
    }
  }
}

function do_concat(loop$lists, loop$acc) {
  while (true) {
    let lists = loop$lists;
    let acc = loop$acc;
    if (lists.hasLength(0)) {
      return reverse(acc);
    } else {
      let list = lists.head;
      let further_lists = lists.tail;
      loop$lists = further_lists;
      loop$acc = reverse_and_prepend(list, acc);
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
  return flatten(_pipe);
}

export function fold(loop$list, loop$initial, loop$fun) {
  while (true) {
    let list = loop$list;
    let initial = loop$initial;
    let fun = loop$fun;
    if (list.hasLength(0)) {
      return initial;
    } else {
      let x = list.head;
      let rest$1 = list.tail;
      loop$list = rest$1;
      loop$initial = fun(initial, x);
      loop$fun = fun;
    }
  }
}

export function count(list, predicate) {
  return fold(
    list,
    0,
    (acc, value) => {
      let $ = predicate(value);
      if ($) {
        return acc + 1;
      } else {
        return acc;
      }
    },
  );
}

export function group(list, key) {
  return fold(list, $dict.new$(), update_group(key));
}

export function map_fold(list, initial, fun) {
  let _pipe = fold(
    list,
    [initial, toList([])],
    (acc, item) => {
      let current_acc = acc[0];
      let items = acc[1];
      let $ = fun(current_acc, item);
      let next_acc = $[0];
      let next_item = $[1];
      return [next_acc, listPrepend(next_item, items)];
    },
  );
  return $pair.map_second(_pipe, reverse);
}

export function fold_right(list, initial, fun) {
  if (list.hasLength(0)) {
    return initial;
  } else {
    let x = list.head;
    let rest$1 = list.tail;
    return fun(fold_right(rest$1, initial, fun), x);
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
    } else {
      let first$1 = over.head;
      let rest$1 = over.tail;
      loop$over = rest$1;
      loop$acc = with$(acc, first$1, index);
      loop$with = with$;
      loop$index = index + 1;
    }
  }
}

export function index_fold(list, initial, fun) {
  return do_index_fold(list, initial, fun, 0);
}

export function try_fold(loop$list, loop$initial, loop$fun) {
  while (true) {
    let list = loop$list;
    let initial = loop$initial;
    let fun = loop$fun;
    if (list.hasLength(0)) {
      return new Ok(initial);
    } else {
      let first$1 = list.head;
      let rest$1 = list.tail;
      let $ = fun(initial, first$1);
      if ($.isOk()) {
        let result = $[0];
        loop$list = rest$1;
        loop$initial = result;
        loop$fun = fun;
      } else {
        let error = $;
        return error;
      }
    }
  }
}

export function fold_until(loop$list, loop$initial, loop$fun) {
  while (true) {
    let list = loop$list;
    let initial = loop$initial;
    let fun = loop$fun;
    if (list.hasLength(0)) {
      return initial;
    } else {
      let first$1 = list.head;
      let rest$1 = list.tail;
      let $ = fun(initial, first$1);
      if ($ instanceof Continue) {
        let next_accumulator = $[0];
        loop$list = rest$1;
        loop$initial = next_accumulator;
        loop$fun = fun;
      } else {
        let b = $[0];
        return b;
      }
    }
  }
}

export function find(loop$list, loop$is_desired) {
  while (true) {
    let list = loop$list;
    let is_desired = loop$is_desired;
    if (list.hasLength(0)) {
      return new Error(undefined);
    } else {
      let x = list.head;
      let rest$1 = list.tail;
      let $ = is_desired(x);
      if ($) {
        return new Ok(x);
      } else {
        loop$list = rest$1;
        loop$is_desired = is_desired;
      }
    }
  }
}

export function find_map(loop$list, loop$fun) {
  while (true) {
    let list = loop$list;
    let fun = loop$fun;
    if (list.hasLength(0)) {
      return new Error(undefined);
    } else {
      let x = list.head;
      let rest$1 = list.tail;
      let $ = fun(x);
      if ($.isOk()) {
        let x$1 = $[0];
        return new Ok(x$1);
      } else {
        loop$list = rest$1;
        loop$fun = fun;
      }
    }
  }
}

export function all(loop$list, loop$predicate) {
  while (true) {
    let list = loop$list;
    let predicate = loop$predicate;
    if (list.hasLength(0)) {
      return true;
    } else {
      let first$1 = list.head;
      let rest$1 = list.tail;
      let $ = predicate(first$1);
      if ($) {
        loop$list = rest$1;
        loop$predicate = predicate;
      } else {
        return false;
      }
    }
  }
}

export function any(loop$list, loop$predicate) {
  while (true) {
    let list = loop$list;
    let predicate = loop$predicate;
    if (list.hasLength(0)) {
      return false;
    } else {
      let first$1 = list.head;
      let rest$1 = list.tail;
      let $ = predicate(first$1);
      if ($) {
        return true;
      } else {
        loop$list = rest$1;
        loop$predicate = predicate;
      }
    }
  }
}

function do_zip(loop$one, loop$other, loop$acc) {
  while (true) {
    let one = loop$one;
    let other = loop$other;
    let acc = loop$acc;
    if (one.atLeastLength(1) && other.atLeastLength(1)) {
      let first_one = one.head;
      let rest_one = one.tail;
      let first_other = other.head;
      let rest_other = other.tail;
      loop$one = rest_one;
      loop$other = rest_other;
      loop$acc = listPrepend([first_one, first_other], acc);
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
  } else {
    return new Error(undefined);
  }
}

function do_unzip(loop$input, loop$one, loop$other) {
  while (true) {
    let input = loop$input;
    let one = loop$one;
    let other = loop$other;
    if (input.hasLength(0)) {
      return [reverse(one), reverse(other)];
    } else {
      let first_one = input.head[0];
      let first_other = input.head[1];
      let rest$1 = input.tail;
      loop$input = rest$1;
      loop$one = listPrepend(first_one, one);
      loop$other = listPrepend(first_other, other);
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
    } else {
      let x = list.head;
      let rest$1 = list.tail;
      loop$list = rest$1;
      loop$separator = separator;
      loop$acc = listPrepend(x, listPrepend(separator, acc));
    }
  }
}

export function intersperse(list, elem) {
  if (list.hasLength(0)) {
    return list;
  } else if (list.hasLength(1)) {
    return list;
  } else {
    let x = list.head;
    let rest$1 = list.tail;
    return do_intersperse(rest$1, elem, toList([x]));
  }
}

export function unique(list) {
  if (list.hasLength(0)) {
    return toList([]);
  } else {
    let x = list.head;
    let rest$1 = list.tail;
    return listPrepend(
      x,
      unique(filter(rest$1, (y) => { return !isEqual(y, x); })),
    );
  }
}

function sequences(
  loop$list,
  loop$compare,
  loop$growing,
  loop$direction,
  loop$prev,
  loop$acc
) {
  while (true) {
    let list = loop$list;
    let compare = loop$compare;
    let growing = loop$growing;
    let direction = loop$direction;
    let prev = loop$prev;
    let acc = loop$acc;
    let growing$1 = listPrepend(prev, growing);
    if (list.hasLength(0)) {
      if (direction instanceof Ascending) {
        return listPrepend(do_reverse(growing$1, toList([])), acc);
      } else {
        return listPrepend(growing$1, acc);
      }
    } else {
      let new$1 = list.head;
      let rest$1 = list.tail;
      let $ = compare(prev, new$1);
      if ($ instanceof $order.Gt && direction instanceof Descending) {
        loop$list = rest$1;
        loop$compare = compare;
        loop$growing = growing$1;
        loop$direction = direction;
        loop$prev = new$1;
        loop$acc = acc;
      } else if ($ instanceof $order.Lt && direction instanceof Ascending) {
        loop$list = rest$1;
        loop$compare = compare;
        loop$growing = growing$1;
        loop$direction = direction;
        loop$prev = new$1;
        loop$acc = acc;
      } else if ($ instanceof $order.Eq && direction instanceof Ascending) {
        loop$list = rest$1;
        loop$compare = compare;
        loop$growing = growing$1;
        loop$direction = direction;
        loop$prev = new$1;
        loop$acc = acc;
      } else if ($ instanceof $order.Gt && direction instanceof Ascending) {
        let acc$1 = (() => {
          if (direction instanceof Ascending) {
            return listPrepend(do_reverse(growing$1, toList([])), acc);
          } else {
            return listPrepend(growing$1, acc);
          }
        })();
        if (rest$1.hasLength(0)) {
          return listPrepend(toList([new$1]), acc$1);
        } else {
          let next = rest$1.head;
          let rest$2 = rest$1.tail;
          let direction$1 = (() => {
            let $1 = compare(new$1, next);
            if ($1 instanceof $order.Lt) {
              return new Ascending();
            } else if ($1 instanceof $order.Eq) {
              return new Ascending();
            } else {
              return new Descending();
            }
          })();
          loop$list = rest$2;
          loop$compare = compare;
          loop$growing = toList([new$1]);
          loop$direction = direction$1;
          loop$prev = next;
          loop$acc = acc$1;
        }
      } else if ($ instanceof $order.Lt && direction instanceof Descending) {
        let acc$1 = (() => {
          if (direction instanceof Ascending) {
            return listPrepend(do_reverse(growing$1, toList([])), acc);
          } else {
            return listPrepend(growing$1, acc);
          }
        })();
        if (rest$1.hasLength(0)) {
          return listPrepend(toList([new$1]), acc$1);
        } else {
          let next = rest$1.head;
          let rest$2 = rest$1.tail;
          let direction$1 = (() => {
            let $1 = compare(new$1, next);
            if ($1 instanceof $order.Lt) {
              return new Ascending();
            } else if ($1 instanceof $order.Eq) {
              return new Ascending();
            } else {
              return new Descending();
            }
          })();
          loop$list = rest$2;
          loop$compare = compare;
          loop$growing = toList([new$1]);
          loop$direction = direction$1;
          loop$prev = next;
          loop$acc = acc$1;
        }
      } else {
        let acc$1 = (() => {
          if (direction instanceof Ascending) {
            return listPrepend(do_reverse(growing$1, toList([])), acc);
          } else {
            return listPrepend(growing$1, acc);
          }
        })();
        if (rest$1.hasLength(0)) {
          return listPrepend(toList([new$1]), acc$1);
        } else {
          let next = rest$1.head;
          let rest$2 = rest$1.tail;
          let direction$1 = (() => {
            let $1 = compare(new$1, next);
            if ($1 instanceof $order.Lt) {
              return new Ascending();
            } else if ($1 instanceof $order.Eq) {
              return new Ascending();
            } else {
              return new Descending();
            }
          })();
          loop$list = rest$2;
          loop$compare = compare;
          loop$growing = toList([new$1]);
          loop$direction = direction$1;
          loop$prev = next;
          loop$acc = acc$1;
        }
      }
    }
  }
}

function merge_ascendings(loop$list1, loop$list2, loop$compare, loop$acc) {
  while (true) {
    let list1 = loop$list1;
    let list2 = loop$list2;
    let compare = loop$compare;
    let acc = loop$acc;
    if (list1.hasLength(0)) {
      let list = list2;
      return do_reverse(list, acc);
    } else if (list2.hasLength(0)) {
      let list = list1;
      return do_reverse(list, acc);
    } else {
      let first1 = list1.head;
      let rest1 = list1.tail;
      let first2 = list2.head;
      let rest2 = list2.tail;
      let $ = compare(first1, first2);
      if ($ instanceof $order.Lt) {
        loop$list1 = rest1;
        loop$list2 = list2;
        loop$compare = compare;
        loop$acc = listPrepend(first1, acc);
      } else if ($ instanceof $order.Gt) {
        loop$list1 = list1;
        loop$list2 = rest2;
        loop$compare = compare;
        loop$acc = listPrepend(first2, acc);
      } else {
        loop$list1 = list1;
        loop$list2 = rest2;
        loop$compare = compare;
        loop$acc = listPrepend(first2, acc);
      }
    }
  }
}

function merge_ascending_pairs(loop$sequences, loop$compare, loop$acc) {
  while (true) {
    let sequences = loop$sequences;
    let compare = loop$compare;
    let acc = loop$acc;
    if (sequences.hasLength(0)) {
      return do_reverse(acc, toList([]));
    } else if (sequences.hasLength(1)) {
      let sequence = sequences.head;
      return do_reverse(
        listPrepend(do_reverse(sequence, toList([])), acc),
        toList([]),
      );
    } else {
      let ascending1 = sequences.head;
      let ascending2 = sequences.tail.head;
      let rest$1 = sequences.tail.tail;
      let descending = merge_ascendings(
        ascending1,
        ascending2,
        compare,
        toList([]),
      );
      loop$sequences = rest$1;
      loop$compare = compare;
      loop$acc = listPrepend(descending, acc);
    }
  }
}

function merge_descendings(loop$list1, loop$list2, loop$compare, loop$acc) {
  while (true) {
    let list1 = loop$list1;
    let list2 = loop$list2;
    let compare = loop$compare;
    let acc = loop$acc;
    if (list1.hasLength(0)) {
      let list = list2;
      return do_reverse(list, acc);
    } else if (list2.hasLength(0)) {
      let list = list1;
      return do_reverse(list, acc);
    } else {
      let first1 = list1.head;
      let rest1 = list1.tail;
      let first2 = list2.head;
      let rest2 = list2.tail;
      let $ = compare(first1, first2);
      if ($ instanceof $order.Lt) {
        loop$list1 = list1;
        loop$list2 = rest2;
        loop$compare = compare;
        loop$acc = listPrepend(first2, acc);
      } else if ($ instanceof $order.Gt) {
        loop$list1 = rest1;
        loop$list2 = list2;
        loop$compare = compare;
        loop$acc = listPrepend(first1, acc);
      } else {
        loop$list1 = rest1;
        loop$list2 = list2;
        loop$compare = compare;
        loop$acc = listPrepend(first1, acc);
      }
    }
  }
}

function merge_descending_pairs(loop$sequences, loop$compare, loop$acc) {
  while (true) {
    let sequences = loop$sequences;
    let compare = loop$compare;
    let acc = loop$acc;
    if (sequences.hasLength(0)) {
      return do_reverse(acc, toList([]));
    } else if (sequences.hasLength(1)) {
      let sequence = sequences.head;
      return do_reverse(
        listPrepend(do_reverse(sequence, toList([])), acc),
        toList([]),
      );
    } else {
      let descending1 = sequences.head;
      let descending2 = sequences.tail.head;
      let rest$1 = sequences.tail.tail;
      let ascending = merge_descendings(
        descending1,
        descending2,
        compare,
        toList([]),
      );
      loop$sequences = rest$1;
      loop$compare = compare;
      loop$acc = listPrepend(ascending, acc);
    }
  }
}

function merge_all(loop$sequences, loop$direction, loop$compare) {
  while (true) {
    let sequences = loop$sequences;
    let direction = loop$direction;
    let compare = loop$compare;
    if (sequences.hasLength(0)) {
      return toList([]);
    } else if (sequences.hasLength(1) && direction instanceof Ascending) {
      let sequence = sequences.head;
      return sequence;
    } else if (sequences.hasLength(1) && direction instanceof Descending) {
      let sequence = sequences.head;
      return do_reverse(sequence, toList([]));
    } else if (direction instanceof Ascending) {
      let sequences$1 = merge_ascending_pairs(sequences, compare, toList([]));
      loop$sequences = sequences$1;
      loop$direction = new Descending();
      loop$compare = compare;
    } else {
      let sequences$1 = merge_descending_pairs(sequences, compare, toList([]));
      loop$sequences = sequences$1;
      loop$direction = new Ascending();
      loop$compare = compare;
    }
  }
}

export function sort(list, compare) {
  if (list.hasLength(0)) {
    return toList([]);
  } else if (list.hasLength(1)) {
    let x = list.head;
    return toList([x]);
  } else {
    let x = list.head;
    let y = list.tail.head;
    let rest$1 = list.tail.tail;
    let direction = (() => {
      let $ = compare(x, y);
      if ($ instanceof $order.Lt) {
        return new Ascending();
      } else if ($ instanceof $order.Eq) {
        return new Ascending();
      } else {
        return new Descending();
      }
    })();
    let sequences$1 = sequences(
      rest$1,
      compare,
      toList([x]),
      direction,
      y,
      toList([]),
    );
    return merge_all(sequences$1, new Ascending(), compare);
  }
}

function tail_recursive_range(loop$start, loop$stop, loop$acc) {
  while (true) {
    let start = loop$start;
    let stop = loop$stop;
    let acc = loop$acc;
    let $ = $int.compare(start, stop);
    if ($ instanceof $order.Eq) {
      return listPrepend(stop, acc);
    } else if ($ instanceof $order.Gt) {
      loop$start = start;
      loop$stop = stop + 1;
      loop$acc = listPrepend(stop, acc);
    } else {
      loop$start = start;
      loop$stop = stop - 1;
      loop$acc = listPrepend(stop, acc);
    }
  }
}

export function range(start, stop) {
  return tail_recursive_range(start, stop, toList([]));
}

function do_repeat(loop$item, loop$times, loop$acc) {
  while (true) {
    let item = loop$item;
    let times = loop$times;
    let acc = loop$acc;
    let $ = times <= 0;
    if ($) {
      return acc;
    } else {
      loop$item = item;
      loop$times = times - 1;
      loop$acc = listPrepend(item, acc);
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
    } else {
      if (list.hasLength(0)) {
        return [reverse(taken), toList([])];
      } else {
        let first$1 = list.head;
        let rest$1 = list.tail;
        loop$list = rest$1;
        loop$n = n - 1;
        loop$taken = listPrepend(first$1, taken);
      }
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
    } else {
      let first$1 = list.head;
      let rest$1 = list.tail;
      let $ = f(first$1);
      if (!$) {
        return [reverse(acc), list];
      } else {
        loop$list = rest$1;
        loop$f = f;
        loop$acc = listPrepend(first$1, acc);
      }
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
      } else {
        return new Error(undefined);
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
      } else {
        return new Error(undefined);
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
    } else {
      let x = haystack.head;
      let rest$1 = haystack.tail;
      let $ = predicate(x);
      if ($) {
        return new Ok([x, append(reverse(checked), rest$1)]);
      } else {
        loop$haystack = rest$1;
        loop$predicate = predicate;
        loop$checked = listPrepend(x, checked);
      }
    }
  }
}

export function pop(list, is_desired) {
  return do_pop(list, is_desired, toList([]));
}

function do_pop_map(loop$list, loop$mapper, loop$checked) {
  while (true) {
    let list = loop$list;
    let mapper = loop$mapper;
    let checked = loop$checked;
    if (list.hasLength(0)) {
      return new Error(undefined);
    } else {
      let x = list.head;
      let rest$1 = list.tail;
      let $ = mapper(x);
      if ($.isOk()) {
        let y = $[0];
        return new Ok([y, append(reverse(checked), rest$1)]);
      } else {
        loop$list = rest$1;
        loop$mapper = mapper;
        loop$checked = listPrepend(x, checked);
      }
    }
  }
}

export function pop_map(haystack, is_desired) {
  return do_pop_map(haystack, is_desired, toList([]));
}

export function key_pop(list, key) {
  return pop_map(
    list,
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
  } else if (list.atLeastLength(1) && (isEqual(list.head[0], key))) {
    let k = list.head[0];
    let rest$1 = list.tail;
    return listPrepend([key, value], rest$1);
  } else {
    let first$1 = list.head;
    let rest$1 = list.tail;
    return listPrepend(first$1, key_set(rest$1, key, value));
  }
}

export function each(loop$list, loop$f) {
  while (true) {
    let list = loop$list;
    let f = loop$f;
    if (list.hasLength(0)) {
      return undefined;
    } else {
      let first$1 = list.head;
      let rest$1 = list.tail;
      f(first$1);
      loop$list = rest$1;
      loop$f = f;
    }
  }
}

export function try_each(loop$list, loop$fun) {
  while (true) {
    let list = loop$list;
    let fun = loop$fun;
    if (list.hasLength(0)) {
      return new Ok(undefined);
    } else {
      let first$1 = list.head;
      let rest$1 = list.tail;
      let $ = fun(first$1);
      if ($.isOk()) {
        loop$list = rest$1;
        loop$fun = fun;
      } else {
        let e = $[0];
        return new Error(e);
      }
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
    } else {
      let first$1 = list.head;
      let rest$1 = list.tail;
      let $ = categorise(first$1);
      if ($) {
        loop$list = rest$1;
        loop$categorise = categorise;
        loop$trues = listPrepend(first$1, trues);
        loop$falses = falses;
      } else {
        loop$list = rest$1;
        loop$categorise = categorise;
        loop$trues = trues;
        loop$falses = listPrepend(first$1, falses);
      }
    }
  }
}

export function partition(list, categorise) {
  return do_partition(list, categorise, toList([]), toList([]));
}

export function permutations(list) {
  if (list.hasLength(0)) {
    return toList([toList([])]);
  } else {
    let _pipe = index_map(
      list,
      (i, i_idx) => {
        let _pipe = index_fold(
          list,
          toList([]),
          (acc, j, j_idx) => {
            let $ = i_idx === j_idx;
            if ($) {
              return acc;
            } else {
              return listPrepend(j, acc);
            }
          },
        );
        let _pipe$1 = reverse(_pipe);
        let _pipe$2 = permutations(_pipe$1);
        return map(
          _pipe$2,
          (permutation) => { return listPrepend(i, permutation); },
        );
      },
    );
    return flatten(_pipe);
  }
}

function do_window(loop$acc, loop$list, loop$n) {
  while (true) {
    let acc = loop$acc;
    let list = loop$list;
    let n = loop$n;
    let window$1 = take(list, n);
    let $ = length(window$1) === n;
    if ($) {
      loop$acc = listPrepend(window$1, acc);
      loop$list = drop(list, 1);
      loop$n = n;
    } else {
      return acc;
    }
  }
}

export function window(list, n) {
  let $ = n <= 0;
  if ($) {
    return toList([]);
  } else {
    let _pipe = do_window(toList([]), list, n);
    return reverse(_pipe);
  }
}

export function window_by_2(list) {
  return zip(list, drop(list, 1));
}

export function drop_while(loop$list, loop$predicate) {
  while (true) {
    let list = loop$list;
    let predicate = loop$predicate;
    if (list.hasLength(0)) {
      return toList([]);
    } else {
      let first$1 = list.head;
      let rest$1 = list.tail;
      let $ = predicate(first$1);
      if ($) {
        loop$list = rest$1;
        loop$predicate = predicate;
      } else {
        return listPrepend(first$1, rest$1);
      }
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
    } else {
      let first$1 = list.head;
      let rest$1 = list.tail;
      let $ = predicate(first$1);
      if ($) {
        loop$list = rest$1;
        loop$predicate = predicate;
        loop$acc = listPrepend(first$1, acc);
      } else {
        return reverse(acc);
      }
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
        let new_acc = listPrepend(reverse(current_chunk), acc);
        loop$list = rest$1;
        loop$f = f;
        loop$previous_key = key;
        loop$current_chunk = toList([first$1]);
        loop$acc = new_acc;
      } else {
        loop$list = rest$1;
        loop$f = f;
        loop$previous_key = key;
        loop$current_chunk = listPrepend(first$1, current_chunk);
        loop$acc = acc;
      }
    } else {
      return reverse(listPrepend(reverse(current_chunk), acc));
    }
  }
}

export function chunk(list, f) {
  if (list.hasLength(0)) {
    return toList([]);
  } else {
    let first$1 = list.head;
    let rest$1 = list.tail;
    return do_chunk(rest$1, f, f(first$1), toList([first$1]), toList([]));
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
        return reverse(listPrepend(reverse(remaining), acc));
      }
    } else {
      let first$1 = list.head;
      let rest$1 = list.tail;
      let chunk$1 = listPrepend(first$1, current_chunk);
      let $ = left > 1;
      if (!$) {
        loop$list = rest$1;
        loop$count = count;
        loop$left = count;
        loop$current_chunk = toList([]);
        loop$acc = listPrepend(reverse(chunk$1), acc);
      } else {
        loop$list = rest$1;
        loop$count = count;
        loop$left = left - 1;
        loop$current_chunk = chunk$1;
        loop$acc = acc;
      }
    }
  }
}

export function sized_chunk(list, count) {
  return do_sized_chunk(list, count, count, toList([]), toList([]));
}

export function reduce(list, fun) {
  if (list.hasLength(0)) {
    return new Error(undefined);
  } else {
    let first$1 = list.head;
    let rest$1 = list.tail;
    return new Ok(fold(rest$1, first$1, fun));
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
    } else {
      let first$1 = list.head;
      let rest$1 = list.tail;
      let next = fun(accumulator, first$1);
      loop$list = rest$1;
      loop$accumulator = next;
      loop$accumulated = listPrepend(next, accumulated);
      loop$fun = fun;
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
    } else {
      let first$1 = items.head;
      let rest$1 = items.tail;
      let first_combinations = (() => {
        let _pipe = map(
          combinations(rest$1, n - 1),
          (com) => { return listPrepend(first$1, com); },
        );
        return reverse(_pipe);
      })();
      return fold(
        first_combinations,
        combinations(rest$1, n),
        (acc, c) => { return listPrepend(c, acc); },
      );
    }
  }
}

function do_combination_pairs(items) {
  if (items.hasLength(0)) {
    return toList([]);
  } else {
    let first$1 = items.head;
    let rest$1 = items.tail;
    let first_combinations = map(
      rest$1,
      (other) => { return [first$1, other]; },
    );
    return listPrepend(first_combinations, do_combination_pairs(rest$1));
  }
}

export function combination_pairs(items) {
  let _pipe = do_combination_pairs(items);
  return flatten(_pipe);
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
      } else {
        let f = list.head;
        return toList([f]);
      }
    };
    if (list_of_list.hasLength(0)) {
      return toList([]);
    } else if (list_of_list.atLeastLength(1) && list_of_list.head.hasLength(0)) {
      let rest$1 = list_of_list.tail;
      loop$list_of_list = rest$1;
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
      return listPrepend(firsts, rest$1);
    }
  }
}

export function interleave(list) {
  let _pipe = transpose(list);
  return flatten(_pipe);
}

function do_shuffle_pair_unwrap(loop$list, loop$acc) {
  while (true) {
    let list = loop$list;
    let acc = loop$acc;
    if (list.hasLength(0)) {
      return acc;
    } else {
      let elem_pair = list.head;
      let enumerable = list.tail;
      loop$list = enumerable;
      loop$acc = listPrepend(elem_pair[1], acc);
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
    (acc, a) => { return listPrepend([$float.random(), a], acc); },
  );
  let _pipe$2 = do_shuffle_by_pair_indexes(_pipe$1);
  return do_shuffle_pair_unwrap(_pipe$2, toList([]));
}
