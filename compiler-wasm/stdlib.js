export default {
  "gleam/list": `import gleam/int
import gleam/float
import gleam/order.{type Order}
import gleam/pair
import gleam/dict.{type Dict}
pub type LengthMismatch {
  LengthMismatch
}
pub fn length(of list: List(a)) -> Int {
  do_length(list)
}
@target(erlang)
fn do_length(a: List(a)) -> Int
@target(javascript)
fn do_length(list: List(a)) -> Int {
  do_length_acc(list, 0)
}
@target(javascript)
fn do_length_acc(list: List(a), count: Int) -> Int {
  case list {
    [_, ..list] -> do_length_acc(list, count + 1)
    _ -> count
  }
}
pub fn reverse(xs: List(a)) -> List(a) {
  do_reverse(xs)
}
@target(erlang)
fn do_reverse(a: List(a)) -> List(a)
@target(javascript)
fn do_reverse(list) {
  do_reverse_acc(list, [])
}
@target(javascript)
fn do_reverse_acc(remaining, accumulator) {
  case remaining {
    [] -> accumulator
    [item, ..rest] -> do_reverse_acc(rest, [item, ..accumulator])
  }
}
pub fn is_empty(list: List(a)) -> Bool {
  list == []
}
pub fn contains(list: List(a), any elem: a) -> Bool {
  case list {
    [] -> False
    [first, ..] if first == elem -> True
    [_, ..rest] -> contains(rest, elem)
  }
}
pub fn first(list: List(a)) -> Result(a, Nil) {
  case list {
    [] -> Error(Nil)
    [x, ..] -> Ok(x)
  }
}
pub fn rest(list: List(a)) -> Result(List(a), Nil) {
  case list {
    [] -> Error(Nil)
    [_, ..xs] -> Ok(xs)
  }
}
fn update_group(
  f: fn(element) -> key,
) -> fn(Dict(key, List(element)), element) -> Dict(key, List(element)) {
  fn(groups, elem) {
    case dict.get(groups, f(elem)) {
      Ok(existing) -> dict.insert(groups, f(elem), [elem, ..existing])
      Error(_) -> dict.insert(groups, f(elem), [elem])
    }
  }
}
pub fn group(list: List(v), by key: fn(v) -> k) -> Dict(k, List(v)) {
  fold(list, dict.new(), update_group(key))
}
fn do_filter(list: List(a), fun: fn(a) -> Bool, acc: List(a)) -> List(a) {
  case list {
    [] -> reverse(acc)
    [x, ..xs] -> {
      let new_acc = case fun(x) {
        True -> [x, ..acc]
        False -> acc
      }
      do_filter(xs, fun, new_acc)
    }
  }
}
pub fn filter(list: List(a), keeping predicate: fn(a) -> Bool) -> List(a) {
  do_filter(list, predicate, [])
}
fn do_filter_map(
  list: List(a),
  fun: fn(a) -> Result(b, e),
  acc: List(b),
) -> List(b) {
  case list {
    [] -> reverse(acc)
    [x, ..xs] -> {
      let new_acc = case fun(x) {
        Ok(x) -> [x, ..acc]
        Error(_) -> acc
      }
      do_filter_map(xs, fun, new_acc)
    }
  }
}
pub fn filter_map(list: List(a), with fun: fn(a) -> Result(b, e)) -> List(b) {
  do_filter_map(list, fun, [])
}
fn do_map(list: List(a), fun: fn(a) -> b, acc: List(b)) -> List(b) {
  case list {
    [] -> reverse(acc)
    [x, ..xs] -> do_map(xs, fun, [fun(x), ..acc])
  }
}
pub fn map(list: List(a), with fun: fn(a) -> b) -> List(b) {
  do_map(list, fun, [])
}
pub fn map2(list1: List(a), list2: List(b), with fun: fn(a, b) -> c) -> List(c) {
  do_map2(list1, list2, fun, [])
}
fn do_map2(
  list1: List(a),
  list2: List(b),
  fun: fn(a, b) -> c,
  acc: List(c),
) -> List(c) {
  case list1, list2 {
    [], _ | _, [] -> reverse(acc)
    [a, ..as_], [b, ..bs] -> do_map2(as_, bs, fun, [fun(a, b), ..acc])
  }
}
pub fn map_fold(
  over list: List(a),
  from acc: acc,
  with fun: fn(acc, a) -> #(acc, b),
) -> #(acc, List(b)) {
  fold(over: list, from: #(acc, []), with: fn(acc, item) {
    let #(current_acc, items) = acc
    let #(next_acc, next_item) = fun(current_acc, item)
    #(next_acc, [next_item, ..items])
  })
  |> pair.map_second(reverse)
}
fn do_index_map(
  list: List(a),
  fun: fn(Int, a) -> b,
  index: Int,
  acc: List(b),
) -> List(b) {
  case list {
    [] -> reverse(acc)
    [x, ..xs] -> {
      let acc = [fun(index, x), ..acc]
      do_index_map(xs, fun, index + 1, acc)
    }
  }
}
pub fn index_map(list: List(a), with fun: fn(Int, a) -> b) -> List(b) {
  do_index_map(list, fun, 0, [])
}
fn do_try_map(
  list: List(a),
  fun: fn(a) -> Result(b, e),
  acc: List(b),
) -> Result(List(b), e) {
  case list {
    [] -> Ok(reverse(acc))
    [x, ..xs] ->
      case fun(x) {
        Ok(y) -> do_try_map(xs, fun, [y, ..acc])
        Error(error) -> Error(error)
      }
  }
}
pub fn try_map(
  over list: List(a),
  with fun: fn(a) -> Result(b, e),
) -> Result(List(b), e) {
  do_try_map(list, fun, [])
}
pub fn drop(from list: List(a), up_to n: Int) -> List(a) {
  case n <= 0 {
    True -> list
    False ->
      case list {
        [] -> []
        [_, ..xs] -> drop(xs, n - 1)
      }
  }
}
fn do_take(list: List(a), n: Int, acc: List(a)) -> List(a) {
  case n <= 0 {
    True -> reverse(acc)
    False ->
      case list {
        [] -> reverse(acc)
        [x, ..xs] -> do_take(xs, n - 1, [x, ..acc])
      }
  }
}
pub fn take(from list: List(a), up_to n: Int) -> List(a) {
  do_take(list, n, [])
}
pub fn new() -> List(a) {
  []
}
pub fn append(first: List(a), second: List(a)) -> List(a) {
  do_append(first, second)
}
@target(erlang)
fn do_append(a: List(a), b: List(a)) -> List(a)
@target(javascript)
fn do_append(first: List(a), second: List(a)) -> List(a) {
  do_append_acc(reverse(first), second)
}
@target(javascript)
fn do_append_acc(first: List(a), second: List(a)) -> List(a) {
  case first {
    [] -> second
    [item, ..rest] -> do_append_acc(rest, [item, ..second])
  }
}
pub fn prepend(to list: List(a), this item: a) -> List(a) {
  [item, ..list]
}
fn reverse_and_prepend(list prefix: List(a), to suffix: List(a)) -> List(a) {
  case prefix {
    [] -> suffix
    [first, ..rest] -> reverse_and_prepend(list: rest, to: [first, ..suffix])
  }
}
fn do_concat(lists: List(List(a)), acc: List(a)) -> List(a) {
  case lists {
    [] -> reverse(acc)
    [list, ..further_lists] ->
      do_concat(further_lists, reverse_and_prepend(list: list, to: acc))
  }
}
pub fn concat(lists: List(List(a))) -> List(a) {
  do_concat(lists, [])
}
pub fn flatten(lists: List(List(a))) -> List(a) {
  do_concat(lists, [])
}
pub fn flat_map(over list: List(a), with fun: fn(a) -> List(b)) -> List(b) {
  map(list, fun)
  |> concat
}
pub fn fold(
  over list: List(a),
  from initial: acc,
  with fun: fn(acc, a) -> acc,
) -> acc {
  case list {
    [] -> initial
    [x, ..rest] -> fold(rest, fun(initial, x), fun)
  }
}
pub fn fold_right(
  over list: List(a),
  from initial: acc,
  with fun: fn(acc, a) -> acc,
) -> acc {
  case list {
    [] -> initial
    [x, ..rest] -> fun(fold_right(rest, initial, fun), x)
  }
}
fn do_index_fold(
  over: List(a),
  acc: acc,
  with: fn(acc, a, Int) -> acc,
  index: Int,
) -> acc {
  case over {
    [] -> acc
    [first, ..rest] ->
      do_index_fold(rest, with(acc, first, index), with, index + 1)
  }
}
pub fn index_fold(
  over over: List(a),
  from initial: acc,
  with fun: fn(acc, a, Int) -> acc,
) -> acc {
  do_index_fold(over, initial, fun, 0)
}
pub fn try_fold(
  over collection: List(a),
  from accumulator: acc,
  with fun: fn(acc, a) -> Result(acc, e),
) -> Result(acc, e) {
  case collection {
    [] -> Ok(accumulator)
    [first, ..rest] ->
      case fun(accumulator, first) {
        Ok(result) -> try_fold(rest, result, fun)
        Error(_) as error -> error
      }
  }
}
pub type ContinueOrStop(a) {
  Continue(a)
  Stop(a)
}
pub fn fold_until(
  over collection: List(a),
  from accumulator: acc,
  with fun: fn(acc, a) -> ContinueOrStop(acc),
) -> acc {
  case collection {
    [] -> accumulator
    [first, ..rest] ->
      case fun(accumulator, first) {
        Continue(next_accumulator) -> fold_until(rest, next_accumulator, fun)
        Stop(b) -> b
      }
  }
}
pub fn find(
  in haystack: List(a),
  one_that is_desired: fn(a) -> Bool,
) -> Result(a, Nil) {
  case haystack {
    [] -> Error(Nil)
    [x, ..rest] ->
      case is_desired(x) {
        True -> Ok(x)
        _ -> find(in: rest, one_that: is_desired)
      }
  }
}
pub fn find_map(
  in haystack: List(a),
  with fun: fn(a) -> Result(b, c),
) -> Result(b, Nil) {
  case haystack {
    [] -> Error(Nil)
    [x, ..rest] ->
      case fun(x) {
        Ok(x) -> Ok(x)
        _ -> find_map(in: rest, with: fun)
      }
  }
}
pub fn all(in list: List(a), satisfying predicate: fn(a) -> Bool) -> Bool {
  case list {
    [] -> True
    [first, ..rest] ->
      case predicate(first) {
        True -> all(rest, predicate)
        False -> False
      }
  }
}
pub fn any(in list: List(a), satisfying predicate: fn(a) -> Bool) -> Bool {
  case list {
    [] -> False
    [first, ..rest] ->
      case predicate(first) {
        True -> True
        False -> any(rest, predicate)
      }
  }
}
fn do_zip(xs: List(a), ys: List(b), acc: List(#(a, b))) -> List(#(a, b)) {
  case xs, ys {
    [x, ..xs], [y, ..ys] -> do_zip(xs, ys, [#(x, y), ..acc])
    _, _ -> reverse(acc)
  }
}
pub fn zip(list: List(a), with other: List(b)) -> List(#(a, b)) {
  do_zip(list, other, [])
}
pub fn strict_zip(
  list: List(a),
  with other: List(b),
) -> Result(List(#(a, b)), LengthMismatch) {
  case length(of: list) == length(of: other) {
    True -> Ok(zip(list, other))
    False -> Error(LengthMismatch)
  }
}
fn do_unzip(input, xs, ys) {
  case input {
    [] -> #(reverse(xs), reverse(ys))
    [#(x, y), ..rest] -> do_unzip(rest, [x, ..xs], [y, ..ys])
  }
}
pub fn unzip(input: List(#(a, b))) -> #(List(a), List(b)) {
  do_unzip(input, [], [])
}
fn do_intersperse(list: List(a), separator: a, acc: List(a)) -> List(a) {
  case list {
    [] -> reverse(acc)
    [x, ..rest] -> do_intersperse(rest, separator, [x, separator, ..acc])
  }
}
pub fn intersperse(list: List(a), with elem: a) -> List(a) {
  case list {
    [] | [_] -> list
    [x, ..rest] -> do_intersperse(rest, elem, [x])
  }
}
pub fn at(in list: List(a), get index: Int) -> Result(a, Nil) {
  case index >= 0 {
    True ->
      list
      |> drop(index)
      |> first
    False -> Error(Nil)
  }
}
pub fn unique(list: List(a)) -> List(a) {
  case list {
    [] -> []
    [x, ..rest] -> [x, ..unique(filter(rest, fn(y) { y != x }))]
  }
}
fn merge_up(
  na: Int,
  nb: Int,
  a: List(a),
  b: List(a),
  acc: List(a),
  compare: fn(a, a) -> Order,
) {
  case na, nb, a, b {
    0, 0, _, _ -> acc
    _, 0, [ax, ..ar], _ -> merge_up(na - 1, nb, ar, b, [ax, ..acc], compare)
    0, _, _, [bx, ..br] -> merge_up(na, nb - 1, a, br, [bx, ..acc], compare)
    _, _, [ax, ..ar], [bx, ..br] ->
      case compare(ax, bx) {
        order.Gt -> merge_up(na, nb - 1, a, br, [bx, ..acc], compare)
        _ -> merge_up(na - 1, nb, ar, b, [ax, ..acc], compare)
      }
    _, _, _, _ -> acc
  }
}
fn merge_down(
  na: Int,
  nb: Int,
  a: List(a),
  b: List(a),
  acc: List(a),
  compare: fn(a, a) -> Order,
) {
  case na, nb, a, b {
    0, 0, _, _ -> acc
    _, 0, [ax, ..ar], _ -> merge_down(na - 1, nb, ar, b, [ax, ..acc], compare)
    0, _, _, [bx, ..br] -> merge_down(na, nb - 1, a, br, [bx, ..acc], compare)
    _, _, [ax, ..ar], [bx, ..br] ->
      case compare(bx, ax) {
        order.Lt -> merge_down(na - 1, nb, ar, b, [ax, ..acc], compare)
        _ -> merge_down(na, nb - 1, a, br, [bx, ..acc], compare)
      }
    _, _, _, _ -> acc
  }
}
fn merge_sort(
  l: List(a),
  ln: Int,
  compare: fn(a, a) -> Order,
  down: Bool,
) -> List(a) {
  let n = ln / 2
  let a = l
  let b = drop(l, n)
  case ln < 3 {
    True ->
      case down {
        True -> merge_down(n, ln - n, a, b, [], compare)
        False -> merge_up(n, ln - n, a, b, [], compare)
      }
    False ->
      case down {
        True ->
          merge_down(
            n,
            ln - n,
            merge_sort(a, n, compare, False),
            merge_sort(b, ln - n, compare, False),
            [],
            compare,
          )
        False ->
          merge_up(
            n,
            ln - n,
            merge_sort(a, n, compare, True),
            merge_sort(b, ln - n, compare, True),
            [],
            compare,
          )
      }
  }
}
pub fn sort(list: List(a), by compare: fn(a, a) -> Order) -> List(a) {
  merge_sort(list, length(list), compare, True)
}
pub fn range(from start: Int, to stop: Int) -> List(Int) {
  tail_recursive_range(start, stop, [])
}
fn tail_recursive_range(start: Int, stop: Int, acc: List(Int)) -> List(Int) {
  case int.compare(start, stop) {
    order.Eq -> [stop, ..acc]
    order.Gt -> tail_recursive_range(start, stop + 1, [stop, ..acc])
    order.Lt -> tail_recursive_range(start, stop - 1, [stop, ..acc])
  }
}
fn do_repeat(a: a, times: Int, acc: List(a)) -> List(a) {
  case times <= 0 {
    True -> acc
    False -> do_repeat(a, times - 1, [a, ..acc])
  }
}
pub fn repeat(item a: a, times times: Int) -> List(a) {
  do_repeat(a, times, [])
}
fn do_split(list: List(a), n: Int, taken: List(a)) -> #(List(a), List(a)) {
  case n <= 0 {
    True -> #(reverse(taken), list)
    False ->
      case list {
        [] -> #(reverse(taken), [])
        [x, ..xs] -> do_split(xs, n - 1, [x, ..taken])
      }
  }
}
pub fn split(list list: List(a), at index: Int) -> #(List(a), List(a)) {
  do_split(list, index, [])
}
fn do_split_while(
  list: List(a),
  f: fn(a) -> Bool,
  acc: List(a),
) -> #(List(a), List(a)) {
  case list {
    [] -> #(reverse(acc), [])
    [x, ..xs] ->
      case f(x) {
        False -> #(reverse(acc), list)
        _ -> do_split_while(xs, f, [x, ..acc])
      }
  }
}
pub fn split_while(
  list list: List(a),
  satisfying predicate: fn(a) -> Bool,
) -> #(List(a), List(a)) {
  do_split_while(list, predicate, [])
}
pub fn key_find(
  in keyword_list: List(#(k, v)),
  find desired_key: k,
) -> Result(v, Nil) {
  find_map(keyword_list, fn(keyword) {
    let #(key, value) = keyword
    case key == desired_key {
      True -> Ok(value)
      False -> Error(Nil)
    }
  })
}
pub fn key_filter(
  in keyword_list: List(#(k, v)),
  find desired_key: k,
) -> List(v) {
  filter_map(keyword_list, fn(keyword) {
    let #(key, value) = keyword
    case key == desired_key {
      True -> Ok(value)
      False -> Error(Nil)
    }
  })
}
fn do_pop(haystack, predicate, checked) {
  case haystack {
    [] -> Error(Nil)
    [x, ..rest] ->
      case predicate(x) {
        True -> Ok(#(x, append(reverse(checked), rest)))
        False -> do_pop(rest, predicate, [x, ..checked])
      }
  }
}
pub fn pop(
  in haystack: List(a),
  one_that is_desired: fn(a) -> Bool,
) -> Result(#(a, List(a)), Nil) {
  do_pop(haystack, is_desired, [])
}
fn do_pop_map(haystack, mapper, checked) {
  case haystack {
    [] -> Error(Nil)
    [x, ..rest] ->
      case mapper(x) {
        Ok(y) -> Ok(#(y, append(reverse(checked), rest)))
        Error(_) -> do_pop_map(rest, mapper, [x, ..checked])
      }
  }
}
pub fn pop_map(
  in haystack: List(a),
  one_that is_desired: fn(a) -> Result(b, c),
) -> Result(#(b, List(a)), Nil) {
  do_pop_map(haystack, is_desired, [])
}
pub fn key_pop(
  haystack: List(#(k, v)),
  key: k,
) -> Result(#(v, List(#(k, v))), Nil) {
  pop_map(haystack, fn(entry) {
    let #(k, v) = entry
    case k {
      k if k == key -> Ok(v)
      _ -> Error(Nil)
    }
  })
}
pub fn key_set(list: List(#(a, b)), key: a, value: b) -> List(#(a, b)) {
  case list {
    [] -> [#(key, value)]
    [#(k, _), ..rest] if k == key -> [#(key, value), ..rest]
    [first, ..rest] -> [first, ..key_set(rest, key, value)]
  }
}
pub fn each(list: List(a), f: fn(a) -> b) -> Nil {
  case list {
    [] -> Nil
    [x, ..xs] -> {
      f(x)
      each(xs, f)
    }
  }
}
pub fn try_each(
  over list: List(a),
  with fun: fn(a) -> Result(b, e),
) -> Result(Nil, e) {
  case list {
    [] -> Ok(Nil)
    [x, ..xs] ->
      case fun(x) {
        Ok(_) -> try_each(over: xs, with: fun)
        Error(e) -> Error(e)
      }
  }
}
fn do_partition(list, categorise, trues, falses) {
  case list {
    [] -> #(reverse(trues), reverse(falses))
    [x, ..xs] ->
      case categorise(x) {
        True -> do_partition(xs, categorise, [x, ..trues], falses)
        False -> do_partition(xs, categorise, trues, [x, ..falses])
      }
  }
}
pub fn partition(
  list: List(a),
  with categorise: fn(a) -> Bool,
) -> #(List(a), List(a)) {
  do_partition(list, categorise, [], [])
}
pub fn permutations(l: List(a)) -> List(List(a)) {
  case l {
    [] -> [[]]
    _ ->
      l
      |> index_map(fn(i_idx, i) {
        l
        |> index_fold([], fn(acc, j, j_idx) {
          case i_idx == j_idx {
            True -> acc
            False -> [j, ..acc]
          }
        })
        |> reverse
        |> permutations
        |> map(fn(permutation) { [i, ..permutation] })
      })
      |> concat
  }
}
fn do_window(acc: List(List(a)), l: List(a), n: Int) -> List(List(a)) {
  let window = take(l, n)
  case length(window) == n {
    True -> do_window([window, ..acc], drop(l, 1), n)
    False -> acc
  }
}
pub fn window(l: List(a), by n: Int) -> List(List(a)) {
  do_window([], l, n)
  |> reverse
}
pub fn window_by_2(l: List(a)) -> List(#(a, a)) {
  zip(l, drop(l, 1))
}
pub fn drop_while(
  in list: List(a),
  satisfying predicate: fn(a) -> Bool,
) -> List(a) {
  case list {
    [] -> []
    [x, ..xs] ->
      case predicate(x) {
        True -> drop_while(xs, predicate)
        False -> [x, ..xs]
      }
  }
}
fn do_take_while(
  list: List(a),
  predicate: fn(a) -> Bool,
  acc: List(a),
) -> List(a) {
  case list {
    [] -> reverse(acc)
    [first, ..rest] ->
      case predicate(first) {
        True -> do_take_while(rest, predicate, [first, ..acc])
        False -> reverse(acc)
      }
  }
}
pub fn take_while(
  in list: List(a),
  satisfying predicate: fn(a) -> Bool,
) -> List(a) {
  do_take_while(list, predicate, [])
}
fn do_chunk(
  list: List(a),
  f: fn(a) -> key,
  previous_key: key,
  current_chunk: List(a),
  acc: List(List(a)),
) -> List(List(a)) {
  case list {
    [first, ..rest] -> {
      let key = f(first)
      case key == previous_key {
        False -> {
          let new_acc = [reverse(current_chunk), ..acc]
          do_chunk(rest, f, key, [first], new_acc)
        }
        _true -> do_chunk(rest, f, key, [first, ..current_chunk], acc)
      }
    }
    _empty -> reverse([reverse(current_chunk), ..acc])
  }
}
pub fn chunk(in list: List(a), by f: fn(a) -> key) -> List(List(a)) {
  case list {
    [] -> []
    [first, ..rest] -> do_chunk(rest, f, f(first), [first], [])
  }
}
fn do_sized_chunk(
  list: List(a),
  count: Int,
  left: Int,
  current_chunk: List(a),
  acc: List(List(a)),
) -> List(List(a)) {
  case list {
    [] ->
      case current_chunk {
        [] -> reverse(acc)
        remaining -> reverse([reverse(remaining), ..acc])
      }
    [first, ..rest] -> {
      let chunk = [first, ..current_chunk]
      case left > 1 {
        False -> do_sized_chunk(rest, count, count, [], [reverse(chunk), ..acc])
        True -> do_sized_chunk(rest, count, left - 1, chunk, acc)
      }
    }
  }
}
pub fn sized_chunk(in list: List(a), into count: Int) -> List(List(a)) {
  do_sized_chunk(list, count, count, [], [])
}
pub fn reduce(over list: List(a), with fun: fn(a, a) -> a) -> Result(a, Nil) {
  case list {
    [] -> Error(Nil)
    [first, ..rest] -> Ok(fold(rest, first, fun))
  }
}
fn do_scan(
  list: List(a),
  accumulator: acc,
  accumulated: List(acc),
  fun: fn(acc, a) -> acc,
) -> List(acc) {
  case list {
    [] -> reverse(accumulated)
    [x, ..xs] -> {
      let next = fun(accumulator, x)
      do_scan(xs, next, [next, ..accumulated], fun)
    }
  }
}
pub fn scan(
  over list: List(a),
  from initial: acc,
  with fun: fn(acc, a) -> acc,
) -> List(acc) {
  do_scan(list, initial, [], fun)
}
pub fn last(list: List(a)) -> Result(a, Nil) {
  list
  |> reduce(fn(_, elem) { elem })
}
pub fn combinations(items: List(a), by n: Int) -> List(List(a)) {
  case n {
    0 -> [[]]
    _ ->
      case items {
        [] -> []
        [x, ..xs] -> {
          let first_combinations =
            map(combinations(xs, n - 1), with: fn(com) { [x, ..com] })
            |> reverse
          fold(first_combinations, combinations(xs, n), fn(acc, c) {
            [c, ..acc]
          })
        }
      }
  }
}
fn do_combination_pairs(items: List(a)) -> List(List(#(a, a))) {
  case items {
    [] -> []
    [x, ..xs] -> {
      let first_combinations = map(xs, with: fn(other) { #(x, other) })
      [first_combinations, ..do_combination_pairs(xs)]
    }
  }
}
pub fn combination_pairs(items: List(a)) -> List(#(a, a)) {
  do_combination_pairs(items)
  |> concat
}
pub fn interleave(list: List(List(a))) -> List(a) {
  transpose(list)
  |> concat
}
pub fn transpose(list_of_list: List(List(a))) -> List(List(a)) {
  let take_first = fn(list) {
    case list {
      [] -> []
      [f] -> [f]
      [f, ..] -> [f]
    }
  }
  case list_of_list {
    [] -> []
    [[], ..xss] -> transpose(xss)
    rows -> {
      let firsts =
        rows
        |> map(take_first)
        |> concat
      let rest = transpose(map(rows, drop(_, 1)))
      [firsts, ..rest]
    }
  }
}
fn do_shuffle_pair_unwrap(list: List(#(Float, a)), acc: List(a)) -> List(a) {
  case list {
    [] -> acc
    [elem_pair, ..enumerable] -> {
      do_shuffle_pair_unwrap(enumerable, [elem_pair.1, ..acc])
    }
  }
}
fn do_shuffle_by_pair_indexes(
  list_of_pairs: List(#(Float, a)),
) -> List(#(Float, a)) {
  sort(list_of_pairs, fn(a_pair: #(Float, a), b_pair: #(Float, a)) -> Order {
    float.compare(a_pair.0, b_pair.0)
  })
}
pub fn shuffle(list: List(a)) -> List(a) {
  list
  |> fold(from: [], with: fn(acc, a) { [#(float.random(0.0, 1.0), a), ..acc] })
  |> do_shuffle_by_pair_indexes()
  |> do_shuffle_pair_unwrap([])
}`,
  "gleam/io": `import gleam/string
pub fn print(string: String) -> Nil {
  do_print(string)
}
@external(javascript, "../gleam_stdlib.mjs", "print")
fn do_print(string string: String) -> Nil
pub fn print_error(string: String) -> Nil {
  do_print_error(string)
}
@external(javascript, "../gleam_stdlib.mjs", "print_error")
fn do_print_error(string string: String) -> Nil
pub fn println(string: String) -> Nil {
  do_println(string)
}
@external(javascript, "../gleam_stdlib.mjs", "console_log")
fn do_println(string string: String) -> Nil
pub fn println_error(string: String) -> Nil {
  do_println_error(string)
}
@external(javascript, "../gleam_stdlib.mjs", "console_error")
fn do_println_error(string string: String) -> Nil
pub fn debug(term: anything) -> anything {
  term
  |> string.inspect
  |> do_debug_println
  term
}
@external(javascript, "../gleam_stdlib.mjs", "print_debug")
fn do_debug_println(string string: String) -> Nil`,
  "gleam/int": `import gleam/float
import gleam/order.{type Order}
pub fn absolute_value(x: Int) -> Int {
  case x >= 0 {
    True -> x
    False -> x * -1
  }
}
pub fn power(base: Int, of exponent: Float) -> Result(Float, Nil) {
  base
  |> to_float()
  |> float.power(exponent)
}
pub fn square_root(x: Int) -> Result(Float, Nil) {
  x
  |> to_float()
  |> float.square_root()
}
pub fn parse(string: String) -> Result(Int, Nil) {
  do_parse(string)
}
@external(javascript, "../gleam_stdlib.mjs", "parse_int")
fn do_parse(a: String) -> Result(Int, Nil)
pub fn base_parse(string: String, base: Int) -> Result(Int, Nil) {
  case base >= 2 && base <= 36 {
    True -> do_base_parse(string, base)
    False -> Error(Nil)
  }
}
@external(javascript, "../gleam_stdlib.mjs", "int_from_base_string")
fn do_base_parse(a: String, b: Int) -> Result(Int, Nil)
pub fn to_string(x: Int) {
  do_to_string(x)
}
@external(javascript, "../gleam_stdlib.mjs", "to_string")
fn do_to_string(a: Int) -> String
pub type InvalidBase {
  InvalidBase
}
pub fn to_base_string(x: Int, base: Int) -> Result(String, InvalidBase) {
  case base >= 2 && base <= 36 {
    True -> Ok(do_to_base_string(x, base))
    False -> Error(InvalidBase)
  }
}
@external(javascript, "../gleam_stdlib.mjs", "int_to_base_string")
fn do_to_base_string(a: Int, b: Int) -> String
pub fn to_base2(x: Int) -> String {
  do_to_base_string(x, 2)
}
pub fn to_base8(x: Int) -> String {
  do_to_base_string(x, 8)
}
pub fn to_base16(x: Int) -> String {
  do_to_base_string(x, 16)
}
pub fn to_base36(x: Int) -> String {
  do_to_base_string(x, 36)
}
pub fn to_float(x: Int) -> Float {
  do_to_float(x)
}
@external(javascript, "../gleam_stdlib.mjs", "identity")
fn do_to_float(a: Int) -> Float
pub fn clamp(x: Int, min min_bound: Int, max max_bound: Int) -> Int {
  x
  |> min(max_bound)
  |> max(min_bound)
}
pub fn compare(a: Int, with b: Int) -> Order {
  case a == b {
    True -> order.Eq
    False ->
      case a < b {
        True -> order.Lt
        False -> order.Gt
      }
  }
}
pub fn min(a: Int, b: Int) -> Int {
  case a < b {
    True -> a
    False -> b
  }
}
pub fn max(a: Int, b: Int) -> Int {
  case a > b {
    True -> a
    False -> b
  }
}
pub fn is_even(x: Int) -> Bool {
  x % 2 == 0
}
pub fn is_odd(x: Int) -> Bool {
  x % 2 != 0
}
pub fn negate(x: Int) -> Int {
  -1 * x
}
pub fn sum(numbers: List(Int)) -> Int {
  numbers
  |> do_sum(0)
}
fn do_sum(numbers: List(Int), initial: Int) -> Int {
  case numbers {
    [] -> initial
    [x, ..rest] -> do_sum(rest, x + initial)
  }
}
pub fn product(numbers: List(Int)) -> Int {
  case numbers {
    [] -> 1
    _ -> do_product(numbers, 1)
  }
}
fn do_product(numbers: List(Int), initial: Int) -> Int {
  case numbers {
    [] -> initial
    [x, ..rest] -> do_product(rest, x * initial)
  }
}
pub fn digits(x: Int, base: Int) -> Result(List(Int), InvalidBase) {
  case base < 2 {
    True -> Error(InvalidBase)
    False -> Ok(do_digits(x, base, []))
  }
}
fn do_digits(x: Int, base: Int, acc: List(Int)) -> List(Int) {
  case absolute_value(x) < base {
    True -> [x, ..acc]
    False -> do_digits(x / base, base, [x % base, ..acc])
  }
}
pub fn undigits(numbers: List(Int), base: Int) -> Result(Int, InvalidBase) {
  case base < 2 {
    True -> Error(InvalidBase)
    False -> do_undigits(numbers, base, 0)
  }
}
fn do_undigits(
  numbers: List(Int),
  base: Int,
  acc: Int,
) -> Result(Int, InvalidBase) {
  case numbers {
    [] -> Ok(acc)
    [digit, ..] if digit >= base -> Error(InvalidBase)
    [digit, ..rest] -> do_undigits(rest, base, acc * base + digit)
  }
}
pub fn random(min: Int, max: Int) -> Int {
  float.random(to_float(min), to_float(max))
  |> float.floor()
  |> float.round()
}
pub fn divide(dividend: Int, by divisor: Int) -> Result(Int, Nil) {
  case divisor {
    0 -> Error(Nil)
    divisor -> Ok(dividend / divisor)
  }
}
pub fn remainder(dividend: Int, by divisor: Int) -> Result(Int, Nil) {
  case divisor {
    0 -> Error(Nil)
    divisor -> Ok(dividend % divisor)
  }
}
pub fn modulo(dividend: Int, by divisor: Int) -> Result(Int, Nil) {
  case divisor {
    0 -> Error(Nil)
    _ -> {
      let remainder = dividend % divisor
      case remainder * divisor < 0 {
        True -> Ok(remainder + divisor)
        False -> Ok(remainder)
      }
    }
  }
}
pub fn floor_divide(dividend: Int, by divisor: Int) -> Result(Int, Nil) {
  case divisor {
    0 -> Error(Nil)
    divisor ->
      case dividend * divisor < 0 && dividend % divisor != 0 {
        True -> Ok(dividend / divisor - 1)
        False -> Ok(dividend / divisor)
      }
  }
}
pub fn add(a: Int, b: Int) -> Int {
  a + b
}
pub fn multiply(a: Int, b: Int) -> Int {
  a * b
}
pub fn subtract(a: Int, b: Int) -> Int {
  a - b
}
@external(javascript, "../gleam_stdlib.mjs", "bitwise_and")
pub fn bitwise_and(x: Int, y: Int) -> Int
@external(javascript, "../gleam_stdlib.mjs", "bitwise_not")
pub fn bitwise_not(x: Int) -> Int
@external(javascript, "../gleam_stdlib.mjs", "bitwise_or")
pub fn bitwise_or(x: Int, y: Int) -> Int
@external(javascript, "../gleam_stdlib.mjs", "bitwise_exclusive_or")
pub fn bitwise_exclusive_or(x: Int, y: Int) -> Int
@external(javascript, "../gleam_stdlib.mjs", "bitwise_shift_left")
pub fn bitwise_shift_left(x: Int, y: Int) -> Int
@external(javascript, "../gleam_stdlib.mjs", "bitwise_shift_right")
pub fn bitwise_shift_right(x: Int, y: Int) -> Int`,
  "gleam/queue": `import gleam/list
pub opaque type Queue(element) {
  Queue(in: List(element), out: List(element))
}
pub fn new() -> Queue(a) {
  Queue(in: [], out: [])
}
pub fn from_list(list: List(a)) -> Queue(a) {
  Queue(in: [], out: list)
}
pub fn to_list(queue: Queue(a)) -> List(a) {
  queue.out
  |> list.append(list.reverse(queue.in))
}
pub fn is_empty(queue: Queue(a)) -> Bool {
  queue.in == [] && queue.out == []
}
pub fn length(queue: Queue(a)) -> Int {
  list.length(queue.in) + list.length(queue.out)
}
pub fn push_back(onto queue: Queue(a), this item: a) -> Queue(a) {
  Queue(in: [item, ..queue.in], out: queue.out)
}
pub fn push_front(onto queue: Queue(a), this item: a) -> Queue(a) {
  Queue(in: queue.in, out: [item, ..queue.out])
}
pub fn pop_back(from queue: Queue(a)) -> Result(#(a, Queue(a)), Nil) {
  case queue {
    Queue(in: [], out: []) -> Error(Nil)
    Queue(in: [], out: out) -> pop_back(Queue(in: list.reverse(out), out: []))
    Queue(in: [first, ..rest], out: out) -> {
      let queue = Queue(in: rest, out: out)
      Ok(#(first, queue))
    }
  }
}
pub fn pop_front(from queue: Queue(a)) -> Result(#(a, Queue(a)), Nil) {
  case queue {
    Queue(in: [], out: []) -> Error(Nil)
    Queue(in: in, out: []) -> pop_front(Queue(in: [], out: list.reverse(in)))
    Queue(in: in, out: [first, ..rest]) -> {
      let queue = Queue(in: in, out: rest)
      Ok(#(first, queue))
    }
  }
}
pub fn reverse(queue: Queue(a)) -> Queue(a) {
  Queue(in: queue.out, out: queue.in)
}
fn check_equal(
  xs: List(t),
  x_tail: List(t),
  ys: List(t),
  y_tail: List(t),
  eq: fn(t, t) -> Bool,
) -> Bool {
  case xs, x_tail, ys, y_tail {
    [], [], [], [] -> True
    [x, ..xs], _, [y, ..ys], _ ->
      case eq(x, y) {
        False -> False
        True -> check_equal(xs, x_tail, ys, y_tail, eq)
      }
    [], [_, ..], _, _ -> check_equal(list.reverse(x_tail), [], ys, y_tail, eq)
    _, _, [], [_, ..] -> check_equal(xs, x_tail, list.reverse(y_tail), [], eq)
    _, _, _, _ -> False
  }
}
pub fn is_logically_equal(
  a: Queue(t),
  to b: Queue(t),
  checking element_is_equal: fn(t, t) -> Bool,
) -> Bool {
  check_equal(a.out, a.in, b.out, b.in, element_is_equal)
}
pub fn is_equal(a: Queue(t), to b: Queue(t)) -> Bool {
  check_equal(a.out, a.in, b.out, b.in, fn(a, b) { a == b })
}`,
  "gleam/string": `import gleam/iterator.{type Iterator}
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/order
import gleam/string_builder.{type StringBuilder}
pub fn is_empty(str: String) -> Bool {
  str == ""
}
pub fn length(string: String) -> Int {
  do_length(string)
}
@external(javascript, "../gleam_stdlib.mjs", "string_length")
fn do_length(a: String) -> Int
pub fn reverse(string: String) -> String {
  do_reverse(string)
}
@target(erlang)
fn do_reverse(string: String) -> String {
  string
  |> string_builder.from_string
  |> string_builder.reverse
  |> string_builder.to_string
}
@target(javascript)
fn do_reverse(string: String) -> String {
  string
  |> to_graphemes
  |> list.reverse
  |> concat
}
pub fn replace(
  in string: String,
  each pattern: String,
  with substitute: String,
) -> String {
  string
  |> string_builder.from_string
  |> string_builder.replace(each: pattern, with: substitute)
  |> string_builder.to_string
}
pub fn lowercase(string: String) -> String {
  do_lowercase(string)
}
@external(javascript, "../gleam_stdlib.mjs", "lowercase")
fn do_lowercase(a: String) -> String
pub fn uppercase(string: String) -> String {
  do_uppercase(string)
}
@external(javascript, "../gleam_stdlib.mjs", "uppercase")
fn do_uppercase(a: String) -> String
pub fn compare(a: String, b: String) -> order.Order {
  case a == b {
    True -> order.Eq
    _ ->
      case less_than(a, b) {
        True -> order.Lt
        _ -> order.Gt
      }
  }
}
@external(javascript, "../gleam_stdlib.mjs", "less_than")
fn less_than(a: String, b: String) -> Bool
pub fn slice(from string: String, at_index idx: Int, length len: Int) -> String {
  case len < 0 {
    True -> ""
    False ->
      case idx < 0 {
        True -> {
          let translated_idx = length(string) + idx
          case translated_idx < 0 {
            True -> ""
            False -> do_slice(string, translated_idx, len)
          }
        }
        False -> do_slice(string, idx, len)
      }
  }
}
@target(erlang)
fn do_slice(a: String, b: Int, c: Int) -> String
@target(javascript)
fn do_slice(string: String, idx: Int, len: Int) -> String {
  string
  |> to_graphemes
  |> list.drop(idx)
  |> list.take(len)
  |> concat
}
@external(javascript, "../gleam_stdlib.mjs", "crop_string")
pub fn crop(from string: String, before substring: String) -> String
pub fn drop_left(from string: String, up_to num_graphemes: Int) -> String {
  case num_graphemes < 0 {
    True -> string
    False -> slice(string, num_graphemes, length(string) - num_graphemes)
  }
}
pub fn drop_right(from string: String, up_to num_graphemes: Int) -> String {
  case num_graphemes < 0 {
    True -> string
    False -> slice(string, 0, length(string) - num_graphemes)
  }
}
@external(javascript, "../gleam_stdlib.mjs", "contains_string")
pub fn contains(does haystack: String, contain needle: String) -> Bool
pub fn starts_with(string: String, prefix: String) -> Bool {
  do_starts_with(string, prefix)
}
@external(javascript, "../gleam_stdlib.mjs", "starts_with")
fn do_starts_with(a: String, b: String) -> Bool
pub fn ends_with(string: String, suffix: String) -> Bool {
  do_ends_with(string, suffix)
}
@external(javascript, "../gleam_stdlib.mjs", "ends_with")
fn do_ends_with(a: String, b: String) -> Bool
pub fn split(x: String, on substring: String) -> List(String) {
  case substring {
    "" -> to_graphemes(x)
    _ ->
      x
      |> string_builder.from_string
      |> string_builder.split(on: substring)
      |> list.map(with: string_builder.to_string)
  }
}
pub fn split_once(
  x: String,
  on substring: String,
) -> Result(#(String, String), Nil) {
  do_split_once(x, substring)
}
@target(erlang)
fn erl_split(a: String, b: String) -> List(String)
@target(erlang)
fn do_split_once(x: String, substring: String) -> Result(#(String, String), Nil) {
  case erl_split(x, substring) {
    [first, rest] -> Ok(#(first, rest))
    _ -> Error(Nil)
  }
}
@target(javascript)
@external(javascript, "../gleam_stdlib.mjs", "split_once")
fn do_split_once(
  x x: String,
  substring substring: String,
) -> Result(#(String, String), Nil)
pub fn append(to first: String, suffix second: String) -> String {
  first
  |> string_builder.from_string
  |> string_builder.append(second)
  |> string_builder.to_string
}
pub fn concat(strings: List(String)) -> String {
  strings
  |> string_builder.from_strings
  |> string_builder.to_string
}
pub fn repeat(string: String, times times: Int) -> String {
  iterator.repeat(string)
  |> iterator.take(times)
  |> iterator.to_list
  |> concat
}
pub fn join(strings: List(String), with separator: String) -> String {
  do_join(strings, separator)
}
@target(erlang)
fn do_join(strings: List(String), separator: String) -> String {
  strings
  |> list.intersperse(with: separator)
  |> concat
}
@target(javascript)
@external(javascript, "../gleam_stdlib.mjs", "join")
fn do_join(strings strings: List(String), string string: String) -> String
pub fn pad_left(string: String, to desired_length: Int, with pad_string: String) {
  let current_length = length(string)
  let to_pad_length = desired_length - current_length
  padding(to_pad_length, pad_string)
  |> iterator.append(iterator.single(string))
  |> iterator.to_list
  |> concat
}
pub fn pad_right(
  string: String,
  to desired_length: Int,
  with pad_string: String,
) {
  let current_length = length(string)
  let to_pad_length = desired_length - current_length
  iterator.single(string)
  |> iterator.append(padding(to_pad_length, pad_string))
  |> iterator.to_list
  |> concat
}
fn padding(size: Int, pad_string: String) -> Iterator(String) {
  let pad_length = length(pad_string)
  let num_pads = size / pad_length
  let extra = size % pad_length
  iterator.repeat(pad_string)
  |> iterator.take(num_pads)
  |> iterator.append(iterator.single(slice(pad_string, 0, extra)))
}
pub fn trim(string: String) -> String {
  do_trim(string)
}
@target(erlang)
fn do_trim(string: String) -> String {
  erl_trim(string, Both)
}
@target(erlang)
type Direction {
  Leading
  Trailing
  Both
}
@target(erlang)
fn erl_trim(a: String, b: Direction) -> String
@target(javascript)
@external(javascript, "../gleam_stdlib.mjs", "trim")
fn do_trim(string string: String) -> String
pub fn trim_left(string: String) -> String {
  do_trim_left(string)
}
@target(erlang)
fn do_trim_left(string: String) -> String {
  erl_trim(string, Leading)
}
@target(javascript)
@external(javascript, "../gleam_stdlib.mjs", "trim_left")
fn do_trim_left(string string: String) -> String
pub fn trim_right(string: String) -> String {
  do_trim_right(string)
}
@target(erlang)
fn do_trim_right(string: String) -> String {
  erl_trim(string, Trailing)
}
@target(javascript)
@external(javascript, "../gleam_stdlib.mjs", "trim_right")
fn do_trim_right(string string: String) -> String
pub fn pop_grapheme(string: String) -> Result(#(String, String), Nil) {
  do_pop_grapheme(string)
}
@external(javascript, "../gleam_stdlib.mjs", "pop_grapheme")
fn do_pop_grapheme(string string: String) -> Result(#(String, String), Nil)
@external(javascript, "../gleam_stdlib.mjs", "graphemes")
pub fn to_graphemes(string: String) -> List(String) {
  do_to_graphemes(string, [])
  |> list.reverse
}
fn do_to_graphemes(string: String, acc: List(String)) -> List(String) {
  case pop_grapheme(string) {
    Ok(#(grapheme, rest)) -> do_to_graphemes(rest, [grapheme, ..acc])
    _ -> acc
  }
}
@external(javascript, "../gleam_stdlib.mjs", "codepoint")
fn unsafe_int_to_utf_codepoint(a: Int) -> UtfCodepoint
pub fn to_utf_codepoints(string: String) -> List(UtfCodepoint) {
  do_to_utf_codepoints(string)
}
@target(erlang)
fn do_to_utf_codepoints(string: String) -> List(UtfCodepoint) {
  do_to_utf_codepoints_impl(<<string:utf8>>, [])
  |> list.reverse
}
@target(erlang)
fn do_to_utf_codepoints_impl(
  bit_array: BitArray,
  acc: List(UtfCodepoint),
) -> List(UtfCodepoint) {
  case bit_array {
    <<first:utf8_codepoint, rest:bytes>> ->
      do_to_utf_codepoints_impl(rest, [first, ..acc])
    _ -> acc
  }
}
@target(javascript)
fn do_to_utf_codepoints(string: String) -> List(UtfCodepoint) {
  string
  |> string_to_codepoint_integer_list
  |> list.map(unsafe_int_to_utf_codepoint)
}
@target(javascript)
@external(javascript, "../gleam_stdlib.mjs", "string_to_codepoint_integer_list")
fn string_to_codepoint_integer_list(a: String) -> List(Int)
@external(javascript, "../gleam_stdlib.mjs", "utf_codepoint_list_to_string")
pub fn from_utf_codepoints(utf_codepoints: List(UtfCodepoint)) -> String
pub fn utf_codepoint(value: Int) -> Result(UtfCodepoint, Nil) {
  case value {
    i if i > 1_114_111 -> Error(Nil)
    65_534 | 65_535 -> Error(Nil)
    i if i >= 55_296 && i <= 57_343 -> Error(Nil)
    i -> Ok(unsafe_int_to_utf_codepoint(i))
  }
}
pub fn utf_codepoint_to_int(cp: UtfCodepoint) -> Int {
  do_utf_codepoint_to_int(cp)
}
@external(javascript, "../gleam_stdlib.mjs", "utf_codepoint_to_int")
fn do_utf_codepoint_to_int(cp cp: UtfCodepoint) -> Int
pub fn to_option(s: String) -> Option(String) {
  case s {
    "" -> None
    _ -> Some(s)
  }
}
pub fn first(s: String) -> Result(String, Nil) {
  case pop_grapheme(s) {
    Ok(#(first, _)) -> Ok(first)
    Error(e) -> Error(e)
  }
}
pub fn last(s: String) -> Result(String, Nil) {
  case pop_grapheme(s) {
    Ok(#(first, "")) -> Ok(first)
    Ok(#(_, rest)) -> Ok(slice(rest, -1, 1))
    Error(e) -> Error(e)
  }
}
pub fn capitalise(s: String) -> String {
  case pop_grapheme(s) {
    Ok(#(first, rest)) -> append(to: uppercase(first), suffix: lowercase(rest))
    _ -> ""
  }
}
pub fn inspect(term: anything) -> String {
  do_inspect(term)
  |> string_builder.to_string
}
@external(javascript, "../gleam_stdlib.mjs", "inspect")
fn do_inspect(term term: anything) -> StringBuilder
@external(javascript, "../gleam_stdlib.mjs", "byte_size")
pub fn byte_size(string: String) -> Int`,
  "gleam/bytes_builder": `import gleam/string_builder.{type StringBuilder}
import gleam/list
import gleam/bit_array
pub opaque type BytesBuilder {
  Bytes(BitArray)
  Text(StringBuilder)
  Many(List(BytesBuilder))
}
pub fn new() -> BytesBuilder {
  concat([])
}
pub fn prepend(to second: BytesBuilder, prefix first: BitArray) -> BytesBuilder {
  append_builder(from_bit_array(first), second)
}
pub fn append(to first: BytesBuilder, suffix second: BitArray) -> BytesBuilder {
  append_builder(first, from_bit_array(second))
}
pub fn prepend_builder(
  to second: BytesBuilder,
  prefix first: BytesBuilder,
) -> BytesBuilder {
  append_builder(first, second)
}
pub fn append_builder(
  to first: BytesBuilder,
  suffix second: BytesBuilder,
) -> BytesBuilder {
  case second {
    Many(builders) -> Many([first, ..builders])
    _ -> Many([first, second])
  }
}
pub fn prepend_string(
  to second: BytesBuilder,
  prefix first: String,
) -> BytesBuilder {
  append_builder(from_string(first), second)
}
pub fn append_string(
  to first: BytesBuilder,
  suffix second: String,
) -> BytesBuilder {
  append_builder(first, from_string(second))
}
pub fn concat(builders: List(BytesBuilder)) -> BytesBuilder {
  Many(builders)
}
pub fn concat_bit_arrays(bits: List(BitArray)) -> BytesBuilder {
  bits
  |> list.map(fn(b) { from_bit_array(b) })
  |> concat()
}
pub fn from_string(string: String) -> BytesBuilder {
  Text(string_builder.from_string(string))
}
pub fn from_string_builder(builder: StringBuilder) -> BytesBuilder {
  Text(builder)
}
pub fn from_bit_array(bits: BitArray) -> BytesBuilder {
  Bytes(bits)
}
pub fn to_bit_array(builder: BytesBuilder) -> BitArray {
  [[builder]]
  |> to_list([])
  |> list.reverse
  |> bit_array.concat
}
fn to_list(
  stack: List(List(BytesBuilder)),
  acc: List(BitArray),
) -> List(BitArray) {
  case stack {
    [] -> acc
    [[], ..remaining_stack] -> to_list(remaining_stack, acc)
    [[Bytes(bits), ..rest], ..remaining_stack] ->
      to_list([rest, ..remaining_stack], [bits, ..acc])
    [[Text(builder), ..rest], ..remaining_stack] -> {
      let bits = bit_array.from_string(string_builder.to_string(builder))
      to_list([rest, ..remaining_stack], [bits, ..acc])
    }
    [[Many(builders), ..rest], ..remaining_stack] ->
      to_list([builders, rest, ..remaining_stack], acc)
  }
}
pub fn byte_size(builder: BytesBuilder) -> Int {
  [[builder]]
  |> to_list([])
  |> list.fold(0, fn(acc, builder) { bit_array.byte_size(builder) + acc })
}`,
  "gleam/regex": `import gleam/option.{type Option}
pub type Regex
pub type Match {
  Match(
    content: String,
    submatches: List(Option(String)),
  )
}
pub type CompileError {
  CompileError(
    error: String,
    byte_index: Int,
  )
}
pub type Options {
  Options(case_insensitive: Bool, multi_line: Bool)
}
pub fn compile(
  pattern: String,
  with options: Options,
) -> Result(Regex, CompileError) {
  do_compile(pattern, options)
}
@external(javascript, "../gleam_stdlib.mjs", "compile_regex")
fn do_compile(a: String, with with: Options) -> Result(Regex, CompileError)
pub fn from_string(pattern: String) -> Result(Regex, CompileError) {
  compile(pattern, Options(case_insensitive: False, multi_line: False))
}
pub fn check(with regex: Regex, content content: String) -> Bool {
  do_check(regex, content)
}
@external(javascript, "../gleam_stdlib.mjs", "regex_check")
fn do_check(a: Regex, b: String) -> Bool
pub fn split(with regex: Regex, content string: String) -> List(String) {
  do_split(regex, string)
}
@target(erlang)
fn do_split(a: Regex, b: String) -> List(String)
@target(javascript)
fn do_split(regex, string) -> List(String) {
  js_split(string, regex)
}
@target(javascript)
@external(javascript, "../gleam_stdlib.mjs", "split")
fn js_split(a: String, b: Regex) -> List(String)
pub fn scan(with regex: Regex, content string: String) -> List(Match) {
  do_scan(regex, string)
}
@external(javascript, "../gleam_stdlib.mjs", "regex_scan")
fn do_scan(a: Regex, b: String) -> List(Match)`,
  "gleam/string_builder": `import gleam/list
pub type StringBuilder
pub fn new() -> StringBuilder {
  do_from_strings([])
}
pub fn prepend(
  to builder: StringBuilder,
  prefix prefix: String,
) -> StringBuilder {
  append_builder(from_string(prefix), builder)
}
pub fn append(to builder: StringBuilder, suffix second: String) -> StringBuilder {
  append_builder(builder, from_string(second))
}
pub fn prepend_builder(
  to builder: StringBuilder,
  prefix prefix: StringBuilder,
) -> StringBuilder {
  do_append(prefix, builder)
}
pub fn append_builder(
  to builder: StringBuilder,
  suffix suffix: StringBuilder,
) -> StringBuilder {
  do_append(builder, suffix)
}
@external(javascript, "../gleam_stdlib.mjs", "add")
fn do_append(a: StringBuilder, b: StringBuilder) -> StringBuilder
pub fn from_strings(strings: List(String)) -> StringBuilder {
  do_from_strings(strings)
}
@external(javascript, "../gleam_stdlib.mjs", "concat")
fn do_from_strings(a: List(String)) -> StringBuilder
pub fn concat(builders: List(StringBuilder)) -> StringBuilder {
  do_concat(builders)
}
@external(javascript, "../gleam_stdlib.mjs", "concat")
fn do_concat(a: List(StringBuilder)) -> StringBuilder
pub fn from_string(string: String) -> StringBuilder {
  do_from_string(string)
}
@external(javascript, "../gleam_stdlib.mjs", "identity")
fn do_from_string(a: String) -> StringBuilder
pub fn to_string(builder: StringBuilder) -> String {
  do_to_string(builder)
}
@external(javascript, "../gleam_stdlib.mjs", "identity")
fn do_to_string(a: StringBuilder) -> String
pub fn byte_size(builder: StringBuilder) -> Int {
  do_byte_size(builder)
}
@external(javascript, "../gleam_stdlib.mjs", "length")
fn do_byte_size(a: StringBuilder) -> Int
pub fn join(builders: List(StringBuilder), with sep: String) -> StringBuilder {
  builders
  |> list.intersperse(from_string(sep))
  |> concat
}
pub fn lowercase(builder: StringBuilder) -> StringBuilder {
  do_lowercase(builder)
}
@external(javascript, "../gleam_stdlib.mjs", "lowercase")
fn do_lowercase(a: StringBuilder) -> StringBuilder
pub fn uppercase(builder: StringBuilder) -> StringBuilder {
  do_uppercase(builder)
}
@external(javascript, "../gleam_stdlib.mjs", "uppercase")
fn do_uppercase(a: StringBuilder) -> StringBuilder
pub fn reverse(builder: StringBuilder) -> StringBuilder {
  do_reverse(builder)
}
@target(erlang)
fn do_reverse(a: StringBuilder) -> StringBuilder
@target(javascript)
fn do_reverse(builder: StringBuilder) -> StringBuilder {
  builder
  |> to_string
  |> do_to_graphemes
  |> list.reverse
  |> from_strings
}
@target(javascript)
@external(javascript, "../gleam_stdlib.mjs", "graphemes")
fn do_to_graphemes(string string: String) -> List(String)
pub fn split(iodata: StringBuilder, on pattern: String) -> List(StringBuilder) {
  do_split(iodata, pattern)
}
@target(erlang)
type Direction {
  All
}
@target(erlang)
fn erl_split(a: StringBuilder, b: String, c: Direction) -> List(StringBuilder)
@target(erlang)
fn do_split(iodata: StringBuilder, pattern: String) -> List(StringBuilder) {
  erl_split(iodata, pattern, All)
}
@target(javascript)
@external(javascript, "../gleam_stdlib.mjs", "split")
fn do_split(
  builder builder: StringBuilder,
  pattern pattern: String,
) -> List(StringBuilder)
pub fn replace(
  in builder: StringBuilder,
  each pattern: String,
  with substitute: String,
) -> StringBuilder {
  do_replace(builder, pattern, substitute)
}
@target(erlang)
fn do_replace(
  iodata: StringBuilder,
  pattern: String,
  substitute: String,
) -> StringBuilder {
  erl_replace(iodata, pattern, substitute, All)
}
@target(erlang)
fn erl_replace(
  a: StringBuilder,
  b: String,
  c: String,
  d: Direction,
) -> StringBuilder
@target(javascript)
@external(javascript, "../gleam_stdlib.mjs", "string_replace")
fn do_replace(a: StringBuilder, b: String, c: String) -> StringBuilder
pub fn is_equal(a: StringBuilder, b: StringBuilder) -> Bool {
  do_is_equal(a, b)
}
@external(javascript, "../gleam_stdlib.mjs", "equal")
fn do_is_equal(a: StringBuilder, b: StringBuilder) -> Bool
pub fn is_empty(builder: StringBuilder) -> Bool {
  do_is_empty(builder)
}
@target(erlang)
fn do_is_empty(a: StringBuilder) -> Bool
@target(javascript)
fn do_is_empty(builder: StringBuilder) -> Bool {
  from_string("") == builder
}`,
  "gleam/function": `pub fn compose(fun1: fn(a) -> b, fun2: fn(b) -> c) -> fn(a) -> c {
  fn(a) { fun2(fun1(a)) }
}
pub fn curry2(fun: fn(a, b) -> value) {
  fn(a) { fn(b) { fun(a, b) } }
}
pub fn curry3(fun: fn(a, b, c) -> value) {
  fn(a) { fn(b) { fn(c) { fun(a, b, c) } } }
}
pub fn curry4(fun: fn(a, b, c, d) -> value) {
  fn(a) { fn(b) { fn(c) { fn(d) { fun(a, b, c, d) } } } }
}
pub fn curry5(fun: fn(a, b, c, d, e) -> value) {
  fn(a) { fn(b) { fn(c) { fn(d) { fn(e) { fun(a, b, c, d, e) } } } } }
}
pub fn curry6(fun: fn(a, b, c, d, e, f) -> value) {
  fn(a) {
    fn(b) { fn(c) { fn(d) { fn(e) { fn(f) { fun(a, b, c, d, e, f) } } } } }
  }
}
pub fn flip(fun: fn(a, b) -> c) -> fn(b, a) -> c {
  fn(b, a) { fun(a, b) }
}
pub fn identity(x: a) -> a {
  x
}
pub fn constant(value: a) -> fn(b) -> a {
  fn(_) { value }
}
pub fn tap(arg: a, effect: fn(a) -> b) -> a {
  effect(arg)
  arg
}
pub fn apply1(fun: fn(a) -> value, arg1: a) -> value {
  fun(arg1)
}
pub fn apply2(fun: fn(a, b) -> value, arg1: a, arg2: b) -> value {
  fun(arg1, arg2)
}
pub fn apply3(fun: fn(a, b, c) -> value, arg1: a, arg2: b, arg3: c) -> value {
  fun(arg1, arg2, arg3)
}`,
  "gleam/bool": `import gleam/order.{type Order}
pub fn and(a: Bool, b: Bool) -> Bool {
  a && b
}
pub fn or(a: Bool, b: Bool) -> Bool {
  a || b
}
pub fn negate(bool: Bool) -> Bool {
  case bool {
    True -> False
    False -> True
  }
}
pub fn nor(a: Bool, b: Bool) -> Bool {
  case a, b {
    False, False -> True
    False, True -> False
    True, False -> False
    True, True -> False
  }
}
pub fn nand(a: Bool, b: Bool) -> Bool {
  case a, b {
    False, False -> True
    False, True -> True
    True, False -> True
    True, True -> False
  }
}
pub fn exclusive_or(a: Bool, b: Bool) -> Bool {
  case a, b {
    False, False -> False
    False, True -> True
    True, False -> True
    True, True -> False
  }
}
pub fn exclusive_nor(a: Bool, b: Bool) -> Bool {
  case a, b {
    False, False -> True
    False, True -> False
    True, False -> False
    True, True -> True
  }
}
pub fn compare(a: Bool, with b: Bool) -> Order {
  case a, b {
    True, True -> order.Eq
    True, False -> order.Gt
    False, False -> order.Eq
    False, True -> order.Lt
  }
}
pub fn max(a: Bool, b: Bool) -> Bool {
  case a {
    True -> True
    False -> b
  }
}
pub fn min(a: Bool, b: Bool) -> Bool {
  case a {
    False -> False
    True -> b
  }
}
pub fn to_int(bool: Bool) -> Int {
  case bool {
    False -> 0
    True -> 1
  }
}
pub fn to_string(bool: Bool) -> String {
  case bool {
    False -> "False"
    True -> "True"
  }
}
pub fn guard(
  when requirement: Bool,
  return consequence: t,
  otherwise alternative: fn() -> t,
) -> t {
  case requirement {
    True -> consequence
    False -> alternative()
  }
}
pub fn lazy_guard(
  when requirement: Bool,
  return consequence: fn() -> a,
  otherwise alternative: fn() -> a,
) -> a {
  case requirement {
    True -> consequence()
    False -> alternative()
  }
}`,
  "gleam/uri": `import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/string
import gleam/string_builder.{type StringBuilder}
@target(javascript)
import gleam/pair
@target(javascript)
import gleam/regex
@target(javascript)
import gleam/result
pub type Uri {
  Uri(
    scheme: Option(String),
    userinfo: Option(String),
    host: Option(String),
    port: Option(Int),
    path: String,
    query: Option(String),
    fragment: Option(String),
  )
}
pub fn parse(uri_string: String) -> Result(Uri, Nil) {
  do_parse(uri_string)
}
@target(erlang)
fn do_parse(a: String) -> Result(Uri, Nil)
@target(javascript)
fn do_parse(uri_string: String) -> Result(Uri, Nil) {
  let pattern =
    "^(([a-z][a-z0-9\\\\+\\\\-\\\\.]*):)?(//([^/?#]*))?([^?#]*)(\\\\?([^#]*))?(#.*)?"
  let matches =
    pattern
    |> regex_submatches(uri_string)
    |> pad_list(8)
  let #(scheme, authority, path, query, fragment) = case matches {
    [
      _scheme_with_colon,
      scheme,
      authority_with_slashes,
      _authority,
      path,
      query_with_question_mark,
      _query,
      fragment,
    ] -> #(
      scheme,
      authority_with_slashes,
      path,
      query_with_question_mark,
      fragment,
    )
    _ -> #(None, None, None, None, None)
  }
  let scheme = noneify_empty_string(scheme)
  let path = option.unwrap(path, "")
  let query = noneify_query(query)
  let #(userinfo, host, port) = split_authority(authority)
  let fragment =
    fragment
    |> option.to_result(Nil)
    |> result.try(string.pop_grapheme)
    |> result.map(pair.second)
    |> option.from_result
  let scheme =
    scheme
    |> noneify_empty_string
    |> option.map(string.lowercase)
  Ok(Uri(
    scheme: scheme,
    userinfo: userinfo,
    host: host,
    port: port,
    path: path,
    query: query,
    fragment: fragment,
  ))
}
@target(javascript)
fn regex_submatches(pattern: String, string: String) -> List(Option(String)) {
  pattern
  |> regex.compile(regex.Options(case_insensitive: True, multi_line: False))
  |> result.nil_error
  |> result.map(regex.scan(_, string))
  |> result.try(list.first)
  |> result.map(fn(m: regex.Match) { m.submatches })
  |> result.unwrap([])
}
@target(javascript)
fn noneify_query(x: Option(String)) -> Option(String) {
  case x {
    None -> None
    Some(x) ->
      case string.pop_grapheme(x) {
        Ok(#("?", query)) -> Some(query)
        _ -> None
      }
  }
}
@target(javascript)
fn noneify_empty_string(x: Option(String)) -> Option(String) {
  case x {
    Some("") | None -> None
    Some(_) -> x
  }
}
@target(javascript)
fn split_authority(
  authority: Option(String),
) -> #(Option(String), Option(String), Option(Int)) {
  case option.unwrap(authority, "") {
    "" -> #(None, None, None)
    "//" -> #(None, Some(""), None)
    authority -> {
      let matches =
        "^(//)?((.*)@)?(\\\\[[a-zA-Z0-9:.]*\\\\]|[^:]*)(:(\\\\d*))?"
        |> regex_submatches(authority)
        |> pad_list(6)
      case matches {
        [_, _, userinfo, host, _, port] -> {
          let userinfo = noneify_empty_string(userinfo)
          let host = noneify_empty_string(host)
          let port =
            port
            |> option.unwrap("")
            |> int.parse
            |> option.from_result
          #(userinfo, host, port)
        }
        _ -> #(None, None, None)
      }
    }
  }
}
@target(javascript)
fn pad_list(list: List(Option(a)), size: Int) -> List(Option(a)) {
  list
  |> list.append(list.repeat(None, extra_required(list, size)))
}
@target(javascript)
fn extra_required(list: List(a), remaining: Int) -> Int {
  case list {
    _ if remaining == 0 -> 0
    [] -> remaining
    [_, ..xs] -> extra_required(xs, remaining - 1)
  }
}
pub fn parse_query(query: String) -> Result(List(#(String, String)), Nil) {
  do_parse_query(query)
}
@external(javascript, "../gleam_stdlib.mjs", "parse_query")
fn do_parse_query(a: String) -> Result(List(#(String, String)), Nil)
pub fn query_to_string(query: List(#(String, String))) -> String {
  query
  |> list.map(query_pair)
  |> list.intersperse(string_builder.from_string("&"))
  |> string_builder.concat
  |> string_builder.to_string
}
fn query_pair(pair: #(String, String)) -> StringBuilder {
  string_builder.from_strings([
    percent_encode(pair.0),
    "=",
    percent_encode(pair.1),
  ])
}
pub fn percent_encode(value: String) -> String {
  do_percent_encode(value)
}
@external(javascript, "../gleam_stdlib.mjs", "percent_encode")
fn do_percent_encode(a: String) -> String
pub fn percent_decode(value: String) -> Result(String, Nil) {
  do_percent_decode(value)
}
@external(javascript, "../gleam_stdlib.mjs", "percent_decode")
fn do_percent_decode(a: String) -> Result(String, Nil)
fn do_remove_dot_segments(
  input: List(String),
  accumulator: List(String),
) -> List(String) {
  case input {
    [] -> list.reverse(accumulator)
    [segment, ..rest] -> {
      let accumulator = case segment, accumulator {
        "", accumulator -> accumulator
        ".", accumulator -> accumulator
        "..", [] -> []
        "..", [_, ..accumulator] -> accumulator
        segment, accumulator -> [segment, ..accumulator]
      }
      do_remove_dot_segments(rest, accumulator)
    }
  }
}
fn remove_dot_segments(input: List(String)) -> List(String) {
  do_remove_dot_segments(input, [])
}
pub fn path_segments(path: String) -> List(String) {
  remove_dot_segments(string.split(path, "/"))
}
pub fn to_string(uri: Uri) -> String {
  let parts = case uri.fragment {
    Some(fragment) -> ["#", fragment]
    _ -> []
  }
  let parts = case uri.query {
    Some(query) -> ["?", query, ..parts]
    _ -> parts
  }
  let parts = [uri.path, ..parts]
  let parts = case uri.host, string.starts_with(uri.path, "/") {
    Some(host), False if host != "" -> ["/", ..parts]
    _, _ -> parts
  }
  let parts = case uri.host, uri.port {
    Some(_), Some(port) -> [":", int.to_string(port), ..parts]
    _, _ -> parts
  }
  let parts = case uri.scheme, uri.userinfo, uri.host {
    Some(s), Some(u), Some(h) -> [s, "://", u, "@", h, ..parts]
    Some(s), None, Some(h) -> [s, "://", h, ..parts]
    Some(s), Some(_), None | Some(s), None, None -> [s, ":", ..parts]
    None, None, Some(h) -> ["//", h, ..parts]
    _, _, _ -> parts
  }
  string.concat(parts)
}
pub fn origin(uri: Uri) -> Result(String, Nil) {
  let Uri(scheme: scheme, host: host, port: port, ..) = uri
  case scheme {
    Some("https") if port == Some(443) -> {
      let origin = Uri(scheme, None, host, None, "", None, None)
      Ok(to_string(origin))
    }
    Some("http") if port == Some(80) -> {
      let origin = Uri(scheme, None, host, None, "", None, None)
      Ok(to_string(origin))
    }
    Some(s) if s == "http" || s == "https" -> {
      let origin = Uri(scheme, None, host, port, "", None, None)
      Ok(to_string(origin))
    }
    _ -> Error(Nil)
  }
}
fn drop_last(elements: List(a)) -> List(a) {
  list.take(from: elements, up_to: list.length(elements) - 1)
}
fn join_segments(segments: List(String)) -> String {
  string.join(["", ..segments], "/")
}
pub fn merge(base: Uri, relative: Uri) -> Result(Uri, Nil) {
  case base {
    Uri(scheme: Some(_), host: Some(_), ..) ->
      case relative {
        Uri(host: Some(_), ..) -> {
          let path =
            string.split(relative.path, "/")
            |> remove_dot_segments()
            |> join_segments()
          let resolved =
            Uri(
              option.or(relative.scheme, base.scheme),
              None,
              relative.host,
              option.or(relative.port, base.port),
              path,
              relative.query,
              relative.fragment,
            )
          Ok(resolved)
        }
        _ -> {
          let #(new_path, new_query) = case relative.path {
            "" -> #(base.path, option.or(relative.query, base.query))
            _ -> {
              let path_segments = case string.starts_with(relative.path, "/") {
                True -> string.split(relative.path, "/")
                False ->
                  string.split(base.path, "/")
                  |> drop_last()
                  |> list.append(string.split(relative.path, "/"))
              }
              let path =
                path_segments
                |> remove_dot_segments()
                |> join_segments()
              #(path, relative.query)
            }
          }
          let resolved =
            Uri(
              base.scheme,
              None,
              base.host,
              base.port,
              new_path,
              new_query,
              relative.fragment,
            )
          Ok(resolved)
        }
      }
    _ -> Error(Nil)
  }
}`,
  "gleam/float": `import gleam/order.{type Order}
pub fn parse(string: String) -> Result(Float, Nil) {
  do_parse(string)
}
@external(javascript, "../gleam_stdlib.mjs", "parse_float")
fn do_parse(a: String) -> Result(Float, Nil)
pub fn to_string(x: Float) -> String {
  do_to_string(x)
}
@external(javascript, "../gleam_stdlib.mjs", "float_to_string")
fn do_to_string(a: Float) -> String
pub fn clamp(x: Float, min min_bound: Float, max max_bound: Float) -> Float {
  x
  |> min(max_bound)
  |> max(min_bound)
}
pub fn compare(a: Float, with b: Float) -> Order {
  case a == b {
    True -> order.Eq
    False ->
      case a <. b {
        True -> order.Lt
        False -> order.Gt
      }
  }
}
pub fn loosely_compare(
  a: Float,
  with b: Float,
  tolerating tolerance: Float,
) -> Order {
  let difference = absolute_value(a -. b)
  case difference <=. tolerance {
    True -> order.Eq
    False -> compare(a, b)
  }
}
pub fn loosely_equals(
  a: Float,
  with b: Float,
  tolerating tolerance: Float,
) -> Bool {
  let difference = absolute_value(a -. b)
  difference <=. tolerance
}
pub fn min(a: Float, b: Float) -> Float {
  case a <. b {
    True -> a
    False -> b
  }
}
pub fn max(a: Float, b: Float) -> Float {
  case a >. b {
    True -> a
    False -> b
  }
}
pub fn ceiling(x: Float) -> Float {
  do_ceiling(x)
}
@external(javascript, "../gleam_stdlib.mjs", "ceiling")
fn do_ceiling(a: Float) -> Float
pub fn floor(x: Float) -> Float {
  do_floor(x)
}
@external(javascript, "../gleam_stdlib.mjs", "floor")
fn do_floor(a: Float) -> Float
pub fn round(x: Float) -> Int {
  do_round(x)
}
@target(erlang)
fn do_round(a: Float) -> Int
@target(javascript)
fn do_round(x: Float) -> Int {
  case x >=. 0.0 {
    True -> js_round(x)
    _ -> 0 - js_round(negate(x))
  }
}
@target(javascript)
@external(javascript, "../gleam_stdlib.mjs", "round")
fn js_round(a: Float) -> Int
pub fn truncate(x: Float) -> Int {
  do_truncate(x)
}
@external(javascript, "../gleam_stdlib.mjs", "truncate")
fn do_truncate(a: Float) -> Int
pub fn absolute_value(x: Float) -> Float {
  case x >=. 0.0 {
    True -> x
    _ -> 0.0 -. x
  }
}
pub fn power(base: Float, of exponent: Float) -> Result(Float, Nil) {
  let fractional: Bool = ceiling(exponent) -. exponent >. 0.0
  case base <. 0.0 && fractional || base == 0.0 && exponent <. 0.0 {
    True -> Error(Nil)
    False -> Ok(do_power(base, exponent))
  }
}
@external(javascript, "../gleam_stdlib.mjs", "power")
fn do_power(a: Float, b: Float) -> Float
pub fn square_root(x: Float) -> Result(Float, Nil) {
  power(x, 0.5)
}
pub fn negate(x: Float) -> Float {
  -1.0 *. x
}
pub fn sum(numbers: List(Float)) -> Float {
  numbers
  |> do_sum(0.0)
}
fn do_sum(numbers: List(Float), initial: Float) -> Float {
  case numbers {
    [] -> initial
    [x, ..rest] -> do_sum(rest, x +. initial)
  }
}
pub fn product(numbers: List(Float)) -> Float {
  case numbers {
    [] -> 1.0
    _ -> do_product(numbers, 1.0)
  }
}
fn do_product(numbers: List(Float), initial: Float) -> Float {
  case numbers {
    [] -> initial
    [x, ..rest] -> do_product(rest, x *. initial)
  }
}
pub fn random(min: Float, max: Float) -> Float {
  do_random_uniform() *. { max -. min } +. min
}
@external(javascript, "../gleam_stdlib.mjs", "random_uniform")
fn do_random_uniform() -> Float
pub fn divide(a: Float, by b: Float) -> Result(Float, Nil) {
  case b {
    0.0 -> Error(Nil)
    b -> Ok(a /. b)
  }
}
pub fn add(a: Float, b: Float) -> Float {
  a +. b
}
pub fn multiply(a: Float, b: Float) -> Float {
  a *. b
}
pub fn subtract(a: Float, b: Float) -> Float {
  a -. b
}`,
  "gleam/set": `import gleam/list
import gleam/dict.{type Dict}
import gleam/result
@target(erlang)
type Token =
  List(Nil)
@target(erlang)
const token = []
@target(javascript)
type Token =
  Nil
@target(javascript)
const token = Nil
pub opaque type Set(member) {
  Set(map: Dict(member, Token))
}
pub fn new() -> Set(member) {
  Set(dict.new())
}
pub fn size(set: Set(member)) -> Int {
  dict.size(set.map)
}
pub fn insert(into set: Set(member), this member: member) -> Set(member) {
  Set(map: dict.insert(set.map, member, token))
}
pub fn contains(in set: Set(member), this member: member) -> Bool {
  set.map
  |> dict.get(member)
  |> result.is_ok
}
pub fn delete(from set: Set(member), this member: member) -> Set(member) {
  Set(map: dict.delete(set.map, member))
}
pub fn to_list(set: Set(member)) -> List(member) {
  dict.keys(set.map)
}
pub fn from_list(members: List(member)) -> Set(member) {
  let map =
    list.fold(
      over: members,
      from: dict.new(),
      with: fn(m, k) { dict.insert(m, k, token) },
    )
  Set(map)
}
pub fn fold(
  over set: Set(member),
  from initial: acc,
  with reducer: fn(acc, member) -> acc,
) -> acc {
  dict.fold(over: set.map, from: initial, with: fn(a, k, _) { reducer(a, k) })
}
pub fn filter(
  in set: Set(member),
  keeping predicate: fn(member) -> Bool,
) -> Set(member) {
  Set(dict.filter(in: set.map, keeping: fn(m, _) { predicate(m) }))
}
pub fn drop(from set: Set(member), drop disallowed: List(member)) -> Set(member) {
  list.fold(over: disallowed, from: set, with: delete)
}
pub fn take(from set: Set(member), keeping desired: List(member)) -> Set(member) {
  Set(dict.take(from: set.map, keeping: desired))
}
fn order(first: Set(member), second: Set(member)) -> #(Set(member), Set(member)) {
  case dict.size(first.map) > dict.size(second.map) {
    True -> #(first, second)
    False -> #(second, first)
  }
}
pub fn union(of first: Set(member), and second: Set(member)) -> Set(member) {
  let #(larger, smaller) = order(first, second)
  fold(over: smaller, from: larger, with: insert)
}
pub fn intersection(
  of first: Set(member),
  and second: Set(member),
) -> Set(member) {
  let #(larger, smaller) = order(first, second)
  take(from: larger, keeping: to_list(smaller))
}`,
  "gleam/iterator": `import gleam/result
import gleam/int
import gleam/list
import gleam/dict.{type Dict}
import gleam/option.{type Option, None, Some}
import gleam/order
type Action(element) {
  Stop
  Continue(element, fn() -> Action(element))
}
pub opaque type Iterator(element) {
  Iterator(continuation: fn() -> Action(element))
}
pub type Step(element, accumulator) {
  Next(element: element, accumulator: accumulator)
  Done
}
fn stop() -> Action(element) {
  Stop
}
fn do_unfold(
  initial: acc,
  f: fn(acc) -> Step(element, acc),
) -> fn() -> Action(element) {
  fn() {
    case f(initial) {
      Next(x, acc) -> Continue(x, do_unfold(acc, f))
      Done -> Stop
    }
  }
}
pub fn unfold(
  from initial: acc,
  with f: fn(acc) -> Step(element, acc),
) -> Iterator(element) {
  initial
  |> do_unfold(f)
  |> Iterator
}
pub fn repeatedly(f: fn() -> element) -> Iterator(element) {
  unfold(Nil, fn(_) { Next(f(), Nil) })
}
pub fn repeat(x: element) -> Iterator(element) {
  repeatedly(fn() { x })
}
pub fn from_list(list: List(element)) -> Iterator(element) {
  let yield = fn(acc) {
    case acc {
      [] -> Done
      [head, ..tail] -> Next(head, tail)
    }
  }
  unfold(list, yield)
}
fn do_transform(
  continuation: fn() -> Action(a),
  state: acc,
  f: fn(acc, a) -> Step(b, acc),
) -> fn() -> Action(b) {
  fn() {
    case continuation() {
      Stop -> Stop
      Continue(el, next) ->
        case f(state, el) {
          Done -> Stop
          Next(yield, next_state) ->
            Continue(yield, do_transform(next, next_state, f))
        }
    }
  }
}
pub fn transform(
  over iterator: Iterator(a),
  from initial: acc,
  with f: fn(acc, a) -> Step(b, acc),
) -> Iterator(b) {
  do_transform(iterator.continuation, initial, f)
  |> Iterator
}
fn do_fold(
  continuation: fn() -> Action(e),
  f: fn(acc, e) -> acc,
  accumulator: acc,
) -> acc {
  case continuation() {
    Continue(elem, next) -> do_fold(next, f, f(accumulator, elem))
    Stop -> accumulator
  }
}
pub fn fold(
  over iterator: Iterator(e),
  from initial: acc,
  with f: fn(acc, e) -> acc,
) -> acc {
  iterator.continuation
  |> do_fold(f, initial)
}
pub fn run(iterator: Iterator(e)) -> Nil {
  fold(iterator, Nil, fn(_, _) { Nil })
}
pub fn to_list(iterator: Iterator(element)) -> List(element) {
  iterator
  |> fold([], fn(acc, e) { [e, ..acc] })
  |> list.reverse
}
pub fn step(iterator: Iterator(e)) -> Step(e, Iterator(e)) {
  case iterator.continuation() {
    Stop -> Done
    Continue(e, a) -> Next(e, Iterator(a))
  }
}
fn do_take(continuation: fn() -> Action(e), desired: Int) -> fn() -> Action(e) {
  fn() {
    case desired > 0 {
      False -> Stop
      True ->
        case continuation() {
          Stop -> Stop
          Continue(e, next) -> Continue(e, do_take(next, desired - 1))
        }
    }
  }
}
pub fn take(from iterator: Iterator(e), up_to desired: Int) -> Iterator(e) {
  iterator.continuation
  |> do_take(desired)
  |> Iterator
}
fn do_drop(continuation: fn() -> Action(e), desired: Int) -> Action(e) {
  case continuation() {
    Stop -> Stop
    Continue(e, next) ->
      case desired > 0 {
        True -> do_drop(next, desired - 1)
        False -> Continue(e, next)
      }
  }
}
pub fn drop(from iterator: Iterator(e), up_to desired: Int) -> Iterator(e) {
  fn() { do_drop(iterator.continuation, desired) }
  |> Iterator
}
fn do_map(continuation: fn() -> Action(a), f: fn(a) -> b) -> fn() -> Action(b) {
  fn() {
    case continuation() {
      Stop -> Stop
      Continue(e, continuation) -> Continue(f(e), do_map(continuation, f))
    }
  }
}
pub fn map(over iterator: Iterator(a), with f: fn(a) -> b) -> Iterator(b) {
  iterator.continuation
  |> do_map(f)
  |> Iterator
}
fn do_map2(
  continuation1: fn() -> Action(a),
  continuation2: fn() -> Action(b),
  with fun: fn(a, b) -> c,
) -> fn() -> Action(c) {
  fn() {
    case continuation1() {
      Stop -> Stop
      Continue(a, next_a) ->
        case continuation2() {
          Stop -> Stop
          Continue(b, next_b) ->
            Continue(fun(a, b), do_map2(next_a, next_b, fun))
        }
    }
  }
}
pub fn map2(
  iterator1: Iterator(a),
  iterator2: Iterator(b),
  with fun: fn(a, b) -> c,
) -> Iterator(c) {
  do_map2(iterator1.continuation, iterator2.continuation, fun)
  |> Iterator
}
fn do_append(first: fn() -> Action(a), second: fn() -> Action(a)) -> Action(a) {
  case first() {
    Continue(e, first) -> Continue(e, fn() { do_append(first, second) })
    Stop -> second()
  }
}
pub fn append(to first: Iterator(a), suffix second: Iterator(a)) -> Iterator(a) {
  fn() { do_append(first.continuation, second.continuation) }
  |> Iterator
}
fn do_flatten(flattened: fn() -> Action(Iterator(a))) -> Action(a) {
  case flattened() {
    Stop -> Stop
    Continue(it, next_iterator) ->
      do_append(it.continuation, fn() { do_flatten(next_iterator) })
  }
}
pub fn flatten(iterator: Iterator(Iterator(a))) -> Iterator(a) {
  fn() { do_flatten(iterator.continuation) }
  |> Iterator
}
pub fn concat(iterators: List(Iterator(a))) -> Iterator(a) {
  flatten(from_list(iterators))
}
pub fn flat_map(
  over iterator: Iterator(a),
  with f: fn(a) -> Iterator(b),
) -> Iterator(b) {
  iterator
  |> map(f)
  |> flatten
}
fn do_filter(
  continuation: fn() -> Action(e),
  predicate: fn(e) -> Bool,
) -> Action(e) {
  case continuation() {
    Stop -> Stop
    Continue(e, iterator) ->
      case predicate(e) {
        True -> Continue(e, fn() { do_filter(iterator, predicate) })
        False -> do_filter(iterator, predicate)
      }
  }
}
pub fn filter(
  iterator: Iterator(a),
  keeping predicate: fn(a) -> Bool,
) -> Iterator(a) {
  fn() { do_filter(iterator.continuation, predicate) }
  |> Iterator
}
pub fn cycle(iterator: Iterator(a)) -> Iterator(a) {
  repeat(iterator)
  |> flatten
}
pub fn range(from start: Int, to stop: Int) -> Iterator(Int) {
  case int.compare(start, stop) {
    order.Eq -> once(fn() { start })
    order.Gt ->
      unfold(
        from: start,
        with: fn(current) {
          case current < stop {
            False -> Next(current, current - 1)
            True -> Done
          }
        },
      )
    order.Lt ->
      unfold(
        from: start,
        with: fn(current) {
          case current > stop {
            False -> Next(current, current + 1)
            True -> Done
          }
        },
      )
  }
}
fn do_find(continuation: fn() -> Action(a), f: fn(a) -> Bool) -> Result(a, Nil) {
  case continuation() {
    Stop -> Error(Nil)
    Continue(e, next) ->
      case f(e) {
        True -> Ok(e)
        False -> do_find(next, f)
      }
  }
}
pub fn find(
  in haystack: Iterator(a),
  one_that is_desired: fn(a) -> Bool,
) -> Result(a, Nil) {
  haystack.continuation
  |> do_find(is_desired)
}
fn do_index(
  continuation: fn() -> Action(element),
  next: Int,
) -> fn() -> Action(#(Int, element)) {
  fn() {
    case continuation() {
      Stop -> Stop
      Continue(e, continuation) ->
        Continue(#(next, e), do_index(continuation, next + 1))
    }
  }
}
pub fn index(over iterator: Iterator(element)) -> Iterator(#(Int, element)) {
  iterator.continuation
  |> do_index(0)
  |> Iterator
}
pub fn iterate(
  from initial: element,
  with f: fn(element) -> element,
) -> Iterator(element) {
  unfold(initial, fn(element) { Next(element, f(element)) })
}
fn do_take_while(
  continuation: fn() -> Action(element),
  predicate: fn(element) -> Bool,
) -> fn() -> Action(element) {
  fn() {
    case continuation() {
      Stop -> Stop
      Continue(e, next) ->
        case predicate(e) {
          False -> Stop
          True -> Continue(e, do_take_while(next, predicate))
        }
    }
  }
}
pub fn take_while(
  in iterator: Iterator(element),
  satisfying predicate: fn(element) -> Bool,
) -> Iterator(element) {
  iterator.continuation
  |> do_take_while(predicate)
  |> Iterator
}
fn do_drop_while(
  continuation: fn() -> Action(element),
  predicate: fn(element) -> Bool,
) -> Action(element) {
  case continuation() {
    Stop -> Stop
    Continue(e, next) ->
      case predicate(e) {
        False -> Continue(e, next)
        True -> do_drop_while(next, predicate)
      }
  }
}
pub fn drop_while(
  in iterator: Iterator(element),
  satisfying predicate: fn(element) -> Bool,
) -> Iterator(element) {
  fn() { do_drop_while(iterator.continuation, predicate) }
  |> Iterator
}
fn do_scan(
  continuation: fn() -> Action(element),
  f: fn(acc, element) -> acc,
  accumulator: acc,
) -> fn() -> Action(acc) {
  fn() {
    case continuation() {
      Stop -> Stop
      Continue(el, next) -> {
        let accumulated = f(accumulator, el)
        Continue(accumulated, do_scan(next, f, accumulated))
      }
    }
  }
}
pub fn scan(
  over iterator: Iterator(element),
  from initial: acc,
  with f: fn(acc, element) -> acc,
) -> Iterator(acc) {
  iterator.continuation
  |> do_scan(f, initial)
  |> Iterator
}
fn do_zip(
  left: fn() -> Action(a),
  right: fn() -> Action(b),
) -> fn() -> Action(#(a, b)) {
  fn() {
    case left() {
      Stop -> Stop
      Continue(el_left, next_left) ->
        case right() {
          Stop -> Stop
          Continue(el_right, next_right) ->
            Continue(#(el_left, el_right), do_zip(next_left, next_right))
        }
    }
  }
}
pub fn zip(left: Iterator(a), right: Iterator(b)) -> Iterator(#(a, b)) {
  do_zip(left.continuation, right.continuation)
  |> Iterator
}
type Chunk(element, key) {
  AnotherBy(List(element), key, element, fn() -> Action(element))
  LastBy(List(element))
}
fn next_chunk(
  continuation: fn() -> Action(element),
  f: fn(element) -> key,
  previous_key: key,
  current_chunk: List(element),
) -> Chunk(element, key) {
  case continuation() {
    Stop -> LastBy(list.reverse(current_chunk))
    Continue(e, next) -> {
      let key = f(e)
      case key == previous_key {
        True -> next_chunk(next, f, key, [e, ..current_chunk])
        False -> AnotherBy(list.reverse(current_chunk), key, e, next)
      }
    }
  }
}
fn do_chunk(
  continuation: fn() -> Action(element),
  f: fn(element) -> key,
  previous_key: key,
  previous_element: element,
) -> Action(List(element)) {
  case next_chunk(continuation, f, previous_key, [previous_element]) {
    LastBy(chunk) -> Continue(chunk, stop)
    AnotherBy(chunk, key, el, next) ->
      Continue(chunk, fn() { do_chunk(next, f, key, el) })
  }
}
pub fn chunk(
  over iterator: Iterator(element),
  by f: fn(element) -> key,
) -> Iterator(List(element)) {
  fn() {
    case iterator.continuation() {
      Stop -> Stop
      Continue(e, next) -> do_chunk(next, f, f(e), e)
    }
  }
  |> Iterator
}
type SizedChunk(element) {
  Another(List(element), fn() -> Action(element))
  Last(List(element))
  NoMore
}
fn next_sized_chunk(
  continuation: fn() -> Action(element),
  left: Int,
  current_chunk: List(element),
) -> SizedChunk(element) {
  case continuation() {
    Stop ->
      case current_chunk {
        [] -> NoMore
        remaining -> Last(list.reverse(remaining))
      }
    Continue(e, next) -> {
      let chunk = [e, ..current_chunk]
      case left > 1 {
        False -> Another(list.reverse(chunk), next)
        True -> next_sized_chunk(next, left - 1, chunk)
      }
    }
  }
}
fn do_sized_chunk(
  continuation: fn() -> Action(element),
  count: Int,
) -> fn() -> Action(List(element)) {
  fn() {
    case next_sized_chunk(continuation, count, []) {
      NoMore -> Stop
      Last(chunk) -> Continue(chunk, stop)
      Another(chunk, next_element) ->
        Continue(chunk, do_sized_chunk(next_element, count))
    }
  }
}
pub fn sized_chunk(
  over iterator: Iterator(element),
  into count: Int,
) -> Iterator(List(element)) {
  iterator.continuation
  |> do_sized_chunk(count)
  |> Iterator
}
fn do_intersperse(
  continuation: fn() -> Action(element),
  separator: element,
) -> Action(element) {
  case continuation() {
    Stop -> Stop
    Continue(e, next) -> {
      let next_interspersed = fn() { do_intersperse(next, separator) }
      Continue(separator, fn() { Continue(e, next_interspersed) })
    }
  }
}
pub fn intersperse(
  over iterator: Iterator(element),
  with elem: element,
) -> Iterator(element) {
  fn() {
    case iterator.continuation() {
      Stop -> Stop
      Continue(e, next) -> Continue(e, fn() { do_intersperse(next, elem) })
    }
  }
  |> Iterator
}
fn do_any(
  continuation: fn() -> Action(element),
  predicate: fn(element) -> Bool,
) -> Bool {
  case continuation() {
    Stop -> False
    Continue(e, next) ->
      case predicate(e) {
        True -> True
        False -> do_any(next, predicate)
      }
  }
}
pub fn any(
  in iterator: Iterator(element),
  satisfying predicate: fn(element) -> Bool,
) -> Bool {
  iterator.continuation
  |> do_any(predicate)
}
fn do_all(
  continuation: fn() -> Action(element),
  predicate: fn(element) -> Bool,
) -> Bool {
  case continuation() {
    Stop -> True
    Continue(e, next) ->
      case predicate(e) {
        True -> do_all(next, predicate)
        False -> False
      }
  }
}
pub fn all(
  in iterator: Iterator(element),
  satisfying predicate: fn(element) -> Bool,
) -> Bool {
  iterator.continuation
  |> do_all(predicate)
}
fn update_group_with(el: element) -> fn(Option(List(element))) -> List(element) {
  fn(maybe_group) {
    case maybe_group {
      Some(group) -> [el, ..group]
      None -> [el]
    }
  }
}
fn group_updater(
  f: fn(element) -> key,
) -> fn(Dict(key, List(element)), element) -> Dict(key, List(element)) {
  fn(groups, elem) {
    groups
    |> dict.update(f(elem), update_group_with(elem))
  }
}
pub fn group(
  in iterator: Iterator(element),
  by key: fn(element) -> key,
) -> Dict(key, List(element)) {
  iterator
  |> fold(dict.new(), group_updater(key))
  |> dict.map_values(fn(_, group) { list.reverse(group) })
}
pub fn reduce(
  over iterator: Iterator(e),
  with f: fn(e, e) -> e,
) -> Result(e, Nil) {
  case iterator.continuation() {
    Stop -> Error(Nil)
    Continue(e, next) ->
      do_fold(next, f, e)
      |> Ok
  }
}
pub fn last(iterator: Iterator(element)) -> Result(element, Nil) {
  iterator
  |> reduce(fn(_, elem) { elem })
}
pub fn empty() -> Iterator(element) {
  Iterator(stop)
}
pub fn once(f: fn() -> element) -> Iterator(element) {
  fn() { Continue(f(), stop) }
  |> Iterator
}
pub fn single(elem: element) -> Iterator(element) {
  once(fn() { elem })
}
fn do_interleave(
  current: fn() -> Action(element),
  next: fn() -> Action(element),
) -> Action(element) {
  case current() {
    Stop -> next()
    Continue(e, next_other) ->
      Continue(e, fn() { do_interleave(next, next_other) })
  }
}
pub fn interleave(
  left: Iterator(element),
  with right: Iterator(element),
) -> Iterator(element) {
  fn() { do_interleave(left.continuation, right.continuation) }
  |> Iterator
}
fn do_fold_until(
  continuation: fn() -> Action(e),
  f: fn(acc, e) -> list.ContinueOrStop(acc),
  accumulator: acc,
) -> acc {
  case continuation() {
    Stop -> accumulator
    Continue(elem, next) ->
      case f(accumulator, elem) {
        list.Continue(accumulator) -> do_fold_until(next, f, accumulator)
        list.Stop(accumulator) -> accumulator
      }
  }
}
pub fn fold_until(
  over iterator: Iterator(e),
  from initial: acc,
  with f: fn(acc, e) -> list.ContinueOrStop(acc),
) -> acc {
  iterator.continuation
  |> do_fold_until(f, initial)
}
fn do_try_fold(
  over continuation: fn() -> Action(a),
  with f: fn(acc, a) -> Result(acc, err),
  from accumulator: acc,
) -> Result(acc, err) {
  case continuation() {
    Stop -> Ok(accumulator)
    Continue(elem, next) -> {
      use accumulator <- result.try(f(accumulator, elem))
      do_try_fold(next, f, accumulator)
    }
  }
}
pub fn try_fold(
  over iterator: Iterator(e),
  from initial: acc,
  with f: fn(acc, e) -> Result(acc, err),
) -> Result(acc, err) {
  iterator.continuation
  |> do_try_fold(f, initial)
}
pub fn first(from iterator: Iterator(e)) -> Result(e, Nil) {
  case iterator.continuation() {
    Stop -> Error(Nil)
    Continue(e, _) -> Ok(e)
  }
}
pub fn at(in iterator: Iterator(e), get index: Int) -> Result(e, Nil) {
  iterator
  |> drop(index)
  |> first
}
fn do_length(over continuation: fn() -> Action(e), with length: Int) -> Int {
  case continuation() {
    Stop -> length
    Continue(_, next) -> do_length(next, length + 1)
  }
}
pub fn length(over iterator: Iterator(e)) -> Int {
  iterator.continuation
  |> do_length(0)
}
pub fn each(over iterator: Iterator(a), with f: fn(a) -> b) -> Nil {
  iterator
  |> map(f)
  |> run
}
pub fn yield(element: a, next: fn() -> Iterator(a)) -> Iterator(a) {
  Iterator(fn() { Continue(element, next().continuation) })
}`,
  "gleam/bit_array": `import gleam/string
@external(javascript, "../gleam_stdlib.mjs", "bit_array_from_string")
pub fn from_string(x: String) -> BitArray
@external(javascript, "../gleam_stdlib.mjs", "length")
pub fn byte_size(x: BitArray) -> Int
pub fn append(to first: BitArray, suffix second: BitArray) -> BitArray {
  concat([first, second])
}
@external(javascript, "../gleam_stdlib.mjs", "bit_array_slice")
pub fn slice(
  from string: BitArray,
  at position: Int,
  take length: Int,
) -> Result(BitArray, Nil)
pub fn is_utf8(bits: BitArray) -> Bool {
  do_is_utf8(bits)
}
@target(erlang)
fn do_is_utf8(bits: BitArray) -> Bool {
  case bits {
    <<>> -> True
    <<_:utf8, rest:bytes>> -> do_is_utf8(rest)
    _ -> False
  }
}
@target(javascript)
fn do_is_utf8(bits: BitArray) -> Bool {
  case to_string(bits) {
    Ok(_) -> True
    _ -> False
  }
}
pub fn to_string(bits: BitArray) -> Result(String, Nil) {
  do_to_string(bits)
}
@target(erlang)
fn unsafe_to_string(a: BitArray) -> String
@target(erlang)
fn do_to_string(bits: BitArray) -> Result(String, Nil) {
  case is_utf8(bits) {
    True -> Ok(unsafe_to_string(bits))
    False -> Error(Nil)
  }
}
@target(javascript)
@external(javascript, "../gleam_stdlib.mjs", "bit_array_to_string")
fn do_to_string(a: BitArray) -> Result(String, Nil)
@external(javascript, "../gleam_stdlib.mjs", "bit_array_concat")
pub fn concat(bit_arrays: List(BitArray)) -> BitArray
pub fn base64_encode(input: BitArray, padding: Bool) -> String {
  let encoded = encode64(input)
  case padding {
    True -> encoded
    False -> string.replace(encoded, "=", "")
  }
}
@external(javascript, "../gleam_stdlib.mjs", "encode64")
fn encode64(a: BitArray) -> String
pub fn base64_decode(encoded: String) -> Result(BitArray, Nil) {
  let padded = case byte_size(from_string(encoded)) % 4 {
    0 -> encoded
    n -> string.append(encoded, string.repeat("=", 4 - n))
  }
  decode64(padded)
}
@external(javascript, "../gleam_stdlib.mjs", "decode64")
fn decode64(a: String) -> Result(BitArray, Nil)
pub fn base64_url_encode(input: BitArray, padding: Bool) -> String {
  base64_encode(input, padding)
  |> string.replace("+", "-")
  |> string.replace("/", "_")
}
pub fn base64_url_decode(encoded: String) -> Result(BitArray, Nil) {
  encoded
  |> string.replace("-", "+")
  |> string.replace("_", "/")
  |> base64_decode()
}
@external(javascript, "../gleam_stdlib.mjs", "base16_encode")
pub fn base16_encode(input: BitArray) -> String
@external(javascript, "../gleam_stdlib.mjs", "base16_decode")
pub fn base16_decode(input: String) -> Result(BitArray, Nil)`,
  "gleam/option": `pub type Option(a) {
  Some(a)
  None
}
fn do_all(list: List(Option(a)), acc: List(a)) -> Option(List(a)) {
  case list {
    [] -> Some(acc)
    [x, ..rest] -> {
      let accumulate = fn(acc, item) {
        case acc, item {
          Some(values), Some(value) -> Some([value, ..values])
          _, _ -> None
        }
      }
      accumulate(do_all(rest, acc), x)
    }
  }
}
pub fn all(list: List(Option(a))) -> Option(List(a)) {
  do_all(list, [])
}
pub fn is_some(option: Option(a)) -> Bool {
  option != None
}
pub fn is_none(option: Option(a)) -> Bool {
  option == None
}
pub fn to_result(option: Option(a), e) -> Result(a, e) {
  case option {
    Some(a) -> Ok(a)
    _ -> Error(e)
  }
}
pub fn from_result(result: Result(a, e)) -> Option(a) {
  case result {
    Ok(a) -> Some(a)
    _ -> None
  }
}
pub fn unwrap(option: Option(a), or default: a) -> a {
  case option {
    Some(x) -> x
    None -> default
  }
}
pub fn lazy_unwrap(option: Option(a), or default: fn() -> a) -> a {
  case option {
    Some(x) -> x
    None -> default()
  }
}
pub fn map(over option: Option(a), with fun: fn(a) -> b) -> Option(b) {
  case option {
    Some(x) -> Some(fun(x))
    None -> None
  }
}
pub fn flatten(option: Option(Option(a))) -> Option(a) {
  case option {
    Some(x) -> x
    None -> None
  }
}
pub fn then(option: Option(a), apply fun: fn(a) -> Option(b)) -> Option(b) {
  case option {
    Some(x) -> fun(x)
    None -> None
  }
}
pub fn or(first: Option(a), second: Option(a)) -> Option(a) {
  case first {
    Some(_) -> first
    None -> second
  }
}
pub fn lazy_or(first: Option(a), second: fn() -> Option(a)) -> Option(a) {
  case first {
    Some(_) -> first
    None -> second()
  }
}
fn do_values(list: List(Option(a)), acc: List(a)) -> List(a) {
  case list {
    [] -> acc
    [x, ..xs] -> {
      let accumulate = fn(acc, item) {
        case item {
          Some(value) -> [value, ..acc]
          None -> acc
        }
      }
      accumulate(do_values(xs, acc), x)
    }
  }
}
pub fn values(options: List(Option(a))) -> List(a) {
  do_values(options, [])
}`,
  "gleam/pair": `pub fn first(pair: #(a, b)) -> a {
  let #(a, _) = pair
  a
}
pub fn second(pair: #(a, b)) -> b {
  let #(_, a) = pair
  a
}
pub fn swap(pair: #(a, b)) -> #(b, a) {
  let #(a, b) = pair
  #(b, a)
}
pub fn map_first(of pair: #(a, b), with fun: fn(a) -> c) -> #(c, b) {
  let #(a, b) = pair
  #(fun(a), b)
}
pub fn map_second(of pair: #(a, b), with fun: fn(b) -> c) -> #(a, c) {
  let #(a, b) = pair
  #(a, fun(b))
}
pub fn new(first: a, second: b) -> #(a, b) {
  #(first, second)
}`,
  "gleam/result": `import gleam/list
pub fn is_ok(result: Result(a, e)) -> Bool {
  case result {
    Error(_) -> False
    Ok(_) -> True
  }
}
pub fn is_error(result: Result(a, e)) -> Bool {
  case result {
    Ok(_) -> False
    Error(_) -> True
  }
}
pub fn map(over result: Result(a, e), with fun: fn(a) -> b) -> Result(b, e) {
  case result {
    Ok(x) -> Ok(fun(x))
    Error(e) -> Error(e)
  }
}
pub fn map_error(
  over result: Result(a, e),
  with fun: fn(e) -> f,
) -> Result(a, f) {
  case result {
    Ok(x) -> Ok(x)
    Error(error) -> Error(fun(error))
  }
}
pub fn flatten(result: Result(Result(a, e), e)) -> Result(a, e) {
  case result {
    Ok(x) -> x
    Error(error) -> Error(error)
  }
}
pub fn try(
  result: Result(a, e),
  apply fun: fn(a) -> Result(b, e),
) -> Result(b, e) {
  case result {
    Ok(x) -> fun(x)
    Error(e) -> Error(e)
  }
}
pub fn then(
  result: Result(a, e),
  apply fun: fn(a) -> Result(b, e),
) -> Result(b, e) {
  try(result, fun)
}
pub fn unwrap(result: Result(a, e), or default: a) -> a {
  case result {
    Ok(v) -> v
    Error(_) -> default
  }
}
pub fn lazy_unwrap(result: Result(a, e), or default: fn() -> a) -> a {
  case result {
    Ok(v) -> v
    Error(_) -> default()
  }
}
pub fn unwrap_error(result: Result(a, e), or default: e) -> e {
  case result {
    Ok(_) -> default
    Error(e) -> e
  }
}
pub fn unwrap_both(result: Result(a, a)) -> a {
  case result {
    Ok(a) -> a
    Error(a) -> a
  }
}
pub fn nil_error(result: Result(a, e)) -> Result(a, Nil) {
  map_error(result, fn(_) { Nil })
}
pub fn or(first: Result(a, e), second: Result(a, e)) -> Result(a, e) {
  case first {
    Ok(_) -> first
    Error(_) -> second
  }
}
pub fn lazy_or(
  first: Result(a, e),
  second: fn() -> Result(a, e),
) -> Result(a, e) {
  case first {
    Ok(_) -> first
    Error(_) -> second()
  }
}
pub fn all(results: List(Result(a, e))) -> Result(List(a), e) {
  list.try_map(results, fn(x) { x })
}
pub fn partition(results: List(Result(a, e))) -> #(List(a), List(e)) {
  do_partition(results, [], [])
}
fn do_partition(results: List(Result(a, e)), oks: List(a), errors: List(e)) {
  case results {
    [] -> #(oks, errors)
    [Ok(a), ..rest] -> do_partition(rest, [a, ..oks], errors)
    [Error(e), ..rest] -> do_partition(rest, oks, [e, ..errors])
  }
}
pub fn replace(result: Result(a, e), value: b) -> Result(b, e) {
  case result {
    Ok(_) -> Ok(value)
    Error(error) -> Error(error)
  }
}
pub fn replace_error(result: Result(a, e1), error: e2) -> Result(a, e2) {
  case result {
    Ok(x) -> Ok(x)
    Error(_) -> Error(error)
  }
}
pub fn values(results: List(Result(a, e))) -> List(a) {
  list.filter_map(results, fn(r) { r })
}
pub fn try_recover(
  result: Result(a, e),
  with fun: fn(e) -> Result(a, f),
) -> Result(a, f) {
  case result {
    Ok(value) -> Ok(value)
    Error(error) -> fun(error)
  }
}`,
  "gleam/dict": `import gleam/option.{type Option}
pub type Dict(key, value)
pub fn size(dict: Dict(k, v)) -> Int {
  do_size(dict)
}
@external(javascript, "../gleam_stdlib.mjs", "map_size")
fn do_size(a: Dict(k, v)) -> Int
pub fn to_list(dict: Dict(key, value)) -> List(#(key, value)) {
  do_to_list(dict)
}
@external(javascript, "../gleam_stdlib.mjs", "map_to_list")
fn do_to_list(a: Dict(key, value)) -> List(#(key, value))
pub fn from_list(list: List(#(k, v))) -> Dict(k, v) {
  do_from_list(list)
}
@target(erlang)
fn do_from_list(a: List(#(key, value))) -> Dict(key, value)
@target(javascript)
fn fold_list_of_pair(
  over list: List(#(k, v)),
  from initial: Dict(k, v),
) -> Dict(k, v) {
  case list {
    [] -> initial
    [x, ..rest] -> fold_list_of_pair(rest, insert(initial, x.0, x.1))
  }
}
@target(javascript)
fn do_from_list(list: List(#(k, v))) -> Dict(k, v) {
  fold_list_of_pair(list, new())
}
pub fn has_key(dict: Dict(k, v), key: k) -> Bool {
  do_has_key(key, dict)
}
@target(erlang)
fn do_has_key(a: key, b: Dict(key, v)) -> Bool
@target(javascript)
fn do_has_key(key: k, dict: Dict(k, v)) -> Bool {
  get(dict, key) != Error(Nil)
}
pub fn new() -> Dict(key, value) {
  do_new()
}
@external(javascript, "../gleam_stdlib.mjs", "new_map")
fn do_new() -> Dict(key, value)
pub fn get(from: Dict(key, value), get: key) -> Result(value, Nil) {
  do_get(from, get)
}
@external(javascript, "../gleam_stdlib.mjs", "map_get")
fn do_get(a: Dict(key, value), b: key) -> Result(value, Nil)
pub fn insert(into dict: Dict(k, v), for key: k, insert value: v) -> Dict(k, v) {
  do_insert(key, value, dict)
}
@external(javascript, "../gleam_stdlib.mjs", "map_insert")
fn do_insert(a: key, b: value, c: Dict(key, value)) -> Dict(key, value)
pub fn map_values(in dict: Dict(k, v), with fun: fn(k, v) -> w) -> Dict(k, w) {
  do_map_values(fun, dict)
}
@target(erlang)
fn do_map_values(a: fn(key, value) -> b, b: Dict(key, value)) -> Dict(key, b)
@target(javascript)
fn do_map_values(f: fn(key, value) -> b, dict: Dict(key, value)) -> Dict(key, b) {
  let f = fn(dict, k, v) { insert(dict, k, f(k, v)) }
  dict
  |> fold(from: new(), with: f)
}
pub fn keys(dict: Dict(keys, v)) -> List(keys) {
  do_keys(dict)
}
@target(erlang)
fn do_keys(a: Dict(keys, v)) -> List(keys)
@target(javascript)
fn reverse_and_concat(remaining, accumulator) {
  case remaining {
    [] -> accumulator
    [item, ..rest] -> reverse_and_concat(rest, [item, ..accumulator])
  }
}
@target(javascript)
fn do_keys_acc(list: List(#(k, v)), acc: List(k)) -> List(k) {
  case list {
    [] -> reverse_and_concat(acc, [])
    [x, ..xs] -> do_keys_acc(xs, [x.0, ..acc])
  }
}
@target(javascript)
fn do_keys(dict: Dict(k, v)) -> List(k) {
  let list_of_pairs = to_list(dict)
  do_keys_acc(list_of_pairs, [])
}
pub fn values(dict: Dict(k, values)) -> List(values) {
  do_values(dict)
}
@target(erlang)
fn do_values(a: Dict(k, values)) -> List(values)
@target(javascript)
fn do_values_acc(list: List(#(k, v)), acc: List(v)) -> List(v) {
  case list {
    [] -> reverse_and_concat(acc, [])
    [x, ..xs] -> do_values_acc(xs, [x.1, ..acc])
  }
}
@target(javascript)
fn do_values(dict: Dict(k, v)) -> List(v) {
  let list_of_pairs = to_list(dict)
  do_values_acc(list_of_pairs, [])
}
pub fn filter(
  in dict: Dict(k, v),
  keeping predicate: fn(k, v) -> Bool,
) -> Dict(k, v) {
  do_filter(predicate, dict)
}
@target(erlang)
fn do_filter(a: fn(key, value) -> Bool, b: Dict(key, value)) -> Dict(key, value)
@target(javascript)
fn do_filter(
  f: fn(key, value) -> Bool,
  dict: Dict(key, value),
) -> Dict(key, value) {
  let insert = fn(dict, k, v) {
    case f(k, v) {
      True -> insert(dict, k, v)
      _ -> dict
    }
  }
  dict
  |> fold(from: new(), with: insert)
}
pub fn take(from dict: Dict(k, v), keeping desired_keys: List(k)) -> Dict(k, v) {
  do_take(desired_keys, dict)
}
@target(erlang)
fn do_take(a: List(k), b: Dict(k, v)) -> Dict(k, v)
@target(javascript)
fn insert_taken(
  dict: Dict(k, v),
  desired_keys: List(k),
  acc: Dict(k, v),
) -> Dict(k, v) {
  let insert = fn(taken, key) {
    case get(dict, key) {
      Ok(value) -> insert(taken, key, value)
      _ -> taken
    }
  }
  case desired_keys {
    [] -> acc
    [x, ..xs] -> insert_taken(dict, xs, insert(acc, x))
  }
}
@target(javascript)
fn do_take(desired_keys: List(k), dict: Dict(k, v)) -> Dict(k, v) {
  insert_taken(dict, desired_keys, new())
}
pub fn merge(into dict: Dict(k, v), from new_entries: Dict(k, v)) -> Dict(k, v) {
  do_merge(dict, new_entries)
}
@target(erlang)
fn do_merge(a: Dict(k, v), b: Dict(k, v)) -> Dict(k, v)
@target(javascript)
fn insert_pair(dict: Dict(k, v), pair: #(k, v)) -> Dict(k, v) {
  insert(dict, pair.0, pair.1)
}
@target(javascript)
fn fold_inserts(new_entries: List(#(k, v)), dict: Dict(k, v)) -> Dict(k, v) {
  case new_entries {
    [] -> dict
    [x, ..xs] -> fold_inserts(xs, insert_pair(dict, x))
  }
}
@target(javascript)
fn do_merge(dict: Dict(k, v), new_entries: Dict(k, v)) -> Dict(k, v) {
  new_entries
  |> to_list
  |> fold_inserts(dict)
}
pub fn delete(from dict: Dict(k, v), delete key: k) -> Dict(k, v) {
  do_delete(key, dict)
}
@external(javascript, "../gleam_stdlib.mjs", "map_remove")
fn do_delete(a: k, b: Dict(k, v)) -> Dict(k, v)
pub fn drop(from dict: Dict(k, v), drop disallowed_keys: List(k)) -> Dict(k, v) {
  case disallowed_keys {
    [] -> dict
    [x, ..xs] -> drop(delete(dict, x), xs)
  }
}
pub fn update(
  in dict: Dict(k, v),
  update key: k,
  with fun: fn(Option(v)) -> v,
) -> Dict(k, v) {
  dict
  |> get(key)
  |> option.from_result
  |> fun
  |> insert(dict, key, _)
}
fn do_fold(list: List(#(k, v)), initial: acc, fun: fn(acc, k, v) -> acc) -> acc {
  case list {
    [] -> initial
    [#(k, v), ..rest] -> do_fold(rest, fun(initial, k, v), fun)
  }
}
pub fn fold(
  over dict: Dict(k, v),
  from initial: acc,
  with fun: fn(acc, k, v) -> acc,
) -> acc {
  dict
  |> to_list
  |> do_fold(initial, fun)
}`,
  "gleam/order": `pub type Order {
  Lt
  Eq
  Gt
}
pub fn negate(order: Order) -> Order {
  case order {
    Lt -> Gt
    Eq -> Eq
    Gt -> Lt
  }
}
pub fn to_int(order: Order) -> Int {
  case order {
    Lt -> -1
    Eq -> 0
    Gt -> 1
  }
}
pub fn compare(a: Order, with b: Order) -> Order {
  case a, b {
    x, y if x == y -> Eq
    Lt, _ | Eq, Gt -> Lt
    _, _ -> Gt
  }
}
pub fn max(a: Order, b: Order) -> Order {
  case a, b {
    Gt, _ -> Gt
    Eq, Lt -> Eq
    _, _ -> b
  }
}
pub fn min(a: Order, b: Order) -> Order {
  case a, b {
    Lt, _ -> Lt
    Eq, Gt -> Eq
    _, _ -> b
  }
}
pub fn reverse(orderer: fn(a, a) -> Order) -> fn(a, a) -> Order {
  fn(a, b) { orderer(b, a) }
}`,
  "gleam/dynamic": `import gleam/int
import gleam/list
import gleam/dict.{type Dict}
import gleam/option.{type Option}
import gleam/result
import gleam/string_builder
@target(erlang)
import gleam/bit_array
pub type Dynamic
pub type DecodeError {
  DecodeError(expected: String, found: String, path: List(String))
}
pub type DecodeErrors =
  List(DecodeError)
pub type Decoder(t) =
  fn(Dynamic) -> Result(t, DecodeErrors)
pub fn from(a) -> Dynamic {
  do_from(a)
}
@external(javascript, "../gleam_stdlib.mjs", "identity")
fn do_from(a: anything) -> Dynamic
pub fn unsafe_coerce(a: Dynamic) -> anything {
  do_unsafe_coerce(a)
}
@external(javascript, "../gleam_stdlib.mjs", "identity")
fn do_unsafe_coerce(a: Dynamic) -> a
pub fn dynamic(value: Dynamic) -> Result(Dynamic, List(DecodeError)) {
  Ok(value)
}
pub fn bit_array(from data: Dynamic) -> Result(BitArray, DecodeErrors) {
  decode_bit_array(data)
}
@deprecated("Please use \`bit_array\` instead")
pub fn bit_string(from data: Dynamic) -> Result(BitArray, DecodeErrors) {
  bit_array(data)
}
@external(javascript, "../gleam_stdlib.mjs", "decode_bit_array")
fn decode_bit_array(a: Dynamic) -> Result(BitArray, DecodeErrors)
pub fn string(from data: Dynamic) -> Result(String, DecodeErrors) {
  decode_string(data)
}
fn map_errors(
  result: Result(t, DecodeErrors),
  f: fn(DecodeError) -> DecodeError,
) -> Result(t, DecodeErrors) {
  result.map_error(result, list.map(_, f))
}
@target(erlang)
fn decode_string(data: Dynamic) -> Result(String, DecodeErrors) {
  bit_array(data)
  |> map_errors(put_expected(_, "String"))
  |> result.try(fn(raw) {
    case bit_array.to_string(raw) {
      Ok(string) -> Ok(string)
      Error(Nil) ->
        Error([DecodeError(expected: "String", found: "BitArray", path: [])])
    }
  })
}
@target(erlang)
fn put_expected(error: DecodeError, expected: String) -> DecodeError {
  DecodeError(..error, expected: expected)
}
@target(javascript)
@external(javascript, "../gleam_stdlib.mjs", "decode_string")
fn decode_string(a: Dynamic) -> Result(String, DecodeErrors)
pub fn classify(data: Dynamic) -> String {
  do_classify(data)
}
@external(javascript, "../gleam_stdlib.mjs", "classify_dynamic")
fn do_classify(a: Dynamic) -> String
pub fn int(from data: Dynamic) -> Result(Int, DecodeErrors) {
  decode_int(data)
}
@external(javascript, "../gleam_stdlib.mjs", "decode_int")
fn decode_int(a: Dynamic) -> Result(Int, DecodeErrors)
pub fn float(from data: Dynamic) -> Result(Float, DecodeErrors) {
  decode_float(data)
}
@external(javascript, "../gleam_stdlib.mjs", "decode_float")
fn decode_float(a: Dynamic) -> Result(Float, DecodeErrors)
pub fn bool(from data: Dynamic) -> Result(Bool, DecodeErrors) {
  decode_bool(data)
}
@external(javascript, "../gleam_stdlib.mjs", "decode_bool")
fn decode_bool(a: Dynamic) -> Result(Bool, DecodeErrors)
pub fn shallow_list(from value: Dynamic) -> Result(List(Dynamic), DecodeErrors) {
  decode_list(value)
}
@external(javascript, "../gleam_stdlib.mjs", "decode_list")
fn decode_list(a: Dynamic) -> Result(List(Dynamic), DecodeErrors)
@external(javascript, "../gleam_stdlib.mjs", "decode_result")
fn decode_result(a: Dynamic) -> Result(Result(a, e), DecodeErrors)
pub fn result(
  ok decode_ok: Decoder(a),
  error decode_error: Decoder(e),
) -> Decoder(Result(a, e)) {
  fn(value) {
    use inner_result <- result.try(decode_result(value))
    case inner_result {
      Ok(raw) -> {
        use value <- result.try(
          decode_ok(raw)
          |> map_errors(push_path(_, "ok")),
        )
        Ok(Ok(value))
      }
      Error(raw) -> {
        use value <- result.try(
          decode_error(raw)
          |> map_errors(push_path(_, "error")),
        )
        Ok(Error(value))
      }
    }
  }
}
pub fn list(
  of decoder_type: fn(Dynamic) -> Result(inner, DecodeErrors),
) -> Decoder(List(inner)) {
  fn(dynamic) {
    use list <- result.try(shallow_list(dynamic))
    list
    |> list.try_map(decoder_type)
    |> map_errors(push_path(_, "*"))
  }
}
pub fn optional(of decode: Decoder(inner)) -> Decoder(Option(inner)) {
  fn(value) { decode_optional(value, decode) }
}
@external(javascript, "../gleam_stdlib.mjs", "decode_option")
fn decode_optional(a: Dynamic, b: Decoder(a)) -> Result(Option(a), DecodeErrors)
pub fn field(named name: a, of inner_type: Decoder(t)) -> Decoder(t) {
  fn(value) {
    let missing_field_error =
      DecodeError(expected: "field", found: "nothing", path: [])
    use maybe_inner <- result.try(decode_field(value, name))
    maybe_inner
    |> option.to_result([missing_field_error])
    |> result.try(inner_type)
    |> map_errors(push_path(_, name))
  }
}
pub fn optional_field(
  named name: a,
  of inner_type: Decoder(t),
) -> Decoder(Option(t)) {
  fn(value) {
    use maybe_inner <- result.try(decode_field(value, name))
    case maybe_inner {
      option.None -> Ok(option.None)
      option.Some(dynamic_inner) ->
        dynamic_inner
        |> decode_optional(inner_type)
        |> map_errors(push_path(_, name))
    }
  }
}
@external(javascript, "../gleam_stdlib.mjs", "decode_field")
fn decode_field(a: Dynamic, b: name) -> Result(Option(Dynamic), DecodeErrors)
pub fn element(at index: Int, of inner_type: Decoder(t)) -> Decoder(t) {
  fn(data: Dynamic) {
    use tuple <- result.try(decode_tuple(data))
    let size = tuple_size(tuple)
    use data <- result.try(case index >= 0 {
      True ->
        case index < size {
          True -> tuple_get(tuple, index)
          False -> at_least_decode_tuple_error(index + 1, data)
        }
      False ->
        case int.absolute_value(index) <= size {
          True -> tuple_get(tuple, size + index)
          False -> at_least_decode_tuple_error(int.absolute_value(index), data)
        }
    })
    inner_type(data)
    |> map_errors(push_path(_, index))
  }
}
fn at_least_decode_tuple_error(
  size: Int,
  data: Dynamic,
) -> Result(a, DecodeErrors) {
  let s = case size {
    1 -> ""
    _ -> "s"
  }
  let error =
    ["Tuple of at least ", int.to_string(size), " element", s]
    |> string_builder.from_strings
    |> string_builder.to_string
    |> DecodeError(found: classify(data), path: [])
  Error([error])
}
type UnknownTuple
@external(javascript, "../gleam_stdlib.mjs", "decode_tuple")
fn decode_tuple(a: Dynamic) -> Result(UnknownTuple, DecodeErrors)
@external(javascript, "../gleam_stdlib.mjs", "decode_tuple2")
fn decode_tuple2(a: Dynamic) -> Result(#(Dynamic, Dynamic), DecodeErrors)
@external(javascript, "../gleam_stdlib.mjs", "decode_tuple3")
fn decode_tuple3(
  a: Dynamic,
) -> Result(#(Dynamic, Dynamic, Dynamic), DecodeErrors)
@external(javascript, "../gleam_stdlib.mjs", "decode_tuple4")
fn decode_tuple4(
  a: Dynamic,
) -> Result(#(Dynamic, Dynamic, Dynamic, Dynamic), DecodeErrors)
@external(javascript, "../gleam_stdlib.mjs", "decode_tuple5")
fn decode_tuple5(
  a: Dynamic,
) -> Result(#(Dynamic, Dynamic, Dynamic, Dynamic, Dynamic), DecodeErrors)
@external(javascript, "../gleam_stdlib.mjs", "decode_tuple6")
fn decode_tuple6(
  a: Dynamic,
) -> Result(
  #(Dynamic, Dynamic, Dynamic, Dynamic, Dynamic, Dynamic),
  DecodeErrors,
)
@external(javascript, "../gleam_stdlib.mjs", "tuple_get")
fn tuple_get(a: UnknownTuple, b: Int) -> Result(Dynamic, DecodeErrors)
@external(javascript, "../gleam_stdlib.mjs", "length")
fn tuple_size(a: UnknownTuple) -> Int
fn tuple_errors(
  result: Result(a, List(DecodeError)),
  name: String,
) -> List(DecodeError) {
  case result {
    Ok(_) -> []
    Error(errors) -> list.map(errors, push_path(_, name))
  }
}
fn push_path(error: DecodeError, name: t) -> DecodeError {
  let name = from(name)
  let decoder = any([string, fn(x) { result.map(int(x), int.to_string) }])
  let name = case decoder(name) {
    Ok(name) -> name
    Error(_) ->
      ["<", classify(name), ">"]
      |> string_builder.from_strings
      |> string_builder.to_string
  }
  DecodeError(..error, path: [name, ..error.path])
}
pub fn tuple2(
  first decode1: Decoder(a),
  second decode2: Decoder(b),
) -> Decoder(#(a, b)) {
  fn(value) {
    use #(a, b) <- result.try(decode_tuple2(value))
    case decode1(a), decode2(b) {
      Ok(a), Ok(b) -> Ok(#(a, b))
      a, b ->
        tuple_errors(a, "0")
        |> list.append(tuple_errors(b, "1"))
        |> Error
    }
  }
}
pub fn tuple3(
  first decode1: Decoder(a),
  second decode2: Decoder(b),
  third decode3: Decoder(c),
) -> Decoder(#(a, b, c)) {
  fn(value) {
    use #(a, b, c) <- result.try(decode_tuple3(value))
    case decode1(a), decode2(b), decode3(c) {
      Ok(a), Ok(b), Ok(c) -> Ok(#(a, b, c))
      a, b, c ->
        tuple_errors(a, "0")
        |> list.append(tuple_errors(b, "1"))
        |> list.append(tuple_errors(c, "2"))
        |> Error
    }
  }
}
pub fn tuple4(
  first decode1: Decoder(a),
  second decode2: Decoder(b),
  third decode3: Decoder(c),
  fourth decode4: Decoder(d),
) -> Decoder(#(a, b, c, d)) {
  fn(value) {
    use #(a, b, c, d) <- result.try(decode_tuple4(value))
    case decode1(a), decode2(b), decode3(c), decode4(d) {
      Ok(a), Ok(b), Ok(c), Ok(d) -> Ok(#(a, b, c, d))
      a, b, c, d ->
        tuple_errors(a, "0")
        |> list.append(tuple_errors(b, "1"))
        |> list.append(tuple_errors(c, "2"))
        |> list.append(tuple_errors(d, "3"))
        |> Error
    }
  }
}
pub fn tuple5(
  first decode1: Decoder(a),
  second decode2: Decoder(b),
  third decode3: Decoder(c),
  fourth decode4: Decoder(d),
  fifth decode5: Decoder(e),
) -> Decoder(#(a, b, c, d, e)) {
  fn(value) {
    use #(a, b, c, d, e) <- result.try(decode_tuple5(value))
    case decode1(a), decode2(b), decode3(c), decode4(d), decode5(e) {
      Ok(a), Ok(b), Ok(c), Ok(d), Ok(e) -> Ok(#(a, b, c, d, e))
      a, b, c, d, e ->
        tuple_errors(a, "0")
        |> list.append(tuple_errors(b, "1"))
        |> list.append(tuple_errors(c, "2"))
        |> list.append(tuple_errors(d, "3"))
        |> list.append(tuple_errors(e, "4"))
        |> Error
    }
  }
}
pub fn tuple6(
  first decode1: Decoder(a),
  second decode2: Decoder(b),
  third decode3: Decoder(c),
  fourth decode4: Decoder(d),
  fifth decode5: Decoder(e),
  sixth decode6: Decoder(f),
) -> Decoder(#(a, b, c, d, e, f)) {
  fn(value) {
    use #(a, b, c, d, e, f) <- result.try(decode_tuple6(value))
    case
      decode1(a),
      decode2(b),
      decode3(c),
      decode4(d),
      decode5(e),
      decode6(f)
    {
      Ok(a), Ok(b), Ok(c), Ok(d), Ok(e), Ok(f) -> Ok(#(a, b, c, d, e, f))
      a, b, c, d, e, f ->
        tuple_errors(a, "0")
        |> list.append(tuple_errors(b, "1"))
        |> list.append(tuple_errors(c, "2"))
        |> list.append(tuple_errors(d, "3"))
        |> list.append(tuple_errors(e, "4"))
        |> list.append(tuple_errors(f, "5"))
        |> Error
    }
  }
}
pub fn dict(
  of key_type: Decoder(k),
  to value_type: Decoder(v),
) -> Decoder(Dict(k, v)) {
  fn(value) {
    use map <- result.try(decode_map(value))
    use pairs <- result.try(
      map
      |> dict.to_list
      |> list.try_map(fn(pair) {
        let #(k, v) = pair
        use k <- result.try(
          key_type(k)
          |> map_errors(push_path(_, "keys")),
        )
        use v <- result.try(
          value_type(v)
          |> map_errors(push_path(_, "values")),
        )
        Ok(#(k, v))
      }),
    )
    Ok(dict.from_list(pairs))
  }
}
@deprecated("Use \`dict\` instead")
pub fn map(
  of key_type: Decoder(k),
  to value_type: Decoder(v),
) -> Decoder(Dict(k, v)) {
  dict(key_type, value_type)
}
@external(javascript, "../gleam_stdlib.mjs", "decode_map")
fn decode_map(a: Dynamic) -> Result(Dict(Dynamic, Dynamic), DecodeErrors)
pub fn any(of decoders: List(Decoder(t))) -> Decoder(t) {
  fn(data) {
    case decoders {
      [] ->
        Error([
          DecodeError(found: classify(data), expected: "another type", path: []),
        ])
      [decoder, ..decoders] ->
        case decoder(data) {
          Ok(decoded) -> Ok(decoded)
          Error(_) -> any(decoders)(data)
        }
    }
  }
}
pub fn decode1(constructor: fn(t1) -> t, t1: Decoder(t1)) -> Decoder(t) {
  fn(value) {
    case t1(value) {
      Ok(a) -> Ok(constructor(a))
      a -> Error(all_errors(a))
    }
  }
}
pub fn decode2(
  constructor: fn(t1, t2) -> t,
  t1: Decoder(t1),
  t2: Decoder(t2),
) -> Decoder(t) {
  fn(value) {
    case t1(value), t2(value) {
      Ok(a), Ok(b) -> Ok(constructor(a, b))
      a, b -> Error(list.concat([all_errors(a), all_errors(b)]))
    }
  }
}
pub fn decode3(
  constructor: fn(t1, t2, t3) -> t,
  t1: Decoder(t1),
  t2: Decoder(t2),
  t3: Decoder(t3),
) -> Decoder(t) {
  fn(value) {
    case t1(value), t2(value), t3(value) {
      Ok(a), Ok(b), Ok(c) -> Ok(constructor(a, b, c))
      a, b, c ->
        Error(list.concat([all_errors(a), all_errors(b), all_errors(c)]))
    }
  }
}
pub fn decode4(
  constructor: fn(t1, t2, t3, t4) -> t,
  t1: Decoder(t1),
  t2: Decoder(t2),
  t3: Decoder(t3),
  t4: Decoder(t4),
) -> Decoder(t) {
  fn(x: Dynamic) {
    case t1(x), t2(x), t3(x), t4(x) {
      Ok(a), Ok(b), Ok(c), Ok(d) -> Ok(constructor(a, b, c, d))
      a, b, c, d ->
        Error(list.concat([
          all_errors(a),
          all_errors(b),
          all_errors(c),
          all_errors(d),
        ]))
    }
  }
}
pub fn decode5(
  constructor: fn(t1, t2, t3, t4, t5) -> t,
  t1: Decoder(t1),
  t2: Decoder(t2),
  t3: Decoder(t3),
  t4: Decoder(t4),
  t5: Decoder(t5),
) -> Decoder(t) {
  fn(x: Dynamic) {
    case t1(x), t2(x), t3(x), t4(x), t5(x) {
      Ok(a), Ok(b), Ok(c), Ok(d), Ok(e) -> Ok(constructor(a, b, c, d, e))
      a, b, c, d, e ->
        Error(list.concat([
          all_errors(a),
          all_errors(b),
          all_errors(c),
          all_errors(d),
          all_errors(e),
        ]))
    }
  }
}
pub fn decode6(
  constructor: fn(t1, t2, t3, t4, t5, t6) -> t,
  t1: Decoder(t1),
  t2: Decoder(t2),
  t3: Decoder(t3),
  t4: Decoder(t4),
  t5: Decoder(t5),
  t6: Decoder(t6),
) -> Decoder(t) {
  fn(x: Dynamic) {
    case t1(x), t2(x), t3(x), t4(x), t5(x), t6(x) {
      Ok(a), Ok(b), Ok(c), Ok(d), Ok(e), Ok(f) ->
        Ok(constructor(a, b, c, d, e, f))
      a, b, c, d, e, f ->
        Error(list.concat([
          all_errors(a),
          all_errors(b),
          all_errors(c),
          all_errors(d),
          all_errors(e),
          all_errors(f),
        ]))
    }
  }
}
pub fn decode7(
  constructor: fn(t1, t2, t3, t4, t5, t6, t7) -> t,
  t1: Decoder(t1),
  t2: Decoder(t2),
  t3: Decoder(t3),
  t4: Decoder(t4),
  t5: Decoder(t5),
  t6: Decoder(t6),
  t7: Decoder(t7),
) -> Decoder(t) {
  fn(x: Dynamic) {
    case t1(x), t2(x), t3(x), t4(x), t5(x), t6(x), t7(x) {
      Ok(a), Ok(b), Ok(c), Ok(d), Ok(e), Ok(f), Ok(g) ->
        Ok(constructor(a, b, c, d, e, f, g))
      a, b, c, d, e, f, g ->
        Error(list.concat([
          all_errors(a),
          all_errors(b),
          all_errors(c),
          all_errors(d),
          all_errors(e),
          all_errors(f),
          all_errors(g),
        ]))
    }
  }
}
pub fn decode8(
  constructor: fn(t1, t2, t3, t4, t5, t6, t7, t8) -> t,
  t1: Decoder(t1),
  t2: Decoder(t2),
  t3: Decoder(t3),
  t4: Decoder(t4),
  t5: Decoder(t5),
  t6: Decoder(t6),
  t7: Decoder(t7),
  t8: Decoder(t8),
) -> Decoder(t) {
  fn(x: Dynamic) {
    case t1(x), t2(x), t3(x), t4(x), t5(x), t6(x), t7(x), t8(x) {
      Ok(a), Ok(b), Ok(c), Ok(d), Ok(e), Ok(f), Ok(g), Ok(h) ->
        Ok(constructor(a, b, c, d, e, f, g, h))
      a, b, c, d, e, f, g, h ->
        Error(list.concat([
          all_errors(a),
          all_errors(b),
          all_errors(c),
          all_errors(d),
          all_errors(e),
          all_errors(f),
          all_errors(g),
          all_errors(h),
        ]))
    }
  }
}
pub fn decode9(
  constructor: fn(t1, t2, t3, t4, t5, t6, t7, t8, t9) -> t,
  t1: Decoder(t1),
  t2: Decoder(t2),
  t3: Decoder(t3),
  t4: Decoder(t4),
  t5: Decoder(t5),
  t6: Decoder(t6),
  t7: Decoder(t7),
  t8: Decoder(t8),
  t9: Decoder(t9),
) -> Decoder(t) {
  fn(x: Dynamic) {
    case t1(x), t2(x), t3(x), t4(x), t5(x), t6(x), t7(x), t8(x), t9(x) {
      Ok(a), Ok(b), Ok(c), Ok(d), Ok(e), Ok(f), Ok(g), Ok(h), Ok(i) ->
        Ok(constructor(a, b, c, d, e, f, g, h, i))
      a, b, c, d, e, f, g, h, i ->
        Error(list.concat([
          all_errors(a),
          all_errors(b),
          all_errors(c),
          all_errors(d),
          all_errors(e),
          all_errors(f),
          all_errors(g),
          all_errors(h),
          all_errors(i),
        ]))
    }
  }
}
fn all_errors(result: Result(a, List(DecodeError))) -> List(DecodeError) {
  case result {
    Ok(_) -> []
    Error(errors) -> errors
  }
}`
}
