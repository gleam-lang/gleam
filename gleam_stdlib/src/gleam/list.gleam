import gleam/int
import gleam/order
import gleam/pair

pub enum LengthMismatch = | LengthMismatch

// Using the Erlang C BIF implementation.
//
pub external fn length(List(a)) -> Int = "erlang" "length"

// Using the Erlang C BIF implementation.
//
pub external fn reverse(List(a)) -> List(a) = "lists" "reverse"

pub fn is_empty(list) {
  list == []
}

pub fn contains(list, has elem) {
  case list {
  | [] -> False
  | [head | rest] -> head == elem || contains(rest, elem)
  }
}

pub fn head(list) {
  case list {
  | [] -> Error(Nil)
  | [x | _] -> Ok(x)
  }
}

pub fn tail(list) {
  case list {
  | [] -> Error(Nil)
  | [_ | xs] -> Ok(xs)
  }
}

fn do_filter(list, fun, acc) {
  case list {
  | [] -> reverse(acc)
  | [x | xs] ->
      let new_acc =
        case fun(x) {
        | True -> [x | acc]
        | False -> acc
        }
      do_filter(xs, fun, new_acc)
  }
}

pub fn filter(list, for predicate) {
  do_filter(list, predicate, [])
}

fn do_map(list, fun, acc) {
  case list {
  | [] -> reverse(acc)
  | [x | xs] -> do_map(xs, fun, [fun(x) | acc])
  }
}

pub fn map(list, with fun) {
  do_map(list, fun, [])
}

fn do_index_map(list, fun, index, acc) {
  case list {
  | [] -> reverse(acc)
  | [x | xs] -> do_index_map(xs, fun, index + 1, [fun(index, x) | acc])
  }
}

pub fn index_map(list, with fun) {
  do_index_map(list, fun, 0, [])
}

fn do_traverse(list, fun, acc) {
  case list {
  | [] -> Ok(reverse(acc))
  | [x | xs] ->
      case fun(x) {
      | Ok(y) -> do_traverse(xs, fun, [y | acc])
      | Error(error) -> Error(error)
      }
  }
}

pub fn traverse(list, with fun) {
  do_traverse(list, fun, [])
}

pub fn drop(from list, up_to n) {
  case n <= 0 {
  | True -> list
  | False ->
      case list {
      | [] -> []
      | [_ | xs] -> drop(xs, n - 1)
      }
  }
}

fn do_take(list, n, acc) {
  case n <= 0 {
  | True -> reverse(acc)
  | False ->
      case list {
      | [] -> reverse(acc)
      | [x | xs] -> do_take(xs, n - 1, [x | acc])
      }
  }
}

pub fn take(from list, up_to n) {
  do_take(list, n, [])
}

pub fn new() {
  []
}

pub external fn append(List(a), List(a)) -> List(a) = "lists" "append";

fn do_flatten(lists, acc) {
  case lists {
  | [] -> acc
  | [l | rest] -> do_flatten(rest, append(acc, l))
  }
}

pub fn flatten(lists) {
  do_flatten(lists, [])
}

pub fn fold(list, from initial, with fun) {
  case list {
  | [] -> initial
  | [x | rest] -> fold(rest, fun(x, initial), fun)
  }
}

pub fn fold_right(list, from initial, with fun) {
  case list {
  | [] -> initial
  | [x | rest] -> fun(x, fold_right(rest, initial, fun))
  }
}

pub fn find(in haystack, one_that is_desired) {
  case haystack {
  | [] -> Error(Nil)
  | [x | rest] ->
      case is_desired(x) {
      | True -> Ok(x)
      | _ -> find(in: rest, one_that: is_desired)
      }
  }
}

pub fn find_map(in haystack, with fun) {
  case haystack {
  | [] -> Error(Nil)
  | [x | rest] ->
      case fun(x) {
      | Ok(x) -> Ok(x)
      | _ -> find_map(in: rest, with: fun)
      }
  }
}

pub fn all(in list, satisfying predicate) {
  case list {
    | [] -> True
    | [x | rest] ->
      case predicate(x) {
      | True -> all(rest, predicate)
      | _ -> False
      }
  }
}

pub fn any(in list, satisfying predicate) {
  case list {
  | [] -> False
  | [x | rest] ->
    case predicate(x) {
    | False -> any(rest, predicate)
    | _ -> True
    }
  }
}

pub fn zip(xs, ys) {
  case xs, ys {
  | [], _ -> []
  | _, [] -> []
  | [x | xs], [y | ys] -> [ pair.Pair(x, y) | zip(xs, ys) ]
  }
}

pub fn strict_zip(l1, l2) {
  case length(l1) == length(l2) {
  | True -> Ok(zip(l1, l2))
  | False -> Error(LengthMismatch)
  }
}

pub fn intersperse(list, with elem) {
  case list {
    | [] -> []
    | [x | []] -> [x]
    | [x | rest] -> [x | [elem | intersperse(rest, elem)]]
  }
}

pub fn at(in list, get index) {
  case index < 0 {
  | True -> Error(Nil)
  | False ->
    case list {
    | [] -> Error(Nil)
    | [x | rest] ->
      case index == 0 {
      | True -> Ok(x)
      | False -> at(rest, index - 1)
      }
    }
  }
}

pub fn unique(list) {
  case list {
  | [] -> []
  | [x | rest] -> [x | unique(filter(rest, fn(y) { y != x }))]
  }
}

fn merge_sort(a, b, compare) {
  case a, b {
  | [], _ -> b
  | _, [] -> a
  | [ax | ar], [bx | br] ->
    case compare(ax, bx) {
    | order.Lt -> [ax | merge_sort(ar, b, compare)]
    | _ -> [bx | merge_sort(a, br, compare)]
    }
  }
}

fn do_sort(list, compare, list_length) {
  case list_length < 2 {
  | True -> list
  | False ->
    let split_length = list_length / 2
    let a_list = take(list, split_length)
    let b_list = drop(list, split_length)
    merge_sort(
      do_sort(a_list, compare, split_length),
      do_sort(b_list, compare, list_length - split_length),
      compare,
    )
  }
}

pub fn sort(list, sort_by compare) {
  do_sort(list, compare, length(list))
}

pub fn range(from start, to stop) {
  case int.compare(start, stop) {
  | order.Eq -> []
  | order.Gt -> [start | range(start - 1, stop)]
  | order.Lt -> [start | range(start + 1, stop)]
  }
}

fn do_repeat(a, times, acc) {
  case times <= 0 {
    | True -> acc
    | False -> do_repeat(a, times - 1, [a | acc])
  }
}

pub fn repeat(item a, times times) {
  do_repeat(a, times, [])
}

fn do_split(list, n, taken) {
  case n <= 0 {
  | True -> pair.Pair(reverse(taken), list)
  | False ->
      case list {
      | [] -> pair.Pair(reverse(taken), [])
      | [x | xs] -> do_split(xs, n - 1, [x | taken])
      }
  }
}

pub fn split(list list, on target) {
  do_split(list, target, [])
}

fn do_split_while(list, f, acc) {
  case list {
    | [] -> pair.Pair(reverse(acc), [])
    | [x | xs] ->
      case f(x) {
      | False -> pair.Pair(reverse(acc), list)
      | _ -> do_split_while(xs, f, [x | acc])
      }
  }
}

pub fn split_while(list list, while predicate) {
  do_split_while(list, predicate, [])
}

pub fn key_find(in haystack, find needle) {
  find_map(haystack, fn(p) {
    case pair.first(p) == needle {
    | True -> p |> pair.second |> Ok
    | False -> Error(Nil)
    }
  })
}
