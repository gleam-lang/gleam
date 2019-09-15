import gleam/int
import gleam/order

pub enum Empty =
  | Empty

pub enum NotFound =
  | NotFound

pub enum LengthMismatch =
  | LengthMismatch

// Using the Erlang C BIF implementation.
//
pub external fn length(List(a)) -> Int = "erlang" "length"

// Using the Erlang C BIF implementation.
//
pub external fn reverse(List(a)) -> List(a) = "lists" "reverse"

pub fn is_empty(list) {
  list == []
}

pub fn contains(list, elem) {
  case list {
  | [] -> False
  | [head | rest] -> head == elem || contains(rest, elem)
  }
}

pub fn head(list) {
  case list {
  | [] -> Error(Empty)
  | [x | _] -> Ok(x)
  }
}

pub fn tail(list) {
  case list {
  | [] -> Error(Empty)
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

pub fn filter(list, fun) {
  do_filter(list, fun, [])
}

fn do_map(list, fun, acc) {
  case list {
  | [] -> reverse(acc)
  | [x | xs] -> do_map(xs, fun, [fun(x) | acc])
  }
}

pub fn map(list, fun) {
  do_map(list, fun, [])
}

fn do_index_map(list, fun, index, acc) {
  case list {
  | [] -> reverse(acc)
  | [x | xs] -> do_index_map(xs, fun, index + 1, [fun(index, x) | acc])
  }
}

pub fn index_map(list, fun) {
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

pub fn traverse(list, fun) {
  do_traverse(list, fun, [])
}

pub fn drop(list, n) {
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

pub fn take(list, n) {
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

pub fn fold(list, acc, fun) {
  case list {
  | [] -> acc
  | [x | rest] -> fold(rest, fun(x, acc), fun)
  }
}

pub fn fold_right(list, acc, fun) {
  case list {
  | [] -> acc
  | [x | rest] -> fun(x, fold_right(rest, acc, fun))
  }
}

pub fn find(haystack, f) {
  case haystack {
  | [] -> Error(NotFound)
  | [x | rest] ->
      case f(x) {
      | Ok(x) -> Ok(x)
      | _ -> find(rest, f)
      }
  }
}

pub fn all(list, f) {
  case list {
    | [] -> True
    | [x | rest] ->
      case f(x) {
      | True -> all(rest, f)
      | _ -> False
      }
  }
}

pub fn any(list, f) {
  case list {
    | [] -> False
    | [x | rest] ->
      case f(x) {
      | False -> any(rest, f)
      | _ -> True
      }
  }
}

// TODO: replace struct with Pair
pub fn zip(l1, l2) {
  case struct(l1, l2) {
    | struct([], _) -> []
    | struct(_, []) -> []
    | struct([x1 | rest1], [x2 | rest2]) -> [ struct(x1, x2) | zip(rest1, rest2) ]
  }
}

pub fn strict_zip(l1, l2) {
  case length(l1) == length(l2) {
  | True -> Ok(zip(l1, l2))
  | False -> Error(LengthMismatch)
  }
}

pub fn intersperse(list, elem) {
  case list {
    | [] -> []
    | [x | []] -> [x]
    | [x | rest] -> [x | [elem | intersperse(rest, elem)]]
  }
}

pub fn at(list, i) {
  case i < 0 {
    | True -> Error(NotFound)
    | False ->
      case list {
        | [] -> Error(NotFound)
        | [x | rest] ->
          case i == 0 {
          | True -> Ok(x)
          | False -> at(rest, i - 1)
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
  case struct(a, b) {
    | struct([], _) -> b
    | struct(_, []) -> a
    | struct([ax | ar], [bx | br]) ->
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

pub fn sort(list, compare) {
  do_sort(list, compare, length(list))
}

pub fn range(start, stop) {
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

pub fn repeat(a, times) {
  do_repeat(a, times, [])
}

fn do_split(list, n, taken) {
  case n <= 0 {
  | True -> struct(reverse(taken), list)
  | False ->
      case list {
      | [] -> struct(reverse(taken), [])
      | [x | xs] -> do_split(xs, n - 1, [x | taken])
      }
  }
}

pub fn split(list, n) {
  do_split(list, n, [])
}

fn do_split_while(list, f, acc) {
  case list {
    | [] -> struct(reverse(acc), [])
    | [x | xs] ->
      case f(x) {
      | False -> struct(reverse(acc), list)
      | _ -> do_split_while(xs, f, [x | acc])
      }
  }
}

pub fn split_while(list, f) {
  do_split_while(list, f, [])
}
