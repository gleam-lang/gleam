pub enum Empty =
  | Empty

pub enum NotFound =
  | NotFound

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

fn do_map_with(list, f, acc, map_f) {
  case list {
  | [] -> reverse(acc)
  | [x | xs] -> do_map_with(xs, f, [map_f(f, acc, x) | acc], map_f)
  }
}

pub fn map(list, f) {
  do_map_with(list, f, [], fn(fun, _, x) { fun(x) })
}

pub fn index_map(list, f) {
  do_map_with(list, f, [], fn(fun, a, x) { fun(length(a), x) })
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
    | [ x | rest] ->
      case f(x) {
        | False -> any(rest, f)
        | _ -> True
      }
  }
}

pub fn zip(l1, l2) {
  case {l1, l2} {
    | {[], _} -> []
    | {_, []} -> []
    | {[x1 | rest1], [x2 | rest2] } -> [ {x1, x2} | zip(rest1, rest2) ]
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

fn merge_sort(a, b) {
  case {a, b} {
    | {[], _} -> b
    | {_, []} -> a
    | {[ax | ar], [bx | br]} ->
      case ax < bx {
        | True -> [ax | merge_sort(ar, b)]
        | False -> [bx | merge_sort(a, br)]
      }
  }
}

pub fn sort(list) {
  let list_length = length(list)
  case list_length < 2 {
    | True -> list
    | False ->
      let split_length = list_length / 2
      let a_list = take(list, split_length)
      let b_list = drop(list, split_length)
      merge_sort(sort(a_list), sort(b_list))
  }
}
