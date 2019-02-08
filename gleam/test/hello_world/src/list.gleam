pub enum Result(a, e) =
  | Ok(a)
  | Error(e)

pub enum Error =
  | Empty

pub external fn length(List(a)) -> Int = "erlang" "length"

pub external fn reverse(List(a)) -> List(a) = "erlang" "reverse"

pub fn is_empty(list) {
  list == []
}

pub fn new() {
  []
}

pub fn member(list, elem) {
  case list {
  | [] -> False
  | [head | rest] -> head == elem || member(rest, elem)
  }
}

pub fn head(list) {
  case list {
  | [] -> Error(Empty)
  | [x | xs] -> Ok(x)
  }
}

pub fn tail(list) {
  case list {
  | [] -> Error(Empty)
  | [x | xs] -> Ok(xs)
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

pub fn do_traverse(list, fun, acc) {
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
  case n == 0 {
  | True -> list
  | False ->
      case list {
      | [] -> []
      | [x | xs] -> drop(xs, n - 1)
      }
  }
}

fn do_take(list, n, acc) {
  case n == 0 {
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

pub fn foldl(list, acc, fun) {
  case list {
  | [] -> acc
  | [x | rest] -> foldl(rest, fun(x, acc), fun)
  }
}

pub fn foldr(list, acc, fun) {
  case list {
  | [] -> acc
  | [x | rest] -> fun(x, foldr(rest, acc, fun))
  }
}

