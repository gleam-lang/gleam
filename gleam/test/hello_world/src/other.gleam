pub enum Empty =
  | Empty

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

pub struct Point {
  x: Int
  y: Int
}

fn run() {
  let origin = Point(x: 4, y: 8)
  let cursor = Point(x: 7, y: 6)
  "ok!"
}
