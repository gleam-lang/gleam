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
