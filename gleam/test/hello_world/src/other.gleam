pub enum Empty =
  | Empty

// Using the Erlang C BIF implementation.
//
pub external fn reverse(List(a)) -> List(a) = "lists" "reverse"

// pub external fn reverse2(List(a)) -> List(a) = "lists" "reverse"
// pub external fn reverse3(List(a)) -> List(a) = "lists" "reverse"
// pub external fn reverse4(List(a)) -> List(a) = "lists" "reverse"
// pub external fn reverse5(List(a)) -> List(a) = "lists" "reverse"
// pub external fn reverse6(List(a)) -> List(a) = "lists" "reverse"
// pub external fn reverse7(List(a)) -> List(a) = "lists" "reverse"
// pub external fn reverse8(List(a)) -> List(a) = "lists" "reverse"
// pub external fn reverse9(List(a)) -> List(a) = "lists" "reverse"
// pub external fn reverse10(List(a)) -> List(a) = "lists" "reverse"

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
