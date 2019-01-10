external fn rev(List(a)) -> List(a) = 'lists' 'reverse'

pub fn greet(name) {
  case name {
  | "your mate Dave" -> ["Oi! Dave! What are you doing here? Go on. Clear off. Haven't you got better things to do?"]
  | name -> ["Hello, ", name, "!"]
  }
}

pub fn list() {
  case 1 {
  | 2 ->
      "ok"

  | name ->
      123123123
      "one two three. one two three. one two three. one two three. one two three."
  }
}

pub fn x() {
  {'ok', 2}
}

pub fn run() {
  x = 1
  x = 2 + 3
  x
}
