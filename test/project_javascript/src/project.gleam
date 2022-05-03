pub fn main() {
  println("Hello, from project_javascript!")
}

if javascript {
  external fn println(String) -> Nil =
    "" "globalThis.console.log"
}

if erlang {
  external fn println(String) -> Nil =
    "erlang" "display"
}
