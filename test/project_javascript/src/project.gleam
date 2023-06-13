pub fn main() {
  println("Hello, from project_javascript!")
}

@target(javascript)
external fn println(String) -> Nil =
  "" "globalThis.console.log"

@target(erlang)
external fn println(String) -> Nil =
  "erlang" "display"
