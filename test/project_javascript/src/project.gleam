pub fn main() {
  println("Hello, from project_javascript!")
}

@external(erlang, "erlang", "display")
@external(javascript, "", "globalThis.console.log")
fn println(a: String) -> Nil
