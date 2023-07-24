pub fn main() {
  println("Hello, from project_javascript!")
}

@external(erlang, "erlang", "display")
@external(javascript, "./project_ffi.mjs", "log")
fn println(a: String) -> Nil
