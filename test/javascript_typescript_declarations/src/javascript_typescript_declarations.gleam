import gleam/io

@external(javascript, "./functionality.ts", "greet")
pub fn greet(name: String) -> Nil

pub fn main() -> Nil {
  io.println("Hello from javascript_typescript_declarations!")
}
