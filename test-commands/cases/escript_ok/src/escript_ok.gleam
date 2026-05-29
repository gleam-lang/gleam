pub fn main() -> Nil {
  print("Hello, Joe!\n")
  Nil
}

@external(erlang, "io", "format")
fn print(string: String) -> Atom

type Atom
