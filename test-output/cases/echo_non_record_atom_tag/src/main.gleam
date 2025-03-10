pub fn main() {
  echo #(to_atom("UP"), 1, 2)
  echo #(to_atom("down"), 12.34)
  echo #(to_atom("Both"), "ok")
}

pub type Atom

@external(erlang, "erlang", "binary_to_atom")
pub fn to_atom(string: String) -> Atom
