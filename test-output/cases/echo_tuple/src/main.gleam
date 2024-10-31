pub fn main() {
  echo #()
  echo #(True, 1, "hello")
  target_specific()
}

@target(erlang)
pub type Wibble {
  Wibble
}

@target(erlang)
pub fn target_specific() {
  // On erlang a tuple with an enum variant as the first item is printed as a
  // type with fields.
  echo #(Wibble, 1, "hello")
}

@target(javascript)
pub fn target_specific() {
  Nil
}
