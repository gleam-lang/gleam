pub fn main() {
  echo <<>>
  echo <<1, 2, 3>>
  target_specific()
}

@target(erlang)
pub fn target_specific() {
  // On erlang we also check non byte aligned bit arrays
  echo <<1, 2, 3:2>>
}

@target(javascript)
pub fn target_specific() {
  Nil
}
