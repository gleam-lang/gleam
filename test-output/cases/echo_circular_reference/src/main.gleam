pub fn main() {
  echo circular_reference()
  Nil
}

type Thing

@external(javascript, "./main_ffi.mjs", "circular_reference")
fn circular_reference() -> Thing
