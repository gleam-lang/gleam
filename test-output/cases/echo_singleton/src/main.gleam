import thing

pub fn main() {
  echo #(1, singleton(), singleton())
  Nil
}

@external(javascript, "./main_ffi.mjs", "singleton")
fn singleton() -> thing.Thing {
  thing.Thing
}
