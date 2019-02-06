// Gosh! A COMMENT
//
pub fn hello() {
  "Hello, world!"
}

pub fn times(i, f) {
  case i {
  | 1 -> f()
  | i -> f() times(i - 1, f)
  }
}
