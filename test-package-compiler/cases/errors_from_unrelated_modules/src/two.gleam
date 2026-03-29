import one

pub fn two_error() {
  // This module depends on `one` which itself has an error.
  // So this should not result in an error!!
  1 + False
}
