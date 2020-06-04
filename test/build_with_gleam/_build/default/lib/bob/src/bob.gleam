import helper/one

pub type CanWeFixIt =
  Bool

pub type Box {
  Box(inner: one.One)
}

pub fn can_we_fix_it() -> String {
  "Yes we can!"
}
