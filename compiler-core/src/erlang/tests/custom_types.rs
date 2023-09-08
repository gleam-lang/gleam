use crate::assert_erl;

#[test]
fn phantom() {
    assert_erl!("pub type Map(k, v)");
}

#[test]
fn newtype_use() {
    assert_erl!(
        "
pub opaque type UserId {
  UserId(Int)
}

pub fn main() {
  UserId(1)
}
"
    );
}

#[test]
fn newtype_accessor() {
    assert_erl!(
        "
pub opaque type UserId {
  UserId(inner: Int)
}

pub fn main(x: UserId) {
  x.inner
}
"
    );
}

#[test]
fn newtype_pattern_match() {
    assert_erl!(
        "
pub opaque type UserId {
  UserId(inner: Int)
}

pub fn main(x: UserId) {
  let UserId(y) = x
  y
}
"
    );
}
