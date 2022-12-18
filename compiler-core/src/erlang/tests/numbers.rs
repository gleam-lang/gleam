use crate::assert_erl;

#[test]
fn numbers_with_underscores() {
    assert_erl!(
        r#"
pub fn main() {
  100_000
  100_000.00101
}
"#
    );

    assert_erl!(
        r#"
const i = 100_000
const f = 100_000.00101
pub fn main() {
  i
  f
}
"#
    );

    assert_erl!(
        r#"
pub fn main() {
  let 100_000 = 1
  let 100_000.00101 = 1.
  1
}
"#
    );
}

#[test]
fn numbers_with_scientific_notation() {
    assert_erl!(
        r#"
const i = 100.001e523
const j = -100.001e-523

const k = 100.001e1_230
const l = -100.001e-1_230

const m = 100.001e123_456_789
const n = -100.001e-123_456_789

pub fn main() {
    i
    j
    k
    l
    m
    n
}
"#
    );
}
