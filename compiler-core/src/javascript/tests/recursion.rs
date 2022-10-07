use crate::assert_js;

#[test]
fn tco() {
    assert_js!(
        r#"
pub fn main(x) {
  case x {
    0 -> Nil
    _ -> main(x - 1)
  }
}
"#
    );
}

#[test]
fn tco_case_block() {
    assert_js!(
        r#"
pub fn main(x) {
  case x {
    0 -> Nil
    _ -> {
      let y = x
      main(y - 1)
    }
  }
}
"#
    );
}

// https://github.com/gleam-lang/gleam/issues/1779
#[test]
fn not_tco_due_to_assignment() {
    assert_js!(
        r#"
pub fn main(x) {
  let z = {
    let y = x
    main(y - 1)
  }
  z
}
"#
    );
}
