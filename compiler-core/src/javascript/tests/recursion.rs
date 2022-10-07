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
