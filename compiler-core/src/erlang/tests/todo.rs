use crate::assert_erl;

#[test]
fn plain() {
    assert_erl!(
        r#"
pub fn main() {
  todo
}
"#
    );
}

#[test]
fn named() {
    assert_erl!(
        r#"
pub fn main() {
  todo("testing")
}
"#
    );
}

#[test]
fn piped() {
    assert_erl!(
        r#"
     pub fn main(){
      "lets"
      |> todo("pipe")
      |> todo("other todo")
    }
    "#
    );
}
