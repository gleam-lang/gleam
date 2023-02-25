use crate::assert_erl;

#[test]
fn plain() {
    assert_erl!(
        r#"
pub fn main() {
  panic
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
  |> panic
}
    "#
    );
}
