use crate::assert_infer_with_module;

#[test]
fn bug_3629() {
    assert_infer_with_module!(
        ("imported", "pub type Wibble"),
        r#"
import imported

pub type Exp {
  One(field: imported.Wibble)
  Two(field: imported.Wibble)
}

pub fn main() {
  let exp = One(todo)
  exp.field
}
        "#,
        vec![
            ("One", "fn(Wibble) -> Exp"),
            ("Two", "fn(Wibble) -> Exp"),
            ("main", "fn() -> Wibble")
        ],
    );
}
