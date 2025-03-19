use crate::assert_module_infer;

#[test]
fn bug_3629() {
    assert_module_infer!(
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
