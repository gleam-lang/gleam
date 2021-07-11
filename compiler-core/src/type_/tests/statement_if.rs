use crate::assert_module_infer;

#[test]
fn excluded_error() {
    assert_module_infer!(
        "if javascript { pub type X = Y }
pub const x = 1
",
        vec![("x", "Int")],
    );
}

#[test]
fn alias() {
    assert_module_infer!(
        "if erlang { pub type X = Int }
pub const x: X = 1
",
        vec![("x", "Int")],
    );
}

#[test]
fn alias_in_block() {
    assert_module_infer!(
        "if erlang { 
  pub type X = Int 
  pub const x: X = 1
}
",
        vec![("x", "Int")],
    );
}
