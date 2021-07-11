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

#[test]
fn generalising() {
    assert_module_infer!(
        "if erlang { 
  pub fn id(x) { x }
  pub fn x() { id(1) }
}
",
        vec![("id", "fn(a) -> a"), ("x", "fn() -> Int")],
    );
}

#[test]
fn excluded_generalising() {
    assert_module_infer!(
        "if javascript { 
  pub fn id(x) { x }
  pub fn x() { id(1) }
}
pub const y = 1
",
        vec![("y", "Int")],
    );
}

#[test]
fn included_const_ref_earlier() {
    assert_module_infer!(
        "if erlang { 
  const x = 1
}
pub fn main() { x }
",
        vec![("main", "fn() -> Int")],
    );
}

#[test]
fn included_const_ref_later() {
    assert_module_infer!(
        "pub fn main() { x }
if erlang { 
  const x = 1
}
",
        vec![("main", "fn() -> Int")],
    );
}
