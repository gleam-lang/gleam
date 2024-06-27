use crate::assert_js;

#[test]
fn custom_type_constructor_imported_and_aliased() {
    assert_js!(
        ("package", "other_module", "pub type T { A }"),
        r#"import other_module.{A as B}

pub const local = B
"#,
    );
}

#[test]
fn imported_aliased_ok() {
    assert_js!(
        r#"import gleam.{Ok as Y}

pub type X {
  Ok
}

pub const y = Y
"#,
    );
}

#[test]
fn imported_ok() {
    assert_js!(
        r#"import gleam

pub type X {
  Ok
}

pub const y = gleam.Ok
"#,
    );
}
