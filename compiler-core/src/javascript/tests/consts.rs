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
