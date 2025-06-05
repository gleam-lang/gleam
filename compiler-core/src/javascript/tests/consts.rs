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

#[test]
fn constant_constructor_gets_pure_annotation() {
    assert_js!(
        r#"
pub type X {
  X(Int, List(String))
}

pub const x = X(1, ["1"])
pub const y = X(1, [])
        "#
    );
}

#[test]
fn constant_list_with_constructors_gets_pure_annotation() {
    assert_js!(
        r#"
pub type X {
  X(Int, List(String))
}

pub const x = [X(1, ["1"])]
pub const y = [X(1, ["1"])]
        "#
    );
}

#[test]
fn constant_tuple_with_constructors_gets_pure_annotation() {
    assert_js!(
        r#"
pub type X {
  X(Int, List(String))
}

pub const x = #(X(1, ["1"]))
pub const y = #(X(1, ["1"]))
        "#
    );
}

#[test]
fn literal_int_does_not_get_constant_annotation() {
    assert_js!("pub const a = 1");
}

#[test]
fn literal_float_does_not_get_constant_annotation() {
    assert_js!("pub const a = 1.1");
}

#[test]
fn literal_string_does_not_get_constant_annotation() {
    assert_js!("pub const a = \"1\"");
}

#[test]
fn literal_bool_does_not_get_constant_annotation() {
    assert_js!(
        "
        pub const a = True
        pub const b = False
    "
    );
}

#[test]
fn literal_list_does_not_get_constant_annotation() {
    assert_js!("pub const a = [1, 2, 3]");
}

#[test]
fn literal_tuple_does_not_get_constant_annotation() {
    assert_js!("pub const a = #(1, 2, 3)");
}

#[test]
fn literal_nil_does_not_get_constant_annotation() {
    assert_js!("pub const a = Nil");
}

// https://github.com/lpil/decode/pull/6
#[test]
fn constructor_function_in_constant() {
    assert_js!("pub const a = Ok");
}
