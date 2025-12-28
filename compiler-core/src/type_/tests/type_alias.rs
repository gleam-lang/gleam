use crate::{assert_module_error, assert_module_infer};

#[test]
fn alias_dep() {
    assert_module_infer!(
        r#"
type E = #(F, C)
type F = fn(CustomA) -> CustomB(B)
type A = Int
type B = C
type C = CustomA
type D = CustomB(C)

type CustomA {
  CustomA()
}
type CustomB(a) {
  CustomB(a)
}
"#,
        vec![],
    )
}

#[test]
fn custom_type_dep() {
    assert_module_infer!(
        r#"
type A {
    A(Blah)
}

type Blah {
    B(Int)
}
"#,
        vec![],
    )
}

#[test]
fn alias_cycle() {
    assert_module_error!(
        r#"
type A = B
type B = C
type C = D
type D = E
type E = A
"#
    );
}

#[test]
fn alias_direct_cycle() {
    assert_module_error!(
        r#"
type A = #(A, A)
"#
    );
}

#[test]
fn alias_different_module() {
    assert_module_infer!(
        ("other", "pub type Blah = Bool"),
        r#"
            import other

            type Blah = #(other.Blah, other.Blah)
        "#,
        vec![],
    );
}

#[test]
fn duplicate_parameter() {
    assert_module_error!(
        r#"
type A(a, a) =
  List(a)
"#
    );
}

#[test]
fn unused_parameter() {
    assert_module_error!(
        r#"
type A(a) =
  Int
"#
    );
}

#[test]
fn type_alias_error_does_not_stop_analysis() {
    // Both these aliases have errors! We do not stop on the first one.
    assert_module_error!(
        r#"
type UnusedParameter(a) =
  Int

type UnknownType =
  Dunno
"#
    );
}

#[test]
fn duplicate_variable_error_does_not_stop_analysis() {
    // Both these aliases have errors! We do not stop on the first one.
    assert_module_error!(
        r#"
type Two(a, a) =
  #(a, a)

type UnknownType =
  Dunno
"#
    );
}

// https://github.com/gleam-lang/gleam/issues/3191
#[test]
fn both_errors_are_shown() {
    // The alias has an error, and it causes the function to have an error as it
    // refers to the type that does not exist.
    assert_module_error!(
        r#"
type X =
  List(Intt)

fn example(a: X) {
  todo
}
"#
    );
}

#[test]
fn conflict_with_import() {
    // We cannot declare a type with the same name as an imported type
    assert_module_error!(
        ("wibble", "pub type Wobble = String"),
        "import wibble.{type Wobble} type Wobble = Int",
    );
}
