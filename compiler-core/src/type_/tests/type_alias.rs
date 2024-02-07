use crate::{assert_infer_with_module, assert_module_error, assert_module_infer};

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
    assert_infer_with_module!(
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
