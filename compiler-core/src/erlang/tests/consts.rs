use crate::assert_erl;

// https://github.com/gleam-lang/gleam/issues/2163
#[test]
fn record_constructor() {
    assert_erl!(
        r#"
pub type X {
  X(Int)
}

pub const z = X

pub fn main() {
  z
}"#
    );
}

// https://github.com/gleam-lang/gleam/issues/2163
#[test]
fn record_constructor_in_tuple() {
    assert_erl!(
        r#"
pub type X {
  X(Int)
}

pub const z = #(X)

pub fn main() {
  z
}"#
    );
}

// https://github.com/gleam-lang/gleam/issues/2179
#[test]
fn const_type_variable() {
    assert_erl!(
        r#"
fn identity(a: a) -> a {
  a
}

const id: fn(a) -> a = identity
"#
    );
}

#[test]
fn const_generalise() {
    assert_erl!(
        r#"
fn identity(a: a) -> a {
a
}

const id  = identity

pub fn main(){
  let num  = id(1)
  let word = id("Word")
}"#
    );
}

#[test]
fn pub_const_equal_to_private_function() {
    assert_erl!(
        r#"
          fn identity(a) {
            a
          }

          pub const id = identity
        "#
    );
}

#[test]
fn pub_const_equal_to_record_with_private_function_field() {
    assert_erl!(
        r#"
          fn identity(a) {
            a
          }

          pub type Mapper(b) {
            Mapper(fn(b) -> b)
          }

          pub const id_mapper = Mapper(identity)
        "#
    );
}

#[test]
fn pub_const_equal_to_record_with_nested_private_function_field() {
    assert_erl!(
        r#"
          fn identity(a) {
            a
          }

          pub type Mapper(b) {
            Mapper(fn(b) -> b)
          }

          pub type Funcs(b) {
            Funcs(mapper: Mapper(b))
          }

          pub const id_mapper = Funcs(Mapper(identity))
        "#
    );
}

#[test]
fn use_unqualified_pub_const_equal_to_private_function() {
    assert_erl!(
        (
            "package",
            "mappers",
            r#"
              fn identity(a) {
                a
              }

              pub const id = identity
        "#
        ),
        r#"
          import mappers.{ id } as _

          pub fn main() {
            id
          }
        "#
    );
}

#[test]
fn use_qualified_pub_const_equal_to_private_function() {
    assert_erl!(
        (
            "package",
            "mappers",
            r#"
              fn identity(a) {
                a
              }

              pub const id = identity
        "#
        ),
        r#"
          import mappers

          pub fn main() {
            mappers.id
          }
        "#
    );
}

#[test]
fn use_unqualified_pub_const_equal_to_record_with_private_function_field() {
    assert_erl!(
        (
            "package",
            "mappers",
            r#"
              fn identity(a) {
                a
              }

              pub type Mapper(b) {
                Mapper(fn(b) -> b)
              }

              pub const id_mapper = Mapper(identity)
        "#
        ),
        r#"
          import mappers.{ id_mapper } as _

          pub fn main() {
            id_mapper
          }
        "#
    );
}

#[test]
fn use_qualified_pub_const_equal_to_record_with_private_function_field() {
    assert_erl!(
        (
            "package",
            "mappers",
            r#"
              fn identity(a) {
                a
              }

              pub type Mapper(b) {
                Mapper(fn(b) -> b)
              }

              pub const id_mapper = Mapper(identity)
        "#
        ),
        r#"
          import mappers

          pub fn main() {
            mappers.id_mapper
          }
        "#
    );
}
