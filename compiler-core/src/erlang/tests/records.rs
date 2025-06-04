use crate::assert_erl;
use crate::erlang::*;
use crate::type_;

#[test]
fn basic() {
    insta::assert_snapshot!(record_definition(
        "PetCat",
        &[
            ("name", type_::tuple(vec![])),
            ("is_cute", type_::tuple(vec![]))
        ]
    ));
}

#[test]
fn reserve_words() {
    // Reserved words are escaped in record names and fields
    insta::assert_snapshot!(record_definition(
        "div",
        &[
            ("receive", type_::int()),
            ("catch", type_::tuple(vec![])),
            ("unreserved", type_::tuple(vec![]))
        ]
    ));
}

#[test]
fn type_vars() {
    // Type vars are printed as `any()` because records don't support generics
    insta::assert_snapshot!(record_definition(
        "PetCat",
        &[
            ("name", type_::generic_var(1)),
            ("is_cute", type_::unbound_var(1)),
            ("linked", type_::link(type_::int()))
        ]
    ));
}

#[test]
fn module_types() {
    // Types are printed with module qualifiers
    let module_name = "name".into();
    insta::assert_snapshot!(record_definition(
        "PetCat",
        &[(
            "name",
            Arc::new(Type::Named {
                publicity: Publicity::Public,
                package: "package".into(),
                module: module_name,
                name: "my_type".into(),
                args: vec![],
                inferred_variant: None,
            })
        )]
    ));
}

#[test]
fn long_definition_formatting() {
    // Long definition formatting
    insta::assert_snapshot!(record_definition(
        "PetCat",
        &[
            ("name", type_::generic_var(1)),
            ("is_cute", type_::unbound_var(1)),
            ("linked", type_::link(type_::int())),
            (
                "whatever",
                type_::list(type_::tuple(vec![
                    type_::nil(),
                    type_::list(type_::tuple(vec![type_::nil(), type_::nil(), type_::nil()])),
                    type_::nil(),
                    type_::list(type_::tuple(vec![type_::nil(), type_::nil(), type_::nil()])),
                    type_::nil(),
                    type_::list(type_::tuple(vec![type_::nil(), type_::nil(), type_::nil()])),
                ]))
            ),
        ]
    ));
}

#[test]
fn record_accessors() {
    // We can use record accessors for types with only one constructor
    assert_erl!(
        r#"
pub type Person { Person(name: String, age: Int) }
pub fn get_age(person: Person) { person.age }
pub fn get_name(person: Person) { person.name }
"#
    );
}

#[test]
fn record_accessor_multiple_variants() {
    // We can access fields on custom types with multiple variants
    assert_erl!(
        "
pub type Person {
    Teacher(name: String, title: String)
    Student(name: String, age: Int)
}
pub fn get_name(person: Person) { person.name }"
    );
}

#[test]
fn record_accessor_multiple_variants_positions_other_than_first() {
    // We can access fields on custom types with multiple variants
    // In positions other than the 1st field
    assert_erl!(
        "
pub type Person {
    Teacher(name: String, age: Int, title: String)
    Student(name: String, age: Int)
}
pub fn get_name(person: Person) { person.name }
pub fn get_age(person: Person) { person.age }"
    );
}

#[test]
fn record_accessor_multiple_variants_parameterised_types() {
    // We can access fields on custom types with multiple variants
    // In positions other than the 1st field
    assert_erl!(
        "
pub type Person {
    Teacher(name: String, age: List(Int), title: String)
    Student(name: String, age: List(Int))
}
pub fn get_name(person: Person) { person.name }
pub fn get_age(person: Person) { person.age }"
    );
}

#[test]
fn record_accessor_multiple_with_first_position_different_types() {
    // We can access fields on custom types with multiple variants
    // In positions other than the 1st field
    assert_erl!(
        "
pub type Person {
    Teacher(name: Nil, age: Int)
    Student(name: String, age: Int)
}
pub fn get_age(person: Person) { person.age }"
    );
}

#[test]
fn record_spread() {
    // Test binding to a record field with the spread operator
    assert_erl!(
        r#"
pub type Triple {
    Triple(a: Int, b: Int, c: Int)
}

pub fn main() {
  let triple = Triple(1,2,3)
  let Triple(the_a, ..) = triple
  the_a
}
"#
    );
}

#[test]
fn record_spread1() {
    // Test binding to a record field with the spread operator
    // Test binding to a record field with the spread operator and a labelled argument
    assert_erl!(
        r#"
pub type Triple {
  Triple(a: Int, b: Int, c: Int)
}

pub fn main() {
  let triple = Triple(1,2,3)
  let Triple(b: the_b, ..) = triple
  the_b
}
"#
    );
}

#[test]
fn record_spread2() {
    // Test binding to a record field with the spread operator with both a labelled argument and a positional argument
    assert_erl!(
        r#"
pub type Triple {
  Triple(a: Int, b: Int, c: Int)
}

pub fn main() {
  let triple = Triple(1,2,3)
  let Triple(the_a, c: the_c, ..) = triple
  the_c
}
"#
    );
}

#[test]
fn record_spread3() {
    // Test binding to a record field with the spread operator in a match
    assert_erl!(
        r#"
pub type Triple {
  Triple(a: Int, b: Int, c: Int)
}

pub fn main() {
  let triple = Triple(1,2,3)
  case triple {
    Triple(b: the_b, ..) -> the_b
  }
}
"#
    );
}

#[test]
fn record_updates() {
    // Record updates
    assert_erl!(
        r#"
pub type Person { Person(name: String, age: Int) }

pub fn main() {
    let p = Person("Quinn", 27)
    let new_p = Person(..p, age: 28)
    new_p
}
"#
    );
}

#[test]
fn record_updates1() {
    // Record updates with field accesses
    assert_erl!(
        r#"
pub type Person { Person(name: String, age: Int) }

pub fn main() {
    let p = Person("Quinn", 27)
    let new_p = Person(..p, age: p.age + 1)
    new_p
}
"#
    );
}

#[test]
fn record_updates2() {
    // Record updates with multiple fields
    assert_erl!(
        r#"
pub type Person { Person(name: String, age: Int) }

pub fn main() {
    let p = Person("Quinn", 27)
    let new_p = Person(..p, age: 28, name: "Riley")
    new_p
}
"#
    );
}

#[test]
fn record_updates3() {
    // Record updates when record is returned from function
    assert_erl!(
        r#"
pub type Person { Person(name: String, age: Int) }

pub fn main() {
    let new_p = Person(..return_person(), age: 28)
    new_p
}

fn return_person() {
    Person("Quinn", 27)
}
"#
    );
}

#[test]
fn record_updates4() {
    // Record updates when record is field on another record
    assert_erl!(
        r#"
pub type Car { Car(make: String, model: String, driver: Person) }
pub type Person { Person(name: String, age: Int) }

pub fn main() {
    let car = Car(make: "Amphicar", model: "Model 770", driver: Person(name: "John Doe", age: 27))
    let new_p = Person(..car.driver, age: 28)
    new_p
}
"#
    );
}

#[test]
fn record_constants() {
    assert_erl!(
        "pub type Test { A }
const some_test = A
pub fn a() { A }"
    );
}

// https://github.com/gleam-lang/gleam/issues/1698
#[test]
fn pipe_update_subject() {
    assert_erl!(
        "pub type Thing {
  Thing(a: Int, b: Int)
}

pub fn identity(x) { x }

pub fn main() {
  let thing = Thing(1, 2)
  Thing(..thing |> identity, b: 1000)
}"
    );
}

// https://github.com/gleam-lang/gleam/issues/1698
#[test]
fn record_access_block() {
    assert_erl!(
        "pub type Thing {
  Thing(a: Int, b: Int)
}

pub fn main() {
  {
    let thing = Thing(1, 2)
    thing
  }.a
}"
    );
}

// https://github.com/gleam-lang/gleam/issues/1981
#[test]
fn imported_qualified_constructor_as_fn_name_escape() {
    assert_erl!(
        ("other_package", "other_module", "pub type Let { Let(Int) }"),
        "import other_module

pub fn main() {
  other_module.Let
}"
    );
}

#[test]
fn nested_record_update() {
    assert_erl!(
        "pub type Wibble {
  Wibble(a: Int, b: Wobble, c: Int)
}

pub type Wobble {
  Wobble(a: Int, b: Int)
}

pub fn main() {
  let base = Wibble(1, Wobble(2, 3), 4)
  Wibble(..base, b: Wobble(..base.b, b: 5))
}"
    );
}

#[test]
fn nested_record_update_with_blocks() {
    assert_erl!(
        "pub type A { A(b: B) }
pub type B { B(c: C) }
pub type C { C(val: Int) }

pub fn main(a: A) {
    A(..a, b: {
        B(..a.b, c: {
            C(..a.b.c, val: 0)
        })
    })
}"
    )
}

#[test]
fn private_unused_records() {
    assert_erl!(
        "type A { A(inner: Int) }
type B { B(String) }
type C { C(Int) }

pub fn main(x: Int) -> Int {
  let a = A(x)
  a.inner
}
"
    )
}
