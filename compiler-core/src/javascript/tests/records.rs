use crate::assert_js;

#[test]
fn record_accessors() {
    // We can use record accessors for types with only one constructor
    assert_js!(
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
    assert_js!(
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
    assert_js!(
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
fn record_accessor_multiple_with_first_position_different_types() {
    // We can access fields on custom types with multiple variants
    // In positions other than the 1st field
    assert_js!(
        "
pub type Person {
    Teacher(name: Nil, age: Int)
    Student(name: String, age: Int)
}
pub fn get_age(person: Person) { person.age }"
    );
}

#[test]
fn record_accessor_multiple_variants_parameterised_types() {
    // We can access fields on custom types with multiple variants
    // In positions other than the 1st field
    assert_js!(
        "
pub type Person {
    Teacher(name: String, age: List(Int), title: String)
    Student(name: String, age: List(Int))
}
pub fn get_name(person: Person) { person.name }
pub fn get_age(person: Person) { person.age }"
    );
}
