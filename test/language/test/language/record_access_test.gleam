type Person {
  Person(name: String, age: Int, country: String)
}

pub fn record_access_name_test() {
  let person = Person(name: "Quinn", age: 27, country: "Canada")
  assert person.name == "Quinn"
}

pub fn record_access_age_test() {
  let person = Person(name: "Quinn", age: 27, country: "Canada")
  assert person.age == 27
}

// https://github.com/gleam-lang/gleam/issues/1093
pub fn contextual_info_for_access_test() {
  let person = Person(name: "Quinn", age: 27, country: "Canada")
  let apply = fn(a, f) { f(a) }
  assert apply(person, fn(x) { x.name }) == "Quinn"
}
