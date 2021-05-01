pub type Person {
  Person(age: Int)
}

pub fn celebrate_birthday(person: Person) -> Person {
  Person(age: person.age + 1)
}
