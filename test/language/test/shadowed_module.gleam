pub type ShadowPerson {
  ShadowPerson(age: Int)
}

pub fn celebrate_birthday(person: ShadowPerson) -> ShadowPerson {
  ShadowPerson(age: person.age + 1)
}
