// SPDX-License-Identifier: Apache-2.0
// SPDX-FileCopyrightText: 2021 The Gleam contributors

pub type ShadowPerson {
  ShadowPerson(age: Int)
}

pub fn celebrate_birthday(person: ShadowPerson) -> ShadowPerson {
  ShadowPerson(age: person.age + 1)
}
