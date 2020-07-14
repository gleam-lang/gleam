import should
import record_update

pub type Person {
  Person(
    name: String,
    age: Int,
    country: String
  )
}

pub fn record_update_test() {
  let past = Person("Quinn", 27, "Canada")
  let present = Person(..past, country: "USA", age: past.age + 1)

  should.equal(present, Person("Quinn", 28, "USA"))

  let module_box = record_update.Box(5)
  let updated = record_update.Box(..module_box, value: 6)

  should.equal(updated, record_update.Box(6))
}
