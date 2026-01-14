import record_update

type Person {
  Person(name: String, age: Int, country: String)
}

type MixedRecord {
  MixedRecord(Int, Float, labelled_1: Int, labelled_2: String)
}

fn id(x) {
  x
}

pub fn unqualified_record_update_test() {
  let past = Person("Quinn", 27, "Canada")
  let present = Person(..past, country: "USA", age: past.age + 1)
  assert present == Person("Quinn", 28, "USA")
}

pub fn qualified_record_update_test() {
  let module_box = record_update.Box("a", 5)
  let updated = record_update.Box(..module_box, value: 6)
  assert updated == record_update.Box("a", 6)
}

// https://github.com/gleam-lang/gleam/issues/1379
pub fn pipe_in_record_update_test() {
  let module_box = record_update.Box("a", 5)
  let updated =
    record_update.Box(
      ..module_box,
      value: 6
        |> id,
    )
  assert updated == record_update.Box("a", 6)
}

pub fn unlabelled_field_in_record_update_test() {
  let record = MixedRecord(1, 3.14, labelled_1: 3982, labelled_2: "Something")
  let updated = MixedRecord(..record, labelled_1: 12)
  assert updated == MixedRecord(1, 3.14, 12, "Something")
}
