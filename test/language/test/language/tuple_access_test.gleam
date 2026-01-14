type ContainsTuple {
  ContainsTuple(data: #(Int, #(Int, Person)))
}

type Person {
  Person(name: String, age: Int, country: String)
}

// https://github.com/gleam-lang/gleam/issues/1980
pub fn access_regular_tuple_item_test() {
  let tup = #(3, 4, 5)
  let x = tup.0
  let y = tup.1
  let z = tup.2
  assert #(z, y, x) == #(5, 4, 3)
}

pub fn access_nested_tuple_item_test() {
  let tup = #(#(4, 5), #(6, 7))
  assert #(tup.0.1, tup.1.1, tup.1.0, tup.0.0) == #(5, 7, 6, 4)
}

pub fn access_deeply_nested_tuple_item_test() {
  let tup = #(#(5, #(6, 7, #(8))))
  assert tup.0.1.2.0 == 8
}

pub fn access_nested_struct_in_a_tuple_item_test() {
  let tup = #(Person("Quinn", 27, "Canada"), Person("Nikita", 99, "Internet"))
  assert { tup.0 }.name == "Quinn"
}

pub fn access_nested_tuple_in_a_struct_test() {
  let person = Person("Nikita", 99, "Internet")
  let container = ContainsTuple(#(5, #(6, person)))
  assert { container.data.1.1 }.name == "Nikita"
}

pub fn access_tuple_then_struct_then_tuple_test() {
  let person = Person("Nikita", 99, "Internet")
  let container = ContainsTuple(#(5, #(6, person)))
  let tup = #(container)
  assert { tup.0 }.data.0 == 5
}
