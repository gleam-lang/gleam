const const_int = 5

const const_float = 1.0

const const_string = "Gleam"

const const_nil = Nil

const const_ok = Ok(1)

const const_list_empty = []

const const_list_1 = [1]

const const_list_2 = [1, 2]

pub fn int_test() {
  assert const_int == 5
}

pub fn float_test() {
  assert const_float == 1.0
}

pub fn string_test() {
  assert const_string == "Gleam"
}

pub fn nil_test() {
  assert const_nil == Nil
}

pub fn ok_test() {
  assert const_ok == Ok(1)
}

pub fn list_empty_test() {
  assert const_list_empty == []
}

pub fn list_1_test() {
  assert const_list_1 == [1]
}

pub fn list_2_test() {
  assert const_list_2 == [1, 2]
}
