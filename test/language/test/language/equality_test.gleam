pub fn list_eq_1_test() {
  let left = []
  let right = []
  assert left == right
}

pub fn list_eq_2_test() {
  let left = []
  let right = [0]
  assert left != right
}

pub fn list_eq_3_test() {
  let left = [0]
  let right = []
  assert left != right
}

pub fn list_eq_4_test() {
  let left = [0]
  let right = [0]
  assert left == right
}

pub fn list_neq_1_test() {
  let left = []
  let right = []
  assert left == right
}

pub fn list_neq_2_test() {
  let left = []
  let right = [0]
  assert left != right
}

pub fn list_neq_3_test() {
  let left = [0]
  let right = []
  assert left != right
}

pub fn list_neq_4_test() {
  let left = [0]
  let right = [0]
  assert left == right
}

pub fn int_eq_1_test() {
  let left = 0
  let right = 0
  assert left == right
}

pub fn int_neq_1_test() {
  let left = 0
  let right = 0
  assert left == right
}

pub fn int_eq_2_test() {
  let left = 1
  let right = 0
  assert left != right
}

pub fn int_neq_2_test() {
  let left = 1
  let right = 0
  assert left != right
}

pub fn int_eq_3_test() {
  let left = 1
  let right = 1
  assert left == right
}

pub fn int_neq_3_test() {
  let left = 1
  let right = 1
  assert left == right
}

pub fn bit_array_eq_1_test() {
  let left = <<>>
  let right = <<>>
  assert left == right
}

pub fn bit_array_neq_1_test() {
  let left = <<>>
  let right = <<>>
  assert left == right
}

pub fn bit_array_eq_2_test() {
  let left = <<1, 2>>
  let right = <<1, 2>>
  assert left == right
}

pub fn bit_array_neq_2_test() {
  let left = <<1, 2>>
  let right = <<1, 2>>
  assert left == right
}

pub fn bit_array_eq_3_test() {
  let left = <<1, 2>>
  let right = <<2>>
  assert left != right
}

pub fn bit_array_neq_3_test() {
  let left = <<1, 2>>
  let right = <<2>>
  assert left != right
}

pub fn record_ok_eq_1_test() {
  let left = Ok(1)
  let right = Ok(1)
  assert left == right
}

pub fn record_ok_neq_1_test() {
  let left = Ok(1)
  let right = Ok(1)
  assert left == right
}

pub fn record_ok_eq_2_test() {
  let left = Ok(2)
  let right = Ok(1)
  assert left != right
}

pub fn record_ok_neq_2_test() {
  let left = Ok(2)
  let right = Ok(1)
  assert left != right
}

pub fn record_error_eq_1_test() {
  let left = Error(1)
  let right = Error(1)
  assert left == right
}

pub fn record_error_neq_1_test() {
  let left = Error(1)
  let right = Error(1)
  assert left == right
}

pub fn record_error_eq_2_test() {
  let left = Error(2)
  let right = Error(1)
  assert left != right
}

pub fn record_error_neq_2_test() {
  let left = Error(2)
  let right = Error(1)
  assert left != right
}
