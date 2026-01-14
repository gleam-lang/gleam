pub fn prepend_1_test() {
  let left = [1, ..[]]
  let right = [1]
  assert left == right
}

pub fn prepend_2_test() {
  let left = [1, 2, ..[]]
  let right = [1, 2]
  assert left == right
}

pub fn prepend_3_test() {
  let left = [1, 2, ..[3]]
  let right = [1, 2, 3]
  assert left == right
}

pub fn prepend_4_test() {
  let left = [1, 2, ..[3, 4]]
  let right = [1, 2, 3, 4]
  assert left == right
}
