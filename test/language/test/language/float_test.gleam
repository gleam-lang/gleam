pub fn addition_1_test() {
  assert 0.0 +. 0.0 == 0.0
}

pub fn addition_2_test() {
  assert 1.0 +. 1.0 == 2.0
}

pub fn addition_3_test() {
  assert 5.0 +. 1.0 == 6.0
}

pub fn addition_4_test() {
  assert 1.0 +. 3.0 == 4.0
}

pub fn addition_5_test() {
  assert 1.0 +. -3.0 == -2.0
}

pub fn subtraction_1_test() {
  assert 0.0 -. 0.0 == 0.0
}

pub fn subtraction_2_test() {
  assert 1.0 -. 1.0 == 0.0
}

pub fn subtraction_3_test() {
  assert 5.0 -. 1.0 == 4.0
}

pub fn subtraction_4_test() {
  assert 1.0 -. 3.0 == -2.0
}

pub fn subtraction_5_test() {
  assert 1.0 -. -3.0 == 4.0
}

pub fn subtraction_6_test() {
  assert 0.5 -. 0.0 == 0.5
}

pub fn subtraction_7_test() {
  assert 1.0 -. 4.5 == -3.5
}

pub fn multiplication_1_test() {
  assert 0.0 *. 0.0 == 0.0
}

pub fn multiplication_2_test() {
  assert 1.0 *. 1.0 == 1.0
}

pub fn multiplication_3_test() {
  assert 2.0 *. 2.0 == 4.0
}

pub fn multiplication_4_test() {
  assert 2.0 *. 4.0 == 8.0
}

pub fn multiplication_5_test() {
  assert -2.0 *. 4.0 == -8.0
}

pub fn multiplication_6_test() {
  assert 2.0 *. -4.0 == -8.0
}

pub fn precedence_1_test() {
  assert 2.0 *. 2.0 +. 3.0 == 7.0
}

pub fn precedence_2_test() {
  assert 2.0 +. 2.0 *. 3.0 == 8.0
}

pub fn precedence_3_test() {
  assert 2.0 *. { 2.0 +. 3.0 } == 10.0
}

pub fn precedence_4_test() {
  assert { 2.0 +. 2.0 } *. 3.0 == 12.0
}

pub fn scientific_notation_addition_1_test() {
  assert 0.0e0 +. 0.0 == 0.0
}

pub fn scientific_notation_addition_2_test() {
  assert 0.0 +. 0.0e0 == 0.0
}

pub fn scientific_notation_addition_3_test() {
  assert 0.0e-0 +. 0.0 == 0.0
}

pub fn scientific_notation_addition_4_test() {
  assert 0.0 +. 0.0e-0 == 0.0
}

pub fn scientific_notation_addition_5_test() {
  assert 1.0e3 +. 1.0 == 1001.0
}

pub fn scientific_notation_addition_6_test() {
  assert 5.0 +. 1.0e3 == 1005.0
}

pub fn scientific_notation_addition_7_test() {
  assert 1.0e5 +. -3.0e5 == -200_000.0
}

pub fn scientific_notation_addition_8_test() {
  assert 1.0e50 +. -1.0e50 == 0.0
}

pub fn scientific_notation_addition_9_test() {
  assert 1.0e-3 +. 1.0 == 1.001
}

pub fn scientific_notation_addition_10_test() {
  assert 5.0 +. 1.0e-3 == 5.001
}

pub fn scientific_notation_addition_11_test() {
  assert 10.0e-2 +. -3.0e-2 == 0.07
}

pub fn scientific_notation_subtraction_1_test() {
  assert 0.0e0 -. 0.0 == 0.0
}

pub fn scientific_notation_subtraction_2_test() {
  assert 0.0 -. 0.0e0 == 0.0
}

pub fn scientific_notation_subtraction_3_test() {
  assert 0.0e-0 -. 0.0 == 0.0
}

pub fn scientific_notation_subtraction_4_test() {
  assert 0.0 -. 0.0e-0 == 0.0
}

pub fn scientific_notation_subtraction_5_test() {
  assert 1.0e3 -. 1.0 == 999.0
}

pub fn scientific_notation_subtraction_6_test() {
  assert 5.0 -. 1.0e3 == -995.0
}

pub fn scientific_notation_subtraction_7_test() {
  assert 1.0e5 -. 1.0e5 == 0.0
}

pub fn scientific_notation_subtraction_8_test() {
  assert 1.0e-3 -. 1.0 == -0.999
}

pub fn scientific_notation_subtraction_9_test() {
  assert 5.0 -. 1.0e-3 == 4.999
}

pub fn scientific_notation_subtraction_10_test() {
  assert 10.0e-2 -. -3.0e-2 == 0.13
}

pub fn scientific_notation_multiplication_1_test() {
  assert 0.0e0 *. 0.0 == 0.0
}

pub fn scientific_notation_multiplication_2_test() {
  assert 0.0 *. 0.0e0 == 0.0
}

pub fn scientific_notation_multiplication_3_test() {
  assert 0.0e-0 *. 0.0 == 0.0
}

pub fn scientific_notation_multiplication_4_test() {
  assert 0.0 *. 0.0e-0 == 0.0
}

pub fn scientific_notation_multiplication_5_test() {
  assert 2.0e1 *. 2.0e1 == 400.0
}

pub fn scientific_notation_multiplication_6_test() {
  assert 1.0e-5 *. 1.0e5 == 1.0
}

pub fn scientific_notation_multiplication_7_test() {
  assert 2.0e5 *. 2.0e-5 == 4.0
}

pub fn scientific_notation_multiplication_8_test() {
  assert 2.0e5 *. 4.0e-4 == 80.0
}

pub fn scientific_notation_multiplication_9_test() {
  assert 2.0e-5 *. 2.0e5 == 4.0
}

pub fn scientific_notation_multiplication_10_test() {
  assert 2.0e5 *. 4.0e-5 == 8.0
}

pub fn scientific_notation_multiplication_11_test() {
  assert -2.0e-5 *. 2.0e5 == -4.0
}

pub fn scientific_notation_multiplication_12_test() {
  assert -2.0e5 *. -4.0e-5 == 8.0
}

pub fn divide_by_2_test() {
  let left = 2.0
  let right = 2.0
  assert left /. right == 1.0
}

pub fn divide_by_0_test() {
  let left = 2.0
  let right = 0.0
  assert left /. right == 0.0
}
