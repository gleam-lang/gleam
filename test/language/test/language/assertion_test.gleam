pub fn let_assert_ok_discard_test() {
  let result = {
    let x = ok_one()
    let assert Ok(_) = x
  }
  assert Ok(1) == result
}

pub fn let_assert_ok_bind_test() {
  let x = ok_one()
  let assert Ok(x) = x
  assert 1 == x
}

fn ok_one() -> Result(Int, a) {
  Ok(1)
}
