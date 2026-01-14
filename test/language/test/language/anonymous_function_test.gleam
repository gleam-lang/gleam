// https://github.com/gleam-lang/gleam/issues/1637
pub fn anonymous_function_test() {
  let f = fn(x) {
    let x = x
    x
  }
  assert 1 == f(1)
}

pub fn immutable_scope_test() {
  let x = 1
  let f = fn() { x }
  let x = 2
  assert f() == 1
  assert x == 2
}

type FnBox {
  FnBox(f: fn(Int) -> Int)
}

pub fn call_record_access_test() {
  let b = FnBox(f: fn(x) { x })
  assert b.f(5) == 5
}

pub fn call_tuple_access_test() {
  let t = #(fn(x) { x })
  assert t.0(5) == 5
}
