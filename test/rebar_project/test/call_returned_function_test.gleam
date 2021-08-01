import should

type FnBox {
  FnBox(f: fn(Int) -> Int)
}

pub fn call_record_access_function_test() {
  let b = FnBox(f: fn(x) { x })

  should.equal(5, b.f(5))
}

pub fn call_tuple_access_function_test() {
  let t = #(fn(x) { x })

  should.equal(5, t.0(5))
}
