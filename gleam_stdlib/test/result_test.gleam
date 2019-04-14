import expect
import result

pub fn is_ok_test() {
  result:is_ok(Ok(1)) |> expect:true
  result:is_ok(Error(1)) |> expect:false
}

pub fn is_error_test() {
  result:is_error(Ok(1))
    |> expect:false

  result:is_error(Error(1))
    |> expect:true
}

pub fn map_test() {
  Ok(1)
    |> result:map(_, fn(x) { x + 1 })
    |> expect:equal(_, Ok(2))

  Ok(1)
    |> result:map(_, fn(_) { "2" })
    |> expect:equal(_, Ok("2"))

  Error(1)
    |> result:map(_, fn(x) { x + 1 })
    |> expect:equal(_, Error(1))
}

pub fn map_error_test() {
  Ok(1)
    |> result:map_error(_, fn(x) { x + 1 })
    |> expect:equal(_, Ok(1))

  Error(1)
    |> result:map_error(_, fn(x) { x + 1 })
    |> expect:equal(_, Error(2))
}

pub fn flatten_test() {
  Ok(Ok(1))
    |> result:flatten
    |> expect:equal(_, Ok(1))

  Ok(Error(1))
    |> result:flatten
    |> expect:equal(_, Error(1))

  Error(1)
    |> result:flatten
    |> expect:equal(_, Error(1))

  Error(Error(1))
    |> result:flatten
    |> expect:equal(_, Error(Error(1)))
}

pub fn then_test() {
  Error(1)
    |> result:then(_, fn(x) { Ok(x + 1) })
    |> expect:equal(_, Error(1))

  Ok(1)
    |> result:then(_, fn(x) { Ok(x + 1) })
    |> expect:equal(_, Ok(2))

  Ok(1)
    |> result:then(_, fn(_) { Ok("type change") })
    |> expect:equal(_, Ok("type change"))

  Ok(1)
    |> result:then(_, fn(_) { Error(1) })
    |> expect:equal(_, Error(1))
}

pub fn unwrap_test() {
  Ok(1)
    |> result:unwrap(_, 50)
    |> expect:equal(_, 1)

  Error("nope")
    |> result:unwrap(_, 50)
    |> expect:equal(_, 50)
}
