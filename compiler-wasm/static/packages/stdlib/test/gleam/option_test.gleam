import gleam/should
import gleam/option.{None, Some}

pub fn all_test() {
  option.all([Some(1), Some(2), Some(3)])
  |> should.equal(Some([1, 2, 3]))

  option.all([Some(1), None, Some(3)])
  |> should.equal(None)
}

pub fn is_some_test() {
  option.is_some(Some(1))
  |> should.be_true

  option.is_some(None)
  |> should.be_false
}

pub fn is_none_test() {
  option.is_none(Some(1))
  |> should.be_false

  option.is_none(None)
  |> should.be_true
}

pub fn to_result_test() {
  option.to_result(Some(1), "possible_error")
  |> should.equal(Ok(1))

  option.to_result(None, "possible_error")
  |> should.equal(Error("possible_error"))
}

pub fn from_result_test() {
  option.from_result(Ok(1))
  |> should.equal(Some(1))

  option.from_result(Error("some_error"))
  |> should.equal(None)
}

pub fn unwrap_option_test() {
  option.unwrap(Some(1), 0)
  |> should.equal(1)

  option.unwrap(None, 0)
  |> should.equal(0)
}

pub fn map_option_test() {
  Some(1)
  |> option.map(fn(x) { x + 1 })
  |> should.equal(Some(2))

  Some(1)
  |> option.map(fn(_) { "2" })
  |> should.equal(Some("2"))

  None
  |> option.map(fn(x) { x + 1 })
  |> should.equal(None)
}

pub fn flatten_option_test() {
  Some(Some(1))
  |> option.flatten()
  |> should.equal(Some(1))

  Some(None)
  |> option.flatten()
  |> should.equal(None)

  None
  |> option.flatten()
  |> should.equal(None)
}

pub fn then_option_test() {
  Some(1)
  |> option.then(fn(x) { Some(x + 1) })
  |> should.equal(Some(2))

  Some(1)
  |> option.then(fn(_) { Some("2") })
  |> should.equal(Some("2"))

  None
  |> option.then(fn(x) { Some(x + 1) })
  |> should.equal(None)
}

pub fn or_option_test() {
  Some(1)
  |> option.or(Some(2))
  |> should.equal(Some(1))

  Some(1)
  |> option.or(None)
  |> should.equal(Some(1))

  None
  |> option.or(Some(2))
  |> should.equal(Some(2))

  None
  |> option.or(None)
  |> should.equal(None)
}

pub fn values_test() {
  option.values([Some(1), None, Some(3)])
  |> should.equal([1, 3])
}
