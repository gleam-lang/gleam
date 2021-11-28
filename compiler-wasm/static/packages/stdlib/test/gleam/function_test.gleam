import gleam/should
import gleam/dynamic
import gleam/function
import gleam/int
import gleam/list
import gleam/result
import gleam/string

pub fn compose_test() {
  let add_two = fn(int: Int) { int + 2 }
  let add_three = fn(int: Int) { int + 3 }

  let add_five = function.compose(add_two, add_three)

  1
  |> add_five
  |> should.equal(6)

  // Takes a list of ints and returns the head as a string (if there is one, or
  // else "0" if there is not)
  let first_to_string =
    list.first
    |> function.compose(result.unwrap(_, 0))
    |> function.compose(int.to_string)

  [1]
  |> first_to_string
  |> should.equal("1")

  []
  |> first_to_string
  |> should.equal("0")
}

pub fn curry2_test() {
  let fun = fn(a, b) { a + b }
  let curried = function.curry2(fun)

  curried(1)(2)
  |> should.equal(3)
}

pub fn curry3_test() {
  let fun = fn(a, b, c) { a + b + c }
  let curried = function.curry3(fun)

  curried(1)(2)(4)
  |> should.equal(7)
}

pub fn curry4_test() {
  let fun = fn(a, b, c, d) { a + b + c + d }
  let curried = function.curry4(fun)

  curried(1)(2)(4)(8)
  |> should.equal(15)
}

pub fn curry5_test() {
  let fun = fn(a, b, c, d, e) { a + b + c + d + e }
  let curried = function.curry5(fun)

  curried(1)(2)(4)(8)(16)
  |> should.equal(31)
}

pub fn curry6_test() {
  let fun = fn(a, b, c, d, e, f) { a + b + c + d + e + f }
  let curried = function.curry6(fun)

  curried(1)(2)(4)(8)(16)(32)
  |> should.equal(63)
}

pub fn flip_test() {
  let fun = fn(s: String, i: Int) {
    s
    |> string.append("String: '", _)
    |> string.append("', Int: '")
    |> string.append(int.to_string(i))
    |> string.append("'")
  }

  let flipped_fun = function.flip(fun)

  fun("Bob", 1)
  |> should.equal("String: 'Bob', Int: '1'")

  flipped_fun(2, "Alice")
  |> should.equal("String: 'Alice', Int: '2'")
}

pub fn identity_test() {
  1
  |> function.identity
  |> should.equal(1)

  ""
  |> function.identity
  |> should.equal("")

  []
  |> function.identity
  |> should.equal([])

  #(1, 2.0)
  |> function.identity
  |> should.equal(#(1, 2.0))
}
