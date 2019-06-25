// TODO: Move this module into another package so it can be used as a
// dep only in test.

pub external type Expectation;

pub external fn equal(a, a) -> Expectation = "gleam__stdlib" "expect_equal";

pub external fn not_equal(a, a) -> Expectation = "gleam__stdlib" "expect_not_equal";

pub external fn true(Bool) -> Expectation = "gleam__stdlib" "expect_true";

pub external fn false(Bool) -> Expectation = "gleam__stdlib" "expect_false";

pub external fn is_ok(Result(a, b)) -> Expectation = "gleam__stdlib" "expect_is_ok";

pub external fn is_error(Result(a, b)) -> Expectation = "gleam__stdlib" "expect_is_error";

pub fn fail() {
  true(False)
}
