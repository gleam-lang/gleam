pub external type Expectation;

pub external fn equal(a, a) -> Expectation = "gleam_should" "should_equal";

pub external fn not_equal(a, a) -> Expectation = "gleam_should" "should_not_equal";
