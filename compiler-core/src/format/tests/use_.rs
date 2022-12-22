use crate::assert_format;

#[test]
fn use_1() {
    assert_format!(
        r#"pub fn main() {
  use <- benchmark("thingy")
}
"#
    );
}

#[test]
fn use_2() {
    assert_format!(
        r#"pub fn main() {
  use user <- login()
}
"#
    );
}

#[test]
fn use_3() {
    assert_format!(
        r#"pub fn main() {
  use one, two, three, four <- get_multiple_things()
}
"#
    );
}

#[test]
fn use_4() {
    assert_format!(
        r#"pub fn main() {
  use
    one,
    two,
    three,
    four,
    five,
    six,
    seven,
    eight,
    nine,
    ten,
    eleven,
    twelve,
    thirteen
  <- get_multiple_things_with_a_longer_function
}
"#
    );
}

#[test]
fn use_5() {
    assert_format!(
        r#"pub fn main() {
  use
    one,
    two,
    three,
    four,
    five,
    six,
    seven,
    eight,
    nine,
    ten,
    eleven,
    twelve,
    thirteen
  <- get_multiple_things_with_a_longer_function(a, b, c, d)
}
"#
    );
}

#[test]
fn use_6() {
    assert_format!(
        r#"pub fn main() {
  use
    one,
    two,
    three,
    four,
    five,
    six,
    seven,
    eight,
    nine,
    ten,
    eleven,
    twelve,
    thirteen
  <-
    get_multiple_things_with_a_longer_function(
      "one",
      "two",
      "three",
      "four",
      "five",
      "six",
      "seven",
      "eight",
    )
}
"#
    );
}

#[test]
fn pipe_call() {
    assert_format!(
        r#"pub fn main() {
  use <-
    a
    |> b
  c
}
"#
    );
}

#[test]
fn use_pipe_everything() {
    assert_format!(
        r#"pub fn main() {
  {
    use <- a
  }
  |> b
  c
}
"#
    );
}

#[test]
fn long_right_hand_side_0_arguments() {
    assert_format!(
        r#"pub fn main() {
  use <-
    some_really_long_function_call(
      "one",
      "two",
      "three",
      "four",
      "five",
      "six",
      "seven",
      "eight",
    )
}
"#
    );
}

#[test]
fn long_right_hand_side_1_argument() {
    assert_format!(
        r#"pub fn main() {
  use x <-
    some_really_long_function_call(
      "one",
      "two",
      "three",
      "four",
      "five",
      "six",
      "seven",
      "eight",
    )
}
"#
    );
}

#[test]
fn long_right_hand_side_2_arguments() {
    assert_format!(
        r#"pub fn main() {
  use x, y <-
    some_really_long_function_call(
      "one",
      "two",
      "three",
      "four",
      "five",
      "six",
      "seven",
      "eight",
    )
}
"#
    );
}
