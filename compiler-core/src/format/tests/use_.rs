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
  <- get_multiple_things_with_a_longer_function(
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
  use <- some_really_long_function_call(
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
  use x <- some_really_long_function_call(
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
  use x, y <- some_really_long_function_call(
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
fn arity_1_var_call() {
    assert_format!(
        r#"pub fn main() {
  use x, y <- await(
    file.read()
    |> promise.map(something),
  )
}
"#
    );
}

#[test]
fn arity_1_access_call() {
    assert_format!(
        r#"pub fn main() {
  use x, y <- promise.await(
    file.read()
    |> promise.map(something),
  )
}
"#
    );
}

#[test]
fn patterns() {
    assert_format!(
        r#"pub fn main() {
  use Box(x) <- apply(Box(1))
  x
}
"#
    );
}

#[test]
fn patterns_with_annotation() {
    assert_format!(
        r#"pub fn main() {
  use Box(x): Box(Int) <- apply(Box(1))
  x
}
"#
    );
}

#[test]
fn long_patterns() {
    assert_format!(
        r#"pub fn main() {
  use
    Box(
      xxxxxxxxxxxxxxxxxxxxxxx,
      yyyyyyyyyyyyyyyyyyyyyyyyyyy,
      zzzzzzzzzzzzzzzzzzzzzzzzzzzz,
    )
  <- apply(Box(1))
  x
}
"#
    );
}

#[test]
fn multiple_long_patterns() {
    assert_format!(
        r#"pub fn main() {
  use
    Box(
      xxxxxxxxxxxxxxxxxxxxxxx,
      yyyyyyyyyyyyyyyyyyyyyyyyyyy,
      zzzzzzzzzzzzzzzzzzzzzzzzzzzz,
    ),
    Box(_),
    Box(_),
    Box(_)
  <- apply(Box(1))
  x
}
"#
    );
}

#[test]
fn multiple_long_patterns_with_annotations() {
    assert_format!(
        r#"pub fn main() {
  use
    Box(
      xxxxxxxxxxxxxxxxxxxxxxx,
      yyyyyyyyyyyyyyyyyyyyyyyyyyy,
      zzzzzzzzzzzzzzzzzzzzzzzzzzzz,
    ): Box(Int, Bool, String),
    Box(_)
  <- apply(Box(1))
  x
}
"#
    );
}

#[test]
fn multiple_long_annotations() {
    assert_format!(
        r#"pub fn main() {
  use
    Box(_, _): Box(
      Xxzxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx,
      Yyyyyyyyyyyyyyyyyyyyyyyy,
    ),
    Box(_)
  <- apply(Box(1))
  x
}
"#
    );
}
