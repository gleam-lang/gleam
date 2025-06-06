use crate::assert_js;

#[test]
fn block() {
    assert_js!(
        r#"
pub fn go() {
  let x = {
    1
    2
  }
  x
}
"#,
    );
}

#[test]
fn nested_simple_blocks() {
    assert_js!(
        r#"
pub fn go() {
  let x = {
    {
      3
    }
  }
  x
}
"#,
    );
}

#[test]
fn nested_multiexpr_blocks() {
    assert_js!(
        r#"
pub fn go() {
  let x = {
    1
    {
      2
      3
    }
  }
  x
}
"#,
    );
}

#[test]
fn nested_multiexpr_blocks_with_pipe() {
    assert_js!(
        r#"
pub fn add1(a) {
  a + 1
}
pub fn go() {
  let x = {
    1
    {
      2
      3 |> add1
    } |> add1
  }
  x
}
"#,
    );
}

#[test]
fn nested_multiexpr_non_ending_blocks() {
    assert_js!(
        r#"
pub fn go() {
  let x = {
    1
    {
      2
      3
    }
    4
  }
  x
}
"#,
    );
}

#[test]
fn nested_multiexpr_blocks_with_case() {
    assert_js!(
        r#"
pub fn go() {
  let x = {
    1
    {
      2
      case True {
        _ -> 3
      }
    }
  }
  x
}
"#,
    );
}

#[test]
fn sequences() {
    assert_js!(
        r#"
pub fn go() {
  "one"
  "two"
  "three"
}
"#,
    );
}

#[test]
fn left_operator_sequence() {
    assert_js!(
        r#"
pub fn go() {
  1 == {
    1
    2
  }
}
"#,
    );
}

#[test]
fn right_operator_sequence() {
    assert_js!(
        r#"
pub fn go() {
  {
    1
    2
  } == 1
}
"#,
    );
}

#[test]
fn concat_blocks() {
    assert_js!(
        r#"
pub fn main(f, a, b) {
  {
    a
    |> f
  } <> {
    b
    |> f
  }
}
"#,
    );
}

#[test]
fn blocks_returning_functions() {
    assert_js!(
        r#"
pub fn b() {
  {
    fn(cb) { cb(1) }
  }
  {
    fn(cb) { cb(2) }
  }
  3
}
"#
    );
}

#[test]
fn blocks_returning_use() {
    assert_js!(
        r#"
pub fn b() {
  {
    use a <- fn(cb) { cb(1) }
    a
  }
  {
    use b <- fn(cb) { cb(2) }
    b
  }
  3
}
    "#
    );
}

#[test]
fn block_with_parenthesised_expression_returning_from_function() {
    assert_js!(
        r#"
pub fn b() {
  {
    1 + 2
  }
}
"#
    );
}

#[test]
fn block_in_tail_position_is_not_an_iife() {
    assert_js!(
        r#"
pub fn b() {
  let x = 1
  {
    Nil
    x + 1
  }
}
"#
    );
}

#[test]
fn block_in_tail_position_shadowing_variables() {
    assert_js!(
        r#"
pub fn b() {
  let x = 1
  {
    let x = 2
    x + 1
  }
}
"#
    );
}

#[test]
fn block_in_tail_position_with_just_an_assignment() {
    assert_js!(
        r#"
pub fn b() {
  let x = 1
  {
    let x = x
  }
}
"#
    );
}

#[test]
fn shadowed_variable_in_nested_scope() {
    assert_js!(
        "
pub fn main() {
  {
    let x = 1
    let _ = {
      let x = 2
      x
    }
    x
  }
}
"
    )
}

// https://github.com/gleam-lang/gleam/issues/4393
#[test]
fn let_assert_only_statement_in_block() {
    assert_js!(
        "
pub fn main() {
  {
    let assert Ok(1) = Error(Nil)
  }
}
"
    )
}

// https://github.com/gleam-lang/gleam/issues/4394
#[test]
fn assignment_last_in_block() {
    assert_js!(
        "
pub fn main() {
  let a = {
    let b = 1
    let c = b + 1
  }
  a
}
"
    )
}

// https://github.com/gleam-lang/gleam/issues/4394
#[test]
fn pattern_assignment_last_in_block() {
    assert_js!(
        "
pub fn main() {
  let a = {
    let b = #(1, 2)
    let #(x, y) = b
  }
  a
}
"
    )
}

// https://github.com/gleam-lang/gleam/issues/4395
#[test]
fn let_assert_message_no_lifted() {
    assert_js!(
        r#"
fn side_effects(x) {
  // Some side effects
  x
}

pub fn main() {
  let assert Error(Nil) = side_effects(Ok(10))
    as {
    let message = side_effects("some message")
    message
  }
}
"#
    )
}
