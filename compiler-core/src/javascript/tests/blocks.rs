use crate::assert_js;

#[test]
fn block() {
    assert_js!(
        r#"
fn go() {
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
fn go() {
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
fn go() {
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
fn add1(a) {
  a + 1
}
fn go() {
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
fn go() {
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
fn go() {
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
fn go() {
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
fn go() {
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
fn go() {
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
fn main(f, a, b) {
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
fn b() {
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
fn b() {
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
fn b() {
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
fn b() {
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
fn b() {
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
fn b() {
  let x = 1
  {
    let x = x
  }
}
"#
    );
}
