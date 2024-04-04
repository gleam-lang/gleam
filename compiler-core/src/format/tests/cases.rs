use crate::assert_format;

#[test]
fn case_with_two_long_subjects() {
    assert_format!(
        r#"pub fn main() {
  case
    wibble(one_long_argument, something_else),
    wobble(another_argument, this_is_long)
  {
    _ -> todo
  }
}
"#
    );
}

#[test]
fn multiple_patterns_get_split_one_on_each_line() {
    assert_format!(
        r#"pub fn main() {
  case wibble, wobble, wubble {
    Wibble(one_thing, something_else, wibble),
      Wobble(
        this_will_go_over_the_line_limit,
        and_the_arguments_get_broken_as_well,
        wobble,
      ),
      Wubble(this_will_go_over_the_line_limit, wubble)
    -> todo
  }
}
"#
    );
}

#[test]
fn multiple_patterns_with_guard_get_split_one_on_each_line() {
    assert_format!(
        r#"pub fn main() {
  case wibble, wobble, wubble {
    Wibble(one_thing, something_else, wibble),
      Wobble(this_will_go_over_the_line_limit, wobble),
      Wubble(this_will_go_over_the_line_limit, wubble)
      if wibble && wobble || wubble
    -> todo
  }
}
"#
    );
}

#[test]
fn multiple_patterns_with_long_guard_get_split_one_on_each_line() {
    assert_format!(
        r#"pub fn main() {
  case wibble, wobble, wubble {
    Wibble(one_thing, something_else, wibble),
      Wobble(this_will_go_over_the_line_limit, wobble),
      Wubble(this_will_go_over_the_line_limit, wubble)
      if { wibble || wobble }
      && { wibble || wobble && wibble > 10 }
      || wobble < 10_000_000
    -> todo
  }
}
"#
    );
}

#[test]
fn multiple_patterns_and_alternative_patterns_mixed_together() {
    assert_format!(
        r#"pub fn main() {
  case wibble, wobble, wubble {
    Wibble(one_thing, something_else, wibble),
      Wobble(this_will_go_over_the_line_limit, wobble),
      Wubble(this_will_go_over_the_line_limit, wubble)
    | Wibble(a), Wobble(b), Wubble(c)
    | Wibble(one_thing, something_else, wibble),
      Wobble(this_will_go_over_the_line_limit, wobble),
      Wubble(this_will_go_over_the_line_limit, wubble)
      if { wibble || wobble }
      && { wibble || wobble && wibble > 10 }
      || wobble < 10_000_000
    -> todo
  }
}
"#
    );
}
