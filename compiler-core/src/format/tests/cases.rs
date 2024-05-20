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

#[test]
fn case_pattern_split_on_multiple_lines_is_not_needlessly_nested() {
    assert_format!(
        r#"pub fn main() {
  case thing {
    CannotSaveNewSnapshot(
      reason: reason,
      title: title,
      destination: destination,
    ) -> todo
  }
}
"#
    );
}

// https://github.com/gleam-lang/gleam/issues/3140
#[test]
fn long_comment_before_case_with_multiple_subjects_doesnt_force_a_break() {
    assert_format!(
        r#"fn main() {
  case a, b {
    // a very long comment a very long comment a very long comment a very long comment
    _, _ -> True
  }
}
"#
    );
}

// https://github.com/gleam-lang/gleam/issues/3136
#[test]
fn splitting_alternatives_doesnt_force_the_consequence_to_break() {
    assert_format!(
        r#"fn main() {
  case a, b, c {
    _ignored, _ignored, _ignored
    | _ignored, _ignored, _ignored
    | _ignored, _ignored, _ignored -> True
  }
}
"#
    );
}

// https://github.com/gleam-lang/gleam/issues/3136
#[test]
fn splitting_the_last_alternative_forces_the_consequence_to_break() {
    assert_format!(
        r#"fn main() {
  case a, b, c {
    _, _, _
    | _, _, _
    | this_is_long_and_is_going_to_split_on_multiple,
      lines,
      and_force_the_arrow_to_break
    -> True
  }
}
"#
    );
}

// https://github.com/gleam-lang/gleam/issues/3136
#[test]
fn splitting_an_alternative_in_the_middle_doesnt_force_the_consequence_to_break() {
    assert_format!(
        r#"fn main() {
  case a, b, c {
    _, _, _
    | this_is_long_and_is_going_to_split_on_multiple,
      lines,
      and_force_the_alternative_to_break
    | _, _, _ -> True
  }
}
"#
    );
}
