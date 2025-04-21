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

#[test]
fn alternatives_are_not_split_if_not_necessary() {
    assert_format!(
        r#"fn main() {
  case thing {
    Wibble | Wobble -> {
      todo
      todo
    }
  }
}
"#
    );
}

#[test]
fn alternatives_are_not_split_if_not_necessary_2() {
    assert_format!(
        r#"fn main() {
  case thing {
    Wibble | Wobble | Wabble ->
      loooooooong_function_call_that_barely_goes_over_the_limit()
  }
}
"#
    );
}

#[test]
fn subjects_are_not_split_if_not_necessary() {
    assert_format!(
        r#"fn main() {
  case
    is_all_uppercase(remark),
    string.ends_with(remark, "?"),
    string.trim(remark) == ""
  {
    _, _, _ -> todo
  }
}
"#
    );
}

#[test]
fn case_in_call_gets_broken_if_it_goes_over_the_limit_with_subject() {
    assert_format!(
        r#"fn main() {
  do_diff_attributes(
    dict.delete(prev, attr.name),
    rest,
    case attr.value == old.value {
      True -> added
      False -> [attr, ..added]
    },
  )
}
"#
    );
}

#[test]
fn case_in_call_is_not_broken_if_it_goes_over_the_limit_with_branches() {
    assert_format!(
        r#"fn main() {
  do_diff_attributes(rest, case attr.value == old.value {
    True -> added
    False -> [attr, ..added]
  })
}
"#
    );
}
