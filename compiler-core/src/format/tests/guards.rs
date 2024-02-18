use crate::assert_format;

#[test]
fn field_access() {
    assert_format!(
        r#"pub fn main() {
  case x {
    _ if a.b -> 1
    _ -> 0
  }
}
"#
    );
}

#[test]
fn nested_field_access() {
    assert_format!(
        r#"pub fn main() {
  case x {
    _ if a.b.c.d -> 1
    _ -> 0
  }
}
"#
    );
}

#[test]
fn operators_in_guard() {
    assert_format!(
        r#"pub fn main() {
  case list.map(codepoints, string.utf_codepoint_to_int) {
    [drive, colon, slash]
      if { slash == 47 || slash == 92 }
      && colon == 58
      && drive >= 65
      && drive <= 90
      || drive >= 97
      && drive <= 122
    -> {
      1
      |> 2
    }
  }
}
"#
    );
}

#[test]
fn a_comment_before_a_guard_doesnt_force_it_to_break() {
    assert_format!(
        r#"pub fn main() {
  case wibble {
    // Apparently this comment breaks everything
    _ if wobble -> Ok(state.newest)
  }
}
"#
    );
}
