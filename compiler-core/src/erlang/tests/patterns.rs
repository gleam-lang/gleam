use crate::assert_erl;

#[test]
fn alternative_patterns() {
    // reassigning name in alternative patterns
    assert_erl!(
        r#"
pub fn main() {
  let duplicate_name = 1

  case 1 {
    1 | 2 -> {
      let duplicate_name = duplicate_name + 1
      duplicate_name
    }
    _ -> 0
  }
}"#
    );
}

#[test]
fn alternative_patterns1() {
    // Alternative patterns with a clause containing vars
    assert_erl!(
        r#"
pub fn main() {
  case Ok(1) {
    Ok(duplicate_name) | Error(duplicate_name) -> duplicate_name
  }
}"#
    );
}

#[test]
fn alternative_patterns2() {
    // Alternative patterns with a guard clause containing vars
    assert_erl!(
        r#"
pub fn main() {
    let duplicate_name = 1

    case 1 {
        1 | 2 if duplicate_name == 1 -> duplicate_name
        _ -> 0
    }
}"#
    );
}

#[test]
fn alternative_patterns3() {
    assert_erl!(
        r#"
pub const constant = Ok(1)

pub fn main(arg) {
  let _ = constant
  case arg {
    _ if arg == constant -> 1
    _ -> 0
  }
}
"#
    );
}

#[test]
fn pattern_as() {
    assert_erl!(
        "pub fn a(x) {
  case x {
    Ok(1 as y) -> 1
    _ -> 0
  }
}"
    );
}

#[test]
fn string_prefix_as_pattern_with_multiple_subjects() {
    assert_erl!(
        "pub fn a(x) {
  case x, x {
    _, \"a\" as a <> _  -> a
    _, _ -> \"a\"
  }
}"
    );
}

#[test]
fn string_prefix_as_pattern_with_multiple_subjects_and_guard() {
    assert_erl!(
        "pub fn a(x) {
  case x, x {
    _, \"a\" as a <> rest if rest == \"a\" -> a
    _, _ -> \"a\"
  }
}"
    );
}

#[test]
fn string_prefix_as_pattern_with_list() {
    assert_erl!(
        "pub fn a(x) {
  case x {
    [\"a\" as a <> _, \"b\" as b <> _] -> a <> b
    _ -> \"\"
  }
}"
    );
}

#[test]
fn string_prefix_as_pattern_with_assertion() {
    assert_erl!(
        "pub fn a(x) {
  let assert \"a\" as a <> rest = \"wibble\"
  a
}"
    );
}
