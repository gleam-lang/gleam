use crate::assert_erl;

#[test]
fn alternative_patterns() {
    // reassigning name in alternative patterns
    assert_erl!(
        r#"
pub fn test() {
  let duplicate_name = 1

  case 1 {
    1 | 2 -> {
      let duplicate_name = duplicate_name + 1
      duplicate_name
    }
  }
}"#
    );

    // Alternative patterns with a clause containing vars
    assert_erl!(
        r#"
pub fn test() {
  case Ok(1) {
    Ok(duplicate_name) | Error(duplicate_name) -> duplicate_name
  }
}"#
    );

    // Alternative patterns with a guard clause containing vars
    assert_erl!(
        r#"
pub fn test() {
    let duplicate_name = 1

    case 1 {
        1 | 2 if duplicate_name == 1 -> duplicate_name
    }
}"#
    );

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
