use crate::assert_js;

#[test]
fn case_on_error() {
    assert_js!(
        r#"
fn a_result() { Error(1) }

pub fn main() {
  case a_result() {
    Error(_) -> 1
    _ -> 2
  }
}"#
    );
}

#[test]
fn tuple_and_guard() {
    assert_js!(
        r#"
pub fn go(x) {
  case #(1, 2) {
    #(1, a) if a == 2 -> 1
    #(_, _) -> 2
  }
}
"#,
    )
}

#[test]
fn guard_variable_only_brought_into_scope_when_needed() {
    assert_js!(
        r#"
pub fn go(x) {
  case x {
    // We want `a` to be defined before the guard check, and
    // `b` to be defined only if the predicate on a matches!
    [a, b] if a == 1 -> a + b
    _ -> 2
  }
}
"#
    )
}

// https://github.com/gleam-lang/gleam/issues/4221
#[test]
fn guard_variable_only_brought_into_scope_when_needed_1() {
    assert_js!(
        r#"
pub fn main() {
  case 1 {
    i if i == 1 -> True
    i if i < 2 -> True
    _ -> False
  }
}
"#
    )
}

// https://github.com/gleam-lang/gleam/issues/1187
#[test]
fn pointless() {
    assert_js!(
        r#"
pub fn go(x) {
  case x {
    _ -> x
  }
}
"#,
    )
}

// https://github.com/gleam-lang/gleam/issues/1188
#[test]
fn following_todo() {
    assert_js!(
        r#"
pub fn go(x) {
  case x {
    True -> todo
    _ -> 1
  }
}
"#,
    )
}

#[test]
fn multi_subject_catch_all() {
    assert_js!(
        r#"
pub fn go(x, y) {
  case x, y {
    True, True -> 1
    _, _ -> 0
  }
}
"#,
    )
}

#[test]
fn multi_subject_or() {
    assert_js!(
        r#"
pub fn go(x, y) {
  case x, y {
    True, _ | _, True -> 1
    _, _ -> 0
  }
}
"#,
    )
}

#[test]
fn multi_subject_no_catch_all() {
    assert_js!(
        r#"
pub fn go(x, y) {
  case x, y {
    True, _ -> 1
    _, True -> 2
    False, False -> 0
  }
}
"#,
    )
}

#[test]
fn multi_subject_subject_assignments() {
    assert_js!(
        r#"
pub fn go() {
  case True, False {
    True, True -> 1
    _, _ -> 0
  }
}
"#,
    )
}

#[test]
fn assignment() {
    assert_js!(
        r#"
pub fn go(x) {
  let y = case x {
    True -> 1
    _ -> 0
  }
  y
}
"#,
    )
}

#[test]
fn preassign_assignment() {
    assert_js!(
        r#"
pub fn go(x) {
  let y = case x() {
    True -> 1
    _ -> 0
  }
  y
}
"#,
    )
}

// https://github.com/gleam-lang/gleam/issues/1237
#[test]
fn pipe() {
    assert_js!(
        r#"
pub fn go(x, f) {
  case x |> f {
    0 -> 1
    _ -> 2
  }
}
"#,
    )
}

#[test]
fn result() {
    assert_js!(
        r#"
pub fn go(x) {
  case x {
    Ok(_) -> 1
    Error(_) -> 0
  }
}
"#,
    )
}

// https://github.com/gleam-lang/gleam/issues/1506
#[test]
fn called_case() {
    assert_js!(
        r#"
pub fn go(x, y) {
  case x {
    0 -> y
    _ -> y
  }()
}
"#,
    )
}

// https://github.com/gleam-lang/gleam/issues/1978
#[test]
fn case_local_var_in_tuple() {
    assert_js!(
        r#"
pub fn go(x, y) {
  let z = False
  case True {
    x if #(x, z) == #(True, False) -> x
    _ -> False
  }
}
"#,
    )
}

// https://github.com/gleam-lang/gleam/issues/2665
#[test]
fn case_branches_guards_are_wrapped_in_parentheses() {
    assert_js!(
        r#"
pub fn anything() -> a {
  case [] {
    [a] if False || True -> a
    _ -> anything()
  }
}
"#,
    )
}

// https://github.com/gleam-lang/gleam/issues/2759
#[test]
fn nested_string_prefix_match() {
    assert_js!(
        r#"
pub fn main() {
  case Ok(["a", "b c", "d"]) {
    Ok(["a", "b " <> _, "d"]) -> 1
    _ -> 1
  }
}
"#
    );
}

// https://github.com/gleam-lang/gleam/issues/2759
#[test]
fn nested_string_prefix_match_that_would_crash_on_js() {
    assert_js!(
        r#"
pub fn main() {
  case Ok(["b c", "d"]) {
    Ok(["b " <> _, "d"]) -> 1
    _ -> 1
  }
}
"#
    );
}

#[test]
fn slicing_is_handled_properly_with_multiple_branches() {
    assert_js!(
        r#"
pub fn main() {
  case "12345" {
    "0" <> rest -> rest
    "123" <> rest -> rest
    _ -> ""
  }
}
"#
    )
}

// https://github.com/gleam-lang/gleam/issues/3379
#[test]
fn single_clause_variables() {
    assert_js!(
        r#"
pub fn main() {
  let text = "first defined"
  case "defined again" {
    text -> Nil
  }
  let text = "a third time"
}
"#
    )
}

// https://github.com/gleam-lang/gleam/issues/3379
#[test]
fn single_clause_variables_assigned() {
    assert_js!(
        r#"
pub fn main() {
  let text = "first defined"
  let other = case "defined again" {
    text -> Nil
  }
  let text = "a third time"
}
"#
    )
}

// https://github.com/gleam-lang/gleam/issues/3894
#[test]
fn nested_string_prefix_assignment() {
    assert_js!(
        r#"
type Wibble {
  Wibble(wobble: String)
}

pub fn main() {
  let tmp = Wibble(wobble: "wibble")
  case tmp {
    Wibble(wobble: "w" as wibble <> rest) -> wibble <> rest
    _ -> panic
  }
}
"#
    )
}

#[test]
fn deeply_nested_string_prefix_assignment() {
    assert_js!(
        r#"
type Wibble {
  Wibble(Wobble)
}
type Wobble {
  Wobble(wabble: Wabble)
}
type Wabble {
  Wabble(tuple: #(Int, String))
}

pub fn main() {
  let tmp = Wibble(Wobble(Wabble(#(42, "wibble"))))
  case tmp {
    Wibble(Wobble(Wabble(#(_int, "w" as wibble <> rest)))) -> wibble <> rest
    _ -> panic
  }
}
"#
    )
}

// https://github.com/gleam-lang/gleam/issues/4383
#[test]
fn record_update_in_pipeline_in_case_clause() {
    assert_js!(
        "
pub type Wibble {
  Wibble(wibble: Int, wobble: Int)
}

fn identity(x) {
  x
}

pub fn go(x) {
  case x {
    Wibble(1, _) -> Wibble(..x, wibble: 4) |> identity
    Wibble(_, 3) -> Wibble(..x, wobble: 10) |> identity
    _ -> panic
  }
}
"
    );
}

#[test]
fn pattern_matching_on_aliased_result_constructor() {
    assert_js!(
        "
import gleam.{Error as E, Ok as O}

pub fn go(x) {
  case x {
    E(_) -> 1
    O(_) -> 2
  }
}
"
    );
}

#[test]
fn list_with_guard() {
    assert_js!(
        "
pub fn go(x) {
  case x {
    [] -> 0
    [first, ..] if first < 10 -> first * 2
    [first, ..] -> first
  }
}
"
    );
}

#[test]
fn list_with_guard_no_binding() {
    assert_js!(
        "
pub fn go(x) {
  case x {
    [] -> 0
    [first, ..] if 1 < 10 -> first * 2
    [first, ..] -> first
  }
}
"
    );
}

#[test]
fn case_building_simple_value_matched_by_pattern() {
    assert_js!(
        "pub fn go(x) {
   case x {
     1 -> 2
     n -> n
   }
}"
    )
}

#[test]
fn case_building_list_matched_by_pattern() {
    assert_js!(
        "pub fn go(x) {
   case x {
     [] -> []
     [a, b] -> [a, b]
     [1, ..rest] -> [1, ..rest]
     _ -> x
   }
}"
    )
}

#[test]
fn case_building_record_matched_by_pattern() {
    assert_js!(
        "pub fn go(x) {
   case x {
     Ok(1) -> Ok(1)
     Ok(n) -> Ok(n)
     Error(_) -> Error(Nil)
   }
}"
    )
}

#[test]
fn case_building_record_with_select_matched_by_pattern() {
    assert_js!(
        "
import gleam

pub fn go(x) {
   case x {
     Ok(1) -> gleam.Ok(1)
     _ -> Error(Nil)
   }
}"
    )
}

#[test]
fn case_building_record_with_select_matched_by_pattern_2() {
    assert_js!(
        "
import gleam

pub fn go(x) {
   case x {
     gleam.Ok(1) -> gleam.Ok(1)
     _ -> Error(Nil)
   }
}"
    )
}

#[test]
fn case_building_record_with_select_matched_by_pattern_3() {
    assert_js!(
        "
import gleam

pub fn go(x) {
   case x {
     gleam.Ok(1) -> Ok(1)
     _ -> Error(Nil)
   }
}"
    )
}

#[test]
fn case_building_matched_string_1() {
    assert_js!(
        r#"
import gleam

pub fn go(x) {
   case x {
     "a" <> rest -> "a" <> rest
     _ -> ""
   }
}"#
    )
}

#[test]
fn case_building_matched_string_2() {
    assert_js!(
        r#"
import gleam

pub fn go(x) {
   case x {
     "a" as a <> rest -> a <> rest
     _ -> ""
   }
}"#
    )
}

#[test]
fn case_building_matched_value_wrapped_in_block() {
    assert_js!(
        r#"
import gleam

pub fn go(x) {
   case x {
     1 -> { 1 }
     _ -> 2
   }
}"#
    )
}

#[test]
fn case_building_matched_value_alias() {
    assert_js!(
        r#"
import gleam

pub fn go(x) {
   case x {
     Ok(_) as a -> a
     Error(Nil) -> Error(Nil)
   }
}"#
    )
}

#[test]
fn case_building_matched_value_alias_2() {
    assert_js!(
        r#"
import gleam

pub fn go(x) {
   case x {
     Ok(1) as a -> Ok(1)
     Ok(_) -> Ok(2)
     Error(Nil) -> Error(Nil)
   }
}"#
    )
}

#[test]
fn case_building_matched_value_alias_3() {
    assert_js!(
        r#"
import gleam

pub fn go(x) {
   case x {
     Ok(1 as a) -> Ok(a)
     Ok(_) -> Ok(2)
     Error(Nil) -> Error(Nil)
   }
}"#
    )
}

#[test]
fn case_building_matched_no_variant_record() {
    assert_js!(
        r#"
pub fn go(x) {
   case x {
     Ok(Nil) -> Ok(Nil)
     _ -> Error(Nil)
   }
}"#
    )
}

#[test]
fn case_building_matched_no_variant_record_2() {
    assert_js!(
        r#"
import gleam

pub fn go(x) {
   case x {
     Ok(gleam.Nil) -> Ok(Nil)
     _ -> Error(Nil)
   }
}"#
    )
}

#[test]
fn case_building_matched_no_variant_record_3() {
    assert_js!(
        r#"
import gleam

pub fn go(x) {
   case x {
     Ok(Nil) -> Ok(gleam.Nil)
     _ -> Error(Nil)
   }
}"#
    )
}

#[test]
fn case_building_matched_no_variant_record_4() {
    assert_js!(
        r#"
import gleam

pub fn go(x) {
   case x {
     Ok(gleam.Nil) -> Ok(gleam.Nil)
     _ -> Error(Nil)
   }
}"#
    )
}

#[test]
fn case_building_record_with_labels_matched_by_pattern_1() {
    assert_js!(
        "
pub type Wibble {
  Wibble(int: Int, string: String)
  Wobble(Int)
}

pub fn go(x) {
   case x {
     Wibble(1, s) -> Wibble(1, s)
     _ -> Wobble(1)
   }
}"
    )
}

#[test]
fn case_building_record_with_labels_matched_by_pattern_2() {
    assert_js!(
        "
pub type Wibble {
  Wibble(int: Int, string: String)
  Wobble(Int)
}

pub fn go(x) {
   case x {
     Wibble(string:, int:) -> Wibble(string:, int:)
     _ -> Wobble(1)
   }
}"
    )
}

#[test]
fn case_building_record_with_labels_matched_by_pattern_3() {
    assert_js!(
        "
pub type Wibble {
  Wibble(int: Int, string: String)
  Wobble(Int)
}

pub fn go(x) {
   case x {
     // This should not be optimised away!
     Wibble(string:, int:) -> Wibble(string:, int: 1)
     _ -> Wobble(1)
   }
}"
    )
}

#[test]
fn case_building_record_with_labels_matched_by_pattern_4() {
    assert_js!(
        "
pub type Wibble {
  Wibble(int: Int, string: String)
  Wobble(Int)
}

pub fn go(x) {
   case x {
     Wibble(string:, int:) -> Wibble(int:, string:)
     _ -> Wobble(1)
   }
}"
    )
}

#[test]
fn case_building_record_with_labels_matched_by_pattern_5() {
    assert_js!(
        "
pub type Wibble {
  Wibble(int: Int, string: String)
  Wobble(Int)
}

pub fn go(x) {
   case x {
     Wibble(string:, int: 1) -> Wibble(1, string:)
     _ -> Wobble(1)
   }
}"
    )
}

#[test]
fn case_building_record_with_labels_matched_by_pattern_6() {
    assert_js!(
        "
pub type Wibble {
  Wibble(int: Int, string: String)
  Wobble(Int)
}

pub fn go(x) {
   case x {
     Wibble(1, string:) -> Wibble(string:, int: 1)
     _ -> Wobble(1)
   }
}"
    )
}

#[test]
fn case_with_multiple_subjects_building_simple_value_matched_by_pattern() {
    assert_js!(
        "pub fn go(x) {
   case x, x + 1 {
     1, _ -> 2
     _, n -> n
   }
}"
    )
}

#[test]
fn case_with_multiple_subjects_building_list_matched_by_pattern() {
    assert_js!(
        "pub fn go(n, x) {
   case n, x {
     1, [] -> []
     _, [a, b] -> [a, b]
     3, [1, ..rest] -> [1, ..rest]
     _, _ -> x
   }
}"
    )
}

#[test]
fn case_with_multiple_subjects_building_record_matched_by_pattern() {
    assert_js!(
        "pub fn go(x, y) {
   case x, y {
     Ok(1), Error(_) -> Ok(1)
     Error(_), Ok(n) -> Ok(n)
     _, _ -> Error(Nil)
   }
}"
    )
}

#[test]
fn case_with_multiple_subjects_building_same_value_as_two_subjects_one_is_picked() {
    assert_js!(
        "
import gleam

pub fn go(x, y) {
   case x, y {
     gleam.Ok(1), Ok(1) -> Ok(1)
     _, Error(Nil) -> Error(Nil)
     _, _ -> Error(Nil)
   }
}"
    )
}

#[test]
fn interfering_string_pattern_succeeds_if_succeeding() {
    assert_js!(
        r#"
pub fn wibble(bits) {
  case bits {
    <<"aaa", 0, _:bits>> -> 1
    // If the first one succeeds, so will the second check, so it won't be
    // performed twice inside the first if branch!
    <<"aaa", 1, _:bits>> -> 2
    _ -> 3
  }
}"#
    );
}

#[test]
fn string_concatenation_in_clause_guards() {
    assert_js!(
        r#"
pub fn main() {
  let wibble = "wob"
  case wibble {
    x if x <> "ble" == "wobble" -> 1
    _ -> 0
  }
}"#
    );
}

#[test]
fn var_true() {
    assert_js!(
        r#"
fn true() { True }
pub fn main() {
    let true_ = true()
    assert 0 == case Nil {
        _ if true_ -> 0
        _ -> 1
    }
}
"#
    )
}

#[test]
// https://github.com/gleam-lang/gleam/issues/5283
fn duplicate_name_for_variables_used_in_guards() {
    assert_js!(
        r#"
pub fn wibble() {
  let a = case 1337 {
    n if n == 1347 -> Nil
    _ -> Nil
  }
  let b = case 1337 {
    n -> Nil
  }
}"#
    )
}

#[test]
// https://github.com/gleam-lang/gleam/issues/5283
fn duplicate_name_for_variables_used_in_guards_shadowing_outer_name() {
    assert_js!(
        r#"
pub fn wibble() {
  let n = 1
  let a = case 1337 {
    n if n == 1347 -> n
    _ -> n
  }
  let b = case 1337 {
    n -> Nil
  }
}"#
    )
}

#[test]
fn directly_matching_case_subject() {
    assert_js!(
        r#"
pub fn go() {
  let x = "ABC"
  case True {
    True -> {
      let x = 79
      0
    }
    False -> {
      let x = True
      0
    }
  }
  x
}"#
    )
}
