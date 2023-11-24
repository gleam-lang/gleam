use crate::{assert_no_warnings, assert_warning};

#[test]
fn whatever() {
    assert_no_warnings!(
        "
pub fn main(x) {
  case x {
    _ -> 0
  }
}
"
    );
}

#[test]
fn nil() {
    assert_no_warnings!(
        "
pub fn main(x) {
  case x {
    Nil -> 0
  }
}
"
    );
}

#[test]
fn bool() {
    assert_no_warnings!(
        "
pub fn main(x) {
  case x {
    True -> 1
    False -> 0
  }
}
"
    );
}

#[test]
fn bool_true() {
    assert_warning!(
        "
pub fn main(x) {
  case x {
    True -> 1
  }
}
"
    );
}

#[test]
fn bool_false() {
    assert_warning!(
        "
pub fn main(x) {
  case x {
    False -> 1
  }
}
"
    );
}

#[test]
fn result() {
    assert_no_warnings!(
        "
pub fn main(x) {
  case x {
    Ok(_) -> 1
    Error(_) -> 2
  }
}
"
    );
}

#[test]
fn result_ok() {
    assert_warning!(
        "
pub fn main(x) {
  case x {
    Ok(_) -> 1
  }
}
"
    );
}

#[test]
fn result_error() {
    assert_warning!(
        "
pub fn main(x) {
  case x {
    Error(_) -> 1
  }
}
"
    );
}

#[test]
fn result_nil() {
    assert_no_warnings!(
        "
pub fn main(x) {
  case x {
    Ok(Nil) -> 1
    Error(Nil) -> 2
  }
}
"
    );
}

#[test]
fn result_nil_ok() {
    assert_warning!(
        "
pub fn main(x) {
  case x {
    Ok(Nil) -> 1
  }
}
"
    );
}

#[test]
fn result_nil_error() {
    assert_warning!(
        "
pub fn main(x) {
  case x {
    Error(Nil) -> 1
  }
}
"
    );
}

#[test]
fn result_bool() {
    assert_no_warnings!(
        "
pub fn main(x) {
  case x {
    Ok(True) -> 1
    Ok(False) -> 3
    Error(True) -> 2
    Error(False) -> 4
  }
}
"
    );
}

#[test]
fn result_bool_1() {
    assert_warning!(
        "
pub fn main(x) {
  case x {
    Ok(False) -> 1
    Error(True) -> 2
    Error(False) -> 3
  }
}
"
    );
}

#[test]
fn result_bool_2() {
    assert_warning!(
        "
pub fn main(x) {
  case x {
    Ok(True) -> 1
    Error(True) -> 2
    Error(False) -> 3
  }
}
"
    );
}

#[test]
fn result_bool_3() {
    assert_warning!(
        "
pub fn main(x) {
  case x {
    Ok(True) -> 1
    Ok(False) -> 2
    Error(False) -> 3
  }
}
"
    );
}

#[test]
fn result_bool_4() {
    assert_warning!(
        "
pub fn main(x) {
  case x {
    Ok(True) -> 1
    Ok(False) -> 2
    Error(True) -> 3
  }
}
"
    );
}

#[test]
fn result_bool_5() {
    assert_warning!(
        "
pub fn main(x) {
  case x {
    Ok(True) -> 1
    Ok(False) -> 2
  }
}
"
    );
}

#[test]
fn result_bool_6() {
    assert_warning!(
        "
pub fn main(x) {
  case x {
    Error(True) -> 1
    Error(False) -> 2
  }
}
"
    );
}

#[test]
fn result_bool_7() {
    assert_warning!(
        "
pub fn main(x) {
  case x {
    Error(True) -> 1
  }
}
"
    );
}

#[test]
fn result_bool_8() {
    assert_warning!(
        "
pub fn main(x) {
  case x {
    Ok(False) -> 1
  }
}
"
    );
}

#[test]
fn list() {
    assert_no_warnings!(
        "
pub fn main(x) {
  case x {
    [_, ..] -> 1
    [] -> 2
  }
}
"
    );
}

#[test]
fn list_empty() {
    assert_warning!(
        "
pub fn main(x) {
  case x {
    [] -> 1
  }
}
"
    );
}

#[test]
fn list_non_empty() {
    assert_warning!(
        "
pub fn main(x) {
  case x {
    [_, ..] -> 1
  }
}
"
    );
}

#[test]
fn list_one() {
    assert_warning!(
        "
pub fn main(x) {
  case x {
    [_] -> 1
  }
}
"
    );
}

#[test]
fn list_one_two() {
    assert_warning!(
        "
pub fn main(x) {
  case x {
    [_] -> 1
    [_, _] -> 1
  }
}
"
    );
}

#[test]
fn list_zero_one_two() {
    assert_warning!(
        "
pub fn main(x) {
  case x {
    [] -> 1
    [_] -> 1
    [_, _] -> 1
  }
}
"
    );
}

#[test]
fn list_zero_one_two_any() {
    assert_no_warnings!(
        "
pub fn main(x) {
  case x {
    [] -> 1
    [_] -> 1
    [_, _] -> 1
    [_, _, ..] -> 1
  }
}
"
    );
}

#[test]
fn list_zero_two_any() {
    assert_warning!(
        "
pub fn main(x) {
  case x {
    [] -> 1
    [_, _] -> 1
    [_, _, ..] -> 1
  }
}
"
    );
}

#[test]
fn string() {
    assert_no_warnings!(
        r#"
pub fn main(x) {
  case x {
    "" -> 1
    "a" -> 1
    "b" -> 1
    _ -> 1
  }
}
"#
    );
}

#[test]
fn string_1() {
    assert_warning!(
        r#"
pub fn main(x) {
  case x {
    "" -> 1
  }
}
"#
    );
}

#[test]
fn string_2() {
    assert_warning!(
        r#"
pub fn main(x) {
  case x {
    "a" -> 1
  }
}
"#
    );
}

#[test]
fn string_3() {
    assert_warning!(
        r#"
pub fn main(x) {
  case x {
    "a" -> 1
    "b" -> 1
  }
}
"#
    );
}

#[test]
fn bit_array() {
    assert_no_warnings!(
        r#"
pub fn main(x) {
  case x {
    <<>> -> 1
    <<1>> -> 1
    <<2>> -> 1
    _ -> 1
  }
}
"#
    );
}

#[test]
fn bit_array_1() {
    assert_warning!(
        r#"
pub fn main(x) {
  case x {
    <<>> -> 1
    <<1>> -> 1
    <<2>> -> 1
  }
}
"#
    );
}

#[test]
fn bit_array_2() {
    assert_warning!(
        r#"
pub fn main(x) {
  case x {
    <<>> -> 1
    <<1>> -> 1
  }
}
"#
    );
}

#[test]
fn int() {
    assert_no_warnings!(
        r#"
pub fn main(x) {
  case x {
    0 -> 1
    1 -> 1
    2 -> 1
    _ -> 1
  }
}
"#
    );
}

#[test]
fn int_1() {
    assert_warning!(
        r#"
pub fn main(x) {
  case x {
    0 -> 1
    1 -> 1
    2 -> 1
  }
}
"#
    );
}

#[test]
fn int_2() {
    assert_warning!(
        r#"
pub fn main(x) {
  case x {
    0 -> 1
    1 -> 1
  }
}
"#
    );
}

#[test]
fn float() {
    assert_no_warnings!(
        r#"
pub fn main(x) {
  case x {
    0.0 -> 1
    1.1 -> 1
    2.2 -> 1
    _ -> 1
  }
}
"#
    );
}

#[test]
fn float_1() {
    assert_warning!(
        r#"
pub fn main(x) {
  case x {
    0.0 -> 1
    1.1 -> 1
    2.2 -> 1
  }
}
"#
    );
}

#[test]
fn float_2() {
    assert_warning!(
        r#"
pub fn main(x) {
  case x {
    0.0 -> 1
    1.1 -> 1
  }
}
"#
    );
}

#[test]
fn list_bool_1() {
    assert_warning!(
        r#"
pub fn main(x) {
  case x {
    [] -> 1
    [True] -> 2
    [_, _, ..] -> 2
  }
}
"#
    );
}

#[test]
fn list_bool_2() {
    assert_warning!(
        r#"
pub fn main(x) {
  case x {
    [] -> 1
    [True] -> 2
    [_, False] -> 2
    [_, _, _, ..] -> 2
  }
}
"#
    );
}

// #[test]
// fn let_exhaustiveness1() {
//     assert_module_error!(
//         r#"
// pub fn main(b) {
//     let True = b
//     Nil
// }
// "#
//     );
// }

// #[test]
// fn let_exhaustiveness2() {
//     assert_module_error!(
//         r#"
// pub fn main(r) {
//     let Error(_) = r
//     Nil
// }
// "#
//     );
// }

// #[test]
// fn let_exhaustiveness3() {
//     assert_module_error!(
//         r#"
// pub type Media {
//     Audio(BitArray)
//     Video(BitArray)
//     Text(String)
// }
// pub fn main(m) {
//     let Video(_) = m
//     Nil
// }
// "#
//     );
// }

// #[test]
// fn let_exhaustiveness4() {
//     assert_module_error!(
//         r#"
// pub type Media {
//     Audio(BitArray)
//     Video(BitArray)
//     Text(String)
// }
// pub fn main(m) {
//     let Video(_) as v = m
//     v
// }
// "#
//     );
// }

// #[test]
// fn case_exhaustiveness1() {
//     assert_module_error!(
//         r#"
// pub fn main(b) {
//     case b {
//         True -> Nil
//     }
// }
// "#
//     );
// }

// #[test]
// fn case_exhaustiveness2() {
//     assert_module_error!(
//         r#"
// pub fn main(r) {
//     case r {
//         Error(_) -> Nil
//     }
// }
// "#
//     );
// }

// #[test]
// fn case_exhaustiveness3() {
//     assert_module_error!(
//         r#"
// pub type Media {
//     Audio(BitArray)
//     Video(BitArray)
//     Text(String)
// }
// pub fn main(m) {
//     case m {
//         Audio(_) as a -> a
//         Video(_) -> m
//     }
// }
// "#
//     );
// }

// #[test]
// fn case_exhaustiveness4() {
//     assert_module_error!(
//         r#"
// pub type Media {
//     Audio(BitArray)
//     Video(BitArray)
//     Text(String)
// }
// pub fn main(m) {
//     case m {
//         Video(_) -> m
//     }
// }
// "#
//     );
// }

// #[test]
// fn case_exhaustiveness5() {
//     assert_module_error!(
//         r#"
// pub type Media {
//     Audio(BitArray)
//     Video(BitArray)
//     Text(String)
// }
// pub fn main(m) {
//     case m {
//         Audio(_) | Text(_) -> m
//     }
// }
// "#
//     );
// }

// #[test]
// fn case_exhaustiveness6() {
//     assert_module_error!(
//         r#"
// pub fn main(b) {
//     case b {
//         b if b == True -> Nil
//         b if b != True -> Nil
//     }
// }
// "#
//     );
// }
