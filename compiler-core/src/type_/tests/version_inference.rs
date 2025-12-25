use hexpm::version::Version;

use super::compile_module;

fn infer_version(module: &str) -> Version {
    compile_module("test_module", module, None, vec![])
        .expect("module to compile")
        .type_info
        .minimum_required_version
}

#[test]
fn internal_annotation_on_constant_requires_v1_1() {
    let version = infer_version(
        "
@internal
pub const wibble = 1
",
    );
    assert_eq!(version, Version::new(1, 1, 0));
}

#[test]
fn internal_annotation_on_type_requires_v1_1() {
    let version = infer_version(
        "
@internal
pub type Wibble
",
    );
    assert_eq!(version, Version::new(1, 1, 0));
}

#[test]
fn internal_annotation_on_function_requires_v1_1() {
    let version = infer_version(
        "
@internal
pub fn wibble() {}
",
    );
    assert_eq!(version, Version::new(1, 1, 0));
}

#[test]
fn nested_tuple_access_requires_v1_1() {
    let version = infer_version(
        "
pub fn main() {
  let tuple = #(1, #(1, 1))
  tuple.1.0
}
",
    );
    assert_eq!(version, Version::new(1, 1, 0));
}

#[test]
fn javascript_external_module_with_at_requires_v1_2() {
    let version = infer_version(
        "
@external(javascript, \"module@module\", \"func\")
pub fn main() {}
",
    );
    assert_eq!(version, Version::new(1, 2, 0));
}

#[test]
fn int_plus_in_guards_requires_v1_3() {
    let version = infer_version(
        "
pub fn main() {
  case todo {
    _ if 1 + 1 == 2 -> todo
    _ -> todo
  }
}
",
    );
    assert_eq!(version, Version::new(1, 3, 0));
}

#[test]
fn float_plus_in_guards_requires_v1_3() {
    let version = infer_version(
        "
pub fn main() {
  case todo {
    _ if 1.0 +. 1.0 == 2.0 -> todo
    _ -> todo
  }
}
",
    );
    assert_eq!(version, Version::new(1, 3, 0));
}

#[test]
fn int_minus_in_guards_requires_v1_3() {
    let version = infer_version(
        "
pub fn main() {
  case todo {
    _ if 1 - 1 == 0 -> todo
    _ -> todo
  }
}
",
    );
    assert_eq!(version, Version::new(1, 3, 0));
}

#[test]
fn float_minus_in_guards_requires_v1_3() {
    let version = infer_version(
        "
pub fn main() {
  case todo {
    _ if 1.0 -. 1.0 == 0.0 -> todo
    _ -> todo
  }
}
",
    );
    assert_eq!(version, Version::new(1, 3, 0));
}

#[test]
fn int_multiplication_in_guards_requires_v1_3() {
    let version = infer_version(
        "
pub fn main() {
  case todo {
    _ if 1 * 1 == 0 -> todo
    _ -> todo
  }
}
",
    );
    assert_eq!(version, Version::new(1, 3, 0));
}

#[test]
fn float_multiplication_in_guards_requires_v1_3() {
    let version = infer_version(
        "
pub fn main() {
  case todo {
    _ if 1.0 *. 1.0 == 0.0 -> todo
    _ -> todo
  }
}
",
    );
    assert_eq!(version, Version::new(1, 3, 0));
}

#[test]
fn int_divide_in_guards_requires_v1_3() {
    let version = infer_version(
        "
pub fn main() {
  case todo {
    _ if 1 / 1 == 0 -> todo
    _ -> todo
  }
}
",
    );
    assert_eq!(version, Version::new(1, 3, 0));
}

#[test]
fn float_divide_in_guards_requires_v1_3() {
    let version = infer_version(
        "
pub fn main() {
  case todo {
    _ if 1.0 /. 1.0 == 0.0 -> todo
    _ -> todo
  }
}
",
    );
    assert_eq!(version, Version::new(1, 3, 0));
}

#[test]
fn int_remainder_in_guards_requires_v1_3() {
    let version = infer_version(
        "
pub fn main() {
  case todo {
    _ if 1 % 1 == 0 -> todo
    _ -> todo
  }
}
",
    );
    assert_eq!(version, Version::new(1, 3, 0));
}

#[test]
fn label_shorthand_in_constand_requires_v1_4() {
    let version = infer_version(
        "
pub type Wibble { Wibble(wibble: Int) }

pub const wibble = 1
pub const wobble = Wibble(wibble:)
",
    );
    assert_eq!(version, Version::new(1, 4, 0));
}

#[test]
fn label_shorthand_in_call_requires_v1_4() {
    let version = infer_version(
        "
pub type Wibble { Wibble(wibble: Int) }

pub fn main() {
  let wibble = 1
  Wibble(wibble:)
}
",
    );
    assert_eq!(version, Version::new(1, 4, 0));
}

#[test]
fn label_shorthand_in_pattern_requires_v1_4() {
    let version = infer_version(
        "
pub type Wibble { Wibble(wibble: Int) }

pub fn main() {
  case Wibble(1) {
    Wibble(wibble:) -> todo
  }
}
",
    );
    assert_eq!(version, Version::new(1, 4, 0));
}

#[test]
fn label_shorthand_in_record_update_requires_v1_4() {
    let version = infer_version(
        "
pub type Vec2 { Vec2(x: Int, y: Int) }

pub fn main() {
  let x = 1
  Vec2(..Vec2(0, 0), x:)
}
",
    );
    assert_eq!(version, Version::new(1, 4, 0));
}

#[test]
fn constant_string_concatenation_requires_v1_4() {
    let version = infer_version("pub const string = \"wibble\" <> \"wobble\"");
    assert_eq!(version, Version::new(1, 4, 0));
}

#[test]
fn missing_utf_8_option_in_bit_array_segment_requires_v1_5() {
    let version = infer_version(
        "
pub fn main() {
  <<\"hello\", \" world!\">>
}
",
    );
    assert_eq!(version, Version::new(1, 5, 0));
}

#[test]
fn missing_utf_8_option_in_bit_array_constant_segment_requires_v1_5() {
    let version = infer_version("const bits = <<\"hello\", \" world!\">>");
    assert_eq!(version, Version::new(1, 5, 0));
}

#[test]
fn missing_utf_8_option_in_bit_array_pattern_segment_requires_v1_5() {
    let version = infer_version(
        "
pub fn main() {
  case todo {
    <<\"hello\", \" world!\">> -> todo
    _ -> todo
  }
}
",
    );
    assert_eq!(version, Version::new(1, 5, 0));
}

#[test]
fn missing_float_option_in_bit_array_segment_requires_v1_10() {
    let version = infer_version(
        "
pub fn main() {
  <<1.2>>
}
",
    );
    assert_eq!(version, Version::new(1, 10, 0));
}

#[test]
fn missing_float_option_in_bit_array_constant_segment_requires_v1_10() {
    let version = infer_version("const bits = <<1.2>>");
    assert_eq!(version, Version::new(1, 10, 0));
}

#[test]
fn missing_float_option_in_bit_array_pattern_segment_requires_v1_10() {
    let version = infer_version(
        "
pub fn main() {
  case todo {
    <<1.11>> -> todo
    _ -> todo
  }
}
",
    );
    assert_eq!(version, Version::new(1, 10, 0));
}

#[test]
fn const_record_update_requires_v1_14() {
    let version = infer_version(
        "
pub type Wibble { Wibble(a: Int, b: Int) }
const base = Wibble(1, 2)
const wobble = Wibble(..base, a: 3)
",
    );
    assert_eq!(version, Version::new(1, 14, 0));
}

#[test]
fn inference_picks_the_bigger_of_two_versions() {
    let version = infer_version(
        "
pub fn main() {
  case todo {
    <<\"hello\", \" world!\">> -> todo
    _ if 1 + 1 == 2-> todo
    _ -> todo
  }
}
",
    );
    assert_eq!(version, Version::new(1, 5, 0));
}

#[test]
fn inference_picks_the_bigger_of_two_versions_2() {
    let version = infer_version(
        "
@external(javascript, \"module@module\", \"func\")
pub fn main() {
  let tuple = #(1, #(1, 1))
  tuple.1.0
}
",
    );
    assert_eq!(version, Version::new(1, 2, 0));
}

#[test]
fn bool_assert_requires_v1_11() {
    let version = infer_version(
        "
pub fn main() {
  assert 1 != 2
}
",
    );
    assert_eq!(version, Version::new(1, 11, 0));
}

#[test]
fn expression_in_expression_segment_size_requires_v1_12() {
    let version = infer_version(
        "
pub fn main() {
  <<1:size(3 * 8)>>
}
",
    );
    assert_eq!(version, Version::new(1, 12, 0));
}

#[test]
fn expression_in_pattern_segment_size_requires_v1_12() {
    let version = infer_version(
        "
pub fn main(x) {
  case x {
    <<_:size(3*8)>> -> 1
    _ -> 2
  }
}",
    );
    assert_eq!(version, Version::new(1, 12, 0));
}
