use super::*;
use crate::{
    assert_js_no_warnings, assert_js_warning, assert_no_warnings, assert_warning,
    assert_warnings_with_gleam_version,
};

#[test]
fn unknown_label() {
    // https://github.com/gleam-lang/gleam/issues/1098
    // calling function with unused labelled argument should not emit warnings
    assert_no_warnings!(
        r#"fn greet(name name: String, title _title: String) { name }
           pub fn main() { greet(name: "Sam", title: "Mr") }"#,
    );
}

#[test]
fn todo_warning_test() {
    assert_warning!("pub fn main() { 1 == todo }");
}

// https://github.com/gleam-lang/gleam/issues/1669
#[test]
fn todo_warning_correct_location() {
    assert_warning!(
        "pub fn main() {
        todo
      }"
    );
}

#[test]
fn todo_with_known_type() {
    assert_warning!(
        "pub fn main() -> String {
  todo
}"
    );
}

#[test]
fn empty_func_warning_test() {
    assert_warning!(
        "pub fn main() { wibble() }
pub fn wibble() { }
"
    );
}

#[test]
fn warning_variable_never_used_test() {
    assert_warning!(
        "
pub fn wibble() { Ok(5) }
pub fn main() { let five = wibble() }"
    );
}

#[test]
fn warning_private_function_never_used() {
    assert_warning!("fn main() { 5 }");
}

#[test]
fn warning_many_at_same_time() {
    assert_warning!(
        "
fn main() { let five = 5 }"
    );
}

#[test]
fn result_discard_warning_test() {
    // Implicitly discarded Results emit warnings
    assert_warning!(
        "
pub fn wibble() { Ok(5) }
pub fn main() {
  wibble()
  5
}"
    );
}

#[test]
fn result_discard_warning_test2() {
    // Explicitly discarded Results do not emit warnings
    assert_no_warnings!(
        "
pub fn wibble() { Ok(5) }
pub fn main() { let _ = wibble() 5 }",
    );
}

#[test]
fn unused_int() {
    assert_warning!("pub fn main() { 1 2 }");
}

#[test]
fn unused_float() {
    assert_warning!("pub fn main() { 1.0 2 }");
}

#[test]
fn unused_string() {
    assert_warning!(
        "
    pub fn main() {
        \"1\"
                2
    }"
    );
}

#[test]
fn unused_bit_array() {
    assert_warning!(
        "
    pub fn main() {
        <<3>>
				2
    }"
    );
}

#[test]
fn unused_tuple() {
    assert_warning!(
        "
    pub fn main() {
        #(1.0, \"Hello world\")
				2
    }"
    );
}

#[test]
fn unused_list() {
    assert_warning!(
        "
    pub fn main() {
        [1, 2, 3]
				2
    }"
    );
}

#[test]
fn record_update_warnings_test() {
    // Some fields are given in a record update do not emit warnings
    assert_no_warnings!(
        "
        pub type Person {
            Person(name: String, age: Int)
        }
        pub fn update_person() {
            let past = Person(\"Quinn\", 27)
            let present = Person(..past, name: \"Santi\")
            present
        }",
    );
}

#[test]
fn record_update_warnings_test2() {
    // No fields are given in a record update emit warnings
    assert_warning!(
        "
        pub type Person {
            Person(name: String, age: Int)
        }
        pub fn update_person() {
            let past = Person(\"Quinn\", 27)
            let present = Person(..past)
            present
        }"
    );
}

#[test]
fn record_update_warnings_test3() {
    // All fields given in a record update emits warnings
    assert_warning!(
        "
        pub type Person {
            Person(name: String, age: Int)
        }
        pub fn update_person() {
            let past = Person(\"Quinn\", 27)
            let present = Person(..past, name: \"Quinn\", age: 28)
            present
        }"
    );
}

#[test]
fn unused_private_type_warnings_test() {
    // External type
    assert_warning!("type X");
}

#[test]
fn unused_private_type_warnings_test2() {
    assert_no_warnings!("pub type Y");
}

#[test]
fn unused_private_type_warnings_test3() {
    // Type alias
    assert_warning!("type X = Int");
}

#[test]
fn unused_private_type_warnings_test4() {
    assert_no_warnings!("pub type Y = Int");
}

#[test]
fn unused_private_type_warnings_test5() {
    assert_no_warnings!("type Y = Int pub fn run(x: Y) { x }");
}

#[test]
fn unused_private_type_warnings_test6() {
    // Custom type
    assert_warning!("type X { X }");
}

#[test]
fn unused_private_type_warnings_test7() {
    assert_no_warnings!("pub type X { X }");
}

#[test]
fn unused_private_type_warnings_test8() {
    assert_no_warnings!(
        "
type X { X }

pub fn a() {
  let b = X
  case b {
    X -> 1
  }
}"
    );
}

#[test]
fn unused_private_fn_warnings_test() {
    assert_warning!("fn a() { 1 }");
}

#[test]
fn used_private_fn_warnings_test() {
    assert_no_warnings!("pub fn a() { 1 }");
}

#[test]
fn used_private_fn_warnings_test2() {
    assert_no_warnings!("fn a() { 1 } pub fn b() { a }");
}

#[test]
fn unused_private_const_warnings_test() {
    assert_warning!("const a = 1");
}

#[test]
fn used_private_const_warnings_test() {
    assert_no_warnings!("pub const a = 1");
}

#[test]
fn used_private_const_warnings_test2() {
    assert_no_warnings!("const a = 1 pub fn b() { a }");
}

#[test]
fn unused_variable_warnings_test() {
    // function argument
    assert_warning!("pub fn a(b) { 1 }");
}

#[test]
fn used_variable_warnings_test() {
    assert_no_warnings!("pub fn a(b) { b }");
}

#[test]
fn unused_variable_warnings_test2() {
    // Simple let
    assert_warning!("pub fn a() { let b = 1 5 }");
}

#[test]
fn used_variable_warnings_test2() {
    assert_no_warnings!("pub fn a() { let b = 1 b }");
}

#[test]
fn unused_variable_shadowing_test() {
    assert_warning!("pub fn a() { let b = 1 let b = 2 b }");
}

#[test]
fn used_variable_shadowing_test() {
    assert_no_warnings!("pub fn a() { let b = 1 let b = b + 1 b }");
}

#[test]
fn unused_destructure() {
    // Destructure
    assert_warning!("pub fn a(b) { case b { #(c, _) -> 5 } }");
}

#[test]
fn used_destructure() {
    assert_no_warnings!("pub fn a(b) { case b { #(c, _) -> c } }");
}

#[test]
fn unused_imported_module_warnings_test() {
    assert_warning!(
        ("gleam/wibble", "pub fn wobble() { 1 }"),
        "import gleam/wibble"
    );
}

#[test]
fn unused_imported_module_with_alias_warnings_test() {
    assert_warning!(
        ("gleam/wibble", "pub fn wobble() { 1 }"),
        "import gleam/wibble as wobble"
    );
}

// https://github.com/gleam-lang/gleam/issues/2326
#[test]
fn unused_imported_module_with_alias_and_unqualified_name_warnings_test() {
    assert_warning!(
        ("thepackage", "gleam/one", "pub fn two() { 1 }"),
        "import gleam/one.{two} as three"
    );
}

#[test]
fn unused_imported_module_with_alias_and_unqualified_name_no_warnings_test() {
    assert_warning!(
        ("package", "gleam/one", "pub fn two() { 1 }"),
        "import gleam/one.{two} as three\npub fn wibble() { two() }"
    );
}

#[test]
fn unused_imported_module_no_warning_on_used_function_test() {
    assert_no_warnings!(
        ("thepackage", "gleam/wibble", "pub fn wobble() { 1 }"),
        "import gleam/wibble pub fn wibble() { wibble.wobble() }",
    );
}

#[test]
fn unused_imported_module_no_warning_on_used_type_test() {
    assert_no_warnings!(
        ("thepackage", "gleam/wibble", "pub type Wibble = Int"),
        "import gleam/wibble pub fn wibble(a: wibble.Wibble) { a }",
    );
}

#[test]
fn unused_imported_module_no_warning_on_used_unqualified_function_test() {
    assert_no_warnings!(
        ("thepackage", "gleam/wibble", "pub fn wobble() { 1 }"),
        "import gleam/wibble.{wobble} pub fn wibble() { wobble() }",
    );
}

#[test]
fn unused_imported_module_no_warning_on_used_unqualified_type_test() {
    assert_no_warnings!(
        ("thepackage", "gleam/wibble", "pub type Wibble = Int"),
        "import gleam/wibble.{type Wibble} pub fn wibble(a: Wibble) { a }",
    );
}

// https://github.com/gleam-lang/gleam/issues/3313
#[test]
fn imported_module_with_alias_no_warning_when_only_used_in_case_test() {
    assert_no_warnings!(
        (
            "thepackage",
            "gleam/wibble",
            "pub type Wibble { Wibble(Int) }"
        ),
        "import gleam/wibble as f\npub fn wibble(a) { case a { f.Wibble(int) -> { int } }  }",
    );
}

#[test]
fn module_access_registers_import_usage() {
    assert_no_warnings!(
        ("thepackage", "gleam/bibble", "pub const bobble = 1"),
        "import gleam/bibble pub fn main() { bibble.bobble }",
    );
}

// https://github.com/gleam-lang/gleam/issues/978
#[test]
fn bit_pattern_var_use() {
    assert_no_warnings!(
        "
pub fn main(x) {
  let assert <<name_size:8, name:bytes-size(name_size)>> = x
  name
}",
    );
}

// https://github.com/gleam-lang/gleam/issues/989
#[test]
fn alternative_case_clause_pattern_variable_usage() {
    assert_no_warnings!(
        "
pub fn main(s) {
  case s {
    [a] | [a, _] -> a
    _ -> 0
  }
}"
    );
}

// https://github.com/gleam-lang/gleam/issues/1742
#[test]
fn imported_function_referenced_in_constant() {
    assert_no_warnings!(
        ("thepackage", "one", "pub fn two() { 2 }"),
        "
import one

pub const make_two = one.two
"
    );
}

// https://github.com/gleam-lang/gleam/issues/1742
#[test]
fn imported_constructor_referenced_in_constant() {
    assert_no_warnings!(
        ("thepackage", "one", "pub type Two { Two(Int) }"),
        "
import one

pub const make_two = one.Two
"
    );
}

// https://github.com/gleam-lang/gleam/issues/2050
#[test]
fn double_unary_integer_literal() {
    assert_warning!("pub fn main() { let _ = --7 }");
}

#[test]
fn even_number_of_multiple_integer_negations_raise_a_single_warning() {
    assert_warning!("pub fn main() { let _ = ----7 }");
}

#[test]
fn odd_number_of_multiple_integer_negations_raise_a_single_warning_that_highlights_the_unnecessary_ones()
 {
    assert_warning!("pub fn main() { let _ = -----7 }");
}

#[test]
fn even_number_of_multiple_bool_negations_raise_a_single_warning() {
    assert_warning!("pub fn main() { let _ = !!!!True }");
}

#[test]
fn odd_number_of_multiple_bool_negations_raise_a_single_warning_that_highlights_the_unnecessary_ones()
 {
    assert_warning!("pub fn main() { let _ = !!!!!False }");
}

// https://github.com/gleam-lang/gleam/issues/2050
#[test]
fn double_unary_integer_variable() {
    assert_warning!(
        r#"
        pub fn main() {
            let x = 7
            let _ = --x
        }
        "#
    );
}

// https://github.com/gleam-lang/gleam/issues/2050
#[test]
fn double_unary_bool_literal() {
    assert_warning!("pub fn main() { let _ = !!True }");
}

// https://github.com/gleam-lang/gleam/issues/2050
#[test]
fn double_unary_bool_variable() {
    assert_warning!(
        r#"
        pub fn main() {
            let x = True
            let _ = !!x
        }
        "#
    );
}

/// https://github.com/gleam-lang/gleam/issues/2067
#[test]
fn prefer_list_is_empty_over_list_length_eq_0() {
    assert_warning!(
        (
            "gleam_stdlib",
            "gleam/list",
            "pub fn length(_list: List(a)) -> Int { 0 }"
        ),
        r#"
        import gleam/list

        pub fn main() {
            let a_list = []
            let _ = list.length(a_list) == 0
        }
        "#
    );
}

/// https://github.com/gleam-lang/gleam/issues/2067
#[test]
fn prefer_list_is_empty_over_list_length_eq_negative_0() {
    assert_warning!(
        (
            "gleam_stdlib",
            "gleam/list",
            "pub fn length(_list: List(a)) -> Int { 0 }"
        ),
        r#"
        import gleam/list

        pub fn main() {
            let a_list = []
            let _ = list.length(a_list) == -0
        }
        "#
    );
}

/// https://github.com/gleam-lang/gleam/issues/2067
#[test]
fn prefer_list_is_empty_over_0_eq_list_length() {
    assert_warning!(
        (
            "gleam_stdlib",
            "gleam/list",
            "pub fn length(_list: List(a)) -> Int { 0 }"
        ),
        r#"
        import gleam/list

        pub fn main() {
            let a_list = []
            let _ = 0 == list.length(a_list)
        }
        "#
    );
}

/// https://github.com/gleam-lang/gleam/issues/2067
#[test]
fn prefer_list_is_empty_over_negative_0_eq_list_length() {
    assert_warning!(
        (
            "gleam_stdlib",
            "gleam/list",
            "pub fn length(_list: List(a)) -> Int { 0 }"
        ),
        r#"
        import gleam/list

        pub fn main() {
            let a_list = []
            let _ = -0 == list.length(a_list)
        }
        "#
    );
}

/// https://github.com/gleam-lang/gleam/issues/2067
#[test]
fn prefer_list_is_empty_over_list_length_not_eq_0() {
    assert_warning!(
        (
            "gleam_stdlib",
            "gleam/list",
            "pub fn length(_list: List(a)) -> Int { 0 }"
        ),
        r#"
        import gleam/list

        pub fn main() {
            let a_list = []
            let _ = list.length(a_list) != 0
        }
        "#
    );
}

/// https://github.com/gleam-lang/gleam/issues/2067
#[test]
fn prefer_list_is_empty_over_0_not_eq_list_length() {
    assert_warning!(
        (
            "gleam_stdlib",
            "gleam/list",
            "pub fn length(_list: List(a)) -> Int { 0 }"
        ),
        r#"
        import gleam/list

        pub fn main() {
            let a_list = []
            let _ = 0 != list.length(a_list)
        }
        "#
    );
}

/// https://github.com/gleam-lang/gleam/issues/2067
#[test]
fn prefer_list_is_empty_over_list_length_lt_eq_0() {
    assert_warning!(
        (
            "gleam_stdlib",
            "gleam/list",
            "pub fn length(_list: List(a)) -> Int { 0 }"
        ),
        r#"
        import gleam/list

        pub fn main() {
            let a_list = []
            let _ = list.length(a_list) <= 0
        }
        "#
    );
}

/// https://github.com/gleam-lang/gleam/issues/2067
#[test]
fn prefer_list_is_empty_over_list_length_lt_1() {
    assert_warning!(
        (
            "gleam_stdlib",
            "gleam/list",
            "pub fn length(_list: List(a)) -> Int { 0 }"
        ),
        r#"
        import gleam/list

        pub fn main() {
            let a_list = []
            let _ = list.length(a_list) < 1
        }
        "#
    );
}

/// https://github.com/gleam-lang/gleam/issues/4861
#[test]
fn prefer_list_is_empty_over_list_length_gt_negative_0() {
    assert_warning!(
        (
            "gleam_stdlib",
            "gleam/list",
            "pub fn length(_list: List(a)) -> Int { 0 }"
        ),
        r#"
        import gleam/list

        pub fn main() {
            let a_list = []
            let _ = list.length(a_list) > 0
        }
        "#
    );
}

/// https://github.com/gleam-lang/gleam/issues/4861
#[test]
fn prefer_list_is_empty_over_negative_0_lt_list_length() {
    assert_warning!(
        (
            "gleam_stdlib",
            "gleam/list",
            "pub fn length(_list: List(a)) -> Int { 0 }"
        ),
        r#"
        import gleam/list

        pub fn main() {
            let a_list = []
            let _ = 0 < list.length(a_list)
        }
        "#
    );
}

/// https://github.com/gleam-lang/gleam/issues/4861
#[test]
fn prefer_list_is_empty_over_list_length_gt_0() {
    assert_warning!(
        (
            "gleam_stdlib",
            "gleam/list",
            "pub fn length(_list: List(a)) -> Int { 0 }"
        ),
        r#"
        import gleam/list

        pub fn main() {
            let a_list = []
            let _ = list.length(a_list) > 0
        }
        "#
    );
}

/// https://github.com/gleam-lang/gleam/issues/4861
#[test]
fn prefer_list_is_empty_over_0_lt_list_length() {
    assert_warning!(
        (
            "gleam_stdlib",
            "gleam/list",
            "pub fn length(_list: List(a)) -> Int { 0 }"
        ),
        r#"
        import gleam/list

        pub fn main() {
            let a_list = []
            let _ = 0 < list.length(a_list)
        }
        "#
    );
}

/// https://github.com/gleam-lang/gleam/issues/2067
#[test]
fn allow_list_length_eq_1() {
    assert_no_warnings!(
        (
            "gleam_stdlib",
            "gleam/list",
            "pub fn length(_list: List(a)) -> Int { 0 }"
        ),
        r#"
        import gleam/list

        pub fn main() {
            let a_list = []
            let _ = list.length(a_list) == 1
        }
        "#
    );
}

/// https://github.com/gleam-lang/gleam/issues/2067
#[test]
fn allow_1_eq_list_length() {
    assert_no_warnings!(
        (
            "gleam_stdlib",
            "gleam/list",
            "pub fn length(_list: List(a)) -> Int { 0 }"
        ),
        r#"
        import gleam/list

        pub fn main() {
            let a_list = []
            let _ = 1 == list.length(a_list)
        }
        "#
    );
}

/// https://github.com/gleam-lang/gleam/issues/2067
#[test]
fn allow_list_length_eq_3() {
    assert_no_warnings!(
        (
            "gleam_stdlib",
            "gleam/list",
            "pub fn length(_list: List(a)) -> Int { 0 }"
        ),
        r#"
        import gleam/list

        pub fn main() {
            let a_list = []
            let _ = list.length(a_list) == 3
        }
        "#
    );
}

/// https://github.com/gleam-lang/gleam/issues/2067
#[test]
fn allow_1_lt_list_length() {
    assert_no_warnings!(
        (
            "gleam_stdlib",
            "gleam/list",
            "pub fn length(_list: List(a)) -> Int { 0 }"
        ),
        r#"
        import gleam/list

        pub fn main() {
            let a_list = []
            let _ = 1 < list.length(a_list)
        }
        "#
    );
}

/// https://github.com/gleam-lang/gleam/issues/2067
#[test]
fn allow_list_length_gt_1() {
    assert_no_warnings!(
        (
            "gleam_stdlib",
            "gleam/list",
            "pub fn length(_list: List(a)) -> Int { 0 }"
        ),
        r#"
        import gleam/list

        pub fn main() {
            let a_list = []
            let _ = list.length(a_list) > 1
        }
        "#
    );
}

#[test]
fn unused_external_function_arguments() {
    // https://github.com/gleam-lang/gleam/issues/2259
    assert_no_warnings!(
        r#"
@external(erlang, "go", "go")
pub fn go(a: item_a) -> Nil
"#,
    );
}

#[test]
fn importing_non_direct_dep_package() {
    // Warn if an imported module is from a package that is not a direct dependency
    assert_warning!(
        // Magic string package name that the test setup will detect to not
        // register this package as a dep.
        ("non-dependency-package", "some_module", "pub const x = 1"),
        r#"
import some_module
pub const x = some_module.x
        "#
    );
}

#[test]
fn deprecated_constant() {
    assert_warning!(
        r#"
@deprecated("Don't use this!")
pub const a = Nil

pub fn b() {
  a
}
"#
    );
}

#[test]
fn deprecated_imported_constant() {
    assert_warning!(
        (
            "package",
            "module",
            r#"@deprecated("Don't use this!") pub const a = Nil"#
        ),
        r#"
import module

pub fn a() {
  module.a
}
"#
    );
}

#[test]
fn deprecated_imported_unqualified_constant() {
    assert_warning!(
        (
            "package",
            "module",
            r#"@deprecated("Don't use this!") pub const a = Nil"#
        ),
        r#"
import module.{a}

pub fn b() {
  a
}
"#
    );
}

#[test]
fn deprecated_function() {
    assert_warning!(
        r#"
@deprecated("Don't use this!")
pub fn a() {
  Nil
}

pub fn b() {
  a
}
        "#
    );
}

#[test]
fn deprecated_imported_function() {
    assert_warning!(
        (
            "package",
            "module",
            r#"@deprecated("Don't use this!") pub fn a() { Nil }"#
        ),
        r#"
import module

pub fn a() {
  module.a
}
"#
    );
}

#[test]
fn deprecated_imported_call_function() {
    assert_warning!(
        (
            "package",
            "module",
            r#"@deprecated("Don't use this!") pub fn a() { Nil }"#
        ),
        r#"
import module

pub fn a() {
  module.a()
}
"#
    );
}

#[test]
fn deprecated_imported_unqualified_function() {
    assert_warning!(
        (
            "package",
            "module",
            r#"@deprecated("Don't use this!") pub fn a() { Nil }"#
        ),
        r#"
import module.{a}

pub fn b() {
  a
}
"#
    );
}

#[test]
fn deprecated_type_used_in_alias() {
    assert_warning!(
        r#"
@deprecated("Don't use this!")
pub type Cat {
    Cat(name: String)
}

pub type Dog = Cat
        "#
    );
}

#[test]
fn deprecated_type_used_as_arg() {
    assert_warning!(
        r#"
@deprecated("Don't use this!")
pub type Cat {
    Cat(name: String)
}

pub fn cat_name(cat: Cat) {
  cat.name
}
        "#
    );
}

#[test]
fn deprecated_type_used_as_case_clause() {
    assert_warning!(
        r#"
@deprecated("The type Animal has been deprecated.")
pub type Animal {
    Cat
    Dog
}

pub fn sound(animal) -> String {
  case animal {
    Dog -> "Woof"
    Cat -> "Meow"
  }
}

pub fn main(){
    let cat = Cat
    sound(cat)
}
        "#
    );
}

#[test]
fn const_bytes_option() {
    assert_no_warnings!("pub const x = <<<<>>:bits>>");
}

#[test]
fn unused_module_wuth_alias_warning_test() {
    assert_warning!(
        ("gleam/wibble", "pub const one = 1"),
        "import gleam/wibble as wobble"
    );
}

#[test]
fn unused_alias_warning_test() {
    assert_warning!(
        ("gleam/wibble", "pub const one = 1"),
        r#"
            import gleam/wibble.{one} as wobble
            pub const one = one
        "#,
    );
}

#[test]
fn used_type_with_import_alias_no_warning_test() {
    assert_no_warnings!(
        ("gleam", "gleam/wibble", "pub const one = 1"),
        "import gleam/wibble as _wobble"
    );
}

#[test]
fn discarded_module_no_warnings_test() {
    assert_no_warnings!(
        ("gleam", "wibble", "pub const one = 1"),
        "import wibble as _wobble"
    );
}

#[test]
fn unused_alias_for_duplicate_module_no_warning_for_alias_test() {
    assert_warning!(
        ("a/wibble", "pub const one = 1"),
        ("b/wibble", "pub const two = 2"),
        r#"
            import a/wibble
            import b/wibble as wobble
            pub const one = wibble.one
        "#,
    );
}

#[test]
fn result_in_case_discarded() {
    assert_warning!(
        "
pub fn main(x) {
  case x {
    _ -> Error(Nil)
  }
  Nil
}"
    );
}

#[test]
fn pattern_matching_on_literal_tuple() {
    assert_warning!(
        "pub fn main() {
        case #(1, 2) {
            _ -> Nil
        }
      }"
    );
}

#[test]
fn pattern_matching_on_multiple_literal_tuples() {
    assert_warning!(
        "pub fn main() {
        let wibble = 1
        case #(1, 2), #(wibble, wibble) {
            _, _ -> Nil
        }
      }"
    );
}

#[test]
fn pattern_matching_on_tuples_doesnt_raise_a_warning() {
    assert_no_warnings!(
        "pub fn main() {
        let wibble = #(1, 2)
        // This doesn't raise a warning since `wibble` is not a literal tuple.
        case wibble {
            _ -> Nil
        }
      }"
    );
}

#[test]
fn pattern_matching_on_literal_empty_tuple() {
    assert_warning!(
        "pub fn main() {
        case #() {
            _ -> Nil
        }
      }"
    );
}

#[test]
fn pattern_matching_on_literal_list() {
    assert_warning!(
        "pub fn main() {
        case [1, 2] {
            _ -> Nil
        }
      }"
    );
}

#[test]
fn pattern_matching_on_literal_list_with_tail() {
    assert_warning!(
        "pub fn main() {
        case [1, 2, ..[]] {
            _ -> Nil
        }
      }"
    );
}

#[test]
fn pattern_matching_on_literal_empty_list() {
    assert_warning!(
        "pub fn main() {
        case [] {
            _ -> Nil
        }
      }"
    );
}

#[test]
fn pattern_matching_on_literal_empty_bit_array() {
    assert_warning!(
        "pub fn main() {
        case <<>> {
            _ -> Nil
        }
      }"
    );
}

#[test]
fn pattern_matching_on_literal_record() {
    assert_warning!(
        "
pub type Wibble { Wibble(Int) }
pub fn main() {
  let n = 1
  case Wibble(n) {
    _ -> Nil
  }
}"
    );
}

#[test]
fn pattern_matching_on_literal_record_with_no_args() {
    assert_warning!(
        "
pub type Wibble { Wibble }
pub fn main() {
  case Wibble {
    _ -> Nil
  }
}"
    );
}

#[test]
fn pattern_matching_on_literal_int() {
    assert_warning!(
        "
pub type Wibble { Wibble }
pub fn main() {
  case 1 {
    _ -> Nil
  }
}"
    );
}

#[test]
fn pattern_matching_on_literal_float() {
    assert_warning!(
        "
pub type Wibble { Wibble }
pub fn main() {
  case 1.0 {
    _ -> Nil
  }
}"
    );
}

#[test]
fn pattern_matching_on_literal_string() {
    assert_warning!(
        "
pub type Wibble { Wibble }
pub fn main() {
  case \"hello\" {
    _ -> Nil
  }
}"
    );
}

#[test]
fn opaque_external_type_raises_a_warning() {
    assert_warning!("pub opaque type External");
}

#[test]
fn unused_binary_operation_raises_a_warning() {
    assert_warning!(
        r#"
pub fn main() {
  let string = "a" <> "b" "c" <> "d"
  string
}
"#
    );
}

#[test]
fn unused_record_access_raises_a_warning() {
    assert_warning!(
        r#"
pub type Thing {
  Thing(value: Int)
}

pub fn main() {
  let thing = Thing(1)
  thing.value
  1
}
"#
    );
}

#[test]
fn unused_record_constructor_raises_a_warning() {
    assert_warning!(
        r#"
pub type Thing {
  Thing(value: Int)
}

pub fn main() {
  Thing(1)
  1
}
"#
    );
}

#[test]
fn unused_record_update_raises_a_warning() {
    assert_warning!(
        r#"
pub type Thing {
  Thing(value: Int, other: Int)
}

pub fn main() {
  let thing = Thing(1, 2)
  Thing(..thing, value: 1)
  1
}
"#
    );
}

#[test]
fn unused_variable_raises_a_warning() {
    assert_warning!(
        r#"
pub fn main() {
  let number = 1
  number
  1
}
"#
    );
}

#[test]
fn unused_function_literal_raises_a_warning() {
    assert_warning!(
        r#"
pub fn main() {
  fn(n) { n + 1 }
  1
}
"#
    );
}

#[test]
fn unused_tuple_index_raises_a_warning() {
    assert_warning!(
        r#"
pub fn main() {
  #(1, 2).0
  1
}
"#
    );
}

#[test]
fn unused_bool_negation_raises_a_warning() {
    assert_warning!(
        r#"
pub fn main() {
  !True
  1
}
"#
    );
}

#[test]
fn unused_int_negation_raises_a_warning() {
    assert_warning!(
        r#"
pub fn main() {
  -1
  1
}
"#
    );
}

#[test]
fn unused_pipeline_ending_with_variant_raises_a_warning() {
    assert_warning!(
        r#"
pub type Wibble(a) { Wibble(a) }
pub fn wibble(a) { a }

pub fn main() {
  1 |> wibble |> Wibble
  1
}
"#
    );
}

#[test]
fn unused_pipeline_ending_with_variant_raises_a_warning_2() {
    assert_warning!(
        ("wibble", "pub type Wibble { Wibble(Int) }"),
        r#"
import wibble

pub fn wobble(a) { a }

pub fn main() {
  1 |> wobble |> wibble.Wibble
  1
}
"#
    );
}

#[test]
fn unused_pipeline_not_ending_with_variant_raises_no_warnings() {
    assert_no_warnings!(
        r#"
pub type Wibble(a) { Wibble(a) }
pub fn wibble(a) { echo a }

pub fn main() {
  1 |> wibble |> wibble
  1
}
"#
    );
}

#[test]
fn unused_module_select_constructor() {
    assert_warning!(
        ("wibble", "pub type Wibble { Wibble(Int) }"),
        r#"
import wibble

pub fn main() {
  wibble.Wibble
  1
}
"#
    );
}

#[test]
fn unused_module_select_constructor_call() {
    assert_warning!(
        ("wibble", "pub type Wibble { Wibble(Int) }"),
        r#"
import wibble

pub fn main() {
  wibble.Wibble(1)
  1
}
"#
    );
}

#[test]
fn unused_module_select_function() {
    assert_warning!(
        ("wibble", "pub fn println(a) { Nil }"),
        r#"
import wibble

pub fn main() {
  wibble.println
  1
}
"#
    );
}

#[test]
fn unused_module_select_const() {
    assert_warning!(
        ("wibble", "pub const a = 1"),
        r#"
import wibble

pub fn main() {
  wibble.a
  1
}
"#
    );
}

#[test]
fn calling_function_from_other_module_is_not_marked_unused() {
    assert_no_warnings!(
        ("wibble", "wibble", "pub fn println(a) { panic }"),
        r#"
import wibble

pub fn main() {
  wibble.println("hello!")
  1
}
"#
    );
}

/*

TODO: These tests are commented out until we figure out a better way to deal
      with reexports of internal types and reintroduce the warning.
      As things stand it would break both Lustre and Mist.
      You can see the thread starting around here for more context:
      https://discord.com/channels/768594524158427167/768594524158427170/1227250677734969386

#[test]
fn internal_type_in_public_function_return() {
    assert_warning!(
        "
@internal
pub type Wibble {
  Wibble
}

pub fn wibble() -> Wibble { Wibble }
"
    );
}

#[test]
fn type_from_internal_module_in_public_function_return() {
    assert_warning!(
        ("thepackage/internal", "pub type Wibble { Wibble }"),
        "
import thepackage/internal.{type Wibble, Wibble}

pub fn wibble() -> Wibble {
  Wibble
}"
    );
}

#[test]
fn internal_type_in_public_function_argument() {
    assert_warning!(
        "
@internal
pub type Wibble {
  Wibble
}

pub fn wibble(_wibble: Wibble) -> Int { 1 }
"
    );
}

#[test]
fn type_from_internal_module_in_public_function_argument() {
    assert_warning!(
        ("thepackage/internal", "pub type Wibble { Wibble }"),
        "
import thepackage/internal.{type Wibble}

pub fn wibble(_wibble: Wibble) -> Int {
  1
}
"
    );
}

#[test]
fn internal_type_in_public_constructor() {
    assert_warning!(
        "
@internal
pub type Wibble {
  Wibble
}

pub type Wobble {
    Wobble(Wibble)
}
"
    );
}

#[test]
fn type_from_internal_module_in_public_constructor() {
    assert_warning!(
        ("thepackage/internal", "pub type Wibble { Wibble }"),
        "
import thepackage/internal.{type Wibble}

pub type Wobble {
  Wobble(Wibble)
}"
    );
}

#[test]
fn type_from_internal_module_dependency_in_public_constructor() {
    assert_warning!(
        ("dep", "dep/internal", "pub type Wibble { Wibble }"),
        "
import dep/internal.{type Wibble}

pub type Wobble {
  Wobble(Wibble)
}"
    );
}

*/

#[test]
fn redundant_let_assert() {
    assert_warning!(
        "
pub fn main() {
  let assert wibble = [1, 2, 3]
  wibble
}
"
    );
}

#[test]
fn redundant_let_assert_on_custom_type() {
    assert_warning!(
        "
pub type Wibble {
    Wibble(Int, Bool)
}

pub fn main() {
  let assert Wibble(_, bool) = Wibble(1, True)
  bool
}
"
    );
}

#[test]
fn panic_used_as_function() {
    assert_warning!(
        "pub fn main() {
          panic()
        }"
    );
}

#[test]
fn panic_used_as_function_2() {
    assert_warning!(
        "pub fn main() {
          panic(1)
        }"
    );
}

#[test]
fn panic_used_as_function_3() {
    assert_warning!(
        "pub fn main() {
          panic(1, Nil)
        }"
    );
}

#[test]
fn todo_used_as_function() {
    assert_warning!(
        "pub fn main() {
          todo()
        }"
    );
}

#[test]
fn todo_used_as_function_2() {
    assert_warning!(
        "pub fn main() {
          todo(1)
        }"
    );
}

#[test]
fn todo_used_as_function_3() {
    assert_warning!(
        "pub fn main() {
          todo(1, Nil)
        }"
    );
}

#[test]
fn unreachable_warning_1() {
    assert_warning!(
        "pub fn main() {
          panic
          1
        }"
    );
}

#[test]
fn unreachable_warning_2() {
    assert_warning!(
        "pub fn main() {
          let _ = panic
          1
        }"
    );
}

#[test]
fn unreachable_warning_if_all_branches_panic() {
    assert_warning!(
        "pub fn main() {
          let n = 1
          case n {
            0 -> panic
            _ -> panic
          }
          1
        }"
    );
}

#[test]
fn unreachable_warning_if_all_branches_panic_2() {
    assert_warning!(
        "pub fn main() {
          let n = 1
          case n {
            0 -> {
              panic
              2
            }
            _ -> panic
          }
          1
        }"
    );
}

#[test]
fn no_unreachable_warning_if_at_least_a_branch_is_reachable() {
    assert_no_warnings!(
        "pub fn main() {
          let n = 1
          case n {
            0 -> panic
            _ -> 1
          }
          1
        }"
    );
}

#[test]
fn unreachable_warning_doesnt_escape_out_of_a_block_if_panic_is_not_last() {
    assert_warning!(
        "pub fn main() {
          let n = {
            panic
            1
          }
          n
        }"
    );
}

#[test]
fn unreachable_warning_on_following_expression_if_panic_is_last_in_a_block() {
    assert_warning!(
        "pub fn main() {
          let _ = {
            panic
          }
          1
        }"
    );
}

#[test]
fn unreachable_function_argument_if_panic_is_argument() {
    assert_warning!(
        "
        pub fn wibble(_, _) { 1 }
        pub fn main() {
          wibble(panic, 1)
        }"
    );
}

#[test]
fn unreachable_function_call_if_panic_is_last_argument_1() {
    assert_warning!(
        "
        pub fn wibble(_, _) { 1 }
        pub fn main() {
          wibble(1, panic)
          1
        }"
    );
}

#[test]
fn unreachable_function_call_if_panic_is_last_argument_2() {
    assert_warning!(
        "
        pub fn wibble(_, _) { 1 }
        pub fn main() {
          wibble(1, panic)
        }"
    );
}

#[test]
fn no_unreachable_warning_if_panic_comes_last_in_function_body() {
    assert_no_warnings!(
        "
        pub fn wibble() { panic }
        pub fn main() { panic }"
    );
}

#[test]
fn unreachable_code_for_panic_as_first_pipeline_item() {
    assert_warning!(
        "
        pub fn wibble(_) { 1 }
        pub fn main() {
            panic |> wibble
        }
        "
    );
}

#[test]
fn panic_used_as_function_inside_pipeline() {
    assert_warning!(
        "
        pub fn wibble(_) { 1 }
        pub fn main() {
            1 |> panic |> wibble
        }
        "
    );
}

#[test]
fn unreachable_warning_for_panic_as_last_item_of_pipe_on_next_expression() {
    assert_warning!(
        r#"
        pub fn wibble(_) { 1 }
        pub fn main() {
            1 |> wibble |> panic
            "unreachable"
        }
        "#
    );
}

#[test]
fn doesnt_warn_twice_for_unreachable_code_if_has_already_warned_in_a_block_1() {
    assert_warning!(
        r#"
        pub fn wibble(_) { 1 }
        pub fn main() {
            panic
            let _ = "unreachable" // warning here
            panic
            "no warning here!"
        }
        "#
    );
}

#[test]
fn doesnt_warn_twice_for_unreachable_code_if_has_already_warned_in_a_block_2() {
    assert_warning!(
        r#"
        pub fn main() {
            let _ = {
              panic
              1 // warning here
            }
            "no warning here!"
        }
        "#
    );
}

#[test]
fn unreachable_use_after_panic() {
    assert_warning!(
        r#"
        pub fn wibble(_) { 1 }
        pub fn main() {
            panic
            use <- wibble
            1
        }
        "#
    );
}

#[test]
fn unreachable_code_after_case_subject_panics_1() {
    assert_warning!(
        r#"
        pub fn main(a, b) {
            case a, panic, b {
                _, _, _ -> "no warning here!"
            }
        }
        "#
    );
}

#[test]
fn unreachable_code_after_case_subject_panics_2() {
    assert_warning!(
        r#"
        pub fn main(a, b) {
            case a, b, panic {
                _, _, _ -> "no warning here!"
            }
            "warning here!"
        }
        "#
    );
}

#[test]
fn unreachable_code_analysis_treats_anonymous_functions_independently_1() {
    assert_no_warnings!(
        r#"
        pub fn main() {
            let _ = fn() {
              panic
            }
            "no warning here!"
        }
        "#
    );
}

#[test]
fn unreachable_code_analysis_treats_anonymous_functions_independently_2() {
    assert_warning!(
        r#"
        pub fn main() {
            let _ = fn() {
              panic
              "warning here!"
            }
            panic
            "warning here!"
        }
        "#
    );
}

#[test]
fn unreachable_code_analysis_treats_anonymous_functions_independently_3() {
    assert_warning!(
        r#"
        pub fn main() {
            panic
            let _ = "warning here!"
            let _ = fn() {
              panic
              "warning here!"
            }
        }
        "#
    );
}

#[test]
fn no_warnings_for_matches_used_like_ifs() {
    assert_no_warnings!(
        r#"
    pub fn main() {
        case True {
          _ if True -> 1
          _ -> 2
        }
    }
        "#
    );
}

#[test]
fn no_warnings_for_matches_used_like_ifs_2() {
    assert_no_warnings!(
        r#"
    pub fn main() {
        case 1 {
          _ if True -> 1
          _ -> 2
        }
    }
        "#
    );
}

#[test]
fn warnings_for_matches_on_literal_values_that_are_not_like_an_if_1() {
    assert_warning!(
        r#"
    pub fn main() {
        case True {
          _ -> 1
        }
    }
        "#
    );
}

#[test]
fn warnings_for_matches_on_literal_values_that_are_not_like_an_if_2() {
    assert_warning!(
        r#"
    pub fn main() {
        case True {
          True -> 1
        }
    }
        "#
    );
}

#[test]
fn redundant_function_capture_in_pipe_1() {
    assert_warning!(
        "
  pub fn wibble(_, _) { 1 }

  pub fn main() {
    1 |> wibble(_, 2) |> wibble(2)
  }
"
    );
}

#[test]
fn redundant_function_capture_in_pipe_2() {
    assert_warning!(
        "
  pub fn wobble(_) { 1 }

  pub fn main() {
    1 |> wobble(_) |> wobble
  }
"
    );
}

#[test]
fn redundant_function_capture_in_pipe_3() {
    assert_warning!(
        "
  pub fn wobble(_) { 1 }

  pub fn main() {
    1 |> wobble |> wobble(_)
  }
"
    );
}

#[test]
fn redundant_function_capture_in_pipe_4() {
    assert_warning!(
        "
  pub fn wibble(_, _) { 1 }

  pub fn main() {
    1 |> wibble(2) |> wibble(_, 2)
  }
"
    );
}

#[test]
fn redundant_function_capture_in_pipe_5() {
    assert_no_warnings!(
        "
  pub fn wibble(_, _) { 1 }

  pub fn main() {
    1 |> wibble(2, _)
  }
"
    );
}

#[test]
fn deprecated_list_append_syntax() {
    assert_warning!(
        r#"
    pub fn main() {
      let letters = ["b", "c"]
      ["a"..letters]
    }
        "#
    );
}

#[test]
fn deprecated_list_pattern_syntax() {
    assert_warning!(
        r#"
    pub fn main() {
      let letters = ["b", "c"]
      case letters {
        ["a"..rest] -> rest
        _ -> []
      }
    }
        "#
    );
}

// https://github.com/gleam-lang/gleam/issues/3383
#[test]
fn deprecated_list_pattern_syntax_1() {
    assert_warning!(
        r#"
    pub fn main() {
      let letters = ["b", "c"]
      case letters {
        [] -> []
        [..] -> []
      }
    }
        "#
    );
}

// https://github.com/gleam-lang/gleam/issues/3473
#[test]
fn deprecated_record_pattern_syntax() {
    assert_warning!(
        r#"
pub type Wibble {
  Wibble(one: Int, two: Int)
}

pub fn main() {
  let wibble = Wibble(one: 1, two: 2)
  case wibble {
    Wibble(one: one ..) -> one
  }
}
"#
    );
}

#[test]
fn deprecated_record_pattern_syntax_with_no_labels() {
    assert_warning!(
        r#"
pub type Wibble {
  Wibble(one: Int, two: Int)
}

pub fn main() {
  let wibble = Wibble(one: 1, two: 2)
  case wibble {
    Wibble(one ..) -> one
  }
}
"#
    );
}

#[test]
fn deprecated_record_pattern_syntax_with_label_shorthand() {
    assert_warning!(
        r#"
pub type Wibble {
  Wibble(one: Int, two: Int)
}

pub fn main() {
  let wibble = Wibble(one: 1, two: 2)
  case wibble {
    Wibble(one: ..) -> one
  }
}
"#
    );
}

#[test]
fn deprecated_record_pattern_syntax_has_no_warning_if_everything_is_discarded() {
    assert_no_warnings!(
        r#"
pub type Wibble {
  Wibble(one: Int, two: Int)
}

pub fn main() {
  let wibble = Wibble(one: 1, two: 2)
  case wibble {
    Wibble(..) -> 1
  }
}
"#
    );
}

#[test]
fn deprecated_record_pattern_syntax_has_no_warning_if_there_is_a_comma_before_spread() {
    assert_no_warnings!(
        r#"
pub type Wibble {
  Wibble(one: Int, two: Int)
}

pub fn main() {
  let wibble = Wibble(one: 1, two: 2)
  case wibble {
    Wibble(one: one, ..) -> one
  }
}
"#
    );
}

#[test]
fn unused_label_shorthand_pattern_arg() {
    assert_warning!(
        r#"
pub type Wibble { Wibble(arg1: Int, arg2: Bool ) }

pub fn main() {
  let Wibble(arg1:, arg2:) = Wibble(1, True)
  arg1
}
"#
    );
}

#[test]
fn unused_label_shorthand_pattern_arg_shadowing() {
    assert_warning!(
        r#"
pub type Wibble { Wibble(arg1: Int, arg2: Bool ) }

pub fn main() {
  let Wibble(arg1:, arg2:) = Wibble(1, True)
  let arg1 = False
  arg1
}
"#
    );
}

#[test]
fn internal_annotation_on_constant_requires_v1_1() {
    assert_warnings_with_gleam_version!(
        Range::higher_than(Version::new(1, 0, 0)),
        "
@internal
pub const wibble = 1
",
    );
}

#[test]
fn internal_annotation_on_type_requires_v1_1() {
    assert_warnings_with_gleam_version!(
        Range::higher_than(Version::new(1, 0, 0)),
        "
@internal
pub type Wibble
",
    );
}

#[test]
fn internal_annotation_on_function_requires_v1_1() {
    assert_warnings_with_gleam_version!(
        Range::higher_than(Version::new(1, 0, 0)),
        "
@internal
pub fn wibble() { Nil }
",
    );
}

#[test]
fn nested_tuple_access_requires_v1_1() {
    assert_warnings_with_gleam_version!(
        Range::higher_than(Version::new(1, 0, 0)),
        "
pub fn main() {
  let tuple = #(1, #(1, 1))
  tuple.1.0
}
",
    );
}

#[test]
fn javascript_external_module_with_at_requires_v1_2() {
    assert_warnings_with_gleam_version!(
        Range::higher_than(Version::new(1, 0, 0)),
        "
@external(javascript, \"module@module\", \"func\")
pub fn main() { Nil }
",
    );
}

#[test]
fn int_plus_in_guards_requires_v1_3() {
    assert_warnings_with_gleam_version!(
        Range::higher_than(Version::new(1, 0, 0)),
        "
pub fn main() {
  case Nil {
    _ if 1 + 1 == 2 -> Nil
    _ -> Nil
  }
}
",
    );
}

#[test]
fn float_plus_in_guards_requires_v1_3() {
    assert_warnings_with_gleam_version!(
        Range::higher_than(Version::new(1, 0, 0)),
        "
pub fn main() {
  case Nil {
    _ if 1.0 +. 1.0 == 2.0 -> Nil
    _ -> Nil
  }
}
",
    );
}

#[test]
fn int_minus_in_guards_requires_v1_3() {
    assert_warnings_with_gleam_version!(
        Range::higher_than(Version::new(1, 0, 0)),
        "
pub fn main() {
  case Nil {
    _ if 1 - 1 == 0 -> Nil
    _ -> Nil
  }
}
",
    );
}

#[test]
fn float_minus_in_guards_requires_v1_3() {
    assert_warnings_with_gleam_version!(
        Range::higher_than(Version::new(1, 0, 0)),
        "
pub fn main() {
  case Nil {
    _ if 1.0 -. 1.0 == 0.0 -> Nil
    _ -> Nil
  }
}
",
    );
}

#[test]
fn int_multiplication_in_guards_requires_v1_3() {
    assert_warnings_with_gleam_version!(
        Range::higher_than(Version::new(1, 0, 0)),
        "
pub fn main() {
  case Nil {
  _ if 1 * 1 == 0 -> Nil
  _ -> Nil
  }
}
",
    );
}

#[test]
fn float_multiplication_in_guards_requires_v1_3() {
    assert_warnings_with_gleam_version!(
        Range::higher_than(Version::new(1, 0, 0)),
        "
pub fn main() {
  case Nil {
  _ if 1.0 *. 1.0 == 0.0 -> Nil
  _ -> Nil
  }
}
",
    );
}

#[test]
fn int_divide_in_guards_requires_v1_3() {
    assert_warnings_with_gleam_version!(
        Range::higher_than(Version::new(1, 0, 0)),
        "
pub fn main() {
  case Nil {
  _ if 1 / 1 == 0 -> Nil
  _ -> Nil
  }
}
",
    );
}

#[test]
fn float_divide_in_guards_requires_v1_3() {
    assert_warnings_with_gleam_version!(
        Range::higher_than(Version::new(1, 0, 0)),
        "
pub fn main() {
  case Nil {
  _ if 1.0 /. 1.0 == 0.0 -> Nil
  _ -> Nil
  }
}
",
    );
}

#[test]
fn int_remainder_in_guards_requires_v1_3() {
    assert_warnings_with_gleam_version!(
        Range::higher_than(Version::new(1, 0, 0)),
        "
pub fn main() {
  case Nil {
  _ if 1 % 1 == 0 -> Nil
  _ -> Nil
  }
}
",
    );
}

#[test]
fn label_shorthand_in_constand_requires_v1_4() {
    assert_warnings_with_gleam_version!(
        Range::higher_than(Version::new(1, 0, 0)),
        "
pub type Wibble { Wibble(wibble: Int) }

pub const wibble = 1
pub const wobble = Wibble(wibble:)
",
    );
}

#[test]
fn label_shorthand_in_call_requires_v1_4() {
    assert_warnings_with_gleam_version!(
        Range::higher_than(Version::new(1, 0, 0)),
        "
pub type Wibble { Wibble(wibble: Int) }

pub fn main() {
  let wibble = 1
  Wibble(wibble:)
}
",
    );
}

#[test]
fn label_shorthand_in_pattern_requires_v1_4() {
    assert_warnings_with_gleam_version!(
        Range::higher_than(Version::new(1, 0, 0)),
        "
pub type Wibble { Wibble(wibble: Int) }

pub fn main(wibble) {
  case wibble {
    Wibble(wibble:) -> wibble
  }
}
",
    );
}

#[test]
fn constant_string_concatenation_requires_v1_4() {
    assert_warnings_with_gleam_version!(
        Range::higher_than(Version::new(1, 0, 0)),
        "pub const string = \"wibble\" <> \"wobble\""
    );
}

#[test]
fn missing_utf_8_option_in_bit_array_segment_requires_v1_5() {
    assert_warnings_with_gleam_version!(
        Range::higher_than(Version::new(1, 0, 0)),
        "
pub fn main() {
  <<\"hello\">>
}
",
    );
}

#[test]
fn missing_utf_8_option_in_bit_array_constant_segment_requires_v1_5() {
    assert_warnings_with_gleam_version!(
        Range::higher_than(Version::new(1, 0, 0)),
        "pub const bits = <<\"hello\">>"
    );
}

#[test]
fn missing_utf_8_option_in_bit_array_pattern_segment_requires_v1_5() {
    assert_warnings_with_gleam_version!(
        Range::higher_than(Version::new(1, 0, 0)),
        "
pub fn main(a) {
  case a {
    <<\"hello\">> -> Nil
    _ -> Nil
  }
}
",
    );
}

#[test]
fn missing_float_option_in_bit_array_segment_requires_v1_10() {
    assert_warnings_with_gleam_version!(
        Range::higher_than(Version::new(1, 0, 0)),
        "
pub fn main() {
  <<1.2>>
}
",
    );
}

#[test]
fn missing_float_option_in_bit_array_constant_segment_requires_v1_10() {
    assert_warnings_with_gleam_version!(
        Range::higher_than(Version::new(1, 0, 0)),
        "pub const bits = <<1.2>>"
    );
}

#[test]
fn missing_float_option_in_bit_array_pattern_segment_requires_v1_10() {
    assert_warnings_with_gleam_version!(
        Range::higher_than(Version::new(1, 0, 0)),
        "
pub fn main(a) {
  case a {
    <<1.2>> -> Nil
    _ -> Nil
  }
}
",
    );
}

#[test]
fn record_update_variant_inference_requires_v1_6() {
    assert_warnings_with_gleam_version!(
        Range::higher_than(Version::new(1, 0, 0)),
        "
pub type Wibble {
  Wibble(a: Int, b: Int)
  Wobble(a: Int, c: Int)
}

pub fn main(wibble) {
  case wibble {
    Wibble(..) -> Wibble(..wibble, b: 10)
    Wobble(..) -> panic
  }
}
",
    );
}

#[test]
fn record_access_variant_inference_requires_v1_6() {
    assert_warnings_with_gleam_version!(
        Range::higher_than(Version::new(1, 0, 0)),
        "
pub type Wibble {
  Wibble(a: Int, b: Int)
  Wobble(a: Int, c: Int)
}

pub fn main(wibble) {
  case wibble {
    Wibble(..) -> wibble.b
    Wobble(..) -> wibble.c
  }
}
",
    );
}

#[test]
fn let_assert_with_message_requires_v1_7() {
    assert_warnings_with_gleam_version!(
        Range::higher_than(Version::new(1, 0, 0)),
        r#"
pub fn main() {
  let assert Ok(10) = Ok(20) as "This will crash..."
}
"#,
    );
}

#[test]
fn bool_assert_requires_v1_11() {
    assert_warnings_with_gleam_version!(
        Range::higher_than(Version::new(1, 0, 0)),
        "
pub fn go(x) {
  assert x == 2
}
",
    );
}

#[test]
fn javascript_unsafe_int_decimal() {
    assert_js_warning!(
        r#"
pub fn go() {
  [
    9_007_199_254_740_990,
    9_007_199_254_740_991,
    9_007_199_254_740_992,
    -9_007_199_254_740_990,
    -9_007_199_254_740_991,
    -9_007_199_254_740_992,
  ]
}
"#
    );
}

#[test]
fn javascript_unsafe_int_binary() {
    assert_js_warning!(
        r#"
pub fn go() {
  [
    0b11111111111111111111111111111111111111111111111111110,
    0b11111111111111111111111111111111111111111111111111111,
    0b100000000000000000000000000000000000000000000000000000,
  ]
}
"#
    );
}

#[test]
fn javascript_unsafe_int_octal() {
    assert_js_warning!(
        r#"
pub fn go() {
  [
    0o377777777777777776,
    0o377777777777777777,
    0o400000000000000000,
  ]
}
"#
    );
}

#[test]
fn javascript_unsafe_int_hex() {
    assert_js_warning!(
        r#"
pub fn go() {
  [
    0x1FFFFFFFFFFFFE,
    0x1FFFFFFFFFFFFF,
    0x20000000000000,
  ]
}
"#
    );
}

#[test]
fn javascript_unsafe_int_in_tuple() {
    assert_js_warning!(
        r#"
pub fn go() {
  #(9_007_199_254_740_992)
}
"#
    );
}

#[test]
fn javascript_unsafe_int_segment_in_bit_array() {
    assert_js_warning!(
        r#"
pub fn go() {
  <<9_007_199_254_740_992:64>>
}
"#
    );
}

#[test]
fn javascript_unsafe_int_segment_size_in_bit_array() {
    assert_js_warning!(
        r#"
pub fn go() {
  [
    <<0:9_007_199_254_740_992>>,
    <<0:size(9_007_199_254_740_992)>>,
  ]
}
"#
    );
}

#[test]
fn javascript_unsafe_int_in_const() {
    assert_js_warning!(r#"pub const i = 9_007_199_254_740_992"#);
}

#[test]
fn javascript_unsafe_int_in_const_tuple() {
    assert_js_warning!(r#"pub const i = #(9_007_199_254_740_992)"#);
}

#[test]
fn javascript_unsafe_int_segment_in_const_bit_array() {
    assert_js_warning!(
        r#"
pub const i = <<9_007_199_254_740_992:64>>
"#
    );
}

#[test]
fn javascript_unsafe_int_segment_size_in_const_bit_array() {
    assert_js_warning!(
        r#"
pub const ints = [
  <<0:9_007_199_254_740_992>>,
  <<0:size(9_007_199_254_740_992)>>,
]
"#
    );
}

#[test]
fn javascript_unsafe_int_in_pattern() {
    assert_js_warning!(
        r#"
pub fn go() {
  let assert <<9_007_199_254_740_992:64>> = <<>>
}
"#
    );
}

#[test]
fn javascript_unsafe_int_segment_size_in_pattern() {
    assert_js_warning!(
        r#"
pub fn go() {
  let assert <<0:9_007_199_254_740_992>> = <<>>
}
"#
    );
}

#[test]
fn javascript_unsafe_int_with_external_implementation() {
    assert_js_no_warnings!(
        r#"
@external(javascript, "./test.mjs", "go")
pub fn go() -> Int {
  9_007_199_254_740_992
}
"#
    );
}

#[test]
fn javascript_unsafe_int_segment_in_pattern_with_external_implementation() {
    assert_js_no_warnings!(
        r#"
@external(javascript, "./test.mjs", "go")
pub fn go(b: BitArray) -> BitArray {
  let assert <<0xFFF0000000000000:64>> = b
}
"#
    );
}

#[test]
fn javascript_unsafe_int_with_external_function_call() {
    assert_js_warning!(
        r#"
pub fn main() {
  helper() + 9_007_199_254_740_992
}

@external(javascript, "a", "b")
fn helper() -> Int
"#
    );
}

#[test]
fn incomplete_code_block_raises_warning() {
    assert_warning!(
        r#"
pub fn main() {
    {}
}
"#
    );
}

#[test]
fn deprecated_target_shorthand_erlang() {
    assert_warning!(
        "
@target(erl)
pub fn wibble() { panic }
"
    );
}

#[test]
fn deprecated_target_shorthand_javascript() {
    assert_warning!(
        "
@target(js)
pub fn wibble() { panic }
"
    );
}

#[test]
fn unused_block_wrapping_pure_expressions() {
    assert_warning!(
        r#"
pub fn main() {
  {
    True
    1
  }
  Nil
}
"#
    );
}

#[test]
fn unused_block_wrapping_pure_expression() {
    assert_warning!(
        r#"
pub fn main() {
  { 1 }
  Nil
}
"#
    );
}

#[test]
fn unused_block_wrapping_impure_expressions_is_not_reported_as_pure() {
    assert_no_warnings!(
        r#"
pub fn main() {
  {
    wibble()
    1
  }
  Nil
}

fn wibble() { panic }
"#
    );
}

#[test]
fn unused_case_expression() {
    assert_warning!(
        r#"
pub fn main() {
    let a = 1
    case a {
        1 -> a
        _ -> 12
    }
    Nil
}
"#
    );
}

#[test]
fn impure_case_expression_is_not_marked_as_unused() {
    assert_no_warnings!(
        r#"
pub fn main() {
    let a = 1
    case a {
        1 -> wibble()
        _ -> 12
    }
    Nil
}

fn wibble() { panic }
"#
    );
}

#[test]
fn impure_case_expression_is_not_marked_as_unused_2() {
    assert_no_warnings!(
        r#"
pub fn main() {
    let a = 1
    case wibble() {
        1 -> a
        _ -> 12
    }
    Nil
}

fn wibble() { panic }
"#
    );
}

#[test]
fn unused_fn_function_call() {
    assert_warning!(
        r#"
pub fn main() {
    fn(a) { a + 1 }(1)
    Nil
}
"#
    );
}

#[test]
fn impure_fn_function_call_not_mark_as_unused() {
    assert_no_warnings!(
        r#"
pub fn main() {
    fn(_) { panic }(1)
    Nil
}
"#
    );
}

#[test]
fn unused_pipeline_ending_with_pure_fn() {
    assert_warning!(
        r#"
pub fn main() {
    1
    |> fn(n) { n + 1 }

    Nil
}
"#
    );
}

#[test]
fn unused_pipeline_ending_with_impure_fn() {
    assert_no_warnings!(
        r#"
pub fn main() {
    1
    |> fn(_) { panic }

    Nil
}
"#
    );
}

#[test]
fn pipeline_with_regular_function_call_is_never_marked_unused() {
    assert_no_warnings!(
        r#"
pub fn main() {
    1 |> wibble

    Nil
}

fn wibble(n) { echo n }
"#
    );
}

#[test]
fn use_with_pure_fn_expression_is_marked_as_unused() {
    assert_warning!(
        r#"
pub fn main() {
    {
        use _ <- fn(a) { a }
        1
    }

    Nil
}
"#
    );
}

#[test]
fn use_statement_calling_regular_function_is_never_marked_unused() {
    assert_no_warnings!(
        r#"
pub fn main() {
    {
        use _ <- each([1, 2, 3])
        1
    }

    Nil
}

fn each(list, _fun) { echo list }
"#
    );
}

#[test]
// https://github.com/gleam-lang/gleam/issues/3425
fn unused_variable_assignment_pattern() {
    assert_warning!(
        "
type Wibble {
  Wibble(a: Int, b: Int)
}

pub fn main() {
  let Wibble(a:, ..) as w = Wibble(1, 2)
  a
}
"
    );
}

#[test]
fn unused_variable_string_prefix_pattern() {
    assert_warning!(
        r#"
pub fn main() {
  let assert "hello" as hello <> rest = "hello, world"
  rest
}
"#
    );
}

#[test]
fn unused_variable_string_prefix_pattern2() {
    assert_warning!(
        r#"
pub fn main() {
  let assert "hello" as hello <> rest = "hello, world"
  hello
}
"#
    );
}

#[test]
fn echo_followed_by_panic() {
    assert_warning!(
        "
pub fn main() {
  echo panic
}
"
    );
}

#[test]
fn echo_followed_by_panicking_expression() {
    assert_warning!(
        "
pub fn main(a) {
  echo case a {
    1 -> panic
    _ -> [1, panic]
  }
}
"
    );
}

#[test]
fn assert_on_inferred_variant() {
    assert_warning!(
        "
type Wibble {
  Wibble(w: Int)
  Wobble(w: String)
}

pub fn main() {
  let assert Wobble(w) = Wibble(10)
  w
}
"
    );
}

#[test]
fn bit_array_truncated_segment() {
    assert_warning!(
        "
pub fn main() {
  <<12:size(1)>>
}
"
    );
}

#[test]
fn unused_pure_function() {
    assert_warning!(
        "
fn add(a, b) { a + b }

pub fn main() {
  add(1, 2)
  Nil
}
"
    );
}

#[test]
fn bit_array_truncated_segment_in_bytes() {
    assert_warning!(
        "
pub fn main() {
  <<258:size(8)>>
}
"
    );
}
#[test]

fn unused_pure_function_that_calls_other_pure_function() {
    assert_warning!(
        "
fn sub(a, b) { add(a, -b) }

fn add(a, b) { a + b }

pub fn main() {
  sub(1, 2)
  Nil
}
"
    );
}

#[test]
fn bit_array_truncated_segment_in_bytes_2() {
    assert_warning!(
        "
pub fn main() {
  <<65_537:size(2)-unit(8)>>
}
"
    );
}

#[test]
fn bit_array_truncated_segment_in_range() {
    assert_no_warnings!(
        "
pub fn main() {
  <<255>>
}
"
    );
}

#[test]
fn function_is_impure_if_external() {
    assert_no_warnings!(
        r#"
@external(erlang, "maths", "add")
fn add(a: Int, b: Int) -> Int

pub fn main() {
  add(1, 2)
  Nil
}
"#
    );
}

#[test]
fn bit_array_truncated_segment_in_range_2() {
    assert_no_warnings!(
        "
pub fn main() {
  <<0>>
}
"
    );
}

#[test]
fn function_is_impure_if_uses_echo() {
    assert_no_warnings!(
        r#"
fn add(a: Int, b: Int) -> Int {
  echo a + b
}

pub fn main() {
  add(1, 2)
  Nil
}
"#
    );
}

#[test]
fn bit_array_negative_truncated_segment() {
    assert_warning!(
        "
pub fn main() {
  // -5 in 2's complement is 1111...111011
  // so if we truncate it to its first 3 bits we
  // get 011, which is positive 3!
  <<-5:size(3)>>
}
"
    );
}

#[test]
fn bit_array_negative_truncated_segment_2() {
    assert_warning!(
        "
pub fn main() {
  <<-200:size(8)>>
}
"
    );
}

#[test]
fn bit_array_negative_truncated_segment_in_range() {
    assert_no_warnings!(
        "
pub fn main() {
  <<-128>>
}
"
    );
}

#[test]
fn function_is_impure_if_uses_panic() {
    assert_no_warnings!(
        r#"
fn add(a: Int, b: Int) -> Int {
  case a + b {
    0 -> panic as "Cannot add to zero"
    x -> x
  }
}

pub fn main() {
  add(1, 2)
  Nil
}
"#
    );
}

#[test]
fn function_is_impure_if_uses_todo() {
    // We have to use `assert_warning` here instead of `assert_no_warnings`, because
    // `todo` will always emit a warning. However, that should be the only warning;
    // there should not be an unused function warning.
    assert_warning!(
        r#"
fn add(a: Int, b: Int) -> Int {
  case a + b {
    0 -> todo as "Handle zero"
    x -> x
  }
}

pub fn main() {
  add(1, 2)
  Nil
}
"#
    );
}

#[test]
fn function_is_impure_if_uses_let_assert() {
    assert_no_warnings!(
        r#"
fn assert_ok(x: Result(a, b)) -> a {
  let assert Ok(x) = x
  x
}

pub fn main() {
  assert_ok(Ok(10))
  Nil
}
"#
    );
}

#[test]
fn function_is_impure_if_uses_assert() {
    assert_no_warnings!(
        r#"
fn assert_equal(a, b) {
  assert a == b
}

pub fn main() {
  assert_equal(1, 2)
  Nil
}
"#
    );
}

#[test]
fn function_is_impure_if_call_impure_function() {
    assert_no_warnings!(
        r#"
@external(erlang, "erlang", "something")
fn impure() -> Nil

fn add(a: Int, b: Int) -> Int {
  impure()
  a + b
}

pub fn main() {
  add(1, 2)
  Nil
}
"#
    );
}

#[test]
fn pure_pipeline_raises_warning() {
    assert_warning!(
        "
fn add(a, b) { a + b }

pub fn main() {
  1 |> add(2)
  Nil
}
"
    );
}

#[test]
fn pure_pipeline_with_many_steps_raises_warning() {
    assert_warning!(
        "
fn add(a, b) { a + b }

pub fn main() {
  1 |> add(2) |> add(3) |> add(4)
  Nil
}
"
    );
}

#[test]
fn pipeline_with_echo_is_impure() {
    assert_no_warnings!(
        "
fn add(a, b) { a + b }

pub fn main() {
  1 |> add(2) |> echo |> add(3)
  Nil
}
"
    );
}

#[test]
fn pipeline_with_impure_function_raises_no_warnings() {
    assert_no_warnings!(
        "
fn add(a, b) { echo a + b }

pub fn main() {
  1 |> add(2)
  Nil
}
"
    );
}

#[test]
fn function_is_pure_on_js_if_external_on_erlang() {
    assert_js_warning!(
        r#"
@external(erlang, "maths", "add")
fn add(a: Int, b: Int) -> Int { a + b }

pub fn main() {
  add(1, 2)
  Nil
}
"#
    );
}

#[test]
fn function_is_pure_on_erlang_if_external_on_js() {
    assert_warning!(
        r#"
@external(javascript, "./maths.mjs", "add")
fn add(a: Int, b: Int) -> Int { a + b }

pub fn main() {
  add(1, 2)
  Nil
}
"#
    );
}

#[test]
fn pure_standard_library_function() {
    assert_warning!(
        (
            "gleam_stdlib",
            "gleam/dict",
            r#"
pub type Dict(key, value)

@external(erlang, "map", "new")
pub fn new() -> Dict(a, b)

@external(erlang, "map", "insert")
pub fn insert(dict: Dict(key, value), key: key, value: value) -> Dict(key, value)
"#
        ),
        "
import gleam/dict

pub fn main() {
  dict.insert(dict.new(), 1, 2)
  Nil
}
"
    );
}

#[test]
fn impure_standard_library_function() {
    assert_no_warnings!(
        (
            "gleam_stdlib",
            "gleam/io",
            r#"
@external(erlang, "io", "print")
pub fn println(message: String) -> Nil
"#
        ),
        r#"
import gleam/io

pub fn main() {
  io.println("Hello, world!")
  Nil
}
"#
    );
}

#[test]
fn trusted_pure_standard_library_function_that_panics_is_impure() {
    assert_no_warnings!(
        (
            "gleam_stdlib",
            "gleam/int",
            r#"
pub fn add(_a, _b) {
  panic
}
"#
        ),
        "
import gleam/int
pub fn main() {
  int.add(1, 2)
  Nil
}
"
    );
}

#[test]
fn higher_order_function_is_not_marked_as_pure() {
    assert_no_warnings!(
        (
            "gleam/list",
            r#"
pub fn each(list, f) {
  case list {
    [] -> Nil
    [first, ..rest] -> {
      f(first)
      each(rest, f)
    }
  }
}
"#
        ),
        "
import gleam/list
pub fn main() {
  list.each([1, 2, 3, 4], fn(x) { echo x })
  Nil
}
"
    );
}

#[test]
fn calling_local_variable_not_marked_as_pure() {
    assert_no_warnings!(
        "
pub fn main() {
  let side_effects = fn() { panic }

  side_effects()
  Nil
}
"
    );
}

#[test]
fn constructing_anonymous_function_is_pure() {
    assert_warning!(
        r#"
fn make_panic(message) {
  fn() { panic as message }
}

pub fn main() {
  make_panic("This is a crash")
  Nil
}
"#
    );
}

// https://github.com/gleam-lang/gleam/issues/4504
#[test]
fn impure_stdlib_test_function() {
    assert_no_warnings!(
        (
            "gleam_stdlib",
            "gleam/should",
            r#"
@external(erlang, "gleam_test_ffi", "should_equal")
pub fn equal(a: t, b: t) -> Nil
"#
        ),
        "
import gleam/should

pub fn main() {
  1 |> should.equal(1)
}
"
    );
}

// https://github.com/gleam-lang/gleam/issues/4505
#[test]
fn impure_function_using_a_pipe() {
    assert_no_warnings!(
        "
fn impure(_x) {
  panic
}

fn also_impure(x) {
  x |> impure
}

pub fn main() {
  also_impure(10)
  Nil
}
"
    );
}

// https://github.com/gleam-lang/gleam/issues/4505
#[test]
fn impure_function_using_a_pipe_with_a_call() {
    assert_no_warnings!(
        "
fn impure(_x, _y) {
  panic
}

fn also_impure(x) {
  x |> impure(1)
}

pub fn main() {
  also_impure(10)
  Nil
}
"
    );
}

// https://github.com/gleam-lang/gleam/issues/4505
#[test]
fn impure_function_using_a_pipe_into_anonymous_function() {
    assert_no_warnings!(
        "

fn impure(x) {
  x |> fn(_x) { panic }
}

pub fn main() {
  impure(10)
  Nil
}
"
    );
}

// https://github.com/gleam-lang/gleam/issues/4505
#[test]
fn impure_function_using_a_pipe_into_echo() {
    assert_no_warnings!(
        "

fn impure(x) {
  x |> echo
}

pub fn main() {
  impure(10)
  Nil
}
"
    );
}

// https://github.com/gleam-lang/gleam/issues/4505
#[test]
fn impure_function_using_a_pipe_into_result_of_call() {
    assert_no_warnings!(
        "
fn make_panic() {
  fn(_x) { panic }
}

fn impure(x) {
  x |> make_panic()
}

pub fn main() {
  impure(10)
  Nil
}
"
    );
}

#[test]
fn no_assert_warning_for_bit_array_with_variable() {
    assert_no_warnings!(
        r#"
@external(erlang, "gleam@function", "identity")
fn codepoint(value: Int) -> UtfCodepoint

pub fn main() {
  let codepoint = codepoint(32)
  assert <<codepoint:utf8_codepoint>> == <<" ">>
}
"#
    );
}

// https://github.com/gleam-lang/gleam/issues/4637
#[test]
fn pattern_matching_on_32_float_plus_infinity_still_reachable() {
    assert_no_warnings!(
        r#"
pub fn go(x) {
  case x {
    <<_:32-float>> -> "Float"
    <<0x7f800000:32>> -> "+Infinity"
    _ -> "Other"
  }
}
"#
    );
}

#[test]
fn no_assert_warning_for_tuple_with_variable() {
    assert_no_warnings!(
        r#"
pub fn main() {
  let x = 3
  assert #(1, 2, x) == #(1, 2, 3)
}
"#
    );
}

#[test]
fn pattern_matching_on_32_float_plus_infinity_still_reachable_2() {
    assert_no_warnings!(
        r#"
pub fn go(x) {
  case x {
    <<_:32-float>> -> "Float"
    <<0x7f80:16, 0x0000:16>> -> "+Infinity"
    _ -> "Other"
  }
}
"#
    );
}

#[test]
fn no_assert_warning_for_list_with_variable() {
    assert_no_warnings!(
        r#"
pub fn main() {
  let x = 3
  assert [1, 2, x] == [1, 2, 3]
}
"#
    );
}

#[test]
fn pattern_matching_on_32_float_minus_infinity_still_reachable() {
    assert_no_warnings!(
        r#"
pub fn go(x) {
  case x {
    <<_:32-float>> -> "Float"
    <<0xff800000:32>> -> "-Infinity"
    _ -> "Other"
  }
}
"#
    );
}

#[test]
fn no_assert_warning_for_constructor_with_variable() {
    assert_no_warnings!(
        r#"
type Box(t) {
  Box(t)
}

pub fn main() {
  let x = 42
  assert Box(x) == Box(42)
}
"#
    );
}

#[test]
fn pattern_matching_on_32_float_minus_infinity_still_reachable_2() {
    assert_no_warnings!(
        r#"
pub fn go(x) {
  case x {
    <<_:32-float>> -> "Float"
    <<0xff80:16, 0x0000:16>> -> "-Infinity"
    _ -> "Other"
  }
}
"#
    );
}

#[test]
fn pattern_matching_on_32_float_nan_still_reachable() {
    assert_no_warnings!(
        r#"
pub fn go(x) {
  case x {
    <<_:32-float>> -> "Float"
    <<0x7fc00000:32>> -> "NaN"
    _ -> "Other"
  }
}
"#
    );
}

#[test]
fn pattern_matching_on_32_float_nan_still_reachable_2() {
    assert_no_warnings!(
        r#"
pub fn go(x) {
  case x {
    <<_:32-float>> -> "Float"
    <<0x7fc0:16, 0x0000:16>> -> "NaN"
    _ -> "Other"
  }
}
"#
    );
}

#[test]
fn pattern_matching_on_64_float_plus_infinity_still_reachable() {
    assert_no_warnings!(
        r#"
pub fn go(x) {
  case x {
    <<_:64-float>> -> "Float"
    <<0x7ff0000000000000:64>> -> "+Infinity"
    _ -> "Other"
  }
}
"#
    );
}

#[test]
fn pattern_matching_on_64_float_plus_infinity_still_reachable_2() {
    assert_no_warnings!(
        r#"
pub fn go(x) {
  case x {
    <<_:64-float>> -> "Float"
    <<0x7ff00000:32, 0x00000000:32>> -> "+Infinity"
    _ -> "Other"
  }
}
"#
    );
}

#[test]
fn pattern_matching_on_64_float_minus_infinity_still_reachable() {
    assert_no_warnings!(
        r#"
pub fn go(x) {
  case x {
    <<_:64-float>> -> "Float"
    <<0xfff0000000000000:64>> -> "-Infinity"
    _ -> "Other"
  }
}
"#
    );
}

#[test]
fn pattern_matching_on_64_float_minus_infinity_still_reachable_2() {
    assert_no_warnings!(
        r#"
pub fn go(x) {
  case x {
    <<_:64-float>> -> "Float"
    <<0xfff00000:32, 0x00000000:32>> -> "-Infinity"
    _ -> "Other"
  }
}
"#
    );
}

#[test]
fn pattern_matching_on_64_float_nan_still_reachable() {
    assert_no_warnings!(
        r#"
pub fn go(x) {
  case x {
    <<_:64-float>> -> "Float"
    <<0x7ff8000000000000:64>> -> "NaN"
    _ -> "Other"
  }
}
"#
    );
}

#[test]
fn pattern_matching_on_64_float_nan_still_reachable_2() {
    assert_no_warnings!(
        r#"
pub fn go(x) {
  case x {
    <<_:64-float>> -> "Float"
    <<0x7ff80000:32, 0x00000000:32>> -> "NaN"
    _ -> "Other"
  }
}
"#
    );
}

#[test]
fn pattern_matching_on_64_float_int_is_still_reachable() {
    assert_no_warnings!(
        r#"
pub fn go(x) {
  case x {
    <<_:64-float>> -> "Float"
    <<_:64-int>> -> "Int"
    _ -> "Other"
  }
}
"#
    );
}

#[test]
fn pattern_matching_on_64_float_float_is_unreachable() {
    assert_warning!(
        r#"
pub fn go(x) {
  case x {
    <<_:64-float>> -> "Float"
    <<_:64-float>> -> "unreachable"
    _ -> "Other"
  }
}
"#
    );
}

#[test]
fn import_module_twice() {
    assert_warning!(
        ("gleam/wibble", "pub fn wobble() { 1 }"),
        "import gleam/wibble as a
import gleam/wibble as b

pub fn main() {
  a.wobble() + b.wobble()
}
"
    );
}
//https://github.com/gleam-lang/gleam/issues/4666
#[test]
fn shadow_imported_function() {
    assert_warning!(
        (
            "thepackage",
            "module",
            r#"
pub fn wibble() { Nil }
"#
        ),
        r#"
import module.{wibble}

pub fn wibble() { Nil }
"#
    );
}
//https://github.com/gleam-lang/gleam/issues/4666
#[test]
fn shadow_imported_constant() {
    assert_warning!(
        (
            "thepackage",
            "module",
            r#"
pub const value = 1
"#
        ),
        r#"
import module.{value}

pub const value = 1
"#
    );
}

#[test]
fn int_literals_redundant_comparison() {
    assert_warning!("pub fn main() { 1 == 1 }");
}

#[test]
fn int_literals_redundant_comparison_2() {
    assert_warning!("pub fn main() { 1 == 2 }");
}

#[test]
fn int_literals_redundant_comparison_3() {
    assert_warning!("pub fn main() { 1 != 1 }");
}

#[test]
fn int_literals_redundant_comparison_4() {
    assert_warning!("pub fn main() { 1 != 2 }");
}

#[test]
fn int_literals_redundant_comparison_5() {
    assert_warning!("pub fn main() { 1 > 2 }");
}

#[test]
fn int_literals_redundant_comparison_6() {
    assert_warning!("pub fn main() { 1 <= 2 }");
}

#[test]
fn int_literals_redundant_comparison_7() {
    assert_warning!("pub fn main() { 1 < 2 }");
}

#[test]
fn int_literals_redundant_comparison_8() {
    assert_warning!("pub fn main() { 1 >= 2 }");
}

#[test]
fn bool_literals_redundant_comparison() {
    assert_warning!("pub fn main() { True == False }");
}

#[test]
fn bool_literals_redundant_comparison_1() {
    assert_warning!("pub fn main() { True != False }");
}

#[test]
fn list_literals_redundant_comparison() {
    assert_warning!("pub fn main(a, b) { [1] == [a, b(1)] }");
}

#[test]
fn list_literals_redundant_comparison_2() {
    assert_warning!("pub fn main(a, b) { [1] != [a, b(1)] }");
}

#[test]
fn list_literals_redundant_comparison_3() {
    assert_warning!("pub fn main() { [1] != [1] }");
}

#[test]
fn list_literals_redundant_comparison_4() {
    assert_warning!("pub fn main(a) { [1, ..[1, a]] == [1, ..[1, a]] }");
}

#[test]
fn list_literals_redundant_comparison_5() {
    assert_warning!("pub fn main(a) { [1, ..a] == [1, ..a] }");
}

#[test]
fn list_literals_redundant_comparison_6() {
    assert_no_warnings!("pub fn main(a) { [a(1)] == [a(1)] }");
}

#[test]
fn list_literals_redundant_comparison_7() {
    assert_warning!("pub fn main(a) { [a(1), 2] == [a(1), 3] }");
}

#[test]
fn string_literals_redundant_comparison() {
    assert_warning!("pub fn main() { \"wibble\" == \"wobble\" }");
}

#[test]
fn string_literals_redundant_comparison_1() {
    assert_warning!("pub fn main() { \"wibble\" != \"wobble\" }");
}

#[test]
fn variables_redundant_comparison() {
    assert_warning!("pub fn main(a) { a == a }");
}

#[test]
fn variables_not_redundant_comparison() {
    assert_no_warnings!("pub fn main(a, b) { a != b }");
}

#[test]
fn record_select_redundant_comparison() {
    assert_warning!(
        "
pub type Wibble {
  Wibble(field: Int)
}

pub fn main(wibble: Wibble) { wibble.field == wibble.field }
"
    );
}

#[test]
fn record_select_redundant_comparison_1() {
    assert_warning!(
        "
pub type Wibble {
  Wibble(field: Int)
}

pub fn main(wibble: Wibble) { wibble.field != wibble.field }
"
    );
}

#[test]
fn record_select_not_redundant_comparison() {
    assert_no_warnings!(
        "
pub type Wibble {
  Wibble(field: Int)
}

pub fn main(wibble: Wibble, wobble: Wibble) { wibble.field == wobble.field }
"
    );
}

#[test]
fn record_select_not_redundant_comparison_2() {
    assert_no_warnings!(
        "
pub type Wibble {
  Wibble(field: Int)
}

fn new() -> Wibble { Wibble(1) }

pub fn main() {
  new().field == new().field
  // ^^ functions might have side effects so we can't
  //    tell if this is redundant or not!
}
"
    );
}

#[test]
fn different_records_0_redundant_comparison() {
    assert_warning!(
        "
pub type Either {
  Left
  Right
}

pub fn main() -> Bool {
  Left == Right
}
"
    );
}

#[test]
fn different_records_1_redundant_comparison() {
    assert_warning!(
        "
pub type Either {
  Left(Int)
  Right
}

pub fn main() -> Bool {
  Left(1) == Right
}
"
    );
}

#[test]
fn different_records_2_redundant_comparison() {
    assert_warning!(
        "
pub type Either {
  Left(Int)
  Right(Int)
}

pub fn main() -> Bool {
  Left(1) == Right(1)
}
"
    );
}

#[test]
fn different_records_3_redundant_comparison() {
    assert_warning!(
        "
pub type Either {
  Left
  Right(Int)
}

pub fn main() -> Bool {
  Left == Right(1)
}
"
    );
}

#[test]
fn unused_discard_pattern() {
    assert_warning!(
        "pub fn main() {
  let a = 10
  let _ = case a {
    _ as b -> b
  }
}
"
    );
}
