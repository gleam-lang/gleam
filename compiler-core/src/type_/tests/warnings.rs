use super::*;
use crate::ast::TodoKind;
use crate::{assert_no_warnings, assert_warning, assert_warnings_with_imports};

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
    assert_warning!(
        "fn main() { 1 == todo }",
        Warning::Todo {
            kind: TodoKind::Keyword,
            location: SrcSpan { start: 17, end: 21 },
            typ: Arc::new(Type::Var {
                type_: Arc::new(RefCell::new(TypeVar::Link { type_: int() })),
            }),
        },
    );
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
        "pub fn main() { foo() }
        pub fn foo() { }"
    );
}

#[test]
fn warning_variable_never_used_test() {
    assert_warning!(
        "
pub fn foo() { Ok(5) }
pub fn main() { let five = foo() }"
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
fn foo() { Ok(5) }
fn main() {
		foo()
		5
}",
        Warning::ImplicitlyDiscardedResult {
            location: SrcSpan { start: 34, end: 39 }
        }
    );
}

#[test]
fn result_discard_warning_test2() {
    // Explicitly discarded Results do not emit warnings
    assert_no_warnings!(
        "
pub fn foo() { Ok(5) }
pub fn main() { let _ = foo() 5 }",
    );
}

#[test]
fn unused_int() {
    assert_warning!(
        "fn main() { 1 2 }",
        Warning::UnusedLiteral {
            location: SrcSpan { start: 12, end: 13 }
        }
    );
}

#[test]
fn unused_float() {
    assert_warning!(
        "fn main() { 1.0 2 }",
        Warning::UnusedLiteral {
            location: SrcSpan { start: 12, end: 15 }
        }
    );
}

#[test]
fn unused_string() {
    assert_warning!(
        "
    fn main() {
        \"1\"
				2
    }",
        Warning::UnusedLiteral {
            location: SrcSpan { start: 25, end: 28 }
        }
    );
}

#[test]
fn unused_bit_array() {
    assert_warning!(
        "
    fn main() {
        <<3>>
				2
    }",
        Warning::UnusedLiteral {
            location: SrcSpan { start: 25, end: 30 }
        }
    );
}

#[test]
fn unused_tuple() {
    assert_warning!(
        "
    fn main() {
        #(1.0, \"Hello world\")
				2
    }",
        Warning::UnusedLiteral {
            location: SrcSpan { start: 25, end: 46 }
        }
    );
}

#[test]
fn unused_list() {
    assert_warning!(
        "
    fn main() {
        [1, 2, 3]
				2
    }",
        Warning::UnusedLiteral {
            location: SrcSpan { start: 25, end: 34 }
        }
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
        }",
        Warning::NoFieldsRecordUpdate {
            location: SrcSpan {
                start: 182,
                end: 196
            }
        }
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
        }",
        Warning::AllFieldsRecordUpdate {
            location: SrcSpan {
                start: 182,
                end: 220
            }
        }
    );
}

#[test]
fn unused_private_type_warnings_test() {
    // External type
    assert_warning!(
        "type X",
        Warning::UnusedType {
            name: "X".into(),
            location: SrcSpan { start: 0, end: 6 },
            imported: false
        }
    );
}

#[test]
fn unused_private_type_warnings_test2() {
    assert_no_warnings!("pub type Y");
}

#[test]
fn unused_private_type_warnings_test3() {
    // Type alias
    assert_warning!(
        "type X = Int",
        Warning::UnusedType {
            name: "X".into(),
            location: SrcSpan { start: 0, end: 12 },
            imported: false
        }
    );
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
    assert_warning!(
        "type X { X }",
        Warning::UnusedConstructor {
            name: "X".into(),
            location: SrcSpan { start: 9, end: 10 },
            imported: false
        }
    );
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
    assert_warning!(
        "fn a() { 1 }",
        Warning::UnusedPrivateFunction {
            name: "a".into(),
            location: SrcSpan { start: 0, end: 6 },
        }
    );
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
    assert_warning!(
        "const a = 1",
        Warning::UnusedPrivateModuleConstant {
            name: "a".into(),
            location: SrcSpan { start: 6, end: 7 },
        }
    );
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
    assert_warning!(
        "pub fn a(b) { 1 }",
        Warning::UnusedVariable {
            name: "b".into(),
            location: SrcSpan { start: 9, end: 10 },
        }
    );
}

#[test]
fn used_variable_warnings_test() {
    assert_no_warnings!("pub fn a(b) { b }");
}

#[test]
fn unused_variable_warnings_test2() {
    // Simple let
    assert_warning!(
        "pub fn a() { let b = 1 5 }",
        Warning::UnusedVariable {
            name: "b".into(),
            location: SrcSpan { start: 17, end: 18 },
        }
    );
}

#[test]
fn used_variable_warnings_test2() {
    assert_no_warnings!("pub fn a() { let b = 1 b }");
}

#[test]
fn unused_variable_shadowing_test() {
    assert_warning!(
        "pub fn a() { let b = 1 let b = 2 b }",
        Warning::UnusedVariable {
            name: "b".into(),
            location: SrcSpan { start: 17, end: 18 },
        }
    );
}

#[test]
fn used_variable_shadowing_test() {
    assert_no_warnings!("pub fn a() { let b = 1 let b = b + 1 b }");
}

#[test]
fn unused_destructure() {
    // Destructure
    assert_warning!(
        "pub fn a(b) { case b { #(c, _) -> 5 } }",
        Warning::UnusedVariable {
            name: "c".into(),
            location: SrcSpan { start: 25, end: 26 },
        }
    );
}

#[test]
fn used_destructure() {
    assert_no_warnings!("pub fn a(b) { case b { #(c, _) -> c } }");
}

#[test]
fn unused_imported_module_warnings_test() {
    assert_warning!(
        ("gleam/foo", "pub fn bar() { 1 }"),
        "import gleam/foo",
        Warning::UnusedImportedModule {
            name: "foo".into(),
            location: SrcSpan { start: 0, end: 16 },
        }
    );
}

#[test]
fn unused_imported_module_with_alias_warnings_test() {
    assert_warning!(
        ("gleam/foo", "pub fn bar() { 1 }"),
        "import gleam/foo as bar",
        Warning::UnusedImportedModule {
            name: "bar".into(),
            location: SrcSpan { start: 0, end: 23 },
        }
    );
}

// https://github.com/gleam-lang/gleam/issues/2326
#[test]
fn unused_imported_module_with_alias_and_unqualified_name_warnings_test() {
    let warnings = get_warnings(
        "import gleam/one.{two} as three",
        vec![("thepackage", "gleam/one", "pub fn two() { 1 }")],
    );
    assert!(!warnings.is_empty());
    assert_eq!(
        Warning::UnusedImportedValue {
            name: "two".into(),
            location: SrcSpan { start: 18, end: 21 },
        },
        warnings[0]
    );
    assert_eq!(
        Warning::UnusedImportedModuleAlias {
            alias: "three".into(),
            location: SrcSpan { start: 23, end: 31 },
            module_name: "gleam/one".into()
        },
        warnings[1]
    );
}

#[test]
fn unused_imported_module_with_alias_and_unqualified_name_no_warnings_test() {
    assert_warning!(
        ("package", "gleam/one", "pub fn two() { 1 }"),
        "import gleam/one.{two} as three\npub fn baz() { two() }"
    );
}

#[test]
fn unused_imported_module_no_warning_on_used_function_test() {
    assert_no_warnings!(
        ("thepackage", "gleam/foo", "pub fn bar() { 1 }"),
        "import gleam/foo pub fn baz() { foo.bar() }",
    );
}

#[test]
fn unused_imported_module_no_warning_on_used_type_test() {
    assert_no_warnings!(
        ("thepackage", "gleam/foo", "pub type Foo = Int"),
        "import gleam/foo pub fn baz(a: foo.Foo) { a }",
    );
}

#[test]
fn unused_imported_module_no_warning_on_used_unqualified_function_test() {
    assert_no_warnings!(
        ("thepackage", "gleam/foo", "pub fn bar() { 1 }"),
        "import gleam/foo.{bar} pub fn baz() { bar() }",
    );
}

#[test]
fn unused_imported_module_no_warning_on_used_unqualified_type_test() {
    assert_no_warnings!(
        ("thepackage", "gleam/foo", "pub type Foo = Int"),
        "import gleam/foo.{type Foo} pub fn baz(a: Foo) { a }",
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
fn no_unused_warnings_for_broken_code() {
    let src = r#"
pub fn main() {
  let x = 1
  1 + ""
  x
}"#;
    let warnings = VectorWarningEmitterIO::default();
    _ = compile_module(src, Some(Arc::new(warnings.clone())), vec![]).unwrap_err();
    assert!(warnings.take().is_empty());
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
        ("gleam/foo", "pub const one = 1"),
        "import gleam/foo as bar",
        Warning::UnusedImportedModule {
            name: "bar".into(),
            location: SrcSpan { start: 0, end: 23 },
        }
    );
}

#[test]
fn unused_alias_warning_test() {
    assert_warnings_with_imports!(
        ("gleam/foo", "pub const one = 1");
        r#"
            import gleam/foo.{one} as bar
            const one = one
        "#,
        Warning::UnusedPrivateModuleConstant {
            name: "one".into(),
            location: SrcSpan { start: 61, end: 64 },
        },
        Warning::UnusedImportedModuleAlias {
            alias:"bar".into(),
            location: SrcSpan { start: 36, end: 42 },
            module_name: "gleam/foo".into(),
        }
    );
}

#[test]
fn used_type_with_import_alias_no_warning_test() {
    assert_no_warnings!(
        ("gleam", "gleam/foo", "pub const one = 1"),
        "import gleam/foo as _bar"
    );
}

#[test]
fn discarded_module_no_warnings_test() {
    assert_no_warnings!(("gleam", "foo", "pub const one = 1"), "import foo as _bar");
}

#[test]
fn unused_alias_for_duplicate_module_no_warning_for_alias_test() {
    assert_warnings_with_imports!(
        ("a/foo", "pub const one = 1"),
        ("b/foo", "pub const two = 2");
        r#"
            import a/foo
            import b/foo as bar
            const one = foo.one
        "#,
        Warning::UnusedPrivateModuleConstant {
            name: "one".into(),
            location: SrcSpan { start: 76, end: 79 },
        },
        Warning::UnusedImportedModule {
            name: "bar".into(),
            location: SrcSpan { start: 38, end: 57 },
        }
    );
}
