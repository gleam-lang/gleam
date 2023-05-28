use super::*;
use crate::ast::TodoKind;
use crate::{assert_no_warnings, assert_warning};

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
fn unused_bit_string() {
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
        "external type X",
        Warning::UnusedType {
            name: "X".into(),
            location: SrcSpan { start: 0, end: 15 },
            imported: false
        }
    );
}

#[test]
fn unused_private_type_warnings_test2() {
    assert_no_warnings!("pub external type Y");
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
    assert_no_warnings!("type X { X } pub fn a() { let b = X case b { X -> 1 } }");
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
            location: SrcSpan { start: 7, end: 16 },
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
            location: SrcSpan { start: 7, end: 23 },
        }
    );
}

#[test]
fn unused_imported_module_no_warning_on_used_function_test() {
    assert_no_warnings!(
        ("gleam/foo", "pub fn bar() { 1 }"),
        "import gleam/foo pub fn baz() { foo.bar() }",
    );
}

#[test]
fn unused_imported_module_no_warning_on_used_type_test() {
    assert_no_warnings!(
        ("gleam/foo", "pub type Foo = Int"),
        "import gleam/foo pub fn baz(a: foo.Foo) { a }",
    );
}

#[test]
fn unused_imported_module_no_warning_on_used_unqualified_function_test() {
    assert_no_warnings!(
        ("gleam/foo", "pub fn bar() { 1 }"),
        "import gleam/foo.{bar} pub fn baz() { bar() }",
    );
}

#[test]
fn unused_imported_module_no_warning_on_used_unqualified_type_test() {
    assert_no_warnings!(
        ("gleam/foo", "pub type Foo = Int"),
        "import gleam/foo.{Foo} pub fn baz(a: Foo) { a }",
    );
}

#[test]
fn module_access_registers_import_usage() {
    assert_no_warnings!(
        ("gleam/bibble", "pub const bobble = 1"),
        "import gleam/bibble pub fn main() { bibble.bobble }",
    );
}

// https://github.com/gleam-lang/gleam/issues/978
#[test]
fn bit_pattern_var_use() {
    assert_no_warnings!(
        "
pub fn main(x) {
  let <<name_size:8, name:binary-size(name_size)>> = x
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
        ("one", "pub fn two() { 2 }"),
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
        ("one", "pub type Two { Two(Int) }"),
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

#[test]
fn prefer_list_is_empty_over_list_length() {
    assert_warning!(
        ("gleam/list", "pub fn length(_list: List(a)) -> Int { 0 }"),
        r#"
        import gleam/list

        pub fn main() {
            let a_list = []
            let _ = list.length(a_list) == 0
        }
        "#,
        Warning::UnusedVariable {
            name: "b".into(),
            location: SrcSpan { start: 17, end: 18 },
        }
    );
}
