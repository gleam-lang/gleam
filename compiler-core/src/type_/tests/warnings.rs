use super::*;
use crate::ast::TodoKind;

macro_rules! assert_warning {
    ($src:expr) => {
        let (mut ast, _) = crate::parse::parse_module($src).expect("syntax error");
        ast.name = vec!["my_module".to_string()];
        let mut warnings: Vec<Warning> = vec![];
        let ids = UniqueIdGenerator::new();
        let mut modules = im::HashMap::new();
        // DUPE: preludeinsertion
        // TODO: Currently we do this here and also in the tests. It would be better
        // to have one place where we create all this required state for use in each
        // place.
        let _ = modules.insert("gleam".to_string(), build_prelude(&ids));
        let _ = infer_module(
            Target::Erlang,
            &ids,
            ast,
            Origin::Src,
            "thepackage",
            &modules,
            &mut warnings,
        )
        .expect("should successfully infer");

        let mut nocolor = termcolor::Buffer::no_color();
        for w in warnings {
            let warning = w.into_warning(PathBuf::from("/src/warning/wrn.gleam"), $src.to_string());
            warning.pretty(&mut nocolor)
        }

        let output = String::from_utf8(nocolor.into_inner())
            .expect("Error printing produced invalid utf8");

        insta::assert_snapshot!(insta::internals::AutoName, output, $src);
    };
    ($src:expr, $warning:expr $(,)?) => {
        let (mut ast, _) = crate::parse::parse_module($src).expect("syntax error");
        ast.name = vec!["my_module".to_string()];
        let mut warnings = vec![];
        let ids = UniqueIdGenerator::new();
        let mut modules = im::HashMap::new();
        // DUPE: preludeinsertion
        // TODO: Currently we do this here and also in the tests. It would be better
        // to have one place where we create all this required state for use in each
        // place.
        let _ = modules.insert("gleam".to_string(), build_prelude(&ids));
        let _ = infer_module(
            Target::Erlang,
            &ids,
            ast,
            Origin::Src,
            "thepackage",
            &modules,
            &mut warnings,
        )
        .expect("should successfully infer");

        assert!(!warnings.is_empty());
        assert_eq!($warning, warnings[0]);
    };
    ($(($name:expr, $module_src:literal)),+, $src:expr, $warning:expr $(,)?) => {
        let mut warnings = vec![];
        let ids = UniqueIdGenerator::new();
        let mut modules = im::HashMap::new();
        // DUPE: preludeinsertion
        // TODO: Currently we do this here and also in the tests. It would be better
        // to have one place where we create all this required state for use in each
        // place.
        let _ = modules.insert("gleam".to_string(), build_prelude(&ids));
        // Repeatedly create importable modules for each one given
        $(
        let (mut ast, _) = crate::parse::parse_module($module_src).expect("syntax error");
        ast.name = $name;
        let module = infer_module(
            Target::Erlang,
            &ids,
            ast,
            Origin::Src,
            "thepackage",
            &modules,
            &mut warnings,
        )
        .expect("should successfully infer");
        let _ = modules.insert($name.join("/"), module.type_info);
        )*

        let (mut ast, _) = crate::parse::parse_module($src).expect("syntax error");
        ast.name = vec!["my_module".to_string()];
        let _ = infer_module(
            Target::Erlang,
            &ids,
            ast,
            Origin::Src,
            "thepackage",
            &modules,
            &mut warnings,
        )
        .expect("should successfully infer");

        assert!(!warnings.is_empty());
        assert_eq!($warning, warnings[0]);
    };
}

macro_rules! assert_no_warnings {
    ($src:expr $(,)?) => {
        let (mut ast, _) = crate::parse::parse_module($src).expect("syntax error");
        ast.name = vec!["my_module".to_string()];
        let expected: Vec<Warning> = vec![];
        let mut warnings = vec![];
        let ids = UniqueIdGenerator::new();
        let mut modules = im::HashMap::new();
        // DUPE: preludeinsertion
        // TODO: Currently we do this here and also in the tests. It would be better
        // to have one place where we create all this required state for use in each
        // place.
        let _ = modules.insert("gleam".to_string(), build_prelude(&ids));
        let _ = infer_module(
            Target::Erlang,
            &ids,
            ast,
            Origin::Src,
            "thepackage",
            &modules,
            &mut warnings,
        )
        .expect("should successfully infer");

        assert_eq!(expected, warnings);
    };
    ($(($name:expr, $module_src:literal)),+, $src:expr $(,)?) => {
        let expected: Vec<Warning> = vec![];
        let mut warnings = vec![];
        let ids = UniqueIdGenerator::new();
        let mut modules = im::HashMap::new();
        // DUPE: preludeinsertion
        // TODO: Currently we do this here and also in the tests. It would be better
        // to have one place where we create all this required state for use in each
        // place.
        let _ = modules.insert("gleam".to_string(), build_prelude(&ids));
        // Repeatedly create importable modules for each one given
        $(
        let (mut ast, _) = crate::parse::parse_module($module_src).expect("syntax error");
        ast.name = $name;
        let module = infer_module(
            Target::Erlang,
            &ids,
            ast,
            Origin::Src,
            "thepackage",
            &modules,
            &mut warnings,
        )
        .expect("should successfully infer");
        let _ = modules.insert($name.join("/"), module.type_info);
        )*

        let (mut ast, _) = crate::parse::parse_module($src).expect("syntax error");
        ast.name = vec!["my_module".to_string()];
        let _ = infer_module(
            Target::Erlang,
            &ids,
            ast,
            Origin::Src,
            "thepackage",
            &modules,
            &mut warnings,
        )
        .expect("should successfully infer");

        assert_eq!(expected, warnings);
    };
}

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
fn main() { foo(); 5 }",
        Warning::ImplicitlyDiscardedResult {
            location: SrcSpan { start: 32, end: 37 }
        }
    );
}

#[test]
fn result_discard_warning_test2() {
    // Explicitly discarded Results do not emit warnings
    assert_no_warnings!(
        "
pub fn foo() { Ok(5) }
pub fn main() { let _ = foo(); 5 }",
    );
}

#[test]
fn unused_int() {
    assert_warning!(
        "fn main() { 1; 2 }",
        Warning::UnusedLiteral {
            location: SrcSpan { start: 12, end: 13 }
        }
    );
}

#[test]
fn unused_float() {
    assert_warning!(
        "fn main() { 1.0; 2 }",
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
        \"1\"; 2 
    }",
        Warning::UnusedLiteral {
            location: SrcSpan { start: 26, end: 29 }
        }
    );
}

#[test]
fn unused_bit_string() {
    assert_warning!(
        "
    fn main() { 
        <<3>>; 2 
    }",
        Warning::UnusedLiteral {
            location: SrcSpan { start: 26, end: 31 }
        }
    );
}

#[test]
fn unused_tuple() {
    assert_warning!(
        "
    fn main() { 
        #(1.0, \"Hello world\"); 2
    }",
        Warning::UnusedLiteral {
            location: SrcSpan { start: 26, end: 47 }
        }
    );
}

#[test]
fn unused_list() {
    assert_warning!(
        "
    fn main() { 
        [1, 2, 3]; 2 
    }",
        Warning::UnusedLiteral {
            location: SrcSpan { start: 26, end: 35 }
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
        };
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
        };
        pub fn update_person() {
            let past = Person(\"Quinn\", 27)
            let present = Person(..past)
            present
        }",
        Warning::NoFieldsRecordUpdate {
            location: SrcSpan {
                start: 183,
                end: 197
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
        };
        pub fn update_person() {
            let past = Person(\"Quinn\", 27)
            let present = Person(..past, name: \"Quinn\", age: 28)
            present
        }",
        Warning::AllFieldsRecordUpdate {
            location: SrcSpan {
                start: 183,
                end: 221
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
            name: "X".to_string(),
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
            name: "X".to_string(),
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
            name: "X".to_string(),
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
            name: "a".to_string(),
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
            name: "a".to_string(),
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
            name: "b".to_string(),
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
            name: "b".to_string(),
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
            name: "b".to_string(),
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
            name: "c".to_string(),
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
        (
            vec!["gleam".to_string(), "foo".to_string()],
            "pub fn bar() { 1 }"
        ),
        "import gleam/foo",
        Warning::UnusedImportedModule {
            name: "foo".to_string(),
            location: SrcSpan { start: 7, end: 16 },
        }
    );
}

#[test]
fn unused_imported_module_with_alias_warnings_test() {
    assert_warning!(
        (
            vec!["gleam".to_string(), "foo".to_string()],
            "pub fn bar() { 1 }"
        ),
        "import gleam/foo as bar",
        Warning::UnusedImportedModule {
            name: "bar".to_string(),
            location: SrcSpan { start: 7, end: 23 },
        }
    );
}

#[test]
fn unused_imported_module_no_warning_on_used_function_test() {
    assert_no_warnings!(
        (
            vec!["gleam".to_string(), "foo".to_string()],
            "pub fn bar() { 1 }"
        ),
        "import gleam/foo; pub fn baz() { foo.bar() }",
    );
}

#[test]
fn unused_imported_module_no_warning_on_used_type_test() {
    assert_no_warnings!(
        (
            vec!["gleam".to_string(), "foo".to_string()],
            "pub type Foo = Int"
        ),
        "import gleam/foo; pub fn baz(a: foo.Foo) { a }",
    );
}

#[test]
fn unused_imported_module_no_warning_on_used_unqualified_function_test() {
    assert_no_warnings!(
        (
            vec!["gleam".to_string(), "foo".to_string()],
            "pub fn bar() { 1 }"
        ),
        "import gleam/foo.{bar}; pub fn baz() { bar() }",
    );
}

#[test]
fn unused_imported_module_no_warning_on_used_unqualified_type_test() {
    assert_no_warnings!(
        (
            vec!["gleam".to_string(), "foo".to_string()],
            "pub type Foo = Int"
        ),
        "import gleam/foo.{Foo}; pub fn baz(a: Foo) { a }",
    );
}

#[test]
fn module_access_registers_import_usage() {
    assert_no_warnings!(
        (
            vec!["gleam".to_string(), "bibble".to_string()],
            "pub const bobble = 1"
        ),
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
