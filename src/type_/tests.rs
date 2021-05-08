use super::test_helpers::*;
use super::*;
use crate::{
    ast::{BinOp, UntypedExpr},
    bit_string,
};

macro_rules! assert_infer {
    ($src:expr, $typ:expr $(,)?) => {
        println!("\n{}\n", $src);
        let mut printer = pretty::Printer::new();
        let ast = crate::parse::parse_expression_sequence($src).expect("syntax error");

        let mut modules = HashMap::new();
        let mut uid = 0;
        // DUPE: preludeinsertion
        // TODO: Currently we do this here and also in the tests. It would be better
        // to have one place where we create all this required state for use in each
        // place.
        let _ = modules.insert("gleam".to_string(), (Origin::Src, build_prelude(&mut uid)));
        let result = ExprTyper::new(&mut Environment::new(&mut uid, &[], &modules, &mut vec![]))
            .infer(ast)
            .expect("should successfully infer");
        assert_eq!(
            ($src, printer.pretty_print(result.type_().as_ref(), 0),),
            ($src, $typ.to_string()),
        );
    };
}

macro_rules! assert_module_error {
    ($src:expr, $error:expr $(,)?) => {
        let (mut ast, _) = crate::parse::parse_module($src).expect("syntax error");
        ast.name = vec!["my_module".to_string()];
        let mut modules = HashMap::new();
        let mut uid = 0;
        // DUPE: preludeinsertion
        // TODO: Currently we do this here and also in the tests. It would be better
        // to have one place where we create all this required state for use in each
        // place.
        let _ = modules.insert("gleam".to_string(), (Origin::Src, build_prelude(&mut uid)));
        let ast =
            infer_module(&mut uid, ast, &modules, &mut vec![]).expect_err("should infer an error");
        assert_eq!(($src, sort_options($error)), ($src, sort_options(ast)));
    };

    ($src:expr) => {
        let (ast, _) = crate::parse::parse_module($src).expect("syntax error");
        let mut modules = HashMap::new();
        let mut uid = 0;
        // DUPE: preludeinsertion
        // TODO: Currently we do this here and also in the tests. It would be better
        // to have one place where we create all this required state for use in each
        // place.
        let _ = modules.insert("gleam".to_string(), (Origin::Src, build_prelude(&mut uid)));
        let _ =
            infer_module(&mut uid, ast, &modules, &mut vec![]).expect_err("should infer an error");
    };
}

macro_rules! assert_error {
    ($src:expr, $error:expr $(,)?) => {
        let ast = crate::parse::parse_expression_sequence($src).expect("syntax error");
        let mut uid = 0;
        let mut modules = HashMap::new();
        // DUPE: preludeinsertion
        // TODO: Currently we do this here and also in the tests. It would be better
        // to have one place where we create all this required state for use in each
        // place.
        let _ = modules.insert("gleam".to_string(), (Origin::Src, build_prelude(&mut uid)));
        println!("new assert_error test: {}", modules.len());
        let result = ExprTyper::new(&mut Environment::new(
            &mut uid,
            &["somemod".to_string()],
            &modules,
            &mut vec![],
        ))
        .infer(ast)
        .expect_err("should infer an error");
        assert_eq!(($src, sort_options($error)), ($src, sort_options(result)),);
    };
}

macro_rules! assert_module_infer {
    ($src:expr, $module:expr $(,)?) => {{
        use itertools::Itertools;
        let (ast, _) = crate::parse::parse_module($src).expect("syntax error");
        let mut uid = 0;
        let mut modules = HashMap::new();
        // DUPE: preludeinsertion
        // TODO: Currently we do this here and also in the tests. It would be better
        // to have one place where we create all this required state for use in each
        // place.
        let _ = modules.insert("gleam".to_string(), (Origin::Src, build_prelude(&mut uid)));
        let ast =
            infer_module(&mut uid, ast, &modules, &mut vec![]).expect("should successfully infer");
        let constructors: Vec<(_, _)> = ast
            .type_info
            .values
            .iter()
            .map(|(k, v)| {
                let mut printer = pretty::Printer::new();
                (k.clone(), printer.pretty_print(&v.type_, 0))
            })
            .sorted()
            .collect();
        let expected: Vec<_> = $module
            .into_iter()
            .map(|(k, v)| (k.to_string(), v.to_string()))
            .collect();
        assert_eq!(($src, constructors), ($src, expected));
    }};
}

macro_rules! assert_warning {
    ($src:expr, $warning:expr $(,)?) => {
        let (mut ast, _) = crate::parse::parse_module($src).expect("syntax error");
        ast.name = vec!["my_module".to_string()];
        let mut warnings = vec![];
        let mut uid = 0;
        let mut modules = HashMap::new();
        // DUPE: preludeinsertion
        // TODO: Currently we do this here and also in the tests. It would be better
        // to have one place where we create all this required state for use in each
        // place.
        let _ = modules.insert("gleam".to_string(), (Origin::Src, build_prelude(&mut uid)));
        let _ = infer_module(&mut uid, ast, &modules, &mut warnings)
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
        let mut uid = 0;
        let mut modules = HashMap::new();
        // DUPE: preludeinsertion
        // TODO: Currently we do this here and also in the tests. It would be better
        // to have one place where we create all this required state for use in each
        // place.
        let _ = modules.insert("gleam".to_string(), (Origin::Src, build_prelude(&mut uid)));
        let _ = infer_module(&mut uid, ast, &modules, &mut warnings)
            .expect("should successfully infer");

        assert_eq!(expected, warnings);
    };
}

#[test]
fn field_map_reorder_test() {
    let int = |value: &str| UntypedExpr::Int {
        value: value.to_string(),
        location: SrcSpan { start: 0, end: 0 },
    };

    struct Case {
        arity: usize,
        fields: HashMap<String, usize>,
        args: Vec<CallArg<UntypedExpr>>,
        expected_result: Result<(), Error>,
        expected_args: Vec<CallArg<UntypedExpr>>,
    }

    impl Case {
        fn test(self) {
            let mut args = self.args;
            let fm = FieldMap {
                arity: self.arity,
                fields: self.fields,
            };
            let location = SrcSpan { start: 0, end: 0 };
            assert_eq!(self.expected_result, fm.reorder(&mut args, location));
            assert_eq!(self.expected_args, args);
        }
    }

    Case {
        arity: 0,
        fields: HashMap::new(),
        args: vec![],
        expected_result: Ok(()),
        expected_args: vec![],
    }
    .test();

    Case {
        arity: 3,
        fields: HashMap::new(),
        args: vec![
            CallArg {
                location: Default::default(),
                label: None,
                value: int("1"),
            },
            CallArg {
                location: Default::default(),
                label: None,
                value: int("2"),
            },
            CallArg {
                location: Default::default(),
                label: None,
                value: int("3"),
            },
        ],
        expected_result: Ok(()),
        expected_args: vec![
            CallArg {
                location: Default::default(),
                label: None,
                value: int("1"),
            },
            CallArg {
                location: Default::default(),
                label: None,
                value: int("2"),
            },
            CallArg {
                location: Default::default(),
                label: None,
                value: int("3"),
            },
        ],
    }
    .test();

    Case {
        arity: 3,
        fields: [("last".to_string(), 2)].iter().cloned().collect(),
        args: vec![
            CallArg {
                location: Default::default(),
                label: None,
                value: int("1"),
            },
            CallArg {
                location: Default::default(),
                label: None,
                value: int("2"),
            },
            CallArg {
                location: Default::default(),
                label: Some("last".to_string()),
                value: int("3"),
            },
        ],
        expected_result: Ok(()),
        expected_args: vec![
            CallArg {
                location: Default::default(),
                label: None,
                value: int("1"),
            },
            CallArg {
                location: Default::default(),
                label: None,
                value: int("2"),
            },
            CallArg {
                location: Default::default(),
                label: Some("last".to_string()),
                value: int("3"),
            },
        ],
    }
    .test();
}

#[test]
fn infer_module_type_retention_test() {
    let module: UntypedModule = crate::ast::Module {
        documentation: vec![],
        name: vec!["ok".to_string()],
        statements: vec![],
        type_info: (),
    };
    let mut uid = 0;
    let mut modules = HashMap::new();
    // DUPE: preludeinsertion
    // TODO: Currently we do this here and also in the tests. It would be better
    // to have one place where we create all this required state for use in each
    // place.
    let _ = modules.insert("gleam".to_string(), (Origin::Src, build_prelude(&mut uid)));
    let module = infer_module(&mut uid, module, &modules, &mut vec![]).expect("Should infer OK");

    assert_eq!(
        module.type_info,
        Module {
            name: vec!["ok".to_string()],
            types: HashMap::new(), // Core type constructors like String and Int are not included
            values: HashMap::new(),
            accessors: HashMap::new(),
        }
    );
}

#[test]
fn simple_exprs() {
    assert_infer!("True", "Bool");
    assert_infer!("False", "Bool");
    assert_infer!("1", "Int");
    assert_infer!("-2", "Int");
    assert_infer!("1.0", "Float");
    assert_infer!("-8.0", "Float");
    assert_infer!("\"ok\"", "String");
    assert_infer!("\"ok\"", "String");
    assert_infer!("[]", "List(a)");
    assert_infer!("4 % 1", "Int");
    assert_infer!("4 > 1", "Bool");
    assert_infer!("4 >= 1", "Bool");
    assert_infer!("4 <= 1", "Bool");
    assert_infer!("4 < 1", "Bool");

    // Numbers with _'s
    assert_infer!("1000_000", "Int");
    assert_infer!("1_000", "Int");
    assert_infer!("1_000.", "Float");
    assert_infer!("10_000.001", "Float");
    assert_infer!("100_000.", "Float");

    // Nil
    assert_infer!("Nil", "Nil");

    // todo
    assert_infer!("todo", "a");
    assert_infer!("1 == todo", "Bool");
    assert_infer!("todo != 1", "Bool");
    assert_infer!("todo + 1", "Int");
    assert_infer!("todo(\"test\") + 1", "Int");

    // hex, octal, and binary literals
    assert_infer!("0xF", "Int");
    assert_infer!("0o11", "Int");
    assert_infer!("0b1010", "Int");
}

#[test]
fn let_() {
    assert_infer!("let x = 1 2", "Int");
    assert_infer!("let x = 1 x", "Int");
    assert_infer!("let x = 2.0 x", "Float");
    assert_infer!("let x = 2 let y = x y", "Int");
    assert_infer!(
        "let tuple(tuple(_, _) as x, _) = tuple(tuple(0, 1.0), []) x",
        "#(Int, Float)"
    );
    assert_infer!("let x: String = \"\" x", "String");
    assert_infer!("let x: tuple(Int, Int) = tuple(5, 5) x", "#(Int, Int)",);
    assert_infer!(
        "let x: tuple(Int, Float) = tuple(5, 5.0) x",
        "#(Int, Float)",
    );
    assert_infer!("let [1, 2, ..x]: List(Int) = [1,2,3] x", "List(Int)",);
    assert_infer!(
        "let tuple(5, [..x]): tuple(Int, List(Int)) = tuple(5, [1,2,3]) x",
        "List(Int)",
    );
    assert_infer!(
        "let tuple(5.0, [..x]): tuple(Float, List(Int)) = tuple(5.0, [1,2,3]) x",
        "List(Int)",
    );
    assert_infer!("let x: List(_) = [] x", "List(a)");
    assert_infer!("let x: List(_) = [1] x", "List(Int)");

    assert_infer!("let [] = [] 1", "Int");
    assert_infer!("let [a] = [1] a", "Int");
    assert_infer!("let [a, 2] = [1] a", "Int");
    assert_infer!("let [a, .. b] = [1] a", "Int");
    assert_infer!("let [a, .. _] = [1] a", "Int");
    assert_infer!("fn(x) { let [a] = x a }", "fn(List(a)) -> a");
    assert_infer!("fn(x) { let [a] = x a + 1 }", "fn(List(Int)) -> Int");
    assert_infer!("let _x = 1 2.0", "Float");
    assert_infer!("let _ = 1 2.0", "Float");
    assert_infer!("let tuple(tag, x) = tuple(1.0, 1) x", "Int");
    assert_infer!("fn(x) { let tuple(a, b) = x a }", "fn(#(a, b)) -> a");

    // assert
    assert_infer!("assert [] = [] 1", "Int");
    assert_infer!("assert [a] = [1] a", "Int");
    assert_infer!("assert [a, 2] = [1] a", "Int");
    assert_infer!("assert [a, .._] = [1] a", "Int");
    assert_infer!("assert [a, .._,] = [1] a", "Int");
    assert_infer!("fn(x) { assert [a] = x a }", "fn(List(a)) -> a");
    assert_infer!("fn(x) { assert [a] = x a + 1 }", "fn(List(Int)) -> Int");
    assert_infer!("assert _x = 1 2.0", "Float");
    assert_infer!("assert _ = 1 2.0", "Float");
    assert_infer!("assert tuple(tag, x) = tuple(1.0, 1) x", "Int");
    assert_infer!("fn(x) { assert tuple(a, b) = x a }", "fn(#(a, b)) -> a");
    assert_infer!("assert 5: Int = 5 5", "Int");

    // try
    assert_infer!("try x = Ok(1) Ok(x)", "Result(Int, a)");
    assert_infer!("try x = Ok(1) try y = Ok(1) Ok(x + y)", "Result(Int, a)");
    assert_infer!(
        "try x = Error(Nil) try y = Ok(1) Ok(x + 1)",
        "Result(Int, Nil)"
    );
    assert_infer!(
        "try x = Error(Nil) try y = Error(Nil) Ok(x + 1)",
        "Result(Int, Nil)"
    );
    assert_infer!("try x = Error(Nil) Ok(x + 1)", "Result(Int, Nil)");

    // https://github.com/gleam-lang/gleam/issues/786
    assert_infer!("let _x0 = 1 2", "Int");
}

#[test]
fn lists() {
    assert_infer!("[]", "List(a)");
    assert_infer!("[1]", "List(Int)");
    assert_infer!("[1, 2, 3]", "List(Int)");
    assert_infer!("[[]]", "List(List(a))");
    assert_infer!("[[1.0, 2.0]]", "List(List(Float))");
    assert_infer!("[fn(x) { x }]", "List(fn(a) -> a)");
    assert_infer!("[fn(x) { x + 1 }]", "List(fn(Int) -> Int)");
    assert_infer!("[fn(x) { x }, fn(x) { x + 1 }]", "List(fn(Int) -> Int)");
    assert_infer!("[fn(x) { x + 1 }, fn(x) { x }]", "List(fn(Int) -> Int)");
    assert_infer!("[[], []]", "List(List(a))");
    assert_infer!("[[], [1]]", "List(List(Int))");

    assert_infer!("[1, ..[2, ..[]]]", "List(Int)");
    assert_infer!("[fn(x) { x }, ..[]]", "List(fn(a) -> a)");
    assert_infer!("let x = [1, ..[]] [2, ..x]", "List(Int)");

    // Trailing commas
    assert_infer!("[1, ..[2, ..[],]]", "List(Int)");
    assert_infer!("[fn(x) { x },..[]]", "List(fn(a) -> a)");

    assert_infer!("let f = fn(x) { x } [f, f]", "List(fn(a) -> a)");
    assert_infer!("[tuple([], [])]", "List(#(List(a), List(b)))");
}

#[test]
fn tuples() {
    assert_infer!("tuple(1)", "#(Int)");
    assert_infer!("tuple(1, 2.0)", "#(Int, Float)");
    assert_infer!("tuple(1, 2.0, 3)", "#(Int, Float, Int)");
    assert_infer!("tuple(1, 2.0, tuple(1, 1))", "#(Int, Float, #(Int, Int))",);

    // new syntax
    assert_infer!("#(1)", "#(Int)");
    assert_infer!("#(1, 2.0)", "#(Int, Float)");
    assert_infer!("#(1, 2.0, 3)", "#(Int, Float, Int)");
    assert_infer!("#(1, 2.0, #(1, 1))", "#(Int, Float, #(Int, Int))",);
}

#[test]
fn expr_fn() {
    assert_infer!("fn(x) { x }", "fn(a) -> a");
    assert_infer!("fn(x) { x }", "fn(a) -> a");
    assert_infer!("fn(x, y) { x }", "fn(a, b) -> a");
    assert_infer!("fn(x, y) { [] }", "fn(a, b) -> List(c)");
    assert_infer!("let x = 1.0 1", "Int");
    assert_infer!("let id = fn(x) { x } id(1)", "Int");
    assert_infer!("let x = fn() { 1.0 } x()", "Float");
    assert_infer!("fn(x) { x }(1)", "Int");
    assert_infer!("fn() { 1 }", "fn() -> Int");
    assert_infer!("fn() { 1.1 }", "fn() -> Float");
    assert_infer!("fn(x) { 1.1 }", "fn(a) -> Float");
    assert_infer!("fn(x) { x }", "fn(a) -> a");
    assert_infer!("let x = fn(x) { 1.1 } x", "fn(a) -> Float");
    assert_infer!("fn(x, y, z) { 1 }", "fn(a, b, c) -> Int");
    assert_infer!("fn(x) { let y = x y }", "fn(a) -> a");
    assert_infer!("let id = fn(x) { x } id(1)", "Int");
    assert_infer!(
        "let constant = fn(x) { fn(y) { x } } let one = constant(1) one(2.0)",
        "Int",
    );

    assert_infer!("fn(f) { f(1) }", "fn(fn(Int) -> a) -> a");
    assert_infer!("fn(f, x) { f(x) }", "fn(fn(a) -> b, a) -> b");
    assert_infer!("fn(f) { fn(x) { f(x) } }", "fn(fn(a) -> b) -> fn(a) -> b");
    assert_infer!(
        "fn(f) { fn(x) { fn(y) { f(x, y) } } }",
        "fn(fn(a, b) -> c) -> fn(a) -> fn(b) -> c",
    );
    assert_infer!(
        "fn(f) { fn(x, y) { f(x)(y) } }",
        "fn(fn(a) -> fn(b) -> c) -> fn(a, b) -> c",
    );
    assert_infer!(
        "fn(f) { fn(x) { let ff = f ff(x) } }",
        "fn(fn(a) -> b) -> fn(a) -> b",
    );
    assert_infer!(
        "fn(f) { fn(x, y) { let ff = f(x) ff(y) } }",
        "fn(fn(a) -> fn(b) -> c) -> fn(a, b) -> c",
    );
    assert_infer!("fn(x) { fn(y) { x } }", "fn(a) -> fn(b) -> a");
    assert_infer!("fn(f) { f() }", "fn(fn() -> a) -> a");
    assert_infer!("fn(f, x) { f(f(x)) }", "fn(fn(a) -> a, a) -> a");
    assert_infer!(
        "let id = fn(a) { a } fn(x) { x(id) }",
        "fn(fn(fn(a) -> a) -> b) -> b",
    );

    assert_infer!("let add = fn(x, y) { x + y } add(_, 2)", "fn(Int) -> Int");
    assert_infer!("fn(x) { tuple(1, x) }", "fn(a) -> #(Int, a)");
    assert_infer!("fn(x, y) { tuple(x, y) }", "fn(a, b) -> #(a, b)");
    assert_infer!("fn(x) { tuple(x, x) }", "fn(a) -> #(a, a)");
    assert_infer!("fn(x) -> Int { x }", "fn(Int) -> Int");
    assert_infer!("fn(x) -> a { x }", "fn(a) -> a");
    assert_infer!("fn() -> Int { 2 }", "fn() -> Int");
}

#[test]
fn case() {
    assert_infer!("case 1 { a -> 1 }", "Int");
    assert_infer!("case 1 { a -> 1.0 b -> 2.0 c -> 3.0 }", "Float");
    assert_infer!("case 1 { a -> a }", "Int");
    assert_infer!("case 1 { 1 -> 10 2 -> 20 x -> x * 10 }", "Int");
    assert_infer!("case 2.0 { 2.0 -> 1 x -> 0 }", "Int");
    assert_infer!(r#"case "ok" { "ko" -> 1 x -> 0 }"#, "Int");

    // Multiple subject case
    assert_infer!("case 1, 2.0 { a, b -> a }", "Int");
    assert_infer!("case 1, 2.0 { a, b -> b }", "Float");
    assert_infer!("case 1, 2.0, 3 { a, b, c -> a + c }", "Int");
}

#[test]
fn tuple_index() {
    assert_infer!("tuple(1, 2.0).0", "Int");
    assert_infer!("tuple(1, 2.0).1", "Float");
}

#[test]
fn pipe() {
    assert_infer!("1 |> fn(x) { x }", "Int");
    assert_infer!("1.0 |> fn(x) { x }", "Float");
    assert_infer!("let id = fn(x) { x } 1 |> id", "Int");
    assert_infer!("let id = fn(x) { x } 1.0 |> id", "Float");
    assert_infer!("let add = fn(x, y) { x + y } 1 |> add(_, 2)", "Int");
    assert_infer!("let add = fn(x, y) { x + y } 1 |> add(2, _)", "Int");
    assert_infer!("let add = fn(x, y) { x + y } 1 |> add(2)", "Int");
    assert_infer!("let id = fn(x) { x } 1 |> id()", "Int");
    assert_infer!("let add = fn(x) { fn(y) { y + x } } 1 |> add(1)", "Int");
    assert_infer!(
        "let add = fn(x, _, _) { fn(y) { y + x } } 1 |> add(1, 2, 3)",
        "Int"
    );
}

#[test]
fn bit_strings() {
    assert_infer!("let <<x>> = <<1>> x", "Int");
    assert_infer!("let <<x>> = <<1>> x", "Int");
    assert_infer!("let <<x:float>> = <<1>> x", "Float");
    assert_infer!("let <<x:binary>> = <<1>> x", "BitString");
    assert_infer!("let <<x:bytes>> = <<1>> x", "BitString");
    assert_infer!("let <<x:bit_string>> = <<1>> x", "BitString");
    assert_infer!("let <<x:bits>> = <<1>> x", "BitString");

    assert_infer!("let <<x:utf8_codepoint>> = <<128013:32>> x", "UtfCodepoint");
    assert_infer!(
        "let <<x:utf16_codepoint>> = <<128013:32>> x",
        "UtfCodepoint"
    );
    assert_infer!(
        "let <<x:utf32_codepoint>> = <<128013:32>> x",
        "UtfCodepoint"
    );

    assert_infer!(
        "let a = <<1>> let <<x:binary>> = <<1, a:2-bit_string>> x",
        "BitString"
    );
    assert_infer!(
        "let x = <<<<1>>:bit_string, <<2>>:bit_string>> x",
        "BitString"
    );

    assert_module_error!(
        "fn x() { \"test\" }

fn main() {
    let a = <<1:size(x())>>
    a
}",
        Error::CouldNotUnify {
            location: SrcSpan { start: 52, end: 55 },
            expected: int(),
            given: string(),
            situation: None,
        },
    );

    assert_error!(
        "let <<x:utf8>> = <<1>> x",
        Error::BitStringSegmentError {
            error: bit_string::ErrorType::VariableUtfSegmentInPattern,
            location: SrcSpan { start: 6, end: 12 },
        }
    );

    assert_error!(
        "let <<x:utf16>> = <<1>> x",
        Error::BitStringSegmentError {
            error: bit_string::ErrorType::VariableUtfSegmentInPattern,
            location: SrcSpan { start: 6, end: 13 },
        }
    );

    assert_error!(
        "let <<x:utf32>> = <<1>> x",
        Error::BitStringSegmentError {
            error: bit_string::ErrorType::VariableUtfSegmentInPattern,
            location: SrcSpan { start: 6, end: 13 },
        }
    );
}

#[test]
fn infer_bit_string_error_test() {
    assert_error!(
        "case <<1>> { <<2.0, a>> -> 1 }",
        Error::CouldNotUnify {
            situation: None,
            location: SrcSpan { start: 15, end: 18 },
            expected: int(),
            given: float(),
        },
    );

    assert_error!(
        "case <<1>> { <<a:float>> if a > 1 -> 1 }",
        Error::CouldNotUnify {
            situation: None,
            location: SrcSpan { start: 28, end: 29 },
            expected: int(),
            given: float(),
        },
    );

    assert_error!(
        "case <<1>> { <<a:binary>> if a > 1 -> 1 }",
        Error::CouldNotUnify {
            situation: None,
            location: SrcSpan { start: 29, end: 30 },
            expected: int(),
            given: bit_string(),
        },
    );

    assert_error!(
        "case <<1>> { <<a:utf16_codepoint>> if a == \"test\" -> 1 }",
        Error::CouldNotUnify {
            situation: None,
            location: SrcSpan { start: 38, end: 49 },
            expected: utf_codepoint(),
            given: string(),
        },
    );

    // Segments

    assert_error!(
        "case <<1>> { <<_:binary, _:binary>> -> 1 }",
        Error::BitStringSegmentError {
            error: bit_string::ErrorType::SegmentMustHaveSize,
            location: SrcSpan { start: 17, end: 23 },
        },
    );

    assert_error!(
        "case <<1>> { <<_:bit_string, _:binary>> -> 1 }",
        Error::BitStringSegmentError {
            error: bit_string::ErrorType::SegmentMustHaveSize,
            location: SrcSpan { start: 17, end: 27 },
        },
    );

    assert_error!(
        "case <<1>> { <<_:binary, _:bit_string>> -> 1 }",
        Error::BitStringSegmentError {
            error: bit_string::ErrorType::SegmentMustHaveSize,
            location: SrcSpan { start: 17, end: 23 },
        },
    );

    // Options

    assert_error!(
        "let x = <<1:int-binary>> x",
        Error::BitStringSegmentError {
            error: bit_string::ErrorType::ConflictingTypeOptions {
                existing_type: "int".to_string(),
            },
            location: SrcSpan { start: 16, end: 22 },
        },
    );

    assert_error!(
        "case <<1>> { <<1:bit_string-binary>> -> 1 }",
        Error::BitStringSegmentError {
            error: bit_string::ErrorType::ConflictingTypeOptions {
                existing_type: "bit_string".to_string(),
            },
            location: SrcSpan { start: 28, end: 34 },
        },
    );

    assert_error!(
        "let x = <<1:signed-unsigned>> x",
        Error::BitStringSegmentError {
            error: bit_string::ErrorType::ConflictingSignednessOptions {
                existing_signed: "signed".to_string(),
            },
            location: SrcSpan { start: 19, end: 27 },
        }
    );

    assert_error!(
        "case <<1>> { <<1:unsigned-signed>> -> 1 }",
        Error::BitStringSegmentError {
            error: bit_string::ErrorType::ConflictingSignednessOptions {
                existing_signed: "unsigned".to_string(),
            },
            location: SrcSpan { start: 26, end: 32 },
        }
    );

    assert_error!(
        "let x = <<1:big-little>> x",
        Error::BitStringSegmentError {
            error: bit_string::ErrorType::ConflictingEndiannessOptions {
                existing_endianness: "big".to_string(),
            },
            location: SrcSpan { start: 16, end: 22 },
        }
    );

    assert_error!(
        "case <<1>> { <<1:native-big>> -> 1 }",
        Error::BitStringSegmentError {
            error: bit_string::ErrorType::ConflictingEndiannessOptions {
                existing_endianness: "native".to_string(),
            },
            location: SrcSpan { start: 24, end: 27 },
        }
    );

    // Size and unit options

    assert_error!(
        "let x = <<1:8-size(5)>> x",
        Error::BitStringSegmentError {
            error: bit_string::ErrorType::ConflictingSizeOptions,
            location: SrcSpan { start: 14, end: 21 },
        }
    );

    assert_error!(
        "case <<1>> { <<1:size(2)-size(8)>> -> a }",
        Error::BitStringSegmentError {
            error: bit_string::ErrorType::ConflictingSizeOptions,
            location: SrcSpan { start: 25, end: 32 },
        }
    );

    assert_error!(
        "let x = <<1:unit(2)-unit(5)>> x",
        Error::BitStringSegmentError {
            error: bit_string::ErrorType::ConflictingUnitOptions,
            location: SrcSpan { start: 20, end: 27 },
        }
    );

    assert_error!(
        "let x = <<1:utf8_codepoint-unit(5)>> x",
        Error::BitStringSegmentError {
            error: bit_string::ErrorType::TypeDoesNotAllowUnit {
                typ: "utf8_codepoint".to_string(),
            },
            location: SrcSpan { start: 12, end: 26 },
        }
    );

    assert_error!(
        "let x = <<1:utf16_codepoint-unit(5)>> x",
        Error::BitStringSegmentError {
            error: bit_string::ErrorType::TypeDoesNotAllowUnit {
                typ: "utf16_codepoint".to_string(),
            },
            location: SrcSpan { start: 12, end: 27 },
        }
    );

    assert_error!(
        "case <<1>> { <<1:utf32_codepoint-unit(2)>> -> a }",
        Error::BitStringSegmentError {
            error: bit_string::ErrorType::TypeDoesNotAllowUnit {
                typ: "utf32_codepoint".to_string(),
            },
            location: SrcSpan { start: 17, end: 32 },
        }
    );
    assert_error!(
        "let x = <<1:utf8_codepoint-size(5)>> x",
        Error::BitStringSegmentError {
            error: bit_string::ErrorType::TypeDoesNotAllowSize {
                typ: "utf8_codepoint".to_string(),
            },
            location: SrcSpan { start: 12, end: 26 },
        }
    );

    assert_error!(
        "let x = <<1:utf16_codepoint-size(5)>> x",
        Error::BitStringSegmentError {
            error: bit_string::ErrorType::TypeDoesNotAllowSize {
                typ: "utf16_codepoint".to_string(),
            },
            location: SrcSpan { start: 12, end: 27 },
        }
    );

    assert_error!(
        "case <<1>> { <<1:utf32_codepoint-size(5)>> -> a }",
        Error::BitStringSegmentError {
            error: bit_string::ErrorType::TypeDoesNotAllowSize {
                typ: "utf32_codepoint".to_string(),
            },
            location: SrcSpan { start: 17, end: 32 },
        }
    );

    assert_error!(
        "let x = <<1:utf8-unit(5)>> x",
        Error::BitStringSegmentError {
            error: bit_string::ErrorType::TypeDoesNotAllowUnit {
                typ: "utf8".to_string(),
            },
            location: SrcSpan { start: 12, end: 16 },
        }
    );

    assert_error!(
        "let x = <<1:utf16-unit(5)>> x",
        Error::BitStringSegmentError {
            error: bit_string::ErrorType::TypeDoesNotAllowUnit {
                typ: "utf16".to_string(),
            },
            location: SrcSpan { start: 12, end: 17 },
        }
    );

    assert_error!(
        "case <<1>> { <<1:utf32-unit(2)>> -> a }",
        Error::BitStringSegmentError {
            error: bit_string::ErrorType::TypeDoesNotAllowUnit {
                typ: "utf32".to_string(),
            },
            location: SrcSpan { start: 17, end: 22 },
        }
    );
    assert_error!(
        "let x = <<1:utf8-size(5)>> x",
        Error::BitStringSegmentError {
            error: bit_string::ErrorType::TypeDoesNotAllowSize {
                typ: "utf8".to_string(),
            },
            location: SrcSpan { start: 12, end: 16 },
        }
    );

    assert_error!(
        "let x = <<1:utf16-size(5)>> x",
        Error::BitStringSegmentError {
            error: bit_string::ErrorType::TypeDoesNotAllowSize {
                typ: "utf16".to_string(),
            },
            location: SrcSpan { start: 12, end: 17 },
        }
    );

    assert_error!(
        "case <<1>> { <<1:utf32-size(5)>> -> a }",
        Error::BitStringSegmentError {
            error: bit_string::ErrorType::TypeDoesNotAllowSize {
                typ: "utf32".to_string(),
            },
            location: SrcSpan { start: 17, end: 22 },
        }
    );

    assert_error!(
        "let x = <<1:unit(5)>> x",
        Error::BitStringSegmentError {
            error: bit_string::ErrorType::UnitMustHaveSize,
            location: SrcSpan { start: 12, end: 19 },
        }
    );

    // Size and unit values

    assert_error!(
        "let x = <<1:size(\"1\")>> x",
        Error::CouldNotUnify {
            situation: None,
            location: SrcSpan { start: 17, end: 20 },
            expected: int(),
            given: string(),
        },
    );

    assert_error!(
        "let a = 2.0 case <<1>> { <<1:size(a)>> -> a }",
        Error::CouldNotUnify {
            situation: None,
            location: SrcSpan { start: 34, end: 35 },
            expected: int(),
            given: float(),
        },
    );

    // float given size
    assert_error!(
        "let x = <<1:8-float>> x",
        Error::BitStringSegmentError {
            error: bit_string::ErrorType::FloatWithSize,
            location: SrcSpan { start: 12, end: 13 },
        }
    );
    // using binary in value
    assert_error!(
        "let x = <<<<1:1>>:binary>> x",
        Error::BitStringSegmentError {
            error: bit_string::ErrorType::OptionNotAllowedInValue,
            location: SrcSpan { start: 18, end: 24 },
        }
    );
}

#[test]
fn binop_unification_errors() {
    assert_error!(
        "1 + 1.0",
        Error::CouldNotUnify {
            situation: Some(UnifyErrorSituation::Operator(BinOp::AddInt)),
            location: SrcSpan { start: 4, end: 7 },
            expected: int(),
            given: float(),
        },
    );

    assert_error!(
        "1 +. 1.0",
        Error::CouldNotUnify {
            situation: Some(UnifyErrorSituation::Operator(BinOp::AddFloat)),
            location: SrcSpan { start: 0, end: 1 },
            expected: float(),
            given: int(),
        },
    );

    assert_error!(
        "1 == 1.0",
        Error::CouldNotUnify {
            situation: None,
            location: SrcSpan { start: 5, end: 8 },
            expected: int(),
            given: float(),
        },
    );

    assert_error!(
        "1 > 1.0",
        Error::CouldNotUnify {
            situation: Some(UnifyErrorSituation::Operator(BinOp::GtInt)),
            location: SrcSpan { start: 4, end: 7 },
            expected: int(),
            given: float(),
        },
    );

    assert_error!(
        "1.0 >. 1",
        Error::CouldNotUnify {
            situation: Some(UnifyErrorSituation::Operator(BinOp::GtFloat)),
            location: SrcSpan { start: 7, end: 8 },
            expected: float(),
            given: int(),
        },
    );

    assert_error!(
        "fn() { 1 } == fn(x) { x + 1 }",
        Error::CouldNotUnify {
            situation: None,
            location: SrcSpan { start: 14, end: 29 },
            expected: Arc::new(Type::Fn {
                args: vec![],
                retrn: int(),
            }),
            given: Arc::new(Type::Fn {
                args: vec![Arc::new(Type::Var {
                    type_: Arc::new(RefCell::new(TypeVar::Link { type_: int() })),
                })],
                retrn: int(),
            }),
        },
    );
}

#[test]
fn unknown_variable() {
    assert_error!(
        "x",
        Error::UnknownVariable {
            location: SrcSpan { start: 0, end: 1 },
            name: "x".to_string(),
            variables: env_vars(),
        },
    );

    assert_error!(
        "case 1 { x -> 1 1 -> x }",
        Error::UnknownVariable {
            location: SrcSpan { start: 21, end: 22 },
            name: "x".to_string(),
            variables: env_vars(),
        },
    );

    assert_error!(
        "let add = fn(x, y) { x + y } 1 |> add(unknown)",
        Error::UnknownVariable {
            location: SrcSpan { start: 38, end: 45 },
            name: "unknown".to_string(),
            variables: env_vars_with(&["add"]),
        },
    );
}

#[test]
fn incorrect_arity_error() {
    assert_error!(
        "let id = fn(x) { x } id()",
        Error::IncorrectArity {
            labels: vec![],
            location: SrcSpan { start: 21, end: 25 },
            expected: 1,
            given: 0,
        },
    );

    assert_error!(
        "let id = fn(x) { x } id(1, 2)",
        Error::IncorrectArity {
            labels: vec![],
            location: SrcSpan { start: 21, end: 29 },
            expected: 1,
            given: 2,
        },
    );
}

#[test]
fn case_clause_unification_error() {
    assert_error!(
        "case 1 { a -> 1 b -> 2.0 }",
        Error::CouldNotUnify {
            situation: Some(UnifyErrorSituation::CaseClauseMismatch),
            location: SrcSpan { start: 16, end: 24 },
            expected: int(),
            given: float(),
        },
    );

    assert_error!(
        "case 1.0 { 1 -> 1 }",
        Error::CouldNotUnify {
            situation: None,
            location: SrcSpan { start: 11, end: 12 },
            expected: float(),
            given: int(),
        },
    );

    assert_error!(
        "case 1 { 1.0 -> 1 }",
        Error::CouldNotUnify {
            situation: None,
            location: SrcSpan { start: 9, end: 12 },
            expected: int(),
            given: float(),
        },
    );

    assert_error!(
        "case 1, 2.0 { a, b -> a + b }",
        Error::CouldNotUnify {
            situation: Some(UnifyErrorSituation::Operator(BinOp::AddInt)),
            location: SrcSpan { start: 26, end: 27 },
            expected: int(),
            given: float(),
        },
    );

    assert_error!(
        "case 1, 2.0 { a, b -> a 1, 2 -> 0 }",
        Error::CouldNotUnify {
            situation: None,
            location: SrcSpan { start: 27, end: 28 },
            expected: float(),
            given: int(),
        },
    );
}

#[test]
fn annotated_functions_unification_error() {
    assert_error!(
        "let f = fn(x: Int) { x } f(1.0)",
        Error::CouldNotUnify {
            situation: None,
            location: SrcSpan { start: 27, end: 30 },
            expected: int(),
            given: float(),
        },
    );

    assert_error!(
        "fn() -> Int { 2.0 }",
        Error::CouldNotUnify {
            situation: Some(UnifyErrorSituation::ReturnAnnotationMismatch),
            location: SrcSpan { start: 14, end: 17 },
            expected: int(),
            given: float(),
        },
    );

    assert_error!(
        "fn(x: Int) -> Float { x }",
        Error::CouldNotUnify {
            situation: Some(UnifyErrorSituation::ReturnAnnotationMismatch),
            location: SrcSpan { start: 22, end: 23 },
            expected: float(),
            given: int(),
        },
    );
}

#[test]
fn the_rest() {
    assert_error!(
        "case tuple(1, 2, 3) { x if x == tuple(1, 1.0) -> 1 }",
        Error::CouldNotUnify {
            situation: None,
            location: SrcSpan { start: 27, end: 45 },
            expected: tuple(vec![int(), int(), int()]),
            given: tuple(vec![int(), float()]),
        },
    );

    assert_error!(
        "case [1] { x if x == [1, 2.0] -> 1 }",
        Error::CouldNotUnify {
            situation: None,
            location: SrcSpan { start: 25, end: 28 },
            expected: int(),
            given: float(),
        },
    );

    assert_error!(
        "case tuple(1, 2) { x if x == tuple(1, 1.0) -> 1 }",
        Error::CouldNotUnify {
            situation: None,
            location: SrcSpan { start: 24, end: 42 },
            expected: tuple(vec![int(), int()]),
            given: tuple(vec![int(), float()]),
        },
    );

    assert_error!(
        "case 1 { x if x == tuple() -> 1 }",
        Error::CouldNotUnify {
            situation: None,
            location: SrcSpan { start: 14, end: 26 },
            expected: int(),
            given: tuple(vec![]),
        },
    );

    assert_error!(
        "case 1 { _, _ -> 1 }",
        Error::IncorrectNumClausePatterns {
            location: SrcSpan { start: 9, end: 18 },
            expected: 1,
            given: 2,
        },
    );

    assert_error!(
        "let id = fn(x) { x(x) } 1",
        Error::RecursiveType {
            location: SrcSpan { start: 19, end: 20 },
        },
    );

    assert_error!(
        "let True(x) = 1 x",
        Error::IncorrectArity {
            labels: vec![],
            location: SrcSpan { start: 4, end: 11 },
            expected: 0,
            given: 1,
        },
    );

    assert_error!(
        "let Ok(1, x) = 1 x",
        Error::IncorrectArity {
            labels: vec![],
            location: SrcSpan { start: 4, end: 12 },
            expected: 1,
            given: 2,
        },
    );

    assert_error!(
        "let x = 1 x.whatever",
        Error::UnknownField {
            location: SrcSpan { start: 11, end: 20 },
            typ: int(),
            label: "whatever".to_string(),
            fields: vec![],
        },
    );

    assert_error!(
        "tuple(1, 2) == tuple(1, 2, 3)",
        Error::CouldNotUnify {
            situation: None,
            location: SrcSpan { start: 15, end: 29 },
            expected: tuple(vec![int(), int()]),
            given: tuple(vec![int(), int(), int()])
        },
    );

    assert_error!(
        "tuple(1.0, 2, 3) == tuple(1, 2, 3)",
        Error::CouldNotUnify {
            situation: None,
            location: SrcSpan { start: 20, end: 34 },
            expected: tuple(vec![float(), int(), int()]),
            given: tuple(vec![int(), int(), int()]),
        },
    );

    assert_error!(
        "let tuple(a, b) = 1 a",
        Error::CouldNotUnify {
            situation: None,
            location: SrcSpan { start: 4, end: 15 },
            expected: int(),
            given: tuple(vec![
                Arc::new(Type::Var {
                    type_: Arc::new(RefCell::new(TypeVar::Unbound { id: 7, level: 1 })),
                }),
                Arc::new(Type::Var {
                    type_: Arc::new(RefCell::new(TypeVar::Unbound { id: 8, level: 1 })),
                })
            ]),
        },
    );

    assert_error!(
        "[1.0] == [1]",
        Error::CouldNotUnify {
            situation: None,
            location: SrcSpan { start: 9, end: 12 },
            expected: list(Arc::new(Type::Var {
                type_: Arc::new(RefCell::new(TypeVar::Link { type_: float() }))
            })),
            given: list(Arc::new(Type::Var {
                type_: Arc::new(RefCell::new(TypeVar::Link { type_: int() }))
            }))
        },
    );

    assert_error!(
        "let x = 1 let y = 1.0 case x { _ if x == y -> 1 }",
        Error::CouldNotUnify {
            situation: None,
            location: SrcSpan { start: 36, end: 42 },
            expected: int(),
            given: float(),
        },
    );

    assert_error!(
        "let x = 1.0 let y = 1 case x { _ if x == y -> 1 }",
        Error::CouldNotUnify {
            situation: None,
            location: SrcSpan { start: 36, end: 42 },
            expected: float(),
            given: int(),
        },
    );

    assert_error!(
        "let x = 1.0 case x { _ if x -> 1 }",
        Error::CouldNotUnify {
            situation: None,
            location: SrcSpan { start: 26, end: 27 },
            expected: bool(),
            given: float(),
        },
    );

    assert_error!(
        "case tuple(1, 1.0) { tuple(x, _) | tuple(_, x) -> 1 }",
        Error::CouldNotUnify {
            situation: None,
            location: SrcSpan { start: 44, end: 45 },
            expected: int(),
            given: float(),
        },
    );

    assert_error!(
        "case [3.33], 1 { x, y if x > y -> 1 }",
        Error::CouldNotUnify {
            situation: None,
            location: SrcSpan { start: 25, end: 26 },
            expected: int(),
            given: list(float())
        }
    );

    assert_error!(
        "case 1, 2.22, \"three\" { x, _, y if x > y -> 1 }",
        Error::CouldNotUnify {
            situation: None,
            location: SrcSpan { start: 39, end: 40 },
            expected: int(),
            given: string()
        }
    );

    assert_error!(
        "case [3.33], 1 { x, y if x >= y -> 1 }",
        Error::CouldNotUnify {
            situation: None,
            location: SrcSpan { start: 25, end: 26 },
            expected: int(),
            given: list(float())
        }
    );

    assert_error!(
        "case 1, 2.22, \"three\" { x, _, y if x >= y -> 1 }",
        Error::CouldNotUnify {
            situation: None,
            location: SrcSpan { start: 40, end: 41 },
            expected: int(),
            given: string()
        }
    );

    assert_error!(
        "case [3.33], 1 { x, y if x < y -> 1 }",
        Error::CouldNotUnify {
            situation: None,
            location: SrcSpan { start: 25, end: 26 },
            expected: int(),
            given: list(float())
        }
    );

    assert_error!(
        "case 1, 2.22, \"three\" { x, _, y if x < y -> 1 }",
        Error::CouldNotUnify {
            situation: None,
            location: SrcSpan { start: 39, end: 40 },
            expected: int(),
            given: string()
        }
    );

    assert_error!(
        "case [3.33], 1 { x, y if x <= y -> 1 }",
        Error::CouldNotUnify {
            situation: None,
            location: SrcSpan { start: 25, end: 26 },
            expected: int(),
            given: list(float())
        }
    );

    assert_error!(
        "case 1, 2.22, \"three\" { x, _, y if x <= y -> 1 }",
        Error::CouldNotUnify {
            situation: None,
            location: SrcSpan { start: 40, end: 41 },
            expected: int(),
            given: string()
        }
    );

    assert_error!(
        "case [3], 1.1 { x, y if x >. y -> 1 }",
        Error::CouldNotUnify {
            situation: None,
            location: SrcSpan { start: 24, end: 25 },
            expected: float(),
            given: list(int())
        }
    );

    assert_error!(
        "case 2.22, 1, \"three\" { x, _, y if x >. y -> 1 }",
        Error::CouldNotUnify {
            situation: None,
            location: SrcSpan { start: 40, end: 41 },
            expected: float(),
            given: string()
        }
    );

    assert_error!(
        "case [3], 1.1 { x, y if x >=. y -> 1 }",
        Error::CouldNotUnify {
            situation: None,
            location: SrcSpan { start: 24, end: 25 },
            expected: float(),
            given: list(int())
        }
    );

    assert_error!(
        "case 2.22, 1, \"three\" { x, _, y if x >=. y -> 1 }",
        Error::CouldNotUnify {
            situation: None,
            location: SrcSpan { start: 41, end: 42 },
            expected: float(),
            given: string()
        }
    );

    assert_error!(
        "case [3], 1.1 { x, y if x <. y -> 1 }",
        Error::CouldNotUnify {
            situation: None,
            location: SrcSpan { start: 24, end: 25 },
            expected: float(),
            given: list(int())
        }
    );

    assert_error!(
        "case 2.22, 1, \"three\" { x, _, y if x <. y -> 1 }",
        Error::CouldNotUnify {
            situation: None,
            location: SrcSpan { start: 40, end: 41 },
            expected: float(),
            given: string()
        }
    );

    assert_error!(
        "case [3], 1.1 { x, y if x <=. y -> 1 }",
        Error::CouldNotUnify {
            situation: None,
            location: SrcSpan { start: 24, end: 25 },
            expected: float(),
            given: list(int())
        }
    );

    assert_error!(
        "case 2.22, 1, \"three\" { x, _, y if x <=. y -> 1 }",
        Error::CouldNotUnify {
            situation: None,
            location: SrcSpan { start: 41, end: 42 },
            expected: float(),
            given: string()
        }
    );

    assert_error!(
        "case 1 { x if x == \"x\" -> 1 }",
        Error::CouldNotUnify {
            situation: None,
            location: SrcSpan { start: 14, end: 22 },
            expected: int(),
            given: string()
        }
    );

    assert_error!(
        "case [1] { [x] | x -> 1 }",
        Error::CouldNotUnify {
            situation: None,
            location: SrcSpan { start: 17, end: 18 },
            expected: int(),
            given: list(int()),
        },
    );

    assert_error!(
        "case [1] { [x] | [] as x -> 1 }",
        Error::CouldNotUnify {
            situation: None,
            location: SrcSpan { start: 17, end: 18 },
            expected: int(),
            given: list(int()),
        },
    );

    assert_error!(
        "case [1] { [x] | [x, y] -> 1 }",
        Error::ExtraVarInAlternativePattern {
            location: SrcSpan { start: 21, end: 22 },
            name: "y".to_string()
        },
    );

    assert_error!(
        "case tuple(1, 2) { tuple(1, y) | tuple(x, y) -> 1 }",
        Error::ExtraVarInAlternativePattern {
            location: SrcSpan { start: 39, end: 40 },
            name: "x".to_string()
        },
    );

    assert_error!(
        "case tuple(1, 2) { tuple(1, y) | tuple(x, y) -> 1 }",
        Error::ExtraVarInAlternativePattern {
            location: SrcSpan { start: 39, end: 40 },
            name: "x".to_string()
        },
    );

    assert_error!(
        "let x = 1 case tuple(1, 2) { tuple(1, y) | tuple(x, y) -> 1 }",
        Error::ExtraVarInAlternativePattern {
            location: SrcSpan { start: 49, end: 50 },
            name: "x".to_string()
        },
    );

    // https://github.com/gleam-lang/gleam/issues/714
    assert_error!(
        "case tuple(1, 2) { tuple(1, _, _, _) -> 1 }",
        Error::IncorrectArity {
            labels: vec![],
            location: SrcSpan { start: 19, end: 36 },
            expected: 2,
            given: 4,
        },
    );

    // Duplicate vars

    assert_error!(
        "case tuple(1, 2) { tuple(x, x) -> 1 }",
        Error::DuplicateVarInPattern {
            location: SrcSpan { start: 28, end: 29 },
            name: "x".to_string()
        },
    );

    assert_error!(
        "case [3.33], 1 { x, x if x > x -> 1 }",
        Error::DuplicateVarInPattern {
            location: SrcSpan { start: 20, end: 21 },
            name: "x".to_string()
        },
    );

    assert_error!(
        "case [1, 2, 3] { [x, x, y] -> 1 }",
        Error::DuplicateVarInPattern {
            location: SrcSpan { start: 21, end: 22 },
            name: "x".to_string()
        },
    );

    // Tuple indexing

    assert_error!(
        "tuple(0, 1).2",
        Error::OutOfBoundsTupleIndex {
            location: SrcSpan { start: 11, end: 13 },
            index: 2,
            size: 2
        },
    );

    assert_error!(
        "Nil.2",
        Error::NotATuple {
            location: SrcSpan { start: 0, end: 3 },
            given: nil(),
        },
    );

    assert_error!(
        "fn(a) { a.2 }",
        Error::NotATupleUnbound {
            location: SrcSpan { start: 8, end: 9 },
        },
    );

    // Record field access

    assert_error!(
        "fn(a) { a.field }",
        Error::RecordAccessUnknownType {
            location: SrcSpan { start: 8, end: 9 },
        },
    );

    assert_error!(
        "fn(a: a) { a.field }",
        Error::UnknownField {
            location: SrcSpan { start: 12, end: 18 },
            label: "field".to_string(),
            fields: vec![],
            typ: Arc::new(Type::Var {
                type_: Arc::new(RefCell::new(TypeVar::Generic { id: 7 })),
            }),
        },
    );

    assert_error!(
        "try x = Error(1) try y = Error(1.) Ok(x)",
        Error::CouldNotUnify {
            situation: None,
            location: SrcSpan { start: 35, end: 40 },
            expected: result(
                Arc::new(Type::Var {
                    type_: Arc::new(RefCell::new(TypeVar::Link {
                        type_: Arc::new(Type::Var {
                            type_: Arc::new(RefCell::new(TypeVar::Unbound { id: 8, level: 1 }))
                        })
                    })),
                }),
                Arc::new(Type::Var {
                    type_: Arc::new(RefCell::new(TypeVar::Link { type_: int() })),
                }),
            ),
            given: result(
                Arc::new(Type::Var {
                    type_: Arc::new(RefCell::new(TypeVar::Link {
                        type_: Arc::new(Type::Var {
                            type_: Arc::new(RefCell::new(TypeVar::Unbound { id: 8, level: 1 }))
                        })
                    })),
                }),
                Arc::new(Type::Var {
                    type_: Arc::new(RefCell::new(TypeVar::Link { type_: float() })),
                }),
            ),
        },
    );

    assert_error!(
        "try x = Error(1) Error(1.)",
        Error::CouldNotUnify {
            situation: None,
            location: SrcSpan { start: 17, end: 26 },
            expected: result(
                Arc::new(Type::Var {
                    type_: Arc::new(RefCell::new(TypeVar::Link {
                        type_: Arc::new(Type::Var {
                            type_: Arc::new(RefCell::new(TypeVar::Unbound { id: 12, level: 1 }))
                        })
                    })),
                }),
                Arc::new(Type::Var {
                    type_: Arc::new(RefCell::new(TypeVar::Link { type_: int() })),
                }),
            ),
            given: result(
                Arc::new(Type::Var {
                    type_: Arc::new(RefCell::new(TypeVar::Unbound { id: 12, level: 1 }))
                }),
                Arc::new(Type::Var {
                    type_: Arc::new(RefCell::new(TypeVar::Link { type_: float() })),
                }),
            ),
        },
    );

    assert_error!(
        "try x = Error(1) 1",
        Error::CouldNotUnify {
            situation: None,
            location: SrcSpan { start: 17, end: 18 },
            expected: result(
                Arc::new(Type::Var {
                    type_: Arc::new(RefCell::new(TypeVar::Unbound { id: 11, level: 1 }))
                }),
                Arc::new(Type::Var {
                    type_: Arc::new(RefCell::new(TypeVar::Link { type_: int() })),
                }),
            ),
            given: int(),
        },
    );

    assert_error!(
        "try y = Error(1) try z = Error(1.) Ok(1)",
        Error::CouldNotUnify {
            situation: None,
            location: SrcSpan { start: 35, end: 40 },
            expected: result(
                Arc::new(Type::Var {
                    type_: Arc::new(RefCell::new(TypeVar::Link { type_: int() }))
                }),
                Arc::new(Type::Var {
                    type_: Arc::new(RefCell::new(TypeVar::Link { type_: int() })),
                }),
            ),
            given: result(
                Arc::new(Type::Var {
                    type_: Arc::new(RefCell::new(TypeVar::Link { type_: int() }))
                }),
                Arc::new(Type::Var {
                    type_: Arc::new(RefCell::new(TypeVar::Link { type_: float() })),
                }),
            ),
        },
    );

    assert_error!(
        r#"try x = Error(1) Error("Not this one") Error("This one")"#,
        Error::CouldNotUnify {
            situation: None,
            location: SrcSpan { start: 39, end: 56 },
            expected: result(
                Arc::new(Type::Var {
                    type_: Arc::new(RefCell::new(TypeVar::Link {
                        type_: Arc::new(Type::Var {
                            type_: Arc::new(RefCell::new(TypeVar::Unbound { id: 14, level: 1 }))
                        })
                    }))
                }),
                Arc::new(Type::Var {
                    type_: Arc::new(RefCell::new(TypeVar::Link { type_: int() })),
                }),
            ),
            given: result(
                Arc::new(Type::Var {
                    type_: Arc::new(RefCell::new(TypeVar::Unbound { id: 14, level: 1 }))
                }),
                Arc::new(Type::Var {
                    type_: Arc::new(RefCell::new(TypeVar::Link { type_: string() })),
                }),
            ),
        },
    );
}

#[test]
fn infer_module_test() {
    assert_module_infer!(
        "pub fn repeat(i, x) {
           case i {
             0 -> []
             i -> [x .. repeat(i - 1, x)]
           }
         }",
        vec![("repeat", "fn(Int, a) -> List(a)")],
    );

    assert_module_infer!(
        "type Html = String
         pub fn go() { 1 }",
        vec![("go", "fn() -> Int")],
    );
    assert_module_infer!(
        "pub fn length(list) {
           case list {
           [] -> 0
           [x .. xs] -> length(xs) + 1
           }
        }",
        vec![("length", "fn(List(a)) -> Int")],
    );

    assert_module_infer!(
        "fn private() { 1 }
         pub fn public() { 1 }",
        vec![("public", "fn() -> Int")],
    );

    assert_module_infer!(
        "pub type Is { Yes No }
         pub fn yes() { Yes }
         pub fn no() { No }",
        vec![
            ("No", "Is"),
            ("Yes", "Is"),
            ("no", "fn() -> Is"),
            ("yes", "fn() -> Is"),
        ],
    );

    assert_module_infer!(
        "pub type Num { I(Int) }
         pub fn one() { I(1) }",
        vec![("I", "fn(Int) -> Num"), ("one", "fn() -> Num")],
    );

    assert_module_infer!(
        "pub type Box(a) { Box(a) }
        pub fn int() { Box(1) }
        pub fn float() { Box(1.0) }",
        vec![
            ("Box", "fn(a) -> Box(a)"),
            ("float", "fn() -> Box(Float)"),
            ("int", "fn() -> Box(Int)"),
        ],
    );

    assert_module_infer!(
        "pub type Singleton { Singleton }
        pub fn go(x) { let Singleton = x 1 }",
        vec![("Singleton", "Singleton"), ("go", "fn(Singleton) -> Int")],
    );

    assert_module_infer!(
        "pub type Box(a) { Box(a) }
        pub fn unbox(x) { let Box(a) = x a }",
        vec![("Box", "fn(a) -> Box(a)"), ("unbox", "fn(Box(a)) -> a")],
    );

    assert_module_infer!(
        "pub type I { I(Int) }
        pub fn open(x) { case x { I(i) -> i  } }",
        vec![("I", "fn(Int) -> I"), ("open", "fn(I) -> Int")],
    );

    assert_module_infer!(
        "pub fn status() { 1 } pub fn list_of(x) { [x] }",
        vec![("list_of", "fn(a) -> List(a)"), ("status", "fn() -> Int")],
    );

    assert_module_infer!(
        "pub external fn go(String) -> String = \"\" \"\"",
        vec![("go", "fn(String) -> String")],
    );

    assert_module_infer!(
        "pub external fn go(Int) -> Float = \"\" \"\"",
        vec![("go", "fn(Int) -> Float")],
    );

    assert_module_infer!(
        "pub external fn go(Int) -> Int = \"\" \"\"",
        vec![("go", "fn(Int) -> Int")],
    );

    assert_module_infer!(
        "pub external fn ok() -> fn(Int) -> Int = \"\" \"\"",
        vec![("ok", "fn() -> fn(Int) -> Int")],
    );

    assert_module_infer!(
        "pub external fn go(Int) -> b = \"\" \"\"",
        vec![("go", "fn(Int) -> a")],
    );

    assert_module_infer!(
        "pub external fn go(Bool) -> b = \"\" \"\"",
        vec![("go", "fn(Bool) -> a")],
    );

    assert_module_infer!(
        "pub external fn go(List(a)) -> a = \"\" \"\"",
        vec![("go", "fn(List(a)) -> a")],
    );

    assert_module_infer!(
        "external fn go(Int) -> b = \"\" \"\"
        pub fn x() { go(1) }",
        vec![("x", "fn() -> a")],
    );

    assert_module_infer!(
        "external fn id(a) -> a = \"\" \"\"
        pub fn i(x) { id(x) }
        pub fn a() { id(1) }
        pub fn b() { id(1.0) }",
        vec![
            ("a", "fn() -> Int"),
            ("b", "fn() -> Float"),
            ("i", "fn(a) -> a"),
        ],
    );

    assert_module_infer!(
        "pub external fn len(List(a)) -> Int = \"\" \"\"",
        vec![("len", "fn(List(a)) -> Int")],
    );

    assert_module_infer!(
        "pub external type Connection\n
         pub external fn is_open(Connection) -> Bool = \"\" \"\"",
        vec![("is_open", "fn(Connection) -> Bool")],
    );

    assert_module_infer!(
        "pub external type Pair(thing, thing)\n
         pub external fn pair(a) -> Pair(a, a) = \"\" \"\"",
        vec![("pair", "fn(a) -> Pair(a, a)")],
    );

    assert_module_infer!(
        "pub fn one() { 1 }
         pub fn zero() { one() - 1 }
         pub fn two() { one() + zero() }",
        vec![
            ("one", "fn() -> Int"),
            ("two", "fn() -> Int"),
            ("zero", "fn() -> Int"),
        ],
    );

    assert_module_infer!(
        "pub fn one() { 1 }
         pub fn zero() { one() - 1 }
         pub fn two() { one() + zero() }",
        vec![
            ("one", "fn() -> Int"),
            ("two", "fn() -> Int"),
            ("zero", "fn() -> Int"),
        ],
    );

    // Structs
    assert_module_infer!(
        "pub type Box { Box(boxed: Int) }",
        vec![("Box", "fn(Int) -> Box")]
    );
    assert_module_infer!(
        "pub type Tup(a, b) { Tup(first: a, second: b) }",
        vec![("Tup", "fn(a, b) -> Tup(a, b)")]
    );
    assert_module_infer!(
        "pub type Tup(a, b, c) { Tup(first: a, second: b, third: c) }
         pub fn third(t) { let Tup(_ , _, third: a) = t a }",
        vec![
            ("Tup", "fn(a, b, c) -> Tup(a, b, c)"),
            ("third", "fn(Tup(a, b, c)) -> c"),
        ],
    );

    // Anon structs
    assert_module_infer!(
        "pub fn ok(x) { tuple(1, x) }",
        vec![("ok", "fn(a) -> #(Int, a)")],
    );

    assert_module_infer!(
        "pub external fn ok(Int) -> tuple(Int, Int) = \"\" \"\"",
        vec![("ok", "fn(Int) -> #(Int, Int)")],
    );

    assert_module_infer!(
        "pub external fn go(tuple(a, c)) -> c = \"\" \"\"",
        vec![("go", "fn(#(a, b)) -> b")],
    );

    assert_module_infer!(
        "pub fn always(ignore _a, return b) { b }",
        vec![("always", "fn(a, b) -> b")],
    );

    // Using types before they are defined

    assert_module_infer!(
        "pub type I { I(Num) } pub type Num { Num }",
        vec![("I", "fn(Num) -> I"), ("Num", "Num")]
    );

    assert_module_infer!(
        "pub type I { I(Num) } pub external type Num",
        vec![("I", "fn(Num) -> I")]
    );

    // We can create an aliases
    assert_module_infer!(
        "type IntString = Result(Int, String)
         pub fn ok_one() -> IntString { Ok(1) }",
        vec![("ok_one", "fn() -> Result(Int, String)")]
    );

    // We can create an alias with the same name as a built in type
    assert_module_infer!(
        "type Int = Float
         pub fn ok_one() -> Int { 1.0 }",
        vec![("ok_one", "fn() -> Float")]
    );

    // We can access fields on custom types with only one record
    assert_module_infer!(
        "
pub type Person { Person(name: String, age: Int) }
pub fn get_age(person: Person) { person.age }
pub fn get_name(person: Person) { person.name }",
        vec![
            ("Person", "fn(String, Int) -> Person"),
            ("get_age", "fn(Person) -> Int"),
            ("get_name", "fn(Person) -> String"),
        ]
    );

    // We can access fields on custom types with only one record
    assert_module_infer!(
        "
pub type One { One(name: String) }
pub type Two { Two(one: One) }
pub fn get(x: Two) { x.one.name }",
        vec![
            ("One", "fn(String) -> One"),
            ("Two", "fn(One) -> Two"),
            ("get", "fn(Two) -> String"),
        ]
    );

    // Field access correctly handles type parameters
    assert_module_infer!(
        "
pub type Box(a) { Box(inner: a) }
pub fn get_box(x: Box(Box(a))) { x.inner }
pub fn get_generic(x: Box(a)) { x.inner }
pub fn get_get_box(x: Box(Box(a))) { x.inner.inner }
pub fn get_int(x: Box(Int)) { x.inner }
pub fn get_string(x: Box(String)) { x.inner }
",
        vec![
            ("Box", "fn(a) -> Box(a)"),
            ("get_box", "fn(Box(Box(a))) -> Box(a)"),
            ("get_generic", "fn(Box(a)) -> a"),
            ("get_get_box", "fn(Box(Box(a))) -> a"),
            ("get_int", "fn(Box(Int)) -> Int"),
            ("get_string", "fn(Box(String)) -> String"),
        ]
    );

    // Field access works before type is defined
    assert_module_infer!(
        "
pub fn name(cat: Cat) {
  cat.name
}

pub opaque type Cat {
  Cat(name: String)
}",
        vec![("name", "fn(Cat) -> String"),]
    );

    // We can annotate let with custom types
    assert_module_infer!(
        "
        pub type Person {
            Person(name: String, age: Int)
        }

        pub fn create_person(name: String) {
            let x: Person = Person(name: name, age: 1)
            x
        }",
        vec![
            ("Person", "fn(String, Int) -> Person"),
            ("create_person", "fn(String) -> Person"),
        ]
    );

    assert_module_infer!(
        "
        pub type Box(inner) {
            Box(inner)
        }

        pub fn create_int_box(value: Int) {
            let x: Box(Int) = Box(value)
            x
        }

        pub fn create_float_box(value: Float) {
            let x: Box(Float) = Box(value)
            x
        }",
        vec![
            ("Box", "fn(a) -> Box(a)"),
            ("create_float_box", "fn(Float) -> Box(Float)"),
            ("create_int_box", "fn(Int) -> Box(Int)"),
        ]
    );

    // Opaque type constructors are available in the module where they are defined
    // but are not exported
    assert_module_infer!(
        "
pub opaque type One { One(name: String) }
pub fn get(x: One) { x.name }",
        vec![("get", "fn(One) -> String"),]
    );

    // Type variables are shared between function annotations and function annotations within their body
    assert_module_infer!(
        "
        pub type Box(a) {
            Box(value: a)
        };
        pub fn go(box1: Box(a)) {
            fn(box2: Box(a)) { box1.value == box2.value }
        }",
        vec![
            ("Box", "fn(a) -> Box(a)"),
            ("go", "fn(Box(a)) -> fn(Box(a)) -> Bool")
        ]
    );

    // Type variables are shared between function annotations and let annotations within their body
    assert_module_infer!(
        "
        pub type Box(a) {
            Box(value: a)
        };
        pub fn go(box1: Box(a)) {
            let x: Box(a) = box1
            fn(box2: Box(a)) { x.value == box2.value }
        }",
        vec![
            ("Box", "fn(a) -> Box(a)"),
            ("go", "fn(Box(a)) -> fn(Box(a)) -> Bool")
        ]
    );

    assert_module_infer!(
        "
pub type Box {
  Box(a: Nil, b: Int, c: Int, d: Int)
}

pub fn main() {
  Box(b: 1, c: 1, d: 1, a: Nil)
}",
        vec![
            ("Box", "fn(Nil, Int, Int, Int) -> Box"),
            ("main", "fn() -> Box"),
        ],
    );

    // No arguments given to a record update
    assert_module_infer!(
        "
        pub type Person {
            Person(name: String, age: Int)
        };
        pub fn identity(person: Person) {
            Person(..person)
        }",
        vec![
            ("Person", "fn(String, Int) -> Person"),
            ("identity", "fn(Person) -> Person")
        ]
    );

    // Some arguments given to a record update
    assert_module_infer!(
        "
        pub type Person {
            Person(name: String, age: Int)
        };
        pub fn update_name(person: Person, name: String) {
            Person(..person, name: name)
        }",
        vec![
            ("Person", "fn(String, Int) -> Person"),
            ("update_name", "fn(Person, String) -> Person")
        ]
    );

    // All arguments given in order to a record update
    assert_module_infer!(
        "
        pub type Person {
            Person(name: String, age: Int)
        };
        pub fn update_person(person: Person, name: String, age: Int) {
            Person(..person, name: name, age: age, )
        }",
        vec![
            ("Person", "fn(String, Int) -> Person"),
            ("update_person", "fn(Person, String, Int) -> Person")
        ]
    );

    // All arguments given out of order to a record update
    assert_module_infer!(
        "
        pub type Person {
            Person(name: String, age: Int)
        };
        pub fn update_person(person: Person, name: String, age: Int) {
            Person(..person, age: age, name: name)
        }",
        vec![
            ("Person", "fn(String, Int) -> Person"),
            ("update_person", "fn(Person, String, Int) -> Person")
        ]
    );

    // A record update with polymorphic types
    assert_module_infer!(
        "
        pub type Box(a, b) {
            Box(left: a, right: b)
        };

        pub fn combine_boxes(a: Box(Int, Bool), b: Box(Bool, Int)) {
            Box(..a, left: a.left + b.right, right: b.left)
        }",
        vec![
            ("Box", "fn(a, b) -> Box(a, b)"),
            (
                "combine_boxes",
                "fn(Box(Int, Bool), Box(Bool, Int)) -> Box(Int, Bool)"
            )
        ]
    );

    // A record update with unannotated polymorphic types
    assert_module_infer!(
        "
        pub type Box(a, b) {
            Box(left: a, right: b)
        };

        pub fn combine_boxes(a: Box(t1, t2), b: Box(t2, t1)) {
            Box(..a, left: b.right, right: b.left)
        }",
        vec![
            ("Box", "fn(a, b) -> Box(a, b)"),
            ("combine_boxes", "fn(Box(a, b), Box(b, a)) -> Box(a, b)")
        ]
    );
}

#[test]
fn module_could_not_unify() {
    assert_module_error!(
        "fn go() { 1 + 2.0 }",
        Error::CouldNotUnify {
            situation: Some(UnifyErrorSituation::Operator(BinOp::AddInt)),
            location: SrcSpan { start: 14, end: 17 },
            expected: int(),
            given: float(),
        }
    );

    assert_module_error!(
        "fn go() { 1 + 2.0 }",
        Error::CouldNotUnify {
            situation: Some(UnifyErrorSituation::Operator(BinOp::AddInt)),
            location: SrcSpan { start: 14, end: 17 },
            expected: int(),
            given: float(),
        }
    );

    assert_module_error!(
        "
fn id(x: a, y: a) { x }
pub fn x() { id(1, 1.0) }
                ",
        Error::CouldNotUnify {
            situation: None,
            location: SrcSpan { start: 44, end: 47 },
            expected: int(),
            given: float(),
        }
    );

    assert_module_error!(
        "
fn bar() -> Int {
    5
}

fn run(foo: fn() -> String) {
    foo()
}

fn demo() {
    run(bar)
}",
        Error::CouldNotUnify {
            situation: None,
            location: SrcSpan { start: 91, end: 94 },
            expected: Arc::new(Type::Fn {
                args: vec![],
                retrn: string(),
            }),
            given: Arc::new(Type::Fn {
                args: vec![],
                retrn: int(),
            }),
        },
    );

    assert_module_error!(
        "
fn bar(x: Int) -> Int {
    x * 5
}

fn run(foo: fn(String) -> Int) {
    foo(\"Foo.\")
}

fn demo() {
    run(bar)
}",
        Error::CouldNotUnify {
            situation: None,
            location: SrcSpan {
                start: 110,
                end: 113
            },
            expected: Arc::new(Type::Fn {
                args: vec![string()],
                retrn: int(),
            }),
            given: Arc::new(Type::Fn {
                args: vec![int()],
                retrn: int(),
            }),
        },
    );

    assert_module_error!(
        "fn main() { let x: String = 5 x }",
        Error::CouldNotUnify {
            situation: None,
            location: SrcSpan { start: 28, end: 29 },
            expected: string(),
            given: int(),
        },
    );

    assert_module_error!(
        "fn main() { assert 5: Int = \"\" 5 }",
        Error::CouldNotUnify {
            situation: None,
            location: SrcSpan { start: 19, end: 20 },
            expected: string(),
            given: int(),
        },
    );

    assert_module_error!(
        "fn main() { let x: tuple(x, x) = tuple(5, 5.0) x }",
        Error::CouldNotUnify {
            situation: None,
            location: SrcSpan { start: 33, end: 46 },
            expected: tuple(vec![
                Arc::new(Type::Var {
                    type_: Arc::new(RefCell::new(TypeVar::Generic { id: 8 }))
                }),
                Arc::new(Type::Var {
                    type_: Arc::new(RefCell::new(TypeVar::Generic { id: 8 }))
                })
            ]),
            given: tuple(vec![int(), float()]),
        },
    );

    assert_module_error!(
        "fn main() { let [1, 2, ..x]: List(String) = [1,2,3] x }",
        Error::CouldNotUnify {
            situation: None,
            location: SrcSpan { start: 44, end: 51 },
            expected: list(string()),
            given: list(int()),
        },
    );

    assert_module_error!(
        "fn main() {
            let tuple(y, [..x]): tuple(x, List(x)) = tuple(\"foo\", [1,2,3])
            x
        }",
        Error::CouldNotUnify {
            situation: None,
            location: SrcSpan { start: 65, end: 86 },
            expected: tuple(vec![
                Arc::new(Type::Var {
                    type_: Arc::new(RefCell::new(TypeVar::Generic { id: 9 }))
                }),
                list(Arc::new(Type::Var {
                    type_: Arc::new(RefCell::new(TypeVar::Generic { id: 9 }))
                }))
            ]),
            given: tuple(vec![string(), list(int())]),
        },
    );

    assert_module_error!(
        "
        pub type Box(inner) {
            Box(inner)
        }

        pub fn create_int_box(value: Int) {
            let x: Box(Float) = Box(value)
            x
        }",
        Error::CouldNotUnify {
            situation: None,
            location: SrcSpan {
                start: 141,
                end: 151
            },
            expected: Arc::new(Type::App {
                public: true,
                module: vec!["my_module".to_string()],
                name: "Box".to_string(),
                args: vec![float()]
            }),
            given: Arc::new(Type::App {
                public: true,
                module: vec!["my_module".to_string()],
                name: "Box".to_string(),
                args: vec![int()]
            }),
        },
    );

    assert_module_error!(
        "
        pub type Person {
            Person(name: String, age: Int)
        }

        pub fn create_person(age: Float) {
            let x: Person = Person(name: \"Quinn\", age: age)
            x
        }",
        Error::CouldNotUnify {
            situation: None,
            location: SrcSpan {
                start: 179,
                end: 182
            },
            expected: int(),
            given: float(),
        },
    );
}

#[test]
fn module_arity_error() {
    assert_module_error!(
        "external fn go(List(a, b)) -> a = \"\" \"\"",
        Error::IncorrectTypeArity {
            location: SrcSpan { start: 15, end: 25 },
            name: "List".to_string(),
            expected: 1,
            given: 2,
        }
    );
}

#[test]
fn module_private_type_leak() {
    assert_module_error!(
        r#"external type PrivateType
           pub external fn leak_type() -> PrivateType = "" """#,
        Error::PrivateTypeLeak {
            location: SrcSpan { start: 37, end: 87 },
            leaked: Type::App {
                args: vec![],
                public: false,
                module: vec!["my_module".to_string()],
                name: "PrivateType".to_string(),
            },
        }
    );

    assert_module_error!(
        r#"external type PrivateType
           external fn go() -> PrivateType = "" ""
           pub fn leak_type() { go() }"#,
        Error::PrivateTypeLeak {
            location: SrcSpan {
                start: 88,
                end: 106,
            },
            leaked: Type::App {
                args: vec![],
                public: false,
                module: vec!["my_module".to_string()],
                name: "PrivateType".to_string(),
            },
        }
    );

    assert_module_error!(
        r#"external type PrivateType
           external fn go() -> PrivateType = "" ""
           pub fn leak_type() { [go()] }"#,
        Error::PrivateTypeLeak {
            location: SrcSpan {
                start: 88,
                end: 106,
            },
            leaked: Type::App {
                args: vec![],
                public: false,
                module: vec!["my_module".to_string()],
                name: "PrivateType".to_string(),
            },
        }
    );

    assert_module_error!(
        r#"external type PrivateType
                    pub external fn go(PrivateType) -> Int = "" """#,
        Error::PrivateTypeLeak {
            location: SrcSpan { start: 46, end: 92 },
            leaked: Type::App {
                args: vec![],
                public: false,
                module: vec!["my_module".to_string()],
                name: "PrivateType".to_string(),
            },
        }
    );

    assert_module_error!(
        r#"external type PrivateType
           pub type LeakType { Variant(PrivateType) }"#,
        Error::PrivateTypeLeak {
            location: SrcSpan { start: 57, end: 77 },
            leaked: Type::App {
                args: vec![],
                public: false,
                module: vec!["my_module".to_string()],
                name: "PrivateType".to_string(),
            },
        }
    );
}

#[test]
fn module_label_errors() {
    assert_module_error!(
        r#"fn id(x) { x } fn y() { id(x: 4) }"#,
        Error::UnexpectedLabelledArg {
            label: "x".to_string(),
            location: SrcSpan { start: 27, end: 31 },
        }
    );

    assert_module_error!(
        r#"type X { X(a: Int, b: Int, c: Int) }
                    fn x() { X(b: 1, a: 1, 1) }"#,
        Error::PositionalArgumentAfterLabelled {
            location: SrcSpan { start: 80, end: 81 },
        }
    );
}

#[test]
fn unknown_type() {
    assert_module_error!(
        r#"type Thing { Thing(unknown: x) }"#,
        Error::UnknownType {
            location: SrcSpan { start: 28, end: 29 },
            name: "x".to_string(),
            types: env_types_with(&["Thing"]),
        }
    );

    // We cannot refer to unknown types in an alias
    assert_module_error!(
        "type IntMap = IllMap(Int, Int)",
        Error::UnknownType {
            location: SrcSpan { start: 14, end: 30 },
            name: "IllMap".to_string(),
            types: env_types(),
        }
    );

    // We cannot refer to unknown types in an alias
    assert_module_error!(
        "type IntMap = Map(Inf, Int)",
        Error::UnknownType {
            location: SrcSpan { start: 18, end: 21 },
            name: "Inf".to_string(),
            types: env_types(),
        }
    );

    // We cannot use undeclared type vars in a type alias
    assert_module_error!(
        "type X = List(a)",
        Error::UnknownType {
            location: SrcSpan { start: 14, end: 15 },
            name: "a".to_string(),
            types: env_types(),
        }
    );
}

#[test]
fn module_non_local_gaurd_var() {
    assert_module_error!(
        r#"fn one() { 1 }
           fn main() { case 1 { _ if one -> 1 } }"#,
        Error::NonLocalClauseGuardVariable {
            location: SrcSpan { start: 52, end: 55 },
            name: "one".to_string(),
        }
    );

    // An unknown field should report the possible fields' labels
    assert_module_error!(
        "
pub type Box(a) { Box(inner: a) }
pub fn main(box: Box(Int)) { box.unknown }
",
        Error::UnknownField {
            location: SrcSpan { start: 67, end: 75 },
            label: "unknown".to_string(),
            fields: vec!["inner".to_string()],
            typ: Arc::new(Type::App {
                args: vec![int()],
                public: true,
                module: vec!["my_module".to_string()],
                name: "Box".to_string(),
            }),
        },
    );

    // An unknown field should report the possible fields' labels
    assert_module_error!(
        "
pub type Box(a) { Box(inner: a) }
pub fn main(box: Box(Box(Int))) { box.inner.unknown }
    ",
        Error::UnknownField {
            location: SrcSpan { start: 78, end: 86 },
            label: "unknown".to_string(),
            fields: vec!["inner".to_string()],
            typ: Arc::new(Type::Var {
                type_: Arc::new(RefCell::new(TypeVar::Link {
                    type_: Arc::new(Type::App {
                        args: vec![int()],
                        public: true,
                        module: vec!["my_module".to_string()],
                        name: "Box".to_string(),
                    }),
                })),
            }),
        },
    );

    assert_module_error!(
        "
type Triple {
    Triple(a: Int, b: Int, c: Int)
}

fn main() {
  let triple = Triple(1,2,3)
  let Triple(a, b, c, ..) = triple
  a
}",
        Error::UnnecessarySpreadOperator {
            location: SrcSpan {
                start: 116,
                end: 118
            },
            arity: 3
        }
    );

    // Duplicate var in record
    assert_module_error!(
        r#"type X { X(a: Int, b: Int, c: Int) }
                    fn x() {
                        case X(1,2,3) { X(x, y, x) -> 1 }
                    }"#,
        Error::DuplicateVarInPattern {
            location: SrcSpan {
                start: 114,
                end: 115
            },
            name: "x".to_string()
        },
    );

    // Constructor in guard clause errors

    assert_module_error!(
        r#"type X { X(a: Int, b: Float) }
                    fn x() {
                        case X(1, 2.0) { x if x == X(1) -> 1 }
                    }"#,
        Error::IncorrectArity {
            labels: vec!["a".to_string(), "b".to_string()],
            location: SrcSpan {
                start: 111,
                end: 115
            },
            expected: 2,
            given: 1,
        },
    );

    assert_module_error!(
        r#"type X { X(a: Int, b: Float) }
           fn x() { case X(1, 2.0) { x if x == X(2.0, 1) -> 1 } }"#,
        Error::CouldNotUnify {
            situation: None,
            location: SrcSpan { start: 80, end: 83 },
            expected: Arc::new(Type::App {
                public: true,
                module: vec![],
                name: "Int".to_string(),
                args: vec![],
            }),
            given: Arc::new(Type::App {
                public: true,
                module: vec![],
                name: "Float".to_string(),
                args: vec![],
            }),
        },
    );

    // Type variables are shared between function annotations and let annotations within their body
    assert_module_error!(
        "
        pub type Box(a) {
            Box(value: a)
        }
        pub fn go(box1: Box(a), box2: Box(b)) {
            let _: Box(a) = box2
            let _: Box(b) = box1
            5
        }",
        Error::CouldNotUnify {
            situation: None,
            location: SrcSpan {
                start: 139,
                end: 143
            },
            expected: Arc::new(Type::App {
                public: true,
                module: vec!["my_module".to_string()],
                name: "Box".to_string(),
                args: vec![Arc::new(Type::Var {
                    type_: Arc::new(RefCell::new(TypeVar::Generic { id: 8 })),
                })]
            }),
            given: Arc::new(Type::App {
                public: true,
                module: vec!["my_module".to_string()],
                name: "Box".to_string(),
                args: vec![Arc::new(Type::Var {
                    type_: Arc::new(RefCell::new(TypeVar::Generic { id: 10 })),
                })]
            }),
        },
    );
}

#[test]
fn duplicate_functions_test() {
    // We cannot declare two functions with the same name in a module
    assert_module_error!(
        "fn dupe() { 1 }
         fn dupe() { 2 }",
        Error::DuplicateName {
            location: SrcSpan { start: 25, end: 34 },
            previous_location: SrcSpan { start: 0, end: 9 },
            name: "dupe".to_string(),
        }
    );

    // Different types to force a unify error if we don't detect the
    // duplicate during refactoring.
    assert_module_error!(
        "fn dupe() { 1 }
         fn dupe() { 2.0 }",
        Error::DuplicateName {
            location: SrcSpan { start: 25, end: 34 },
            previous_location: SrcSpan { start: 0, end: 9 },
            name: "dupe".to_string(),
        }
    );

    assert_module_error!(
        "fn dupe() { 1 }
         fn dupe(x) { x }",
        Error::DuplicateName {
            location: SrcSpan { start: 25, end: 35 },
            previous_location: SrcSpan { start: 0, end: 9 },
            name: "dupe".to_string(),
        }
    );

    assert_module_error!(
        "fn dupe() { 1 }
         external fn dupe(x) -> x = \"\" \"\"",
        Error::DuplicateName {
            location: SrcSpan { start: 25, end: 57 },
            previous_location: SrcSpan { start: 0, end: 9 },
            name: "dupe".to_string(),
        }
    );

    assert_module_error!(
        "external fn dupe(x) -> x = \"\" \"\"
         fn dupe() { 1 }",
        Error::DuplicateName {
            location: SrcSpan { start: 42, end: 51 },
            previous_location: SrcSpan { start: 0, end: 32 },
            name: "dupe".to_string(),
        }
    );
}

#[test]
fn duplicate_constructors() {
    // We cannot declare two type constructors with the same name in a module
    assert_module_error!(
        "type Box { Box(x: Int) }
         type Boxy { Box(Int) }",
        Error::DuplicateName {
            location: SrcSpan { start: 46, end: 54 },
            previous_location: SrcSpan { start: 11, end: 22 },
            name: "Box".to_string(),
        }
    );

    // We cannot declare two type constructors with the same name in a module
    assert_module_error!(
        "type Boxy { Box(Int) }
         type Box { Box(x: Int) }",
        Error::DuplicateName {
            location: SrcSpan { start: 43, end: 54 },
            previous_location: SrcSpan { start: 12, end: 20 },
            name: "Box".to_string(),
        }
    );

    // We cannot declare two type constructors with the same name in a module
    assert_module_error!(
        "type Boxy { Box(Int) Box(Float) }",
        Error::DuplicateName {
            location: SrcSpan { start: 21, end: 31 },
            previous_location: SrcSpan { start: 12, end: 20 },
            name: "Box".to_string(),
        }
    );
}

#[test]
fn duplicate_type_names() {
    // We cannot reuse an alias name in the same module
    assert_module_error!(
        "type X = Int type X = Int",
        Error::DuplicateTypeName {
            location: SrcSpan { start: 13, end: 25 },
            previous_location: SrcSpan { start: 0, end: 12 },
            name: "X".to_string(),
        }
    );

    // We cannot declare two types with the same name in a module
    assert_module_error!(
        "type DupType { A }
         type DupType { B }",
        Error::DuplicateTypeName {
            location: SrcSpan { start: 28, end: 40 },
            previous_location: SrcSpan { start: 0, end: 12 },
            name: "DupType".to_string(),
        }
    );
}

#[test]
fn correct_pipe_arity_error_location() {
    // https://github.com/gleam-lang/gleam/issues/672
    assert_module_error!(
        "fn x(x, y) { x }
         fn main() { 1 |> x() }",
        Error::IncorrectArity {
            labels: vec![],
            location: SrcSpan { start: 43, end: 46 },
            expected: 2,
            given: 0,
        },
    );
}

#[test]
fn module_constants() {
    assert_module_error!(
        "pub const group_id: Int = \"42\"",
        Error::CouldNotUnify {
            situation: None,
            location: SrcSpan { start: 26, end: 30 },
            expected: int(),
            given: string(),
        }
    );

    assert_module_error!(
        "pub const numbers: List(Int) = [1, 2, 2.3]",
        Error::CouldNotUnify {
            situation: None,
            location: SrcSpan { start: 38, end: 41 },
            expected: int(),
            given: float(),
        }
    );

    assert_module_error!(
        "pub const numbers: List(Int) = [1.1, 2.2, 3.3]",
        Error::CouldNotUnify {
            situation: None,
            location: SrcSpan { start: 31, end: 46 },
            expected: list(Arc::new(Type::Var {
                type_: Arc::new(RefCell::new(TypeVar::Link { type_: int() }))
            })),
            given: list(Arc::new(Type::Var {
                type_: Arc::new(RefCell::new(TypeVar::Link { type_: float() }))
            }))
        }
    );

    assert_module_error!(
        "pub const pair: tuple(Int, Float) = tuple(4.1, 1)",
        Error::CouldNotUnify {
            situation: None,
            location: SrcSpan { start: 36, end: 49 },
            expected: tuple(vec![int(), float()]),
            given: tuple(vec![float(), int()]),
        }
    );

    assert_module_error!(
        "const pair = tuple(1, 2.0)
         fn main() { 1 == pair }",
        Error::CouldNotUnify {
            situation: None,
            location: SrcSpan { start: 53, end: 57 },
            expected: int(),
            given: tuple(vec![int(), float()]),
        },
    );

    assert_module_error!(
        "const pair = [1, 1.0]",
        Error::CouldNotUnify {
            situation: None,
            location: SrcSpan { start: 17, end: 20 },
            expected: int(),
            given: float(),
        },
    );

    assert_module_infer!(
        "
    pub const test_int1 = 123
    pub const test_int2: Int = 321
    pub const test_int3 = 0xE
    pub const test_int4 = 0o10
    pub const test_int5 = 0o10011
    pub const test_float: Float = 4.2
    pub const test_string = \"hey!\"
    pub const test_list = [1,2,3]
    pub const test_tuple = tuple(\"yes!\", 42)",
        vec![
            ("test_float", "Float"),
            ("test_int1", "Int"),
            ("test_int2", "Int"),
            ("test_int3", "Int"),
            ("test_int4", "Int"),
            ("test_int5", "Int"),
            ("test_list", "List(Int)"),
            ("test_string", "String"),
            ("test_tuple", "#(String, Int)"),
        ],
    );
}

#[test]
fn custom_type_module_constants() {
    assert_module_infer!(
        "pub type Test { A }
        pub const test = A",
        vec![("A", "Test"), ("test", "Test")],
    );

    assert_module_error!(
        r#"type X { X }
        const x = unknown.X"#,
        sort_options(Error::UnknownModule {
            location: SrcSpan { start: 31, end: 40 },
            name: "unknown".to_string(),
            imported_modules: vec![],
        })
    );
}

#[test]
fn unknown_label() {
    assert_module_error!(
        r#"type X { X(a: Int, b: Float) }
fn x() {
    let x = X(a: 1, c: 2.0)
    x
}"#,
        sort_options(Error::UnknownLabels {
            unknown: vec![("c".to_string(), SrcSpan { start: 60, end: 66 })],
            valid: vec!["a".to_string(), "b".to_string()],
            supplied: vec!["a".to_string()],
        })
    );
}

#[test]
fn module_update() {
    // A variable of the wrong type given to a record update
    assert_module_error!(
        "
        pub type Person {
            Person(name: String, age: Int)
        };
        pub type Box(a) {
            Box(a)
        };
        pub fn update_person(person: Person, box: Box(a)) {
            Person(..box)
        }",
        Error::CouldNotUnify {
            situation: None,
            location: SrcSpan {
                start: 216,
                end: 221
            },
            expected: Arc::new(Type::App {
                public: true,
                module: vec!["my_module".to_string()],
                name: "Person".to_string(),
                args: vec![]
            }),
            given: Arc::new(Type::App {
                public: true,
                module: vec!["my_module".to_string()],
                name: "Box".to_string(),
                args: vec![Arc::new(Type::Var {
                    type_: Arc::new(RefCell::new(TypeVar::Generic { id: 8 })),
                })]
            }),
        },
    );

    // An undefined variable given to a record update
    assert_module_error!(
        "
        pub type Person {
            Person(name: String, age: Int)
        };
        pub fn update_person() {
            Person(..person)
        }",
        Error::UnknownVariable {
            location: SrcSpan {
                start: 133,
                end: 141
            },
            name: "person".to_string(),
            variables: env_vars_with(&["Person", "update_person"]),
        },
    );

    // An unknown field given to a record update
    assert_module_error!(
        "
        pub type Person {
            Person(name: String)
        };
        pub fn update_person(person: Person) {
            Person(..person, foo: 5)
        }",
        Error::UnknownField {
            location: SrcSpan {
                start: 145,
                end: 153
            },
            label: "foo".to_string(),
            fields: vec!["name".to_string()],
            typ: Arc::new(Type::App {
                args: vec![],
                public: true,
                module: vec!["my_module".to_string()],
                name: "Person".to_string(),
            })
        },
    );

    // An unknown record constructor being used in a record update
    assert_module_error!(
        "
        pub type Person {
            Person(name: String, age: Int)
        };
        pub fn update_person(person: Person) {
            NotAPerson(..person)
        }",
        Error::UnknownVariable {
            location: SrcSpan {
                start: 140,
                end: 150
            },
            name: "NotAPerson".to_string(),
            variables: env_vars_with(&["Person", "update_person", "person"]),
        },
    );

    // Something other than a record constructor being used in a record update
    assert_module_error!(
        "
        pub type Person {
            Person(name: String, age: Int)
        };
        pub fn identity(a) { a }
        pub fn update_person(person: Person) {
            identity(..person)
        }",
        Error::RecordUpdateInvalidConstructor {
            location: SrcSpan {
                start: 173,
                end: 181,
            },
        },
    );

    // A record update with a constructor returned from an expression
    assert_module_error!(
        "
        pub type Person {
            Person(name: String, age: Int)
        };
        pub fn update_person(person: Person) {
            let constructor = Person
            constructor(..person)
        }",
        Error::RecordUpdateInvalidConstructor {
            location: SrcSpan {
                start: 177,
                end: 188,
            },
        },
    );

    // A record update on polymorphic types with a field of the wrong type
    assert_module_error!(
        "
        pub type Box(a) {
            Box(value: a, i: Int)
        };
        pub fn update_box(box: Box(Int), value: String) {
            Box(..box, value: value)
        };",
        Error::CouldNotUnify {
            situation: None,
            location: SrcSpan {
                start: 160,
                end: 165,
            },
            expected: Arc::new(Type::App {
                args: vec![],
                public: true,
                module: vec![],
                name: "Int".to_string(),
            }),
            given: Arc::new(Type::App {
                args: vec![],
                public: true,
                module: vec![],
                name: "String".to_string(),
            })
        },
    );

    // A record update on polymorphic types with generic fields of the wrong type
    assert_module_error!(
        "
        pub type Box(a) {
            Box(value: a, i: Int)
        };
        pub fn update_box(box: Box(a), value: b) {
            Box(..box, value: value)
        };",
        Error::CouldNotUnify {
            situation: None,
            location: SrcSpan {
                start: 153,
                end: 158,
            },
            expected: Arc::new(Type::Var {
                type_: Arc::new(RefCell::new(TypeVar::Generic { id: 8 })),
            }),
            given: Arc::new(Type::Var {
                type_: Arc::new(RefCell::new(TypeVar::Generic { id: 10 })),
            }),
        },
    );
}

#[test]
fn type_vars_must_be_declared() {
    // https://github.com/gleam-lang/gleam/issues/734
    assert_module_error!(
        r#"type A(a) { A };
           type B = a"#,
        sort_options(Error::UnknownType {
            location: SrcSpan { start: 37, end: 38 },
            name: "a".to_string(),
            types: env_types_with(&["A"]),
        })
    );
}

#[test]
fn type_holes() {
    // Type holes cannot be used when decaring types or external functions
    assert_module_error!(
        r#"type A { A(_) };"#,
        Error::UnexpectedTypeHole {
            location: SrcSpan { start: 11, end: 12 },
        },
    );

    assert_module_error!(
        r#"external fn main() -> List(_) = "" """#,
        Error::UnexpectedTypeHole {
            location: SrcSpan { start: 27, end: 28 },
        },
    );

    assert_module_error!(
        r#"external fn main(List(_)) -> Nil = "" """#,
        Error::UnexpectedTypeHole {
            location: SrcSpan { start: 22, end: 23 },
        },
    );

    assert_module_error!(
        r#"type X = List(_)"#,
        Error::UnexpectedTypeHole {
            location: SrcSpan { start: 14, end: 15 },
        },
    );
}

#[test]
fn todo_warning_test() {
    assert_warning!(
        "fn main() { 1 == todo }",
        Warning::Todo {
            location: SrcSpan { start: 17, end: 21 },
            typ: Arc::new(Type::Var {
                type_: Arc::new(RefCell::new(TypeVar::Link { type_: int() })),
            }),
        },
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

    // Explicitly discarded Results do not emit warnings
    assert_no_warnings!(
        "
pub fn foo() { Ok(5) }
pub fn main() { let _ = foo(); 5 }",
    );
}

#[test]
fn unused_literal_warning_test() {
    // Test int
    assert_warning!(
        "fn main() { 1; 2 }",
        Warning::UnusedLiteral {
            location: SrcSpan { start: 12, end: 13 }
        }
    );
    // Test float
    assert_warning!(
        "fn main() { 1.0; 2 }",
        Warning::UnusedLiteral {
            location: SrcSpan { start: 12, end: 15 }
        }
    );
    // Test string
    assert_warning!(
        "
    fn main() { 
        \"1\"; 2 
    }",
        Warning::UnusedLiteral {
            location: SrcSpan { start: 26, end: 29 }
        }
    );
    // Test bit string
    assert_warning!(
        "
    fn main() { 
        <<3>>; 2 
    }",
        Warning::UnusedLiteral {
            location: SrcSpan { start: 26, end: 31 }
        }
    );
    // Test tuple
    assert_warning!(
        "
    fn main() { 
        tuple(1.0, \"Hello world\"); 2 
    }",
        Warning::UnusedLiteral {
            location: SrcSpan { start: 26, end: 51 }
        }
    );
    // Test list
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
    assert_no_warnings!("pub external type Y");

    // Type alias
    assert_warning!(
        "type X = Int",
        Warning::UnusedType {
            name: "X".to_string(),
            location: SrcSpan { start: 0, end: 12 },
            imported: false
        }
    );

    assert_no_warnings!("pub type Y = Int");
    assert_no_warnings!("type Y = Int pub fn run(x: Y) { x }");

    // Custom type
    assert_warning!(
        "type X { X }",
        Warning::UnusedConstructor {
            name: "X".to_string(),
            location: SrcSpan { start: 9, end: 10 },
            imported: false
        }
    );
    assert_no_warnings!("pub type X { X }");
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

    assert_no_warnings!("pub fn a() { 1 }");
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

    assert_no_warnings!("pub const a = 1");
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

    assert_no_warnings!("pub fn a(b) { b }");

    // Simple let
    assert_warning!(
        "pub fn a() { let b = 1 5 }",
        Warning::UnusedVariable {
            name: "b".to_string(),
            location: SrcSpan { start: 17, end: 18 },
        }
    );

    assert_no_warnings!("pub fn a() { let b = 1 b }");

    // Shadowing let
    assert_warning!(
        "pub fn a() { let b = 1 let b = 2 b }",
        Warning::UnusedVariable {
            name: "b".to_string(),
            location: SrcSpan { start: 17, end: 18 },
        }
    );

    assert_no_warnings!("pub fn a() { let b = 1 let b = b + 1 b }");

    // Destructure
    assert_warning!(
        "pub fn a(b) { case b { tuple(c, _) -> 5 } }",
        Warning::UnusedVariable {
            name: "c".to_string(),
            location: SrcSpan { start: 29, end: 30 },
        }
    );

    assert_no_warnings!("pub fn a(b) { case b { tuple(c, _) -> c } }");
}

#[test]
fn functions_used_before_definition() {
    assert_module_infer!(
        "pub fn a() { b() }
         pub fn b() { 1 }",
        vec![("a", "fn() -> Int"), ("b", "fn() -> Int")],
    );

    assert_module_infer!(
        "pub fn a() { b() + c() }
         fn b() { 1 }
         fn c() { 1 }",
        vec![("a", "fn() -> Int")],
    );

    assert_module_infer!(
        "fn b() { 1 }
         pub fn a() { b() + c() }
         fn c() { 1 }",
        vec![("a", "fn() -> Int")],
    );

    assert_module_infer!(
        "pub fn a() { Thing }
         pub type Thing { Thing }",
        vec![("Thing", "Thing"), ("a", "fn() -> Thing"),],
    );

    assert_module_infer!(
        "pub fn a() { b()  }
         external fn b() -> Int = \"\" \"\"",
        vec![("a", "fn() -> Int")],
    );
}

#[test]
fn types_used_before_definition() {
    assert_module_infer!(
        "pub type Y { Y(X) }
         pub external type X",
        vec![("Y", "fn(X) -> Y")],
    );

    assert_module_infer!(
        "pub type Y { Y(x: X) }
         pub external type X",
        vec![("Y", "fn(X) -> Y")],
    );
}

#[test]
fn consts_used_before_definition() {
    assert_module_infer!(
        "pub fn a() { b }
        const b = 1",
        vec![("a", "fn() -> Int")],
    );
}

#[test]
fn mutual_recursion() {
    assert_module_infer!(
        "pub fn a() { b() }
         fn b() { a() }",
        vec![("a", "fn() -> a")],
    );

    assert_module_infer!(
        "pub fn a() { b() }
         fn b() { a() + 1 }",
        vec![("a", "fn() -> Int")],
    );
}

#[test]
fn type_annotations() {
    // OK!

    assert_module_infer!(
        "pub type Box(x) { Box(label: String, contents: x) }
         pub fn id(x: Box(y)) { x }",
        vec![
            ("Box", "fn(String, a) -> Box(a)"),
            ("id", "fn(Box(a)) -> Box(a)"),
        ],
    );

    assert_module_infer!("pub fn go(x: Int) { x }", vec![("go", "fn(Int) -> Int")],);
    assert_module_infer!("pub fn go(x: b) -> b { x }", vec![("go", "fn(a) -> a")],);
    assert_module_infer!("pub fn go(x) -> b { x }", vec![("go", "fn(a) -> a")],);
    assert_module_infer!("pub fn go(x: b) { x }", vec![("go", "fn(a) -> a")],);
    assert_module_infer!(
        "pub fn go(x: List(b)) -> List(b) { x }",
        vec![("go", "fn(List(a)) -> List(a)")],
    );
    assert_module_infer!(
        "pub fn go(x: List(b)) { x }",
        vec![("go", "fn(List(a)) -> List(a)")],
    );
    assert_module_infer!(
        "pub fn go(x: List(String)) { x }",
        vec![("go", "fn(List(String)) -> List(String)")],
    );
    assert_module_infer!("pub fn go(x: b, y: c) { x }", vec![("go", "fn(a, b) -> a")],);
    assert_module_infer!("pub fn go(x) -> Int { x }", vec![("go", "fn(Int) -> Int")],);

    assert_module_infer!(
        "pub fn id(x: x) { x }
         pub fn float() { id(1.0) }
         pub fn int() { id(1) }",
        vec![
            ("float", "fn() -> Float"),
            ("id", "fn(a) -> a"),
            ("int", "fn() -> Int"),
        ],
    );

    // Errors

    assert_module_error!("fn inc(x: a) { x + 1 }");
}

#[test]
fn early_function_generalisation() {
    assert_module_infer!(
        "pub fn id(x) { x }
         pub fn int() { id(1) }",
        vec![("id", "fn(a) -> a"), ("int", "fn() -> Int")],
    );

    assert_module_infer!(
        "pub fn id(x) { x }
         pub fn int() { id(1) }
         pub fn float() { id(1.0) }
         ",
        vec![
            ("float", "fn() -> Float"),
            ("id", "fn(a) -> a"),
            ("int", "fn() -> Int"),
        ],
    );
}

// https://github.com/gleam-lang/gleam/issues/892
#[test]
fn case_clause_pipe_diagnostic() {
    assert_module_error!(
        r#"
pub fn change(x: String) -> String {
    ""
}

pub fn parse(input: BitString) -> String {
  case input {
    <<>> -> 1
    <<"(":utf8, b:binary>> ->
      parse(input)
      |> change
  }
}"#,
        Error::CouldNotUnify {
            situation: Some(UnifyErrorSituation::CaseClauseMismatch),
            location: SrcSpan {
                start: 124,
                end: 184,
            },
            expected: int(),
            given: string(),
        }
    );
}

// https://github.com/gleam-lang/gleam/issues/970
#[test]
fn bitstring_pattern_unification() {
    assert_module_infer!(
        "pub fn m(x) { case x { <<>> -> Nil _ -> Nil} }",
        vec![("m", "fn(BitString) -> Nil")],
    );

    assert_module_infer!(
        "pub fn m(x) { case x { <<>> -> Nil _ -> Nil} }",
        vec![("m", "fn(BitString) -> Nil")],
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

// https://github.com/gleam-lang/gleam/issues/983
#[test]
fn qualified_prelude() {
    assert_module_infer!(
        "import gleam
pub fn a() {
  gleam.Ok(1)
}",
        vec![("a", "fn() -> Result(Int, a)")],
    );
}

// https://github.com/gleam-lang/gleam/issues/1029
#[test]
fn empty_list_const() {
    assert_module_infer!(
        "pub const empty = []
pub fn a() {
    empty
}",
        vec![("a", "fn() -> List(a)"), ("empty", "List(a)")],
    );
}

#[test]
fn let_as_expression() {
    assert_infer!("let x = 1", "Int");
    assert_infer!("let x = let x = 1", "Int");
    assert_infer!("let x = { let x = 1. }", "Float");
}

#[test]
fn assert_as_expression() {
    assert_infer!("assert x = 1", "Int");
    assert_infer!("assert x = assert x = 1", "Int");
    assert_infer!("assert x = { assert x = 1. }", "Float");
    assert_infer!("assert 1 = 1", "Int");
}

// https://github.com/gleam-lang/gleam/issues/1087
#[test]
fn generic_inner_access() {
    assert_module_infer!(
        "pub type B(b) { B(value: b) }
pub fn b_get_first(b: B(#(a))) {
  b.value.0
}",
        vec![("B", "fn(a) -> B(a)"), ("b_get_first", "fn(B(#(a))) -> a")],
    );
}

#[test]
fn module_name_validation() {
    assert!(validate_module_name(&["dream".to_string()]).is_ok());

    assert!(validate_module_name(&["gleam".to_string()]).is_err());

    assert!(validate_module_name(&["gleam".to_string(), "ok".to_string()]).is_ok());

    assert!(validate_module_name(&["ok".to_string(), "gleam".to_string()]).is_ok());

    assert!(validate_module_name(&["external".to_string()]).is_err());

    assert!(validate_module_name(&["type".to_string()]).is_err());

    assert!(validate_module_name(&["pub".to_string()]).is_err());

    assert!(validate_module_name(&["ok".to_string(), "external".to_string()]).is_err());

    assert!(validate_module_name(&["ok".to_string(), "type".to_string()]).is_err());

    assert!(validate_module_name(&["ok".to_string(), "pub".to_string()]).is_err());
}
