use super::*;

#[test]
fn field_map_reorder_test() {
    let int = |value| Expr::Int {
        value,
        typ: (),
        meta: Meta { start: 0, end: 0 },
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
            let meta = &Meta { start: 0, end: 0 };
            assert_eq!(self.expected_result, fm.reorder(&mut args, meta));
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
                meta: Default::default(),
                label: None,
                value: int(1),
            },
            CallArg {
                meta: Default::default(),
                label: None,
                value: int(2),
            },
            CallArg {
                meta: Default::default(),
                label: None,
                value: int(3),
            },
        ],
        expected_result: Ok(()),
        expected_args: vec![
            CallArg {
                meta: Default::default(),
                label: None,
                value: int(1),
            },
            CallArg {
                meta: Default::default(),
                label: None,
                value: int(2),
            },
            CallArg {
                meta: Default::default(),
                label: None,
                value: int(3),
            },
        ],
    }
    .test();

    Case {
        arity: 3,
        fields: [("last".to_string(), 2)].iter().cloned().collect(),
        args: vec![
            CallArg {
                meta: Default::default(),
                label: None,
                value: int(1),
            },
            CallArg {
                meta: Default::default(),
                label: Some("last".to_string()),
                value: int(2),
            },
            CallArg {
                meta: Default::default(),
                label: None,
                value: int(3),
            },
        ],
        expected_result: Ok(()),
        expected_args: vec![
            CallArg {
                meta: Default::default(),
                label: None,
                value: int(1),
            },
            CallArg {
                meta: Default::default(),
                label: None,
                value: int(3),
            },
            CallArg {
                meta: Default::default(),
                label: Some("last".to_string()),
                value: int(2),
            },
        ],
    }
    .test();

    Case {
        arity: 3,
        fields: [("last".to_string(), 2)].iter().cloned().collect(),
        args: vec![
            CallArg {
                meta: Default::default(),
                label: None,
                value: int(1),
            },
            CallArg {
                meta: Default::default(),
                label: None,
                value: int(2),
            },
            CallArg {
                meta: Default::default(),
                label: Some("last".to_string()),
                value: int(3),
            },
        ],
        expected_result: Ok(()),
        expected_args: vec![
            CallArg {
                meta: Default::default(),
                label: None,
                value: int(1),
            },
            CallArg {
                meta: Default::default(),
                label: None,
                value: int(2),
            },
            CallArg {
                meta: Default::default(),
                label: Some("last".to_string()),
                value: int(3),
            },
        ],
    }
    .test();
}

#[test]
fn infer_module_type_retention_test() {
    let module: UntypedModule = crate::ast::Module {
        name: vec!["ok".to_string()],
        statements: vec![],
        type_info: (),
    };

    let module = infer_module(module, &HashMap::new()).expect("Should infer OK");

    assert_eq!(
        module.type_info,
        Module {
            name: vec!["ok".to_string()],
            types: HashMap::new(), // Core type constructors like String and Int are not included
            values: HashMap::new(),
        }
    );
}

#[test]
fn infer_test() {
    macro_rules! assert_infer {
        ($src:expr, $typ:expr $(,)?) => {
            let mut printer = pretty::Printer::new();
            let ast = crate::grammar::ExprParser::new()
                .parse($src)
                .expect("syntax error");
            let result =
                infer(ast, 1, &mut Env::new(&HashMap::new())).expect("should successfully infer");
            assert_eq!(
                ($src, printer.pretty_print(result.typ(), 0),),
                ($src, $typ.to_string()),
            );
        };
    }

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

    // let
    assert_infer!("let x = 1 2", "Int");
    assert_infer!("let x = 1 x", "Int");
    assert_infer!("let x = 2.0 x", "Float");
    assert_infer!("let x = 2 let y = x y", "Int");
    assert_infer!(
        "let tuple(tuple(_, _) as x, _) = tuple(tuple(0, 1.0), []) x",
        "tuple(Int, Float)"
    );

    // list
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
    assert_infer!("[1 | [2 | []]]", "List(Int)");
    assert_infer!("[fn(x) { x } | []]", "List(fn(a) -> a)");
    assert_infer!("let f = fn(x) { x } [f, f]", "List(fn(a) -> a)");
    assert_infer!("let x = [1 | []] [2 | x]", "List(Int)");
    assert_infer!("[tuple([], [])]", "List(tuple(List(a), List(b)))");

    // anon structs
    assert_infer!("tuple(1)", "tuple(Int)");
    assert_infer!("tuple(1, 2.0)", "tuple(Int, Float)");
    assert_infer!("tuple(1, 2.0, 3)", "tuple(Int, Float, Int)");
    assert_infer!(
        "tuple(1, 2.0, tuple(1, 1))",
        "tuple(Int, Float, tuple(Int, Int))",
    );

    // fn
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
    assert_infer!("fn(x) { tuple(1, x) }", "fn(a) -> tuple(Int, a)");
    assert_infer!("fn(x, y) { tuple(x, y) }", "fn(a, b) -> tuple(a, b)");
    assert_infer!("fn(x) { tuple(x, x) }", "fn(a) -> tuple(a, a)");
    assert_infer!("fn(x) -> Int { x }", "fn(Int) -> Int");
    assert_infer!("fn(x) -> a { x }", "fn(a) -> a");
    assert_infer!("fn() -> Int { 2 }", "fn() -> Int");

    // case
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

    // let
    assert_infer!("let [] = [] 1", "Int");
    assert_infer!("let [a] = [1] a", "Int");
    assert_infer!("let [a, 2] = [1] a", "Int");
    assert_infer!("let [a | [b | []]] = [1] a", "Int");
    assert_infer!("fn(x) { let [a] = x a }", "fn(List(a)) -> a");
    assert_infer!("fn(x) { let [a] = x a + 1 }", "fn(List(Int)) -> Int");
    assert_infer!("let _x = 1 2.0", "Float");
    assert_infer!("let _ = 1 2.0", "Float");
    assert_infer!("let tuple(tag, x) = tuple(1.0, 1) x", "Int");
    assert_infer!("fn(x) { let tuple(a, b) = x a }", "fn(tuple(a, b)) -> a");
}

#[test]
fn infer_error_test() {
    macro_rules! assert_error {
        ($src:expr, $error:expr $(,)?) => {
            let ast = crate::grammar::ExprParser::new()
                .parse($src)
                .expect("syntax error");
            let result =
                infer(ast, 1, &mut Env::new(&HashMap::new())).expect_err("should infer an error");
            assert_eq!(($src, sort_options($error)), ($src, sort_options(result)));
        };
    }

    assert_error!(
        "1 + 1.0",
        Error::CouldNotUnify {
            meta: Meta { start: 4, end: 7 },
            expected: int(),
            given: float(),
        },
    );

    assert_error!(
        "1 +. 1.0",
        Error::CouldNotUnify {
            meta: Meta { start: 0, end: 1 },
            expected: float(),
            given: int(),
        },
    );

    assert_error!(
        "1 == 1.0",
        Error::CouldNotUnify {
            meta: Meta { start: 5, end: 8 },
            expected: int(),
            given: float(),
        },
    );

    assert_error!(
        "1 > 1.0",
        Error::CouldNotUnify {
            meta: Meta { start: 4, end: 7 },
            expected: int(),
            given: float(),
        },
    );

    assert_error!(
        "1.0 >. 1",
        Error::CouldNotUnify {
            meta: Meta { start: 7, end: 8 },
            expected: float(),
            given: int(),
        },
    );

    assert_error!(
        "x",
        Error::UnknownVariable {
            meta: Meta { start: 0, end: 1 },
            name: "x".to_string(),
            variables: env_vars(),
        },
    );

    assert_error!(
        "x",
        Error::UnknownVariable {
            meta: Meta { start: 0, end: 1 },
            name: "x".to_string(),
            variables: env_vars(),
        },
    );

    assert_error!(
        "let id = fn(x) { x } id()",
        Error::IncorrectArity {
            meta: Meta { start: 21, end: 25 },
            expected: 1,
            given: 0,
        },
    );

    assert_error!(
        "let id = fn(x) { x } id(1, 2)",
        Error::IncorrectArity {
            meta: Meta { start: 21, end: 29 },
            expected: 1,
            given: 2,
        },
    );

    assert_error!(
        "case 1 { a -> 1 b -> 2.0 }",
        Error::CouldNotUnify {
            meta: Meta { start: 21, end: 24 },
            expected: int(),
            given: float(),
        },
    );

    assert_error!(
        "case 1.0 { 1 -> 1 }",
        Error::CouldNotUnify {
            meta: Meta { start: 11, end: 12 },
            expected: float(),
            given: int(),
        },
    );

    assert_error!(
        "case 1 { 1.0 -> 1 }",
        Error::CouldNotUnify {
            meta: Meta { start: 9, end: 12 },
            expected: int(),
            given: float(),
        },
    );

    assert_error!(
        "case 1, 2.0 { a, b -> a + b }",
        Error::CouldNotUnify {
            meta: Meta { start: 26, end: 27 },
            expected: int(),
            given: float(),
        },
    );

    assert_error!(
        "case 1, 2.0 { a, b -> a 1, 2 -> 0 }",
        Error::CouldNotUnify {
            meta: Meta { start: 27, end: 28 },
            expected: float(),
            given: int(),
        },
    );

    assert_error!(
        "fn() { 1 } == fn(x) { x + 1 }",
        Error::CouldNotUnify {
            meta: Meta { start: 14, end: 29 },
            expected: Type::Fn {
                args: vec![],
                retrn: Box::new(int()),
            },
            given: Type::Fn {
                args: vec![Type::Var {
                    typ: Rc::new(RefCell::new(TypeVar::Link {
                        typ: Box::new(int()),
                    })),
                }],
                retrn: Box::new(int()),
            },
        },
    );

    assert_error!(
        "let f = fn(x: Int) { x } f(1.0)",
        Error::CouldNotUnify {
            meta: Meta { start: 27, end: 30 },
            expected: Type::App {
                public: true,
                module: vec![],
                name: "Int".to_string(),
                args: vec![],
            },
            given: Type::App {
                public: true,
                module: vec![],
                name: "Float".to_string(),
                args: vec![],
            },
        },
    );

    assert_error!(
        "case 1 { x -> 1 1 -> x }",
        Error::UnknownVariable {
            meta: Meta { start: 21, end: 22 },
            name: "x".to_string(),
            variables: env_vars(),
        },
    );

    assert_error!(
        "case 1 { _, _ -> 1 }",
        Error::IncorrectNumClausePatterns {
            meta: Meta { start: 9, end: 18 },
            expected: 1,
            given: 2,
        },
    );

    assert_error!(
        "let id = fn(x) { x(x) } 1",
        Error::RecursiveType {
            meta: Meta { start: 19, end: 20 },
        },
    );

    assert_error!(
        "let True(x) = 1 x",
        Error::IncorrectArity {
            meta: Meta { start: 4, end: 11 },
            expected: 0,
            given: 1,
        },
    );

    assert_error!(
        "let Ok(1, x) = 1 x",
        Error::IncorrectArity {
            meta: Meta { start: 4, end: 12 },
            expected: 1,
            given: 2,
        },
    );

    assert_error!(
        "let x = 1 x.whatever",
        Error::NotModule {
            meta: Meta { start: 10, end: 11 },
            typ: int(),
        },
    );

    assert_error!(
        "tuple(1, 2) == tuple(1, 2, 3)",
        Error::CouldNotUnify {
            meta: Meta { start: 15, end: 29 },
            expected: Type::Tuple {
                elems: vec![int(), int()],
            },
            given: Type::Tuple {
                elems: vec![int(), int(), int()],
            },
        },
    );

    assert_error!(
        "tuple(1.0, 2, 3) == tuple(1, 2, 3)",
        Error::CouldNotUnify {
            meta: Meta { start: 20, end: 34 },
            expected: Type::Tuple {
                elems: vec![float(), int(), int()],
            },
            given: Type::Tuple {
                elems: vec![int(), int(), int()],
            },
        },
    );

    assert_error!(
        "[1.0] == [1]",
        Error::CouldNotUnify {
            meta: Meta { start: 9, end: 12 },
            expected: Type::App {
                args: vec![Type::Var {
                    typ: Rc::new(RefCell::new(TypeVar::Link {
                        typ: Box::new(Type::App {
                            public: true,
                            module: vec![],
                            name: "Float".to_string(),
                            args: vec![],
                        }),
                    }))
                }],
                public: true,
                module: vec![],
                name: "List".to_string(),
            },
            given: Type::App {
                args: vec![Type::Var {
                    typ: Rc::new(RefCell::new(TypeVar::Link {
                        typ: Box::new(Type::App {
                            public: true,
                            module: vec![],
                            name: "Int".to_string(),
                            args: vec![],
                        }),
                    }))
                }],
                public: true,
                module: vec![],
                name: "List".to_string(),
            },
        },
    );

    assert_error!(
        "let x = 1 let y = 1.0 case x { _ if x == y -> 1 }",
        Error::CouldNotUnify {
            meta: Meta { start: 36, end: 42 },
            expected: int(),
            given: float(),
        },
    );

    assert_error!(
        "let x = 1.0 let y = 1 case x { _ if x == y -> 1 }",
        Error::CouldNotUnify {
            meta: Meta { start: 36, end: 42 },
            expected: float(),
            given: int(),
        },
    );

    assert_error!(
        "let x = 1.0 case x { _ if x -> 1 }",
        Error::CouldNotUnify {
            meta: Meta { start: 26, end: 27 },
            expected: bool(),
            given: float(),
        },
    );

    assert_error!(
        "case tuple(1, 1.0) { tuple(x, _) | tuple(_, x) -> 1 }",
        Error::CouldNotUnify {
            meta: Meta { start: 44, end: 45 },
            expected: int(),
            given: float(),
        },
    );

    assert_error!(
        "case [1] { [x] | x -> 1 }",
        Error::CouldNotUnify {
            meta: Meta { start: 17, end: 18 },
            expected: int(),
            given: list(int()),
        },
    );

    assert_error!(
        "case [1] { [x] | [] as x -> 1 }",
        Error::CouldNotUnify {
            meta: Meta { start: 17, end: 19 },
            expected: int(),
            given: list(int()),
        },
    );

    assert_error!(
        "case [1] { [x] | [x, y] -> 1 }",
        Error::ExtraVarInAlternativePattern {
            meta: Meta { start: 21, end: 22 },
            name: "y".to_string()
        },
    );

    assert_error!(
        "case tuple(1, 2) { tuple(1, y) | tuple(x, y) -> 1 }",
        Error::ExtraVarInAlternativePattern {
            meta: Meta { start: 39, end: 40 },
            name: "x".to_string()
        },
    );

    assert_error!(
        "case tuple(1, 2) { tuple(1, y) | tuple(x, y) -> 1 }",
        Error::ExtraVarInAlternativePattern {
            meta: Meta { start: 39, end: 40 },
            name: "x".to_string()
        },
    );

    assert_error!(
        "let x = 1 case tuple(1, 2) { tuple(1, y) | tuple(x, y) -> 1 }",
        Error::ExtraVarInAlternativePattern {
            meta: Meta { start: 49, end: 50 },
            name: "x".to_string()
        },
    );
}

#[test]
fn infer_module_test() {
    macro_rules! assert_infer {
        ($src:expr, $module:expr $(,)?) => {
            let ast = crate::grammar::ModuleParser::new()
                .parse($src)
                .expect("syntax error");
            let result = infer_module(ast, &HashMap::new()).expect("should successfully infer");
            let mut constructors: Vec<(_, _)> = result
                .type_info
                .values
                .iter()
                .map(|(k, v)| {
                    let mut printer = pretty::Printer::new();
                    (k.clone(), printer.pretty_print(&v.typ, 0))
                })
                .collect();
            constructors.sort();
            let expected: Vec<_> = $module
                .into_iter()
                .map(|(k, v)| (k.to_string(), v.to_string()))
                .collect();
            assert_eq!(($src, constructors), ($src, expected));
        };
    }

    assert_infer!(
        "pub fn repeat(i, x) {
           case i {
             0 -> []
             i -> [x | repeat(i - 1, x)]
           }
         }",
        vec![("repeat", "fn(Int, a) -> List(a)")],
    );

    assert_infer!(
        "fn private() { 1 }
         pub fn public() { 1 }",
        vec![("public", "fn() -> Int")],
    );

    assert_infer!(
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

    assert_infer!(
        "pub type Num { I(Int) }
         pub fn one() { I(1) }",
        vec![("I", "fn(Int) -> Num"), ("one", "fn() -> Num")],
    );

    assert_infer!(
        "pub fn id(x) { x }
         pub fn float() { id(1.0) }
         pub fn int() { id(1) }",
        vec![
            ("float", "fn() -> Float"),
            ("id", "fn(a) -> a"),
            ("int", "fn() -> Int"),
        ],
    );

    assert_infer!(
        "pub type Box(a) { Box(a) }
        pub fn int() { Box(1) }
        pub fn float() { Box(1.0) }",
        vec![
            ("Box", "fn(a) -> Box(a)"),
            ("float", "fn() -> Box(Float)"),
            ("int", "fn() -> Box(Int)"),
        ],
    );

    assert_infer!(
        "pub type Singleton { Singleton }
        pub fn go(x) { let Singleton = x 1 }",
        vec![("Singleton", "Singleton"), ("go", "fn(Singleton) -> Int")],
    );

    assert_infer!(
        "pub type Box(a) { Box(a) }
        pub fn unbox(x) { let Box(a) = x a }",
        vec![("Box", "fn(a) -> Box(a)"), ("unbox", "fn(Box(a)) -> a")],
    );

    assert_infer!(
        "pub type I { I(Int) }
        pub fn open(x) { case x { I(i) -> i  } }",
        vec![("I", "fn(Int) -> I"), ("open", "fn(I) -> Int")],
    );

    assert_infer!(
        "pub fn status() { 1 } pub fn list_of(x) { [x] }",
        vec![("list_of", "fn(a) -> List(a)"), ("status", "fn() -> Int")],
    );

    assert_infer!(
        "pub external fn go(String) -> String = \"\" \"\"",
        vec![("go", "fn(String) -> String")],
    );

    assert_infer!(
        "pub external fn go(Int) -> Float = \"\" \"\"",
        vec![("go", "fn(Int) -> Float")],
    );

    assert_infer!(
        "pub external fn go(Int) -> Int = \"\" \"\"",
        vec![("go", "fn(Int) -> Int")],
    );

    assert_infer!(
        "pub external fn ok() -> fn(Int) -> Int = \"\" \"\"",
        vec![("ok", "fn() -> fn(Int) -> Int")],
    );

    assert_infer!(
        "pub external fn go(Int) -> b = \"\" \"\"",
        vec![("go", "fn(Int) -> a")],
    );

    assert_infer!(
        "pub external fn go(Bool) -> b = \"\" \"\"",
        vec![("go", "fn(Bool) -> a")],
    );

    assert_infer!(
        "pub external fn go(List(a)) -> a = \"\" \"\"",
        vec![("go", "fn(List(a)) -> a")],
    );

    assert_infer!(
        "external fn go(Int) -> b = \"\" \"\"
        pub fn x() { go(1) }",
        vec![("x", "fn() -> a")],
    );

    assert_infer!(
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

    assert_infer!(
        "pub external fn len(List(a)) -> Int = \"\" \"\"",
        vec![("len", "fn(List(a)) -> Int")],
    );

    assert_infer!(
        "pub external type Connection\n
         pub external fn is_open(Connection) -> Bool = \"\" \"\"",
        vec![("is_open", "fn(Connection) -> Bool")],
    );

    assert_infer!(
        "pub external type Pair(thing, thing)\n
         pub external fn pair(a) -> Pair(a, a) = \"\" \"\"",
        vec![("pair", "fn(a) -> Pair(a, a)")],
    );

    assert_infer!(
        "pub fn one() { 1 }
         pub fn zero() { one() - 1 }
         pub fn two() { one() + zero() }",
        vec![
            ("one", "fn() -> Int"),
            ("two", "fn() -> Int"),
            ("zero", "fn() -> Int"),
        ],
    );

    assert_infer!(
        "pub fn one() { 1 }
         pub fn zero() { one() - 1 }
         pub fn two() { one() + zero() }",
        vec![
            ("one", "fn() -> Int"),
            ("two", "fn() -> Int"),
            ("zero", "fn() -> Int"),
        ],
    );

    // Type annotations
    assert_infer!("pub fn go(x: Int) { x }", vec![("go", "fn(Int) -> Int")],);
    assert_infer!("pub fn go(x: b) -> b { x }", vec![("go", "fn(a) -> a")],);
    assert_infer!("pub fn go(x) -> b { x }", vec![("go", "fn(a) -> a")],);
    assert_infer!("pub fn go(x: b) { x }", vec![("go", "fn(a) -> a")],);
    assert_infer!(
        "pub fn go(x: List(b)) -> List(b) { x }",
        vec![("go", "fn(List(a)) -> List(a)")],
    );
    assert_infer!(
        "pub fn go(x: List(b)) { x }",
        vec![("go", "fn(List(a)) -> List(a)")],
    );
    assert_infer!(
        "pub fn go(x: List(String)) { x }",
        vec![("go", "fn(List(String)) -> List(String)")],
    );
    assert_infer!("pub fn go(x: b, y: c) { x }", vec![("go", "fn(a, b) -> a")],);
    assert_infer!("pub fn go(x) -> Int { x }", vec![("go", "fn(Int) -> Int")],);

    // // Type aliases
    // assert_infer!(     src: "
    // type Html = String
    // pub fn go() { 1 }",
    //     vec![("go", "fn() -> Int")],
    // );
    assert_infer!(
        "pub fn length(list) {
           case list {
           [] -> 0
           [x | xs] -> length(xs) + 1
           }
        }",
        vec![("length", "fn(List(a)) -> Int")],
    );
    //    // % {
    //    // %pub fn length(list) {\n
    //    // %  case list {\n
    //    // %  | [] -> 0\n
    //    // %  | _ :: tail -> helper_length(tail) + 1\n
    //    // %  }\n
    //    // %}
    //    // %fn helper_length(list) { length(list) }
    //    // %   ,
    //    // %module {
    //    // % fn length(List(a)) -> Int
    //    // %}
    //    // % }

    // Structs
    assert_infer!(
        "pub type Box { Box(boxed: Int) }",
        vec![("Box", "fn(Int) -> Box")]
    );
    assert_infer!(
        "pub type Tup(a, b) { Tup(first: a, second: b) }",
        vec![("Tup", "fn(a, b) -> Tup(a, b)")]
    );
    assert_infer!(
        "pub type Tup(a, b, c) { Tup(first: a, second: b, third: c) }
         pub fn third(t) { let Tup(_, third: a, _) = t a }",
        vec![
            ("Tup", "fn(a, b, c) -> Tup(a, b, c)"),
            ("third", "fn(Tup(a, b, c)) -> c"),
        ],
    );
    assert_infer!(
        "pub type Box(x) { Box(label: String, contents: x) }
         pub fn id(x: Box(y)) { x }",
        vec![
            ("Box", "fn(String, a) -> Box(a)"),
            ("id", "fn(Box(a)) -> Box(a)"),
        ],
    );

    // Anon structs
    assert_infer!(
        "pub fn ok(x) { tuple(1, x) }",
        vec![("ok", "fn(a) -> tuple(Int, a)")],
    );

    assert_infer!(
        "pub external fn ok(Int) -> tuple(Int, Int) = \"\" \"\"",
        vec![("ok", "fn(Int) -> tuple(Int, Int)")],
    );

    assert_infer!(
        "pub external fn go(tuple(a, c)) -> c = \"\" \"\"",
        vec![("go", "fn(tuple(a, b)) -> b")],
    );

    assert_infer!(
        "pub fn always(ignore _a, return b) { b }",
        vec![("always", "fn(a, b) -> b")],
    );

    // Using types before they are defined

    assert_infer!(
        "pub type I { I(Num) } pub type Num { Num }",
        vec![("I", "fn(Num) -> I"), ("Num", "Num")]
    );

    assert_infer!(
        "pub type I { I(Num) } pub external type Num",
        vec![("I", "fn(Num) -> I")]
    );

    // We can create an aliases
    assert_infer!(
        "type IntString = Result(Int, String)
         pub fn ok_one() -> IntString { Ok(1) }",
        vec![("ok_one", "fn() -> Result(Int, String)")]
    );

    // We can create an alias with the same name as a built in type
    assert_infer!(
        "type Int = Float
         pub fn ok_one() -> Int { 1.0 }",
        vec![("ok_one", "fn() -> Float")]
    );
}

#[test]
fn infer_module_error_test() {
    macro_rules! assert_error {
        ($src:expr, $error:expr $(,)?) => {
            let mut ast = crate::grammar::ModuleParser::new()
                .parse($src)
                .expect("syntax error");
            ast.name = vec!["my_module".to_string()];
            let result = infer_module(ast, &HashMap::new()).expect_err("should infer an error");
            assert_eq!(($src, sort_options($error)), ($src, sort_options(result)));
        };

        ($src:expr) => {
            let ast = crate::grammar::ModuleParser::new()
                .parse($src)
                .expect("syntax error");
            infer_module(ast, &HashMap::new()).expect_err("should infer an error");
        };
    }

    assert_error!(
        "fn go() { 1 + 2.0 }",
        Error::CouldNotUnify {
            meta: Meta { start: 14, end: 17 },
            expected: int(),
            given: float(),
        }
    );

    assert_error!(
        "fn go() { 1 + 2.0 }",
        Error::CouldNotUnify {
            meta: Meta { start: 14, end: 17 },
            expected: int(),
            given: float(),
        }
    );

    assert_error!(
        "
fn id(x: a, y: a) { x }
pub fn x() { id(1, 1.0) }
                ",
        Error::CouldNotUnify {
            meta: Meta { start: 44, end: 47 },
            expected: int(),
            given: float(),
        }
    );

    assert_error!(
        "external fn go(List(a, b)) -> a = \"\" \"\"",
        Error::IncorrectTypeArity {
            meta: Meta { start: 15, end: 25 },
            name: "List".to_string(),
            expected: 1,
            given: 2,
        }
    );

    // We cannot declare two functions with the same name in a module
    assert_error!(
        "fn dupe() { 1 }
         fn dupe() { 2 }",
        Error::DuplicateName {
            location: Meta { start: 25, end: 40 },
            previous_location: Meta { start: 0, end: 15 },
            name: "dupe".to_string(),
        }
    );

    // We cannot declare two functions with the same name in a module
    assert_error!(
        "fn dupe() { 1 }
         fn dupe(x) { x }",
        Error::DuplicateName {
            location: Meta { start: 25, end: 41 },
            previous_location: Meta { start: 0, end: 15 },
            name: "dupe".to_string(),
        }
    );

    // We cannot declare two functions with the same name in a module
    assert_error!(
        "fn dupe() { 1 }
         external fn dupe(x) -> x = \"\" \"\"",
        Error::DuplicateName {
            location: Meta { start: 25, end: 57 },
            previous_location: Meta { start: 0, end: 15 },
            name: "dupe".to_string(),
        }
    );

    // We cannot declare two functions with the same name in a module
    assert_error!(
        "external fn dupe(x) -> x = \"\" \"\"
         fn dupe() { 1 }",
        Error::DuplicateName {
            location: Meta { start: 42, end: 57 },
            previous_location: Meta { start: 0, end: 32 },
            name: "dupe".to_string(),
        }
    );

    // We cannot declare two type constructors with the same name in a module
    assert_error!(
        "type Box { Box(x: Int) }
         type Boxy { Box(Int) }",
        Error::DuplicateName {
            location: Meta { start: 46, end: 54 },
            previous_location: Meta { start: 11, end: 22 },
            name: "Box".to_string(),
        }
    );

    // We cannot declare two type constructors with the same name in a module
    assert_error!(
        "type Boxy { Box(Int) }
         type Box { Box(x: Int) }",
        Error::DuplicateName {
            location: Meta { start: 43, end: 54 },
            previous_location: Meta { start: 12, end: 20 },
            name: "Box".to_string(),
        }
    );

    // We cannot declare two type constructors with the same name in a module
    assert_error!(
        "type Boxy { Box(Int) Box(Float) }",
        Error::DuplicateName {
            location: Meta { start: 21, end: 31 },
            previous_location: Meta { start: 12, end: 20 },
            name: "Box".to_string(),
        }
    );

    // We cannot declare two types with the same name in a module
    assert_error!(
        "type DupType { A }
         type DupType { B }",
        Error::DuplicateTypeName {
            location: Meta { start: 28, end: 41 },
            previous_location: Meta { start: 0, end: 13 },
            name: "DupType".to_string(),
        }
    );

    assert_error!(
        r#"external type PrivateType
           pub external fn leak_type() -> PrivateType = "" """#,
        Error::PrivateTypeLeak {
            meta: Meta { start: 37, end: 87 },
            leaked: Type::App {
                args: vec![],
                public: false,
                module: vec!["my_module".to_string()],
                name: "PrivateType".to_string(),
            },
        }
    );

    assert_error!(
        r#"external type PrivateType
           external fn go() -> PrivateType = "" ""
           pub fn leak_type() { go() }"#,
        Error::PrivateTypeLeak {
            meta: Meta {
                start: 88,
                end: 115,
            },
            leaked: Type::App {
                args: vec![],
                public: false,
                module: vec!["my_module".to_string()],
                name: "PrivateType".to_string(),
            },
        }
    );

    assert_error!(
        r#"external type PrivateType
           external fn go() -> PrivateType = "" ""
           pub fn leak_type() { [go()] }"#,
        Error::PrivateTypeLeak {
            meta: Meta {
                start: 88,
                end: 117,
            },
            leaked: Type::App {
                args: vec![],
                public: false,
                module: vec!["my_module".to_string()],
                name: "PrivateType".to_string(),
            },
        }
    );

    assert_error!(
        r#"external type PrivateType
                    pub external fn go(PrivateType) -> Int = "" """#,
        Error::PrivateTypeLeak {
            meta: Meta { start: 46, end: 92 },
            leaked: Type::App {
                args: vec![],
                public: false,
                module: vec!["my_module".to_string()],
                name: "PrivateType".to_string(),
            },
        }
    );

    assert_error!(
        r#"external type PrivateType
           pub type LeakType { Variant(PrivateType) }"#,
        Error::PrivateTypeLeak {
            meta: Meta { start: 57, end: 77 },
            leaked: Type::App {
                args: vec![],
                public: false,
                module: vec!["my_module".to_string()],
                name: "PrivateType".to_string(),
            },
        }
    );

    assert_error!(
        r#"fn id(x) { x } fn y() { id(x: 4) }"#,
        Error::UnexpectedLabelledArg {
            label: "x".to_string(),
            meta: Meta { start: 27, end: 31 },
        }
    );

    assert_error!(
        r#"type X { X(a: Int, b: Int, c: Int) }
                    fn x() { X(b: 1, a: 1, 1) }"#,
        Error::PositionalArgumentAfterLabelled {
            meta: Meta { start: 80, end: 81 },
        }
    );

    assert_error!(
        r#"type Thing { Thing(unknown: x) }"#,
        Error::UnknownType {
            meta: Meta { start: 28, end: 29 },
            name: "x".to_string(),
            types: env_types_with(&["Thing"]),
        }
    );

    assert_error!(
        r#"fn one() { 1 }
           fn main() { case 1 { _ if one -> 1 } }"#,
        Error::NonLocalClauseGuardVariable {
            meta: Meta { start: 52, end: 55 },
            name: "one".to_string(),
        }
    );

    // We cannot refer to unknown types in an alias
    assert_error!(
        "type IntMap = IllMap(Int, Int)",
        Error::UnknownType {
            meta: Meta { start: 14, end: 30 },
            name: "IllMap".to_string(),
            types: env_types(),
        }
    );

    // We cannot refer to unknown types in an alias
    assert_error!(
        "type IntMap = Map(Inf, Int)",
        Error::UnknownType {
            meta: Meta { start: 18, end: 21 },
            name: "Inf".to_string(),
            types: env_types(),
        }
    );

    // We cannot reuse an alias name in the same module
    assert_error!(
        "type X = Int type X = Int",
        Error::DuplicateTypeName {
            location: Meta { start: 13, end: 25 },
            previous_location: Meta { start: 0, end: 12 },
            name: "X".to_string(),
        }
    );

    // We cannot use undeclared type vars in a type alias
    assert_error!(
        "type X = List(a)",
        Error::UnknownType {
            meta: Meta { start: 14, end: 15 },
            name: "a".to_string(),
            types: env_types(),
        }
    );

    // Cases were we can't so easily check for equality-
    // i.e. because the contents of the error are non-deterministic.
    assert_error!("fn inc(x: a) { x + 1 }");
}

fn env_types_with(things: &[&str]) -> Vec<String> {
    let mut types: Vec<_> = env_types();
    for thing in things {
        types.push(thing.to_string());
    }
    types
}

fn env_types() -> Vec<String> {
    Env::new(&HashMap::new())
        .module_types
        .keys()
        .map(|s| s.to_string())
        .collect()
}

fn env_vars() -> Vec<String> {
    Env::new(&HashMap::new())
        .local_values
        .keys()
        .map(|s| s.to_string())
        .collect()
}

fn sort_options(e: Error) -> Error {
    match e {
        Error::UnknownType {
            meta,
            name,
            mut types,
        } => {
            types.sort();
            Error::UnknownType { meta, name, types }
        }

        Error::UnknownVariable {
            meta,
            name,
            mut variables,
        } => {
            variables.sort();
            Error::UnknownVariable {
                meta,
                name,
                variables,
            }
        }

        _ => e,
    }
}
