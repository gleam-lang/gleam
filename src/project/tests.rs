use super::*;

#[test]
fn compile_test() {
    struct Case {
        input: Vec<Input>,
        expected: Result<Vec<Output>, Error>,
    }
    #[derive(Debug, PartialEq)]
    struct Output {
        name: Vec<String>,
        files: Vec<OutputFile>,
        origin: ModuleOrigin,
    }

    let cases = vec![
        Case {
            input: vec![],
            expected: Ok(vec![]),
        },
        Case {
            input: vec![
                Input {
                    origin: ModuleOrigin::Src,
                    source_base_path: PathBuf::from("/src"),
                    path: PathBuf::from("/src/one.gleam"),
                    src: "".to_string(),
                },
                Input {
                    origin: ModuleOrigin::Src,
                    source_base_path: PathBuf::from("/src"),
                    path: PathBuf::from("/src/two.gleam"),
                    src: "".to_string(),
                },
            ],
            expected: Ok(vec![
                Output {
                    origin: ModuleOrigin::Src,
                    name: vec!["two".to_string()],
                    files: vec![OutputFile {
                        path: PathBuf::from("/gen/src/two.erl"),
                        text: "-module(two).\n-compile(no_auto_import).\n\n\n".to_string(),
                    }],
                },
                Output {
                    origin: ModuleOrigin::Src,
                    name: vec!["one".to_string()],
                    files: vec![OutputFile {
                        path: PathBuf::from("/gen/src/one.erl"),
                        text: "-module(one).\n-compile(no_auto_import).\n\n\n".to_string(),
                    }],
                },
            ]),
        },
        Case {
            input: vec![Input {
                origin: ModuleOrigin::Test,
                source_base_path: PathBuf::from("/test"),
                path: PathBuf::from("/test/one.gleam"),
                src: "".to_string(),
            }],
            expected: Ok(vec![Output {
                origin: ModuleOrigin::Test,
                name: vec!["one".to_string()],
                files: vec![OutputFile {
                    path: PathBuf::from("/gen/test/one.erl"),
                    text: "-module(one).\n-compile(no_auto_import).\n\n\n".to_string(),
                }],
            }]),
        },
        Case {
            input: vec![
                Input {
                    origin: ModuleOrigin::Test,
                    source_base_path: PathBuf::from("/test"),
                    path: PathBuf::from("/test/two.gleam"),
                    src: "".to_string(),
                },
                Input {
                    origin: ModuleOrigin::Src,
                    source_base_path: PathBuf::from("/src"),
                    path: PathBuf::from("/src/one.gleam"),
                    src: "import two".to_string(),
                },
            ],
            expected: Err(Error::SrcImportingTest {
                path: PathBuf::from("/src/one.gleam"),
                src: "import two".to_string(),
                meta: crate::ast::Meta { start: 7, end: 10 },
                src_module: "one".to_string(),
                test_module: "two".to_string(),
            }),
        },
        Case {
            input: vec![
                Input {
                    origin: ModuleOrigin::Src,
                    path: PathBuf::from("/src/one.gleam"),
                    source_base_path: PathBuf::from("/src"),
                    src: "import two".to_string(),
                },
                Input {
                    origin: ModuleOrigin::Src,
                    path: PathBuf::from("/src/two.gleam"),
                    source_base_path: PathBuf::from("/src"),
                    src: "".to_string(),
                },
            ],
            expected: Ok(vec![
                Output {
                    origin: ModuleOrigin::Src,
                    name: vec!["two".to_string()],
                    files: vec![OutputFile {
                        path: PathBuf::from("/gen/src/two.erl"),
                        text: "-module(two).\n-compile(no_auto_import).\n\n\n".to_string(),
                    }],
                },
                Output {
                    origin: ModuleOrigin::Src,
                    name: vec!["one".to_string()],
                    files: vec![OutputFile {
                        path: PathBuf::from("/gen/src/one.erl"),
                        text: "-module(one).\n-compile(no_auto_import).\n\n\n".to_string(),
                    }],
                },
            ]),
        },
        Case {
            input: vec![
                Input {
                    origin: ModuleOrigin::Src,
                    path: PathBuf::from("/src/one.gleam"),
                    source_base_path: PathBuf::from("/src"),
                    src: "".to_string(),
                },
                Input {
                    origin: ModuleOrigin::Src,
                    path: PathBuf::from("/src/two.gleam"),
                    source_base_path: PathBuf::from("/src"),
                    src: "import one".to_string(),
                },
            ],
            expected: Ok(vec![
                Output {
                    origin: ModuleOrigin::Src,
                    name: vec!["one".to_string()],
                    files: vec![OutputFile {
                        path: PathBuf::from("/gen/src/one.erl"),
                        text: "-module(one).\n-compile(no_auto_import).\n\n\n".to_string(),
                    }],
                },
                Output {
                    origin: ModuleOrigin::Src,
                    name: vec!["two".to_string()],
                    files: vec![OutputFile {
                        path: PathBuf::from("/gen/src/two.erl"),
                        text: "-module(two).\n-compile(no_auto_import).\n\n\n".to_string(),
                    }],
                },
            ]),
        },
        Case {
            input: vec![
                Input {
                    origin: ModuleOrigin::Src,
                    path: PathBuf::from("/src/one.gleam"),
                    source_base_path: PathBuf::from("/src"),
                    src: "pub type Box { Box(Int) }".to_string(),
                },
                Input {
                    origin: ModuleOrigin::Src,
                    path: PathBuf::from("/src/two.gleam"),
                    source_base_path: PathBuf::from("/src"),
                    src: "import one pub fn unbox(x) { let one.Box(i) = x i }".to_string(),
                },
            ],
            expected: Ok(vec![
                Output {
                    origin: ModuleOrigin::Src,
                    name: vec!["one".to_string()],
                    files: vec![OutputFile {
                        path: PathBuf::from("/gen/src/one.erl"),
                        text: "-module(one).\n-compile(no_auto_import).\n\n\n".to_string(),
                    }],
                },
                Output {
                    origin: ModuleOrigin::Src,
                    name: vec!["two".to_string()],
                    files: vec![OutputFile {
                        path: PathBuf::from("/gen/src/two.erl"),
                        text: "-module(two).\n-compile(no_auto_import).\n\n-export([unbox/1]).\n
unbox(X) ->\n    {box, I} = X,\n    I.\n"
                            .to_string(),
                    }],
                },
            ]),
        },
        Case {
            input: vec![
                Input {
                    origin: ModuleOrigin::Dependency,
                    path: PathBuf::from("/src/one.gleam"),
                    source_base_path: PathBuf::from("/src"),
                    src: "pub type Box { Box(Int) }".to_string(),
                },
                Input {
                    origin: ModuleOrigin::Dependency,
                    path: PathBuf::from("/src/two.gleam"),
                    source_base_path: PathBuf::from("/src"),
                    src: "import one pub fn box(x) { one.Box(x) }".to_string(),
                },
            ],
            expected: Ok(vec![
                Output {
                    origin: ModuleOrigin::Dependency,
                    name: vec!["one".to_string()],
                    files: vec![OutputFile {
                        path: PathBuf::from("/gen/src/one.erl"),
                        text: "-module(one).\n-compile(no_auto_import).\n\n\n".to_string(),
                    }],
                },
                Output {
                    origin: ModuleOrigin::Dependency,
                    name: vec!["two".to_string()],
                    files: vec![OutputFile {
                        path: PathBuf::from("/gen/src/two.erl"),
                        text: "-module(two).\n-compile(no_auto_import).\n\n-export([box/1]).\n
box(X) ->\n    {box, X}.\n"
                            .to_string(),
                    }],
                },
            ]),
        },
        Case {
            input: vec![Input {
                origin: ModuleOrigin::Src,
                path: PathBuf::from("/src/one/two.gleam"),
                source_base_path: PathBuf::from("/src"),
                src: "pub type Box { Box }".to_string(),
            }],
            expected: Ok(vec![Output {
                origin: ModuleOrigin::Src,
                name: vec!["one".to_string(), "two".to_string()],
                files: vec![OutputFile {
                    path: PathBuf::from("/gen/src/one@two.erl"),
                    text: "-module(one@two).\n-compile(no_auto_import).\n\n\n".to_string(),
                }],
            }]),
        },
        Case {
            input: vec![
                Input {
                    origin: ModuleOrigin::Src,
                    path: PathBuf::from("/src/one.gleam"),
                    source_base_path: PathBuf::from("/src"),
                    src: "pub type Box { Box }".to_string(),
                },
                Input {
                    origin: ModuleOrigin::Src,
                    path: PathBuf::from("/src/two.gleam"),
                    source_base_path: PathBuf::from("/src"),
                    src: "import one pub fn box() { one.Box }".to_string(),
                },
            ],
            expected: Ok(vec![
                Output {
                    origin: ModuleOrigin::Src,
                    name: vec!["one".to_string()],
                    files: vec![OutputFile {
                        path: PathBuf::from("/gen/src/one.erl"),
                        text: "-module(one).\n-compile(no_auto_import).\n\n\n".to_string(),
                    }],
                },
                Output {
                    origin: ModuleOrigin::Src,
                    name: vec!["two".to_string()],
                    files: vec![OutputFile {
                        path: PathBuf::from("/gen/src/two.erl"),
                        text: "-module(two).\n-compile(no_auto_import).\n\n-export([box/0]).\n
box() ->\n    box.\n"
                            .to_string(),
                    }],
                },
            ]),
        },
        Case {
            input: vec![
                Input {
                    origin: ModuleOrigin::Src,
                    path: PathBuf::from("/src/one.gleam"),
                    source_base_path: PathBuf::from("/src"),
                    src: "pub fn go() { 1 }".to_string(),
                },
                Input {
                    origin: ModuleOrigin::Src,
                    path: PathBuf::from("/src/two.gleam"),
                    source_base_path: PathBuf::from("/src"),
                    src: "import one as thingy       pub fn call() { thingy.go() }".to_string(),
                },
            ],
            expected: Ok(vec![
                Output {
                    origin: ModuleOrigin::Src,
                    name: vec!["one".to_string()],
                    files: vec![OutputFile {
                        path: PathBuf::from("/gen/src/one.erl"),
                        text: "-module(one).\n-compile(no_auto_import).\n\n-export([go/0]).\n
go() ->
    1.\n"
                            .to_string(),
                    }],
                },
                Output {
                    origin: ModuleOrigin::Src,
                    name: vec!["two".to_string()],
                    files: vec![OutputFile {
                        path: PathBuf::from("/gen/src/two.erl"),
                        text: "-module(two).\n-compile(no_auto_import).\n\n-export([call/0]).\n
call() ->
    one:go().\n"
                            .to_string(),
                    }],
                },
            ]),
        },
        Case {
            input: vec![
                Input {
                    origin: ModuleOrigin::Src,
                    path: PathBuf::from("/src/nested/one.gleam"),
                    source_base_path: PathBuf::from("/src"),
                    src: "pub type Box { Box(Int) }".to_string(),
                },
                Input {
                    origin: ModuleOrigin::Src,
                    path: PathBuf::from("/src/two.gleam"),
                    source_base_path: PathBuf::from("/src"),
                    src: "import nested/one\npub fn go(x) { let one.Box(y) = x y }".to_string(),
                },
            ],
            expected: Ok(vec![
                Output {
                    origin: ModuleOrigin::Src,
                    name: vec!["nested".to_string(), "one".to_string()],
                    files: vec![OutputFile {
                        path: PathBuf::from("/gen/src/nested@one.erl"),
                        text: "-module(nested@one).\n-compile(no_auto_import).\n\n\n".to_string(),
                    }],
                },
                Output {
                    origin: ModuleOrigin::Src,
                    name: vec!["two".to_string()],
                    files: vec![OutputFile {
                        path: PathBuf::from("/gen/src/two.erl"),
                        text: "-module(two).\n-compile(no_auto_import).\n\n-export([go/1]).
\ngo(X) ->\n    {box, Y} = X,\n    Y.\n"
                            .to_string(),
                    }],
                },
            ]),
        },
        Case {
            input: vec![
                Input {
                    origin: ModuleOrigin::Src,
                    path: PathBuf::from("/src/nested/one.gleam"),
                    source_base_path: PathBuf::from("/src"),
                    src: "pub type Box { Box(Int) }".to_string(),
                },
                Input {
                    origin: ModuleOrigin::Src,
                    path: PathBuf::from("/src/two.gleam"),
                    source_base_path: PathBuf::from("/src"),
                    src: "import nested/one as thingy\npub fn go(x) { let thingy.Box(y) = x y }"
                        .to_string(),
                },
            ],
            expected: Ok(vec![
                Output {
                    origin: ModuleOrigin::Src,
                    name: vec!["nested".to_string(), "one".to_string()],
                    files: vec![OutputFile {
                        path: PathBuf::from("/gen/src/nested@one.erl"),
                        text: "-module(nested@one).\n-compile(no_auto_import).\n\n\n".to_string(),
                    }],
                },
                Output {
                    origin: ModuleOrigin::Src,
                    name: vec!["two".to_string()],
                    files: vec![OutputFile {
                        path: PathBuf::from("/gen/src/two.erl"),
                        text: "-module(two).\n-compile(no_auto_import).\n\n-export([go/1]).
\ngo(X) ->\n    {box, Y} = X,\n    Y.\n"
                            .to_string(),
                    }],
                },
            ]),
        },
        Case {
            input: vec![
                Input {
                    origin: ModuleOrigin::Src,
                    path: PathBuf::from("/src/nested/one.gleam"),
                    source_base_path: PathBuf::from("/src"),
                    src: "pub external type Thing pub fn go() { 1 }".to_string(),
                },
                Input {
                    origin: ModuleOrigin::Src,
                    path: PathBuf::from("/src/two.gleam"),
                    source_base_path: PathBuf::from("/src"),
                    src: "import nested/one
                        pub fn go() { one.go() }
                        pub external fn thing() -> one.Thing = \"thing\" \"new\""
                        .to_string(),
                },
            ],
            expected: Ok(vec![
                Output {
                    origin: ModuleOrigin::Src,
                    name: vec!["nested".to_string(), "one".to_string()],
                    files: vec![OutputFile {
                        path: PathBuf::from("/gen/src/nested@one.erl"),
                        text:
                            "-module(nested@one).\n-compile(no_auto_import).\n\n-export([go/0]).\n
go() ->\n    1.\n"
                                .to_string(),
                    }],
                },
                Output {
                    origin: ModuleOrigin::Src,
                    name: vec!["two".to_string()],
                    files: vec![OutputFile {
                        path: PathBuf::from("/gen/src/two.erl"),
                        text:
                            "-module(two).\n-compile(no_auto_import).\n\n-export([go/0, thing/0]).\n
go() ->\n    nested@one:go().\n
thing() ->\n    thing:new().\n"
                                .to_string(),
                    }],
                },
            ]),
        },
        Case {
            input: vec![
                Input {
                    origin: ModuleOrigin::Src,
                    path: PathBuf::from("/src/one.gleam"),
                    source_base_path: PathBuf::from("/src"),
                    src: "".to_string(),
                },
                Input {
                    origin: ModuleOrigin::Src,
                    path: PathBuf::from("/other/src/one.gleam"),
                    source_base_path: PathBuf::from("/other/src"),
                    src: "".to_string(),
                },
            ],
            expected: Err(Error::DuplicateModule {
                module: "one".to_string(),
                first: PathBuf::from("/src/one.gleam"),
                second: PathBuf::from("/other/src/one.gleam"),
            }),
        },
        Case {
            input: vec![
                Input {
                    origin: ModuleOrigin::Src,
                    path: PathBuf::from("/src/one.gleam"),
                    source_base_path: PathBuf::from("/src"),
                    src: "pub type Point { Point(x: Int, y: Int) }".to_string(),
                },
                Input {
                    origin: ModuleOrigin::Src,
                    path: PathBuf::from("/src/two.gleam"),
                    source_base_path: PathBuf::from("/src"),
                    src: "import one
                        fn make() { one.Point(1, 4) }
                        fn x(p) { let one.Point(x, _) = p x }"
                        .to_string(),
                },
            ],
            expected: Ok(vec![
                Output {
                    origin: ModuleOrigin::Src,
                    name: vec!["one".to_string()],
                    files: vec![
                        OutputFile {
                            path: PathBuf::from("/gen/src/one_Point.hrl"),
                            text: "-record(point, {x, y}).\n".to_string(),
                        },
                        OutputFile {
                            path: PathBuf::from("/gen/src/one.erl"),
                            text: "-module(one).\n-compile(no_auto_import).\n\n\n".to_string(),
                        },
                    ],
                },
                Output {
                    origin: ModuleOrigin::Src,
                    name: vec!["two".to_string()],
                    files: vec![OutputFile {
                        path: PathBuf::from("/gen/src/two.erl"),
                        text: "-module(two).\n-compile(no_auto_import).\n
make() ->\n    {point, 1, 4}.\n
x(P) ->\n    {point, X, _} = P,\n    X.\n"
                            .to_string(),
                    }],
                },
            ]),
        },
        Case {
            input: vec![
                Input {
                    origin: ModuleOrigin::Src,
                    path: PathBuf::from("/src/one.gleam"),
                    source_base_path: PathBuf::from("/src"),
                    src: "pub fn div(top x, bottom y) { x/y }".to_string(),
                },
                Input {
                    origin: ModuleOrigin::Src,
                    path: PathBuf::from("/src/two.gleam"),
                    source_base_path: PathBuf::from("/src"),
                    src: "import one.{div}
                    fn run() { 2 |> div(top: _, bottom: 4) |> div(2, bottom: _) }"
                        .to_string(),
                },
            ],
            expected: Ok(vec![
                Output {
                    origin: ModuleOrigin::Src,
                    name: vec!["one".to_string()],
                    files: vec![OutputFile {
                        path: PathBuf::from("/gen/src/one.erl"),
                        text:
                            "-module(one).\n-compile(no_auto_import).\n\n-export([\'div\'/2]).\n\n'div'(X, Y) ->\n    X div Y.\n"
                                .to_string(),
                    }],
                },
                Output {
                    origin: ModuleOrigin::Src,
                    name: vec!["two".to_string()],
                    files: vec![OutputFile {
                        path: PathBuf::from("/gen/src/two.erl"),
                        text: "-module(two).\n-compile(no_auto_import).\n\nrun() ->\n    one:'div'(2, one:'div'(2, 4)).\n"
                            .to_string(),
                    }],
                },
            ]),
        },
        Case {
            input: vec![
                Input {
                    origin: ModuleOrigin::Src,
                    path: PathBuf::from("/src/one.gleam"),
                    source_base_path: PathBuf::from("/src"),
                    src: "pub type Empty { Empty }".to_string(),
                },
                Input {
                    origin: ModuleOrigin::Src,
                    path: PathBuf::from("/src/two.gleam"),
                    source_base_path: PathBuf::from("/src"),
                    src: "import one
                        fn make() { one.Empty }"
                        .to_string(),
                },
            ],
            expected: Ok(vec![
                Output {
                    origin: ModuleOrigin::Src,
                    name: vec!["one".to_string()],
                    files: vec![OutputFile {
                        path: PathBuf::from("/gen/src/one.erl"),
                        text: "-module(one).\n-compile(no_auto_import).\n\n\n".to_string(),
                    }],
                },
                Output {
                    origin: ModuleOrigin::Src,
                    name: vec!["two".to_string()],
                    files: vec![OutputFile {
                        path: PathBuf::from("/gen/src/two.erl"),
                        text: "-module(two).\n-compile(no_auto_import).\n\nmake() ->\n    empty.\n"
                            .to_string(),
                    }],
                },
            ]),
        },
        Case {
            input: vec![
                Input {
                    origin: ModuleOrigin::Src,
                    path: PathBuf::from("/src/one.gleam"),
                    source_base_path: PathBuf::from("/src"),
                    src: "pub fn id(x) { x } pub type Empty { Empty }".to_string(),
                },
                Input {
                    origin: ModuleOrigin::Src,
                    path: PathBuf::from("/src/two.gleam"),
                    source_base_path: PathBuf::from("/src"),
                    src: "import one.{Empty, id} fn make() { id(Empty) }".to_string(),
                },
            ],
            expected: Ok(vec![
                Output {
                    origin: ModuleOrigin::Src,
                    name: vec!["one".to_string()],
                    files: vec![OutputFile {
                        path: PathBuf::from("/gen/src/one.erl"),
                        text: "-module(one).\n-compile(no_auto_import).\n\n-export([id/1]).\n
id(X) ->\n    X.\n"
                            .to_string(),
                    }],
                },
                Output {
                    origin: ModuleOrigin::Src,
                    name: vec!["two".to_string()],
                    files: vec![OutputFile {
                        path: PathBuf::from("/gen/src/two.erl"),
                        text: "-module(two).\n-compile(no_auto_import).\n
make() ->
    one:id(empty).\n"
                            .to_string(),
                    }],
                },
            ]),
        },
        // https://github.com/gleam-lang/gleam/issues/303
        Case {
            input: vec![
                Input {
                    origin: ModuleOrigin::Src,
                    path: PathBuf::from("/src/one.gleam"),
                    source_base_path: PathBuf::from("/src"),
                    src: "pub fn id(x) { x } pub type Empty { Empty }".to_string(),
                },
                Input {
                    origin: ModuleOrigin::Src,
                    path: PathBuf::from("/src/two.gleam"),
                    source_base_path: PathBuf::from("/src"),
                    src: "import one.{Empty as e, id as i} fn make() { i(e) }".to_string(),
                },
            ],
            expected: Ok(vec![
                Output {
                    origin: ModuleOrigin::Src,
                    name: vec!["one".to_string()],
                    files: vec![OutputFile {
                        path: PathBuf::from("/gen/src/one.erl"),
                        text: "-module(one).\n-compile(no_auto_import).\n\n-export([id/1]).\n
id(X) ->\n    X.\n"
                            .to_string(),
                    }],
                },
                Output {
                    origin: ModuleOrigin::Src,
                    name: vec!["two".to_string()],
                    files: vec![OutputFile {
                        path: PathBuf::from("/gen/src/two.erl"),
                        text: "-module(two).\n-compile(no_auto_import).\n
make() ->\n    one:id(empty).\n"
                            .to_string(),
                    }],
                },
            ]),
        },
        Case {
            input: vec![
                Input {
                    origin: ModuleOrigin::Src,
                    path: PathBuf::from("/src/one.gleam"),
                    source_base_path: PathBuf::from("/src"),
                    src: "pub fn receive() { 1 }".to_string(),
                },
                Input {
                    origin: ModuleOrigin::Src,
                    path: PathBuf::from("/src/two.gleam"),
                    source_base_path: PathBuf::from("/src"),
                    src: "import one fn funky() { one.receive }".to_string(),
                },
            ],
            expected: Ok(vec![
                Output {
                    origin: ModuleOrigin::Src,
                    name: vec!["one".to_string()],
                    files: vec![OutputFile {
                        path: PathBuf::from("/gen/src/one.erl"),
                        text: "-module(one).\n-compile(no_auto_import).\n
-export(['receive'/0]).\n
'receive'() ->\n    1.\n"
                            .to_string(),
                    }],
                },
                Output {
                    origin: ModuleOrigin::Src,
                    name: vec!["two".to_string()],
                    files: vec![OutputFile {
                        path: PathBuf::from("/gen/src/two.erl"),
                        text: "-module(two).\n-compile(no_auto_import).\n
funky() ->
    fun one:'receive'/0.\n"
                            .to_string(),
                    }],
                },
            ]),
        },
        Case {
            input: vec![
                Input {
                    origin: ModuleOrigin::Src,
                    path: PathBuf::from("/src/one.gleam"),
                    source_base_path: PathBuf::from("/src"),
                    src: "pub fn receive() { 1 }".to_string(),
                },
                Input {
                    origin: ModuleOrigin::Src,
                    path: PathBuf::from("/src/two.gleam"),
                    source_base_path: PathBuf::from("/src"),
                    src: "import one.{receive} fn funky() { receive }".to_string(),
                },
            ],
            expected: Ok(vec![
                Output {
                    origin: ModuleOrigin::Src,
                    name: vec!["one".to_string()],
                    files: vec![OutputFile {
                        path: PathBuf::from("/gen/src/one.erl"),
                        text: "-module(one).\n-compile(no_auto_import).\n
-export(['receive'/0]).\n
'receive'() ->\n    1.\n"
                            .to_string(),
                    }],
                },
                Output {
                    origin: ModuleOrigin::Src,
                    name: vec!["two".to_string()],
                    files: vec![OutputFile {
                        path: PathBuf::from("/gen/src/two.erl"),
                        text: "-module(two).\n-compile(no_auto_import).\n\nfunky() ->
    fun one:'receive'/0.\n"
                            .to_string(),
                    }],
                },
            ]),
        },
        // https://github.com/gleam-lang/gleam/issues/340
        Case {
            input: vec![
                Input {
                    origin: ModuleOrigin::Src,
                    path: PathBuf::from("/src/one.gleam"),
                    source_base_path: PathBuf::from("/src"),
                    src: "pub fn receive(x) { x }".to_string(),
                },
                Input {
                    origin: ModuleOrigin::Src,
                    path: PathBuf::from("/src/two.gleam"),
                    source_base_path: PathBuf::from("/src"),
                    src: "import one fn funky() { one.receive(1) }".to_string(),
                },
            ],
            expected: Ok(vec![
                Output {
                    origin: ModuleOrigin::Src,
                    name: vec!["one".to_string()],
                    files: vec![OutputFile {
                        path: PathBuf::from("/gen/src/one.erl"),
                        text: "-module(one).\n-compile(no_auto_import).\n
-export(['receive'/1]).\n
'receive'(X) ->\n    X.\n"
                            .to_string(),
                    }],
                },
                Output {
                    origin: ModuleOrigin::Src,
                    name: vec!["two".to_string()],
                    files: vec![OutputFile {
                        path: PathBuf::from("/gen/src/two.erl"),
                        text: "-module(two).\n-compile(no_auto_import).\n\nfunky() ->
    one:'receive'(1).\n"
                            .to_string(),
                    }],
                },
            ]),
        },
    ];

    for Case { input, expected } in cases.into_iter() {
        let output = compile(input).map(|mods| {
            mods.into_iter()
                .map(|compiled| Output {
                    name: compiled.name,
                    files: compiled.files,
                    origin: compiled.origin,
                })
                .collect::<Vec<_>>()
        });
        assert_eq!(expected, output);
    }
}
