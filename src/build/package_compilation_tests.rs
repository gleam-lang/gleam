use super::*;
use crate::{
    ast::SrcSpan,
    build::{
        package_compiler::{Options, PackageCompiler, Source},
        project_root::ProjectRoot,
        Origin,
    },
    codegen,
    config::{BuildTool, Docs, PackageConfig, Repository},
    erl,
    fs::test::FilesChannel,
    typ,
};
use std::{path::PathBuf, sync::Arc};

macro_rules! assert_erlang_compile {
    ($sources:expr, $expected_output:expr  $(,)?) => {
        let mut modules = HashMap::new();
        let options = Options {
            name: "the_package".to_string(),
            src_path: PathBuf::from("_build/default/lib/the_package/src"),
            out_path: PathBuf::from("_build/default/lib/the_package/src"),
            test_path: None,
        };
        let (file_writer, file_receiver) = FilesChannel::new();
        let mut compiler = PackageCompiler::new(options, file_writer);
        compiler.sources = $sources;
        let outputs = compiler
            .compile(&mut modules, &mut HashMap::with_capacity(4))
            .map(|_| {
                let mut outputs = FilesChannel::recv_utf8_files(&file_receiver).unwrap();
                outputs.sort_by(|a, b| a.path.partial_cmp(&b.path).unwrap());
                outputs
            })
            .map_err(|e| normalise_error(e));
        assert_eq!($expected_output, outputs);
    };
}

#[test]
fn package_compiler_test() {
    assert_erlang_compile!(vec![], Ok(vec![]));

    assert_erlang_compile!(
        vec![Source {
            path: PathBuf::from("src/one.gleam"),
            name: "one".to_string(),
            code: "".to_string(),
            origin: Origin::Src,
        }],
        Ok(vec![OutputFile {
            text: "-module(one).\n-compile(no_auto_import).\n\n\n".to_string(),
            path: PathBuf::from("_build/default/lib/the_package/src/one.erl"),
        },])
    );

    assert_erlang_compile!(
        vec![
            Source {
                path: PathBuf::from("src/one.gleam"),
                name: "one".to_string(),
                code: "".to_string(),
                origin: Origin::Src,
            },
            Source {
                path: PathBuf::from("src/one.gleam"),
                name: "two".to_string(),
                code: "".to_string(),
                origin: Origin::Src,
            },
        ],
        Ok(vec![
            OutputFile {
                text: "-module(one).\n-compile(no_auto_import).\n\n\n".to_string(),
                path: PathBuf::from("_build/default/lib/the_package/src/one.erl"),
            },
            OutputFile {
                text: "-module(two).\n-compile(no_auto_import).\n\n\n".to_string(),
                path: PathBuf::from("_build/default/lib/the_package/src/two.erl"),
            },
        ])
    );

    assert_erlang_compile!(
        vec![Source {
            name: "one".to_string(),
            origin: Origin::Src,
            path: PathBuf::from("src/one.gleam"),
            code: "".to_string(),
        }],
        Ok(vec![OutputFile {
            path: PathBuf::from("_build/default/lib/the_package/src/one.erl"),
            text: "-module(one).\n-compile(no_auto_import).\n\n\n".to_string(),
        },]),
    );

    // TODO: src modules cannot be allowed to import test modules
    // TODO: Does this get handled at this level? I forget
    // assert_erlang_compile!(
    //     vec![
    //         Source {
    //             origin: Origin::Test,
    //             name: "two".to_string(),
    //             path: PathBuf::from("/test/two.gleam"),
    //             code: "".to_string(),
    //         },
    //         Source {
    //             origin: Origin::Src,
    //             name: "one".to_string(),
    //             path: PathBuf::from("/src/one.gleam"),
    //             code: "import two".to_string(),
    //         },
    //     ],
    //     Err(Error::SrcImportingTest {
    //         path: PathBuf::from("/src/one.gleam"),
    //         src: "import two".to_string(),
    //         location: crate::ast::SrcSpan { start: 7, end: 10 },
    //         src_module: "one".to_string(),
    //         test_module: "two".to_string(),
    //     }),
    // );

    assert_erlang_compile!(
        vec![
            Source {
                origin: Origin::Src,
                path: PathBuf::from("/src/one.gleam"),
                name: "one".to_string(),
                code: "import two".to_string(),
            },
            Source {
                origin: Origin::Src,
                path: PathBuf::from("/src/two.gleam"),
                name: "two".to_string(),
                code: "".to_string(),
            },
        ],
        Ok(vec![
            OutputFile {
                path: PathBuf::from("_build/default/lib/the_package/src/one.erl"),
                text: "-module(one).\n-compile(no_auto_import).\n\n\n".to_string(),
            },
            OutputFile {
                path: PathBuf::from("_build/default/lib/the_package/src/two.erl"),
                text: "-module(two).\n-compile(no_auto_import).\n\n\n".to_string(),
            },
        ]),
    );

    assert_erlang_compile!(
        vec![
            Source {
                origin: Origin::Src,
                path: PathBuf::from("/src/one.gleam"),
                name: "one".to_string(),
                code: "".to_string(),
            },
            Source {
                origin: Origin::Src,
                path: PathBuf::from("/src/two.gleam"),
                name: "two".to_string(),
                code: "import one".to_string(),
            },
        ],
        Ok(vec![
            OutputFile {
                path: PathBuf::from("_build/default/lib/the_package/src/one.erl"),
                text: "-module(one).\n-compile(no_auto_import).\n\n\n".to_string(),
            },
            OutputFile {
                path: PathBuf::from("_build/default/lib/the_package/src/two.erl"),
                text: "-module(two).\n-compile(no_auto_import).\n\n\n".to_string(),
            },
        ]),
    );

    assert_erlang_compile!(
        vec![
            Source {
                origin: Origin::Src,
                path: PathBuf::from("/src/one.gleam"),
                name: "one".to_string(),
                code: "pub type Box { Box(Int) }".to_string(),
            },
            Source {
                origin: Origin::Src,
                path: PathBuf::from("/src/two.gleam"),
                name: "two".to_string(),
                code: "import one pub fn unbox(x) { let one.Box(i) = x i }".to_string(),
            },
        ],
        Ok(vec![
            OutputFile {
                path: PathBuf::from("_build/default/lib/the_package/src/one.erl"),
                text: "-module(one).\n-compile(no_auto_import).\n\n\n".to_string(),
            },
            OutputFile {
                path: PathBuf::from("_build/default/lib/the_package/src/two.erl"),
                text: "-module(two).\n-compile(no_auto_import).\n\n-export([unbox/1]).\n
unbox(X) ->\n    {box, I} = X,\n    I.\n"
                    .to_string(),
            },
        ]),
    );

    assert_erlang_compile!(
        vec![
            Source {
                origin: Origin::Test,
                path: PathBuf::from("/test/one.gleam"),
                name: "one".to_string(),
                code: "pub type Box { Box(Int) }".to_string(),
            },
            Source {
                origin: Origin::Test,
                path: PathBuf::from("/test/two.gleam"),
                name: "two".to_string(),
                code: "import one pub fn box(x) { one.Box(x) }".to_string(),
            },
        ],
        Ok(vec![
            OutputFile {
                path: PathBuf::from("_build/default/lib/the_package/src/one.erl"),
                text: "-module(one).\n-compile(no_auto_import).\n\n\n".to_string(),
            },
            OutputFile {
                path: PathBuf::from("_build/default/lib/the_package/src/two.erl"),
                text: "-module(two).\n-compile(no_auto_import).\n\n-export([box/1]).\n
box(X) ->\n    {box, X}.\n"
                    .to_string(),
            },
        ]),
    );

    assert_erlang_compile!(
        vec![Source {
            origin: Origin::Src,
            path: PathBuf::from("/src/one/two.gleam"),
            name: "one/two".to_string(),
            code: "pub type Box { Box }".to_string(),
        }],
        Ok(vec![OutputFile {
            path: PathBuf::from("_build/default/lib/the_package/src/one@two.erl"),
            text: "-module(one@two).\n-compile(no_auto_import).\n\n\n".to_string(),
        }]),
    );

    assert_erlang_compile!(
        vec![
            Source {
                origin: Origin::Src,
                path: PathBuf::from("/src/one.gleam"),
                name: "one".to_string(),
                code: "pub type Box { Box }".to_string(),
            },
            Source {
                origin: Origin::Src,
                path: PathBuf::from("/src/two.gleam"),
                name: "two".to_string(),
                code: "import one pub fn box() { one.Box }".to_string(),
            },
        ],
        Ok(vec![
            OutputFile {
                path: PathBuf::from("_build/default/lib/the_package/src/one.erl"),
                text: "-module(one).\n-compile(no_auto_import).\n\n\n".to_string(),
            },
            OutputFile {
                path: PathBuf::from("_build/default/lib/the_package/src/two.erl"),
                text: "-module(two).\n-compile(no_auto_import).\n\n-export([box/0]).\n
box() ->\n    box.\n"
                    .to_string(),
            },
        ]),
    );

    assert_erlang_compile!(
        vec![
            Source {
                origin: Origin::Src,
                path: PathBuf::from("/src/one.gleam"),
                name: "one".to_string(),
                code: "pub fn go() { 1 }".to_string(),
            },
            Source {
                origin: Origin::Src,
                path: PathBuf::from("/src/two.gleam"),
                name: "two".to_string(),
                code: "import one as thingy       pub fn call() { thingy.go() }".to_string(),
            },
        ],
        Ok(vec![
            OutputFile {
                path: PathBuf::from("_build/default/lib/the_package/src/one.erl"),
                text: "-module(one).\n-compile(no_auto_import).\n\n-export([go/0]).\n
go() ->
    1.\n"
                    .to_string(),
            },
            OutputFile {
                path: PathBuf::from("_build/default/lib/the_package/src/two.erl"),
                text: "-module(two).\n-compile(no_auto_import).\n\n-export([call/0]).\n
call() ->
    one:go().\n"
                    .to_string(),
            },
        ]),
    );

    assert_erlang_compile!(
        vec![
            Source {
                origin: Origin::Src,
                path: PathBuf::from("/src/nested/one.gleam"),
                name: "nested/one".to_string(),
                code: "pub type Box { Box(Int) }".to_string(),
            },
            Source {
                origin: Origin::Src,
                path: PathBuf::from("/src/two.gleam"),
                name: "two".to_string(),
                code: "import nested/one\npub fn go(x) { let one.Box(y) = x y }".to_string(),
            },
        ],
        Ok(vec![
            OutputFile {
                path: PathBuf::from("_build/default/lib/the_package/src/nested@one.erl"),
                text: "-module(nested@one).\n-compile(no_auto_import).\n\n\n".to_string(),
            },
            OutputFile {
                path: PathBuf::from("_build/default/lib/the_package/src/two.erl"),
                text: "-module(two).\n-compile(no_auto_import).\n\n-export([go/1]).
\ngo(X) ->\n    {box, Y} = X,\n    Y.\n"
                    .to_string(),
            },
        ]),
    );

    assert_erlang_compile!(
        vec![
            Source {
                origin: Origin::Src,
                path: PathBuf::from("/src/nested/one.gleam"),
                name: "nested/one".to_string(),
                code: "pub type Box { Box(Int) }".to_string(),
            },
            Source {
                origin: Origin::Src,
                path: PathBuf::from("/src/two.gleam"),
                name: "two".to_string(),
                code: "import nested/one as thingy\npub fn go(x) { let thingy.Box(y) = x y }"
                    .to_string(),
            },
        ],
        Ok(vec![
            OutputFile {
                path: PathBuf::from("_build/default/lib/the_package/src/nested@one.erl"),
                text: "-module(nested@one).\n-compile(no_auto_import).\n\n\n".to_string(),
            },
            OutputFile {
                path: PathBuf::from("_build/default/lib/the_package/src/two.erl"),
                text: "-module(two).\n-compile(no_auto_import).\n\n-export([go/1]).
\ngo(X) ->\n    {box, Y} = X,\n    Y.\n"
                    .to_string(),
            },
        ]),
    );

    assert_erlang_compile!(
        vec![
            Source {
                origin: Origin::Src,
                path: PathBuf::from("/src/nested/one.gleam"),
                name: "nested/one".to_string(),
                code: "pub external type Thing pub fn go() { 1 }".to_string(),
            },
            Source {
                origin: Origin::Src,
                path: PathBuf::from("/src/two.gleam"),
                name: "two".to_string(),
                code: "import nested/one
                        pub fn go() { one.go() }
                        pub external fn thing() -> one.Thing = \"thing\" \"new\"
                        pub fn call_thing() { thing() }
                        "
                .to_string(),
            },
        ],
        Ok(vec![
            OutputFile {
                path: PathBuf::from("_build/default/lib/the_package/src/nested@one.erl"),
                text: "-module(nested@one).\n-compile(no_auto_import).\n
-export([go/0]).\n\ngo() ->\n    1.\n"
                    .to_string(),
            },
            OutputFile {
                path: PathBuf::from("_build/default/lib/the_package/src/two.erl"),
                text: "-module(two).\n-compile(no_auto_import).\n
-export([go/0, thing/0, call_thing/0]).\n
go() ->\n    nested@one:go().\n
thing() ->\n    thing:new().\n
call_thing() ->\n    thing:new().\n"
                    .to_string(),
            },
        ]),
    );

    assert_erlang_compile!(
        vec![
            Source {
                origin: Origin::Src,
                path: PathBuf::from("/src/one.gleam"),
                name: "one".to_string(),
                code: "".to_string(),
            },
            Source {
                origin: Origin::Src,
                path: PathBuf::from("/test/one.gleam"),
                name: "one".to_string(),
                code: "".to_string(),
            },
        ],
        Err(Error::DuplicateModule {
            module: "one".to_string(),
            first: PathBuf::from("/src/one.gleam"),
            second: PathBuf::from("/test/one.gleam"),
        }),
    );

    assert_erlang_compile!(
        vec![
            Source {
                origin: Origin::Src,
                path: PathBuf::from("/src/one.gleam"),
                name: "one".to_string(),
                code: "pub type Point { Point(x: Int, y: Int) }".to_string(),
            },
            Source {
                origin: Origin::Src,
                path: PathBuf::from("/src/two.gleam"),
                name: "two".to_string(),
                code: "import one
                        fn make() { one.Point(1, 4) }
                        fn x(p) { let one.Point(x, _) = p x }"
                    .to_string(),
            },
        ],
        Ok(vec![
            OutputFile {
                path: PathBuf::from("_build/default/lib/the_package/src/one.erl"),
                text: "-module(one).\n-compile(no_auto_import).\n\n\n".to_string(),
            },
            OutputFile {
                path: PathBuf::from("_build/default/lib/the_package/src/one_Point.hrl"),
                text: "-record(point, {x, y}).\n".to_string(),
            },
            OutputFile {
                path: PathBuf::from("_build/default/lib/the_package/src/two.erl"),
                text: "-module(two).\n-compile(no_auto_import).\n
make() ->\n    {point, 1, 4}.\n
x(P) ->\n    {point, X, _} = P,\n    X.\n"
                    .to_string(),
            },
        ]),
    );

    // Erlang keywords are escaped
    assert_erlang_compile!(
        vec![
            Source {
                origin: Origin::Src,
                path: PathBuf::from("/src/one.gleam"),
                name: "one".to_string(),
                code: "pub fn div(top x, bottom y) { x%y }".to_string(),
            },
            Source {
                origin: Origin::Src,
                path: PathBuf::from("/src/two.gleam"),
                name: "two".to_string(),
                code: "import one.{div}
                    fn run() { 2 |> div(top: _, bottom: 4) |> div(2, bottom: _) }"
                    .to_string(),
            },
        ],
        Ok(vec![
            OutputFile {
                path: PathBuf::from("_build/default/lib/the_package/src/one.erl"),
                text: "-module(one).\n-compile(no_auto_import).\n\n-export([\'div\'/2]).\n
'div'(X, Y) ->
    case Y of
        0 -> 0;
        Gleam@denominator -> X rem Gleam@denominator
    end.\n"
                    .to_string(),
            },
            OutputFile {
                path: PathBuf::from("_build/default/lib/the_package/src/two.erl"),
                text: "-module(two).\n-compile(no_auto_import).\n
run() ->\n    one:'div'(2, one:'div'(2, 4)).\n"
                    .to_string(),
            },
        ]),
    );

    assert_erlang_compile!(
        vec![
            Source {
                origin: Origin::Src,
                path: PathBuf::from("/src/one.gleam"),
                name: "one".to_string(),
                code: "pub type Empty { Empty }".to_string(),
            },
            Source {
                origin: Origin::Src,
                path: PathBuf::from("/src/two.gleam"),
                name: "two".to_string(),
                code: "import one
                        fn make() { one.Empty }"
                    .to_string(),
            },
        ],
        Ok(vec![
            OutputFile {
                path: PathBuf::from("_build/default/lib/the_package/src/one.erl"),
                text: "-module(one).\n-compile(no_auto_import).\n\n\n".to_string(),
            },
            OutputFile {
                path: PathBuf::from("_build/default/lib/the_package/src/two.erl"),
                text: "-module(two).\n-compile(no_auto_import).\n
make() ->\n    empty.\n"
                    .to_string(),
            },
        ]),
    );

    assert_erlang_compile!(
        vec![
            Source {
                origin: Origin::Src,
                path: PathBuf::from("/src/one.gleam"),
                name: "one".to_string(),
                code: "pub fn id(x) { x } pub type Empty { Empty }".to_string(),
            },
            Source {
                origin: Origin::Src,
                path: PathBuf::from("/src/two.gleam"),
                name: "two".to_string(),
                code: "import one.{Empty, id} fn make() { id(Empty) }".to_string(),
            },
        ],
        Ok(vec![
            OutputFile {
                path: PathBuf::from("_build/default/lib/the_package/src/one.erl"),
                text: "-module(one).\n-compile(no_auto_import).\n\n-export([id/1]).\n
id(X) ->\n    X.\n"
                    .to_string(),
            },
            OutputFile {
                path: PathBuf::from("_build/default/lib/the_package/src/two.erl"),
                text: "-module(two).\n-compile(no_auto_import).\n
make() ->\n    one:id(empty).\n"
                    .to_string(),
            },
        ]),
    );

    // https://github.com/gleam-lang/gleam/issues/303
    assert_erlang_compile!(
        vec![
            Source {
                origin: Origin::Src,
                path: PathBuf::from("/src/one.gleam"),
                name: "one".to_string(),
                code: "pub fn id(x) { x } pub type Empty { Empty }".to_string(),
            },
            Source {
                origin: Origin::Src,
                path: PathBuf::from("/src/two.gleam"),
                name: "two".to_string(),
                code: "import one.{Empty as e, id as i} fn make() { i(e) }".to_string(),
            },
        ],
        Ok(vec![
            OutputFile {
                path: PathBuf::from("_build/default/lib/the_package/src/one.erl"),
                text: "-module(one).\n-compile(no_auto_import).\n
-export([id/1]).\n
id(X) ->\n    X.\n"
                    .to_string(),
            },
            OutputFile {
                path: PathBuf::from("_build/default/lib/the_package/src/two.erl"),
                text: "-module(two).\n-compile(no_auto_import).\n
make() ->\n    one:id(empty).\n"
                    .to_string(),
            },
        ]),
    );

    assert_erlang_compile!(
        vec![
            Source {
                origin: Origin::Src,
                path: PathBuf::from("/src/one.gleam"),
                name: "one".to_string(),
                code: "pub fn receive() { 1 }".to_string(),
            },
            Source {
                origin: Origin::Src,
                path: PathBuf::from("/src/two.gleam"),
                name: "two".to_string(),
                code: "import one fn funky() { one.receive }".to_string(),
            },
        ],
        Ok(vec![
            OutputFile {
                path: PathBuf::from("_build/default/lib/the_package/src/one.erl"),
                text: "-module(one).\n-compile(no_auto_import).\n
-export(['receive'/0]).\n
'receive'() ->\n    1.\n"
                    .to_string(),
            },
            OutputFile {
                path: PathBuf::from("_build/default/lib/the_package/src/two.erl"),
                text: "-module(two).\n-compile(no_auto_import).\n
funky() ->\n    fun one:'receive'/0.\n"
                    .to_string(),
            },
        ]),
    );

    assert_erlang_compile!(
        vec![
            Source {
                origin: Origin::Src,
                path: PathBuf::from("/src/one.gleam"),
                name: "one".to_string(),
                code: "pub fn receive() { 1 }".to_string(),
            },
            Source {
                origin: Origin::Src,
                path: PathBuf::from("/src/two.gleam"),
                name: "two".to_string(),
                code: "import one.{receive} fn funky() { receive }".to_string(),
            },
        ],
        Ok(vec![
            OutputFile {
                path: PathBuf::from("_build/default/lib/the_package/src/one.erl"),
                text: "-module(one).\n-compile(no_auto_import).\n
-export(['receive'/0]).\n
'receive'() ->\n    1.\n"
                    .to_string(),
            },
            OutputFile {
                path: PathBuf::from("_build/default/lib/the_package/src/two.erl"),
                text: "-module(two).\n-compile(no_auto_import).\n
funky() ->\n    fun one:'receive'/0.\n"
                    .to_string(),
            },
        ]),
    );

    // https://github.com/gleam-lang/gleam/issues/340
    assert_erlang_compile!(
        vec![
            Source {
                origin: Origin::Src,
                path: PathBuf::from("/src/one.gleam"),
                name: "one".to_string(),
                code: "pub fn receive(x) { x }".to_string(),
            },
            Source {
                origin: Origin::Src,
                path: PathBuf::from("/src/two.gleam"),
                name: "two".to_string(),
                code: "import one fn funky() { one.receive(1) }".to_string(),
            },
        ],
        Ok(vec![
            OutputFile {
                path: PathBuf::from("_build/default/lib/the_package/src/one.erl"),
                text: "-module(one).\n-compile(no_auto_import).\n
-export(['receive'/1]).\n
'receive'(X) ->\n    X.\n"
                    .to_string(),
            },
            OutputFile {
                path: PathBuf::from("_build/default/lib/the_package/src/two.erl"),
                text: "-module(two).\n-compile(no_auto_import).\n
funky() ->\n    one:'receive'(1).\n"
                    .to_string(),
            },
        ]),
    );

    // We can use record accessors for types with only one constructor, defined
    // in another module
    assert_erlang_compile!(
        vec![
            Source {
                origin: Origin::Src,
                path: PathBuf::from("/src/one.gleam"),
                name: "one".to_string(),
                code: "pub type Person { Person(name: String, age: Int) }".to_string(),
            },
            Source {
                origin: Origin::Src,
                path: PathBuf::from("/src/two.gleam"),
                name: "two".to_string(),
                code: "import one.{Person}
    pub fn get_age(person: Person) { person.age }
    pub fn get_name(person: Person) { person.name }"
                    .to_string(),
            },
        ],
        Ok(vec![
            OutputFile {
                path: PathBuf::from("_build/default/lib/the_package/src/one.erl"),
                text: "-module(one).\n-compile(no_auto_import).\n\n\n".to_string(),
            },
            OutputFile {
                path: PathBuf::from("_build/default/lib/the_package/src/one_Person.hrl"),
                text: "-record(person, {name, age}).\n".to_string(),
            },
            OutputFile {
                path: PathBuf::from("_build/default/lib/the_package/src/two.erl"),
                text: "-module(two).\n-compile(no_auto_import).\n
-export([get_age/1, get_name/1]).\n
get_age(Person) ->\n    erlang:element(3, Person).\n
get_name(Person) ->\n    erlang:element(2, Person).\n"
                    .to_string(),
            },
        ]),
    );

    // Can use imported types in Type Constructors
    assert_erlang_compile!(
        vec![
            Source {
                origin: Origin::Src,
                path: PathBuf::from("/src/one.gleam"),
                name: "one".to_string(),
                code: "pub type Person { Person(name: String, age: Int) }".to_string(),
            },
            Source {
                origin: Origin::Src,
                path: PathBuf::from("/src/two.gleam"),
                name: "two".to_string(),
                code: "import one type Two = one.Person".to_string(),
            },
        ],
        Ok(vec![
            OutputFile {
                path: PathBuf::from("_build/default/lib/the_package/src/one.erl"),
                text: "-module(one).\n-compile(no_auto_import).\n\n\n".to_string(),
            },
            OutputFile {
                path: PathBuf::from("_build/default/lib/the_package/src/one_Person.hrl"),
                text: "-record(person, {name, age}).\n".to_string(),
            },
            OutputFile {
                path: PathBuf::from("_build/default/lib/the_package/src/two.erl"),
                text: "-module(two).\n-compile(no_auto_import).\n\n\n".to_string(),
            },
        ]),
    );

    // Can use imported, fully qualified types in Type Constructors
    assert_erlang_compile!(
        vec![
            Source {
                origin: Origin::Src,
                path: PathBuf::from("/src/one.gleam"),
                name: "one".to_string(),
                code: "pub type Person { Person(name: String, age: Int) }".to_string(),
            },
            Source {
                origin: Origin::Src,
                path: PathBuf::from("/src/two.gleam"),
                name: "two".to_string(),
                code: "import one.{Person} type Two = Person".to_string(),
            },
        ],
        Ok(vec![
            OutputFile {
                path: PathBuf::from("_build/default/lib/the_package/src/one.erl"),
                text: "-module(one).\n-compile(no_auto_import).\n\n\n".to_string(),
            },
            OutputFile {
                path: PathBuf::from("_build/default/lib/the_package/src/one_Person.hrl"),
                text: "-record(person, {name, age}).\n".to_string(),
            },
            OutputFile {
                path: PathBuf::from("_build/default/lib/the_package/src/two.erl"),
                text: "-module(two).\n-compile(no_auto_import).\n\n\n".to_string(),
            },
        ]),
    );

    // Imported type constructors have the correct arity
    assert_erlang_compile!(
        vec![
            Source {
                origin: Origin::Src,
                path: PathBuf::from("/src/one.gleam"),
                name: "one".to_string(),
                code: "pub type T(x) { C(a: Int, b: Int) }".to_string(),
            },
            Source {
                origin: Origin::Src,
                path: PathBuf::from("/src/two.gleam"),
                name: "two".to_string(),
                code: "import one.{C} fn main() { C }".to_string(),
            },
        ],
        Ok(vec![
            OutputFile {
                path: PathBuf::from("_build/default/lib/the_package/src/one.erl"),
                text: "-module(one).\n-compile(no_auto_import).\n\n\n".to_string(),
            },
            OutputFile {
                path: PathBuf::from("_build/default/lib/the_package/src/one_C.hrl"),
                text: "-record(c, {a, b}).\n".to_string(),
            },
            OutputFile {
                path: PathBuf::from("_build/default/lib/the_package/src/two.erl"),
                text: "-module(two).\n-compile(no_auto_import).\n
main() ->\n    fun(A, B) -> {c, A, B} end.\n"
                    .to_string(),
            },
        ]),
    );

    // Unqualified and aliased type constructor imports use the correct name
    assert_erlang_compile!(
        vec![
            Source {
                origin: Origin::Src,
                path: PathBuf::from("/src/one.gleam"),
                name: "one".to_string(),
                code: "pub fn id(x) { x } pub type T { X(x: Int) }".to_string(),
            },
            Source {
                origin: Origin::Src,
                path: PathBuf::from("/src/two.gleam"),
                name: "two".to_string(),
                code: "import one.{X as e, id as i} fn make() { i(e) }".to_string(),
            },
        ],
        Ok(vec![
            OutputFile {
                path: PathBuf::from("_build/default/lib/the_package/src/one.erl"),
                text: "-module(one).\n-compile(no_auto_import).\n
-export([id/1]).\n
id(X) ->\n    X.\n"
                    .to_string(),
            },
            OutputFile {
                path: PathBuf::from("_build/default/lib/the_package/src/one_X.hrl"),
                text: "-record(x, {x}).\n".to_string(),
            },
            OutputFile {
                path: PathBuf::from("_build/default/lib/the_package/src/two.erl"),
                text: "-module(two).\n-compile(no_auto_import).\n
make() ->\n    one:id(fun(A) -> {x, A} end).\n"
                    .to_string(),
            },
        ]),
    );

    // Imported type constructors have the correct arity
    assert_erlang_compile!(
        vec![
            Source {
                origin: Origin::Src,
                path: PathBuf::from("/src/one.gleam"),
                name: "one".to_string(),
                code: "pub type T(x) { C(a: Int, b: Int) }".to_string(),
            },
            Source {
                origin: Origin::Src,
                path: PathBuf::from("/src/two.gleam"),
                name: "two".to_string(),
                code: "import one fn main() { one.C }".to_string(),
            },
        ],
        Ok(vec![
            OutputFile {
                path: PathBuf::from("_build/default/lib/the_package/src/one.erl"),
                text: "-module(one).\n-compile(no_auto_import).\n\n\n".to_string(),
            },
            OutputFile {
                path: PathBuf::from("_build/default/lib/the_package/src/one_C.hrl"),
                text: "-record(c, {a, b}).\n".to_string(),
            },
            OutputFile {
                path: PathBuf::from("_build/default/lib/the_package/src/two.erl"),
                text: "-module(two).\n-compile(no_auto_import).\n
main() ->\n    fun(A, B) -> {c, A, B} end.\n"
                    .to_string(),
            },
        ]),
    );

    // A custom type marked as opaque cannot have its constructors accessed
    // from other modules
    assert_erlang_compile!(
        vec![
            Source {
                origin: Origin::Src,
                path: PathBuf::from("/src/one.gleam"),
                name: "one".to_string(),
                code: "pub opaque type T(x) { C(a: Int, b: Int) }".to_string(),
            },
            Source {
                origin: Origin::Src,
                path: PathBuf::from("/src/two.gleam"),
                name: "two".to_string(),
                code: "import one fn main() { one.C }".to_string(),
            },
        ],
        Err(Error::Type {
            path: PathBuf::from("/src/two.gleam"),
            src: "import one fn main() { one.C }".to_string(),
            error: crate::typ::Error::UnknownModuleValue {
                location: crate::ast::SrcSpan { start: 26, end: 28 },
                name: "C".to_string(),
                module_name: vec!["one".to_string(),],
                value_constructors: vec![],
            }
        }),
    );

    // A custom type marked as opaque cannot have its fields accessed
    // from a different module
    assert_erlang_compile!(
        vec![
            Source {
                origin: Origin::Src,
                path: PathBuf::from("/src/one.gleam"),
                name: "one".to_string(),
                code: "pub opaque type T { C(a: Int, b: Int) }".to_string(),
            },
            Source {
                origin: Origin::Src,
                path: PathBuf::from("/src/two.gleam"),
                name: "two".to_string(),
                code: "import one fn test(t: one.T) { t.a }".to_string(),
            },
        ],
        Err(Error::Type {
            path: PathBuf::from("/src/two.gleam"),
            src: "import one fn test(t: one.T) { t.a }".to_string(),
            error: crate::typ::Error::UnknownField {
                location: crate::ast::SrcSpan { start: 32, end: 34 },
                typ: Arc::new(crate::typ::Type::App {
                    public: true,
                    module: vec!["one".to_string(),],
                    name: "T".to_string(),
                    args: vec![],
                }),
                label: "a".to_string(),
                fields: vec![],
            }
        }),
    );

    // Import cycles between modules are not allowed
    assert_erlang_compile!(
        vec![
            Source {
                origin: Origin::Src,
                path: PathBuf::from("/src/one.gleam"),
                name: "one".to_string(),
                code: "import two".to_string(),
            },
            Source {
                origin: Origin::Src,
                path: PathBuf::from("/src/two.gleam"),
                name: "two".to_string(),
                code: "import three".to_string(),
            },
            Source {
                origin: Origin::Src,
                path: PathBuf::from("/src/three.gleam"),
                name: "three".to_string(),
                code: "import one".to_string(),
            },
        ],
        Err(Error::ImportCycle {
            modules: vec!["one".to_string(), "three".to_string(), "two".to_string()],
        }),
    );

    // Bug: https://github.com/gleam-lang/gleam/issues/752
    assert_erlang_compile!(
        vec![
            Source {
                origin: Origin::Src,
                path: PathBuf::from("/src/one.gleam"),
                name: "one".to_string(),
                code: "pub type One(a) { One(a) }".to_string(),
            },
            Source {
                origin: Origin::Src,
                path: PathBuf::from("/src/two.gleam"),
                name: "two".to_string(),
                code: "import one.{One} pub type Two(b) { Two(thing: One(Int)) }".to_string(),
            },
        ],
        Ok(vec![
            OutputFile {
                path: PathBuf::from("_build/default/lib/the_package/src/one.erl",),
                text: "-module(one).\n-compile(no_auto_import).\n\n\n".to_string(),
            },
            OutputFile {
                path: PathBuf::from("_build/default/lib/the_package/src/two.erl",),
                text: "-module(two).\n-compile(no_auto_import).\n\n\n".to_string(),
            },
            OutputFile {
                path: PathBuf::from("_build/default/lib/the_package/src/two_Two.hrl",),
                text: "-record(two, {thing}).\n".to_string(),
            },
        ]),
    );
}

#[test]
// Discriminate between imported functions, and fields, when shadowing a name:
// https://github.com/gleam-lang/gleam/issues/807
fn variable_module_name_collide() {
    assert_erlang_compile!(
        vec![
            Source {
                origin: Origin::Src,
                path: PathBuf::from("/src/power.gleam"),
                name: "power".to_string(),
                code: "pub type Power { Power(value: Int) } 
pub fn to_int(p: Power) { p.value * 9000 }"
                    .to_string(),
            },
            Source {
                origin: Origin::Src,
                path: PathBuf::from("/src/main.gleam"),
                name: "main".to_string(),
                code: "import power.{Power}
pub fn main(power: Power) { power.to_int(power) }"
                    .to_string(),
            },
        ],
        Ok(vec![
            OutputFile {
                path: PathBuf::from("_build/default/lib/the_package/src/main.erl",),
                text: "-module(main).\n-compile(no_auto_import).\n\n-export([main/1]).\n
main(Power) ->\n    power:to_int(Power).\n"
                    .to_string(),
            },
            OutputFile {
                path: PathBuf::from("_build/default/lib/the_package/src/power.erl",),
                text: "-module(power).\n-compile(no_auto_import).\n\n-export([to_int/1]).\n
to_int(P) ->\n    erlang:element(2, P) * 9000.\n"
                    .to_string(),
            },
            OutputFile {
                path: PathBuf::from("_build/default/lib/the_package/src/power_Power.hrl",),
                text: "-record(power, {value}).\n".to_string(),
            },
        ]),
    );
}

#[test]
fn imported_module_consts() {
    assert_erlang_compile!(
        vec![
            Source {
                origin: Origin::Src,
                path: PathBuf::from("/src/one.gleam"),
                name: "one".to_string(),
                code: "pub type Test { A }".to_string(),
            },
            Source {
                origin: Origin::Src,
                path: PathBuf::from("/src/two.gleam"),
                name: "two".to_string(),
                code: "import one
pub const test = one.A
fn x() { test }"
                    .to_string(),
            },
        ],
        Ok(vec![
            OutputFile {
                path: PathBuf::from("_build/default/lib/the_package/src/one.erl"),
                text: "-module(one).\n-compile(no_auto_import).\n\n\n".to_string(),
            },
            OutputFile {
                path: PathBuf::from("_build/default/lib/the_package/src/two.erl"),
                text: "-module(two).\n-compile(no_auto_import).\n
x() ->\n    a.\n"
                    .to_string(),
            }
        ]),
    );

    assert_erlang_compile!(
        vec![
            Source {
                origin: Origin::Src,
                path: PathBuf::from("/src/one.gleam"),
                name: "one".to_string(),
                code: "pub type A { A } pub type B { B(A) }".to_string(),
            },
            Source {
                origin: Origin::Src,
                path: PathBuf::from("/src/two.gleam"),
                name: "two".to_string(),
                code: "import one
pub const test = one.B(one.A)
fn x() { test }"
                    .to_string(),
            },
        ],
        Ok(vec![
            OutputFile {
                path: PathBuf::from("_build/default/lib/the_package/src/one.erl"),
                text: "-module(one).\n-compile(no_auto_import).\n\n\n".to_string(),
            },
            OutputFile {
                path: PathBuf::from("_build/default/lib/the_package/src/two.erl"),
                text: "-module(two).\n-compile(no_auto_import).\n
x() ->\n    {b, a}.\n"
                    .to_string(),
            }
        ]),
    );

    assert_erlang_compile!(
        vec![
            Source {
                origin: Origin::Src,
                path: PathBuf::from("/src/one.gleam"),
                name: "one".to_string(),
                code: "".to_string(),
            },
            Source {
                origin: Origin::Src,
                path: PathBuf::from("/src/two.gleam"),
                name: "two".to_string(),
                code: "import one
pub const test = one.A
fn x() { test }"
                    .to_string(),
            },
        ],
        Err(Error::Type {
            path: PathBuf::from("/src/two.gleam"),
            src: "import one
pub const test = one.A
fn x() { test }"
                .to_string(),
            error: typ::Error::UnknownModuleValue {
                location: SrcSpan { start: 28, end: 33 },
                module_name: vec!["one".to_string()],
                name: "A".to_string(),
                value_constructors: vec![]
            }
        })
    );
}

#[test]
fn config_compilation_test() {
    macro_rules! assert_config_compile {
        ($config:expr, $sources:expr, $expected_output:expr $(,)?) => {
            let config = $config;
            let mut modules = HashMap::new();
            let root = ProjectRoot::new(PathBuf::new());
            let (file_writer, file_receiver) = FilesChannel::new();
            let options = package_compiler::Options {
                name: config.name.clone(),
                src_path: PathBuf::from("src"),
                out_path: PathBuf::from("out"),
                test_path: None,
            };
            let mut compiler = PackageCompiler::new(options, file_writer.clone());
            compiler.sources = $sources;
            let compiled = compiler
                .compile(&mut modules, &mut HashMap::with_capacity(4))
                .expect("Should compile OK");
            codegen::ErlangApp::new(&PathBuf::from("_build/default/lib/the_package/ebin"))
                .render(&file_writer, &config, compiled.modules.as_slice())
                .unwrap();
            let mut outputs = FilesChannel::recv_utf8_files(&file_receiver).unwrap();
            outputs.sort_by(|a, b| a.path.partial_cmp(&b.path).unwrap());
            assert_eq!($expected_output, outputs);
        };
    };

    fn make_config() -> PackageConfig {
        PackageConfig {
            dependencies: HashMap::new(),
            description: "".to_string(),
            version: "1.0.0".to_string(),
            name: "the_package".to_string(),
            repository: Repository::None,
            docs: Default::default(),
            otp_start_module: None,
            tool: BuildTool::Gleam,
        }
    }

    assert_config_compile!(
        make_config(),
        vec![],
        vec![OutputFile {
            text: r#"{application, the_package, [
    {vsn, "1.0.0"},
    {applications, []},
    {description, ""},
    {modules, []},
    {registered, []},
]}.
"#
            .to_string(),
            path: PathBuf::from("_build/default/lib/the_package/ebin/the_package.app"),
        }]
    );

    // Version is included if given
    let mut config = make_config();
    config.version = "1.3.5".to_string();
    assert_config_compile!(
        config,
        vec![],
        vec![OutputFile {
            text: r#"{application, the_package, [
    {vsn, "1.3.5"},
    {applications, []},
    {description, ""},
    {modules, []},
    {registered, []},
]}.
"#
            .to_string(),
            path: PathBuf::from("_build/default/lib/the_package/ebin/the_package.app"),
        }]
    );

    // We can specify a description
    let mut config = make_config();
    config.description = "Very exciting".to_string();
    assert_config_compile!(
        config,
        vec![],
        vec![OutputFile {
            text: r#"{application, the_package, [
    {vsn, "1.0.0"},
    {applications, []},
    {description, "Very exciting"},
    {modules, []},
    {registered, []},
]}.
"#
            .to_string(),
            path: PathBuf::from("_build/default/lib/the_package/ebin/the_package.app"),
        }]
    );

    // Deps applications are listed
    let mut config = make_config();
    config.dependencies = [
        ("gleam_stdlib", "1.0.0"),
        ("gleam_otp", "1.0.0"),
        ("midas", "1.0.0"),
        ("simple_json", "1.0.0"),
    ]
    .into_iter()
    .map(|(a, b)| (a.to_string(), b.to_string()))
    .collect();
    assert_config_compile!(
        config,
        vec![],
        vec![OutputFile {
            text: r#"{application, the_package, [
    {vsn, "1.0.0"},
    {applications, [gleam_otp,
                    gleam_stdlib,
                    midas,
                    simple_json]},
    {description, ""},
    {modules, []},
    {registered, []},
]}.
"#
            .to_string(),
            path: PathBuf::from("_build/default/lib/the_package/ebin/the_package.app"),
        }]
    );
}

fn normalise_error(e: Error) -> Error {
    match e {
        Error::ImportCycle { mut modules } => {
            modules.sort();
            Error::ImportCycle { modules }
        }
        e => e,
    }
}
