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
            text: "-module(one).
-compile(no_auto_import).


"
            .to_string(),
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
                text: "-module(one).
-compile(no_auto_import).


"
                .to_string(),
                path: PathBuf::from("_build/default/lib/the_package/src/one.erl"),
            },
            OutputFile {
                text: "-module(two).
-compile(no_auto_import).


"
                .to_string(),
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
            text: "-module(one).
-compile(no_auto_import).


"
            .to_string(),
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
                text: "-module(one).
-compile(no_auto_import).


"
                .to_string(),
            },
            OutputFile {
                path: PathBuf::from("_build/default/lib/the_package/src/two.erl"),
                text: "-module(two).
-compile(no_auto_import).


"
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
                path: PathBuf::from("/src/two.gleam"),
                name: "two".to_string(),
                code: "import one".to_string(),
            },
        ],
        Ok(vec![
            OutputFile {
                path: PathBuf::from("_build/default/lib/the_package/src/one.erl"),
                text: "-module(one).
-compile(no_auto_import).


"
                .to_string(),
            },
            OutputFile {
                path: PathBuf::from("_build/default/lib/the_package/src/two.erl"),
                text: "-module(two).
-compile(no_auto_import).


"
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
                text: "-module(one).
-compile(no_auto_import).

-export_type([box/0]).

-type box() :: {box, integer()}.


"
                .to_string(),
            },
            OutputFile {
                path: PathBuf::from("_build/default/lib/the_package/src/two.erl"),
                text: "-module(two).
-compile(no_auto_import).

-export([unbox/1]).

-spec unbox(one:box()) -> integer().
unbox(X) ->
    {box, I} = X,
    I.
"
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
                text: "-module(one).
-compile(no_auto_import).

-export_type([box/0]).

-type box() :: {box, integer()}.


"
                .to_string(),
            },
            OutputFile {
                path: PathBuf::from("_build/default/lib/the_package/src/two.erl"),
                text: "-module(two).
-compile(no_auto_import).

-export([box/1]).

-spec box(integer()) -> one:box().
box(X) ->
    {box, X}.
"
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
            text: "-module(one@two).
-compile(no_auto_import).

-export_type([box/0]).

-type box() :: box.


"
            .to_string(),
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
                text: "-module(one).
-compile(no_auto_import).

-export_type([box/0]).

-type box() :: box.


"
                .to_string(),
            },
            OutputFile {
                path: PathBuf::from("_build/default/lib/the_package/src/two.erl"),
                text: "-module(two).
-compile(no_auto_import).

-export([box/0]).

-spec box() -> one:box().
box() ->
    box.
"
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
                text: "-module(one).
-compile(no_auto_import).

-export([go/0]).

-spec go() -> integer().
go() ->
    1.
"
                .to_string(),
            },
            OutputFile {
                path: PathBuf::from("_build/default/lib/the_package/src/two.erl"),
                text: "-module(two).
-compile(no_auto_import).

-export([call/0]).

-spec call() -> integer().
call() ->
    one:go().
"
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
                code: "import nested/one
pub fn go(x) { let one.Box(y) = x y }"
                    .to_string(),
            },
        ],
        Ok(vec![
            OutputFile {
                path: PathBuf::from("_build/default/lib/the_package/src/nested@one.erl"),
                text: "-module(nested@one).
-compile(no_auto_import).

-export_type([box/0]).

-type box() :: {box, integer()}.


"
                .to_string(),
            },
            OutputFile {
                path: PathBuf::from("_build/default/lib/the_package/src/two.erl"),
                text: "-module(two).
-compile(no_auto_import).

-export([go/1]).

-spec go(nested@one:box()) -> integer().
go(X) ->
    {box, Y} = X,
    Y.
"
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
                code: "import nested/one as thingy
pub fn go(x) { let thingy.Box(y) = x y }"
                    .to_string(),
            },
        ],
        Ok(vec![
            OutputFile {
                path: PathBuf::from("_build/default/lib/the_package/src/nested@one.erl"),
                text: "-module(nested@one).
-compile(no_auto_import).

-export_type([box/0]).

-type box() :: {box, integer()}.


"
                .to_string(),
            },
            OutputFile {
                path: PathBuf::from("_build/default/lib/the_package/src/two.erl"),
                text: "-module(two).
-compile(no_auto_import).

-export([go/1]).

-spec go(nested@one:box()) -> integer().
go(X) ->
    {box, Y} = X,
    Y.
"
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
                text: "-module(nested@one).
-compile(no_auto_import).

-export([go/0]).
-export_type([thing/0]).

-type thing() :: any().

-spec go() -> integer().
go() ->
    1.
"
                .to_string(),
            },
            OutputFile {
                path: PathBuf::from("_build/default/lib/the_package/src/two.erl"),
                text: "-module(two).
-compile(no_auto_import).

-export([go/0, thing/0, call_thing/0]).

-spec go() -> integer().
go() ->
    nested@one:go().

-spec thing() -> nested@one:thing().
thing() ->
    thing:new().

-spec call_thing() -> nested@one:thing().
call_thing() ->
    thing:new().
"
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
                text: "-module(one).
-compile(no_auto_import).

-export_type([point/0]).

-type point() :: {point, integer(), integer()}.


"
                .to_string(),
            },
            OutputFile {
                path: PathBuf::from("_build/default/lib/the_package/src/one_Point.hrl"),
                text: "-record(point, {x, y}).
"
                .to_string(),
            },
            OutputFile {
                path: PathBuf::from("_build/default/lib/the_package/src/two.erl"),
                text: "-module(two).
-compile(no_auto_import).

-spec make() -> one:point().
make() ->
    {point, 1, 4}.

-spec x(one:point()) -> integer().
x(P) ->
    {point, X, _} = P,
    X.
"
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
                text: "-module(one).
-compile(no_auto_import).

-export([\'div\'/2]).

-spec \'div\'(integer(), integer()) -> integer().
'div'(X, Y) ->
    case Y of
        0 -> 0;
        Gleam@denominator -> X rem Gleam@denominator
    end.
"
                .to_string(),
            },
            OutputFile {
                path: PathBuf::from("_build/default/lib/the_package/src/two.erl"),
                text: "-module(two).
-compile(no_auto_import).

-spec run() -> integer().
run() ->
    one:'div'(2, one:'div'(2, 4)).
"
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
                text: "-module(one).
-compile(no_auto_import).

-export_type([empty/0]).\n\n-type empty() :: empty.


"
                .to_string(),
            },
            OutputFile {
                path: PathBuf::from("_build/default/lib/the_package/src/two.erl"),
                text: "-module(two).
-compile(no_auto_import).

-spec make() -> one:empty().
make() ->
    empty.
"
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
                text: "-module(one).
-compile(no_auto_import).

-export([id/1]).
-export_type([empty/0]).

-type empty() :: empty.

-spec id(H) -> H.
id(X) ->
    X.
"
                .to_string(),
            },
            OutputFile {
                path: PathBuf::from("_build/default/lib/the_package/src/two.erl"),
                text: "-module(two).
-compile(no_auto_import).

-spec make() -> one:empty().
make() ->
    one:id(empty).
"
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
                text: "-module(one).
-compile(no_auto_import).

-export([id/1]).
-export_type([empty/0]).

-type empty() :: empty.

-spec id(H) -> H.
id(X) ->
    X.
"
                .to_string(),
            },
            OutputFile {
                path: PathBuf::from("_build/default/lib/the_package/src/two.erl"),
                text: "-module(two).
-compile(no_auto_import).

-spec make() -> one:empty().
make() ->
    one:id(empty).
"
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
                text: "-module(one).
-compile(no_auto_import).

-export(['receive'/0]).

-spec \'receive\'() -> integer().
'receive'() ->
    1.
"
                .to_string(),
            },
            OutputFile {
                path: PathBuf::from("_build/default/lib/the_package/src/two.erl"),
                text: "-module(two).
-compile(no_auto_import).

-spec funky() -> fun(() -> integer()).
funky() ->
    fun one:'receive'/0.
"
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
                text: "-module(one).
-compile(no_auto_import).

-export(['receive'/0]).

-spec \'receive\'() -> integer().
'receive'() ->
    1.
"
                .to_string(),
            },
            OutputFile {
                path: PathBuf::from("_build/default/lib/the_package/src/two.erl"),
                text: "-module(two).
-compile(no_auto_import).

-spec funky() -> fun(() -> integer()).
funky() ->
    fun one:'receive'/0.
"
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
                text: "-module(one).
-compile(no_auto_import).

-export(['receive'/1]).

-spec \'receive\'(H) -> H.
'receive'(X) ->
    X.
"
                .to_string(),
            },
            OutputFile {
                path: PathBuf::from("_build/default/lib/the_package/src/two.erl"),
                text: "-module(two).
-compile(no_auto_import).

-spec funky() -> integer().
funky() ->
    one:'receive'(1).
"
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
                text: "-module(one).
-compile(no_auto_import).

-export_type([person/0]).

-type person() :: {person, binary(), integer()}.


"
                .to_string(),
            },
            OutputFile {
                path: PathBuf::from("_build/default/lib/the_package/src/one_Person.hrl"),
                text: "-record(person, {name, age}).
"
                .to_string(),
            },
            OutputFile {
                path: PathBuf::from("_build/default/lib/the_package/src/two.erl"),
                text: "-module(two).
-compile(no_auto_import).

-export([get_age/1, get_name/1]).

-spec get_age(one:person()) -> integer().
get_age(Person) ->
    erlang:element(3, Person).

-spec get_name(one:person()) -> binary().
get_name(Person) ->
    erlang:element(2, Person).
"
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
                text: "-module(one).
-compile(no_auto_import).

-export_type([person/0]).

-type person() :: {person, binary(), integer()}.


"
                .to_string(),
            },
            OutputFile {
                path: PathBuf::from("_build/default/lib/the_package/src/one_Person.hrl"),
                text: "-record(person, {name, age}).
"
                .to_string(),
            },
            OutputFile {
                path: PathBuf::from("_build/default/lib/the_package/src/two.erl"),
                text: "-module(two).
-compile(no_auto_import).


"
                .to_string(),
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
                text: "-module(one).
-compile(no_auto_import).

-export_type([person/0]).\n\n-type person() :: {person, binary(), integer()}.


"
                .to_string(),
            },
            OutputFile {
                path: PathBuf::from("_build/default/lib/the_package/src/one_Person.hrl"),
                text: "-record(person, {name, age}).
"
                .to_string(),
            },
            OutputFile {
                path: PathBuf::from("_build/default/lib/the_package/src/two.erl"),
                text: "-module(two).
-compile(no_auto_import).


"
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
                code: "import one.{C} fn main() { C }".to_string(),
            },
        ],
        Ok(vec![
            OutputFile {
                path: PathBuf::from("_build/default/lib/the_package/src/one.erl"),
                text: "-module(one).
-compile(no_auto_import).

-export_type([t/1]).\n\n-type t(H) :: {c, integer(), integer()} | {gleam_phantom, H}.


"
                .to_string(),
            },
            OutputFile {
                path: PathBuf::from("_build/default/lib/the_package/src/one_C.hrl"),
                text: "-record(c, {a, b}).
"
                .to_string(),
            },
            OutputFile {
                path: PathBuf::from("_build/default/lib/the_package/src/two.erl"),
                text: "-module(two).
-compile(no_auto_import).

-spec main() -> fun((integer(), integer()) -> one:t(any())).
main() ->
    fun(A, B) -> {c, A, B} end.
"
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
                text: "-module(one).
-compile(no_auto_import).

-export([id/1]).
-export_type([t/0]).

-type t() :: {x, integer()}.

-spec id(H) -> H.
id(X) ->
    X.
"
                .to_string(),
            },
            OutputFile {
                path: PathBuf::from("_build/default/lib/the_package/src/one_X.hrl"),
                text: "-record(x, {x}).
"
                .to_string(),
            },
            OutputFile {
                path: PathBuf::from("_build/default/lib/the_package/src/two.erl"),
                text: "-module(two).
-compile(no_auto_import).

-spec make() -> fun((integer()) -> one:t()).
make() ->
    one:id(fun(A) -> {x, A} end).
"
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
                text: "-module(one).
-compile(no_auto_import).

-export_type([t/1]).

-type t(H) :: {c, integer(), integer()} | {gleam_phantom, H}.


"
                .to_string(),
            },
            OutputFile {
                path: PathBuf::from("_build/default/lib/the_package/src/one_C.hrl"),
                text: "-record(c, {a, b}).
"
                .to_string(),
            },
            OutputFile {
                path: PathBuf::from("_build/default/lib/the_package/src/two.erl"),
                text: "-module(two).
-compile(no_auto_import).

-spec main() -> fun((integer(), integer()) -> one:t(any())).
main() ->
    fun(A, B) -> {c, A, B} end.
"
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
                text: "-module(one).
-compile(no_auto_import).

-export_type([one/1]).

-type one(H) :: {one, H}.


"
                .to_string(),
            },
            OutputFile {
                path: PathBuf::from("_build/default/lib/the_package/src/two.erl",),
                text: "-module(two).
-compile(no_auto_import).

-export_type([two/1]).

-type two(P) :: {two, one:one(integer())} | {gleam_phantom, P}.


"
                .to_string(),
            },
            OutputFile {
                path: PathBuf::from("_build/default/lib/the_package/src/two_Two.hrl",),
                text: "-record(two, {thing}).
"
                .to_string(),
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
                text: "-module(main).
-compile(no_auto_import).

-export([main/1]).

-spec main(power:power()) -> integer().
main(Power) ->
    power:to_int(Power).
"
                .to_string(),
            },
            OutputFile {
                path: PathBuf::from("_build/default/lib/the_package/src/power.erl",),
                text: "-module(power).
-compile(no_auto_import).

-export([to_int/1]).
-export_type([power/0]).

-type power() :: {power, integer()}.

-spec to_int(power()) -> integer().
to_int(P) ->
    erlang:element(2, P) * 9000.
"
                .to_string(),
            },
            OutputFile {
                path: PathBuf::from("_build/default/lib/the_package/src/power_Power.hrl",),
                text: "-record(power, {value}).
"
                .to_string(),
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
                text: "-module(one).
-compile(no_auto_import).

-export_type([test/0]).

-type test() :: a.


"
                .to_string(),
            },
            OutputFile {
                path: PathBuf::from("_build/default/lib/the_package/src/two.erl"),
                text: "-module(two).
-compile(no_auto_import).

-spec x() -> one:test().
x() ->
    a.
"
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
                text: "-module(one).
-compile(no_auto_import).

-export_type([a/0, b/0]).

-type a() :: a.

-type b() :: {b, a()}.


"
                .to_string(),
            },
            OutputFile {
                path: PathBuf::from("_build/default/lib/the_package/src/two.erl"),
                text: "-module(two).
-compile(no_auto_import).

-spec x() -> one:b().
x() ->
    {b, a}.
"
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
fn imported_type_constructor_used_as_function() {
    assert_erlang_compile!(
        vec![
            Source {
                origin: Origin::Src,
                path: PathBuf::from("/src/one.gleam"),
                name: "one".to_string(),
                code: "pub type A { A(String) }".to_string(),
            },
            Source {
                origin: Origin::Src,
                path: PathBuf::from("/src/two.gleam"),
                name: "two".to_string(),
                code: "import one
fn x() { one.A }"
                    .to_string(),
            },
        ],
        Ok(vec![
            OutputFile {
                path: PathBuf::from("_build/default/lib/the_package/src/one.erl"),
                text: "-module(one).
-compile(no_auto_import).

-export_type([a/0]).

-type a() :: {a, binary()}.


"
                .to_string(),
            },
            OutputFile {
                path: PathBuf::from("_build/default/lib/the_package/src/two.erl"),
                text: "-module(two).
-compile(no_auto_import).

-spec x() -> fun((binary()) -> one:a()).
x() ->
    fun(A) -> {a, A} end.
"
                .to_string(),
            }
        ]),
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
