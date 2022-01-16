use super::*;
use crate::io::OutputFile;
use std::sync::Arc;

use pretty_assertions::assert_eq;

#[test]
fn compile_test() {
    struct Case {
        input: Vec<Input>,
        expected: Result<Vec<OutputFile>, Error>,
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
                OutputFile {
                    path: PathBuf::from("/gen/src/two.erl"),
                    text: "-module(two).\n".to_string(),
                },
                OutputFile {
                    path: PathBuf::from("/gen/src/one.erl"),
                    text: "-module(one).\n".to_string(),
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
            expected: Ok(vec![OutputFile {
                path: PathBuf::from("/gen/test/one.erl"),
                text: "-module(one).\n".to_string(),
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
                location: SrcSpan { start: 7, end: 10 },
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
                OutputFile {
                    path: PathBuf::from("/gen/src/two.erl"),
                    text: "-module(two).\n".to_string(),
                },
                OutputFile {
                    path: PathBuf::from("/gen/src/one.erl"),
                    text: "-module(one).\n".to_string(),
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
                OutputFile {
                    path: PathBuf::from("/gen/src/one.erl"),
                    text: "-module(one).\n".to_string(),
                },
                OutputFile {
                    path: PathBuf::from("/gen/src/two.erl"),
                    text: "-module(two).\n".to_string(),
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
                OutputFile {
                    path: PathBuf::from("/gen/src/one.erl"),
                    text: "-module(one).
-compile(no_auto_import).

-export_type([box/0]).

-type box() :: {box, integer()}.


".to_string(),
                },
                OutputFile {
                    path: PathBuf::from("/gen/src/two.erl"),
                    text: "-module(two).
-compile(no_auto_import).

-export([unbox/1]).

-spec unbox(one:box()) -> integer().
unbox(X) ->
    {box, I} = X,
    I.
".to_string(),
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
                OutputFile {
                    path: PathBuf::from("/gen/src/one.erl"),
                    text: "-module(one).
-compile(no_auto_import).

-export_type([box/0]).

-type box() :: {box, integer()}.


".to_string(),
                },
                OutputFile {
                    path: PathBuf::from("/gen/src/two.erl"),
                    text: "-module(two).
-compile(no_auto_import).

-export([box/1]).

-spec box(integer()) -> one:box().
box(X) ->\n    {box, X}.
".to_string(),
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
            expected: Ok(vec![OutputFile {
                path: PathBuf::from("/gen/src/one@two.erl"),
                text: "-module(one@two).
-compile(no_auto_import).

-export_type([box/0]).

-type box() :: box.


".to_string(),
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
                OutputFile {
                    path: PathBuf::from("/gen/src/one.erl"),
                    text: "-module(one).
-compile(no_auto_import).

-export_type([box/0]).\n\n-type box() :: box.


".to_string(),
                },
                OutputFile {
                    path: PathBuf::from("/gen/src/two.erl"),
                    text: "-module(two).
-compile(no_auto_import).

-export([box/0]).

-spec box() -> one:box().
box() ->\n    box.
".to_string(),
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
                OutputFile {
                    path: PathBuf::from("/gen/src/one.erl"),
                    text: "-module(one).
-compile(no_auto_import).

-export([go/0]).

-spec go() -> integer().
go() ->
    1.
".to_string(),
                },
                OutputFile {
                    path: PathBuf::from("/gen/src/two.erl"),
                    text: "-module(two).
-compile(no_auto_import).

-export([call/0]).

-spec call() -> integer().
call() ->
    one:go().
".to_string(),
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
                OutputFile {
                    path: PathBuf::from("/gen/src/nested@one.erl"),
                    text: "-module(nested@one).
-compile(no_auto_import).

-export_type([box/0]).

-type box() :: {box, integer()}.


".to_string(),
                },
                OutputFile {
                    path: PathBuf::from("/gen/src/two.erl"),
                    text: "-module(two).
-compile(no_auto_import).

-export([go/1]).

-spec go(nested@one:box()) -> integer().
go(X) ->
    {box, Y} = X,
    Y.
".to_string(),
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
                OutputFile {
                    path: PathBuf::from("/gen/src/nested@one.erl"),
                    text: "-module(nested@one).
-compile(no_auto_import).

-export_type([box/0]).

-type box() :: {box, integer()}.


".to_string(),
                },
                OutputFile {
                    path: PathBuf::from("/gen/src/two.erl"),
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
                        pub external fn thing() -> one.Thing = \"thing\" \"new\"
                        pub fn call_thing() { thing() }
                        "
                        .to_string(),
                },
            ],
            expected: Ok(vec![
                OutputFile {
                    path: PathBuf::from("/gen/src/nested@one.erl"),
                    text: "-module(nested@one).
-compile(no_auto_import).

-export([go/0]).
-export_type([thing/0]).

-type thing() :: any().

-spec go() -> integer().
go() ->
    1.
".to_string(),
                },
                OutputFile {
                    path: PathBuf::from("/gen/src/two.erl"),
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
".to_string(),
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
                        pub fn make() { one.Point(1, 4) }
                        fn x(p) { let one.Point(x, _) = p x }"
                        .to_string(),
                },
            ],
            expected: Ok(vec![
                OutputFile {
                    path: PathBuf::from("/gen/src/one_Point.hrl"),
                    text: "-record(point, {x :: integer(), y :: integer()}).\n".to_string(),
                },
                OutputFile {
                    path: PathBuf::from("/gen/src/one.erl"),
                    text: "-module(one).
-compile(no_auto_import).

-export_type([point/0]).

-type point() :: {point, integer(), integer()}.


".to_string(),
                },
                OutputFile {
                    path: PathBuf::from("/gen/src/two.erl"),
                    text: "-module(two).
-compile(no_auto_import).

-export([make/0]).

-spec make() -> one:point().
make() ->
    {point, 1, 4}.

-spec x(one:point()) -> integer().
x(P) ->
    {point, X, _@1} = P,
    X.
".to_string(),
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
                    pub fn run() { 2 |> div(top: _, bottom: 4) |> div(2, bottom: _) }"
                        .to_string(),
                },
            ],
            expected: Ok(vec![
                OutputFile {
                    path: PathBuf::from("/gen/src/one.erl"),
                    text: "-module(one).
-compile(no_auto_import).

-export([\'div\'/2]).

-spec \'div\'(integer(), integer()) -> integer().
\'div\'(X, Y) ->
    case Y of
        0 -> 0;
        Gleam@denominator -> X div Gleam@denominator
    end.
".to_string(),
                },
                OutputFile {
                    path: PathBuf::from("/gen/src/two.erl"),
                    text: "-module(two).
-compile(no_auto_import).

-export([run/0]).

-spec run() -> integer().
run() ->
    _pipe = 2,
    _pipe@1 = one:\'div\'(_pipe, 4),
    one:\'div\'(2, _pipe@1).
".to_string(),
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
                        pub fn make() { one.Empty }"
                        .to_string(),
                },
            ],
            expected: Ok(vec![
                OutputFile {
                    path: PathBuf::from("/gen/src/one.erl"),
                    text: "-module(one).
-compile(no_auto_import).

-export_type([empty/0]).

-type empty() :: empty.


".to_string(),
                },
                OutputFile {
                    path: PathBuf::from("/gen/src/two.erl"),
                    text: "-module(two).
-compile(no_auto_import).

-export([make/0]).

-spec make() -> one:empty().
make() ->
    empty.
".to_string(),
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
                    src: "import one.{Empty, id} pub fn make() { id(Empty) }".to_string(),
                },
            ],
            expected: Ok(vec![
                OutputFile {
                    path: PathBuf::from("/gen/src/one.erl"),
                    text: "-module(one).
-compile(no_auto_import).

-export([id/1]).
-export_type([empty/0]).

-type empty() :: empty.

-spec id(I) -> I.
id(X) ->
    X.
".to_string(),
                },
                OutputFile {
                    path: PathBuf::from("/gen/src/two.erl"),
                    text: "-module(two).
-compile(no_auto_import).

-export([make/0]).

-spec make() -> one:empty().
make() ->
    one:id(empty).
".to_string(),
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
                    src: "import one.{Empty as E, id as i} pub fn make() { i(E) }".to_string(),
                },
            ],
            expected: Ok(vec![
                OutputFile {
                    path: PathBuf::from("/gen/src/one.erl"),
                    text: "-module(one).
-compile(no_auto_import).

-export([id/1]).
-export_type([empty/0]).

-type empty() :: empty.

-spec id(I) -> I.
id(X) ->
    X.
".to_string(),
                },
                OutputFile {
                    path: PathBuf::from("/gen/src/two.erl"),
                    text: "-module(two).
-compile(no_auto_import).

-export([make/0]).

-spec make() -> one:empty().
make() ->
    one:id(empty).
".to_string(),
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
                    src: "import one pub fn funky() { one.receive }".to_string(),
                },
            ],
            expected: Ok(vec![
                OutputFile {
                    path: PathBuf::from("/gen/src/one.erl"),
                    text: "-module(one).
-compile(no_auto_import).

-export([\'receive\'/0]).

-spec \'receive\'() -> integer().
\'receive\'() ->
    1.
".to_string(),
                },
                OutputFile {
                    path: PathBuf::from("/gen/src/two.erl"),
                    text: "-module(two).
-compile(no_auto_import).

-export([funky/0]).

-spec funky() -> fun(() -> integer()).
funky() ->
    fun one:\'receive\'/0.
".to_string(),
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
                    src: "import one.{receive} pub fn funky() { receive }".to_string(),
                },
            ],
            expected: Ok(vec![
                OutputFile {
                    path: PathBuf::from("/gen/src/one.erl"),
                    text: "-module(one).
-compile(no_auto_import).

-export([\'receive\'/0]).

-spec \'receive\'() -> integer().
\'receive\'() ->
    1.
".to_string(),
                },
                OutputFile {
                    path: PathBuf::from("/gen/src/two.erl"),
                    text: "-module(two).
-compile(no_auto_import).

-export([funky/0]).

-spec funky() -> fun(() -> integer()).
funky() ->
    fun one:\'receive\'/0.
".to_string(),
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
                    src: "import one pub fn funky() { one.receive(1) }".to_string(),
                },
            ],
            expected: Ok(vec![
                OutputFile {
                    path: PathBuf::from("/gen/src/one.erl"),
                    text: "-module(one).
-compile(no_auto_import).

-export([\'receive\'/1]).

-spec \'receive\'(I) -> I.
'receive\'(X) ->
    X.
".to_string(),
                },
                OutputFile {
                    path: PathBuf::from("/gen/src/two.erl"),
                    text: "-module(two).
-compile(no_auto_import).

-export([funky/0]).

-spec funky() -> integer().
funky() ->
    one:\'receive\'(1).
".to_string(),
                },
            ]),
        },

        // We can use record accessors for types with only one constructor, defined in another
        // module
        Case {
            input: vec![
                Input {
                    origin: ModuleOrigin::Src,
                    path: PathBuf::from("/src/one.gleam"),
                    source_base_path: PathBuf::from("/src"),
                    src: "pub type Person { Person(name: String, age: Int) }".to_string(),
                },
                Input {
                    origin: ModuleOrigin::Src,
                    path: PathBuf::from("/src/two.gleam"),
                    source_base_path: PathBuf::from("/src"),
                    src: "import one.{Person}
pub fn get_age(person: Person) { person.age }
pub fn get_name(person: Person) { person.name }"
                        .to_string(),
                },
            ],
            expected: Ok(vec![
                OutputFile {
                    path: PathBuf::from("/gen/src/one_Person.hrl"),
                    text: "-record(person, {name :: binary(), age :: integer()}).\n".to_string(),
                },
                OutputFile {
                    path: PathBuf::from("/gen/src/one.erl"),
                    text: "-module(one).
-compile(no_auto_import).

-export_type([person/0]).

-type person() :: {person, binary(), integer()}.


".to_string(),
                },
                OutputFile {
                    path: PathBuf::from("/gen/src/two.erl"),
                    text: "-module(two).
-compile(no_auto_import).

-export([get_age/1, get_name/1]).

-spec get_age(one:person()) -> integer().
get_age(Person) ->
    erlang:element(3, Person).

-spec get_name(one:person()) -> binary().
get_name(Person) ->
    erlang:element(2, Person).
".to_string(),
                },
            ]),
        },

        // Can use imported types in Type Constructors
        Case {
            input: vec![
                Input {
                    origin: ModuleOrigin::Src,
                    path: PathBuf::from("/src/one.gleam"),
                    source_base_path: PathBuf::from("/src"),
                    src: "pub type Person { Person(name: String, age: Int) }".to_string(),
                },
                Input {
                    origin: ModuleOrigin::Src,
                    path: PathBuf::from("/src/two.gleam"),
                    source_base_path: PathBuf::from("/src"),
                    src: "import one
type Two = one.Person"
                        .to_string(),
                },
            ],
            expected: Ok(vec![
                OutputFile {
                    path: PathBuf::from("/gen/src/one_Person.hrl"),
                    text: "-record(person, {name :: binary(), age :: integer()}).\n".to_string(),
                },
                OutputFile {
                    path: PathBuf::from("/gen/src/one.erl"),
                    text: "-module(one).
-compile(no_auto_import).

-export_type([person/0]).

-type person() :: {person, binary(), integer()}.


".to_string(),
                },
                OutputFile {
                    path: PathBuf::from("/gen/src/two.erl"),
                    text: "-module(two).\n".to_string(),
                },
            ]),
        },
        // Can use imported, fully qualified types in Type Constructors
        Case {
            input: vec![
                Input {
                    origin: ModuleOrigin::Src,
                    path: PathBuf::from("/src/one.gleam"),
                    source_base_path: PathBuf::from("/src"),
                    src: "pub type Person { Person(name: String, age: Int) }".to_string(),
                },
                Input {
                    origin: ModuleOrigin::Src,
                    path: PathBuf::from("/src/two.gleam"),
                    source_base_path: PathBuf::from("/src"),
                    src: "import one.{Person}
type Two = Person"
                        .to_string(),
                },
            ],
            expected: Ok(vec![
                OutputFile {
                    path: PathBuf::from("/gen/src/one_Person.hrl"),
                    text: "-record(person, {name :: binary(), age :: integer()}).\n".to_string(),
                },
                OutputFile {
                    path: PathBuf::from("/gen/src/one.erl"),
                    text: "-module(one).
-compile(no_auto_import).

-export_type([person/0]).

-type person() :: {person, binary(), integer()}.


".to_string(),
                },
                OutputFile {
                    path: PathBuf::from("/gen/src/two.erl"),
                    text: "-module(two).\n".to_string(),
                },
            ]),
        },

        // Imported type constructors have the correct arity
        Case {
            input: vec![
                Input {
                    origin: ModuleOrigin::Src,
                    path: PathBuf::from("/src/one.gleam"),
                    source_base_path: PathBuf::from("/src"),
                    src: "pub type T(x) { C(a: Int, b: Int) }".to_string(),
                },
                Input {
                    origin: ModuleOrigin::Src,
                    path: PathBuf::from("/src/two.gleam"),
                    source_base_path: PathBuf::from("/src"),
                    src: "import one.{C}
pub fn main() { C }"
                        .to_string(),
                },
            ],
            expected: Ok(vec![
                OutputFile {
                    path: PathBuf::from("/gen/src/one_C.hrl"),
                    text: "-record(c, {a :: integer(), b :: integer()}).\n".to_string(),
                },
                OutputFile {
                    path: PathBuf::from("/gen/src/one.erl"),
                    text: "-module(one).
-compile(no_auto_import).

-export_type([t/1]).

-type t(I) :: {c, integer(), integer()} | {gleam_phantom, I}.


".to_string(),
                },
                OutputFile {
                    path: PathBuf::from("/gen/src/two.erl"),
                    text: "-module(two).
-compile(no_auto_import).

-export([main/0]).

-spec main() -> fun((integer(), integer()) -> one:t(any())).
main() ->
    fun(A, B) -> {c, A, B} end.
".to_string(),
                },
            ]),
        },

        // Unqualified and aliased type constructor imports use the correct name
        Case {
            input: vec![
                Input {
                    origin: ModuleOrigin::Src,
                    path: PathBuf::from("/src/one.gleam"),
                    source_base_path: PathBuf::from("/src"),
                    src: "pub fn id(x) { x } pub type T { X(x: Int) }".to_string(),
                },
                Input {
                    origin: ModuleOrigin::Src,
                    path: PathBuf::from("/src/two.gleam"),
                    source_base_path: PathBuf::from("/src"),
                    src: "import one.{X as E, id as i} pub fn make() { i(E) }".to_string(),
                },
            ],
            expected: Ok(vec![
                OutputFile {
                    path: PathBuf::from("/gen/src/one_X.hrl"),
                    text: "-record(x, {x :: integer()}).\n".to_string(),

                },
                OutputFile {
                    path: PathBuf::from("/gen/src/one.erl"),
                    text: "-module(one).
-compile(no_auto_import).

-export([id/1]).
-export_type([t/0]).

-type t() :: {x, integer()}.

-spec id(I) -> I.
id(X) ->
    X.
".to_string(),
                },
                OutputFile {
                    path: PathBuf::from("/gen/src/two.erl"),
                    text: "-module(two).
-compile(no_auto_import).

-export([make/0]).

-spec make() -> fun((integer()) -> one:t()).
make() ->
    one:id(fun(A) -> {x, A} end).
".to_string(),
                },
            ]),
        },

        // Imported type constructors have the correct arity
        Case {
            input: vec![
                Input {
                    origin: ModuleOrigin::Src,
                    path: PathBuf::from("/src/one.gleam"),
                    source_base_path: PathBuf::from("/src"),
                    src: "pub type T(x) { C(a: Int, b: Int) }".to_string(),
                },
                Input {
                    origin: ModuleOrigin::Src,
                    path: PathBuf::from("/src/two.gleam"),
                    source_base_path: PathBuf::from("/src"),
                    src: "import one
pub fn main() { one.C }"
                        .to_string(),
                },
            ],
            expected: Ok(vec![
                OutputFile {
                    path: PathBuf::from("/gen/src/one_C.hrl"),
                    text: "-record(c, {a :: integer(), b :: integer()}).\n".to_string(),
                },
                OutputFile {
                    path: PathBuf::from("/gen/src/one.erl"),
                    text: "-module(one).
-compile(no_auto_import).

-export_type([t/1]).

-type t(I) :: {c, integer(), integer()} | {gleam_phantom, I}.


".to_string(),
                },
                OutputFile {
                    path: PathBuf::from("/gen/src/two.erl"),
                    text: "-module(two).
-compile(no_auto_import).

-export([main/0]).

-spec main() -> fun((integer(), integer()) -> one:t(any())).
main() ->
    fun(A, B) -> {c, A, B} end.
".to_string(),
                },
            ]),
        },

        // A custom type marked as opaque cannot have its constructors accessed
        // from other modules
        Case {
            input: vec![
                Input {
                    origin: ModuleOrigin::Src,
                    path: PathBuf::from("/src/one.gleam"),
                    source_base_path: PathBuf::from("/src"),
                    src: "pub opaque type T(x) { C(a: Int, b: Int) }".to_string(),
                },
                Input {
                    origin: ModuleOrigin::Src,
                    path: PathBuf::from("/src/two.gleam"),
                    source_base_path: PathBuf::from("/src"),
                    src: "import one
pub fn main() { one.C }"
                        .to_string(),
                },
            ],
            expected: Err(Error::Type {
                path: PathBuf::from("/src/two.gleam"),
                src: "import one\npub fn main() { one.C }".to_string(),
                error: crate::type_::Error::UnknownModuleValue {
                    location: SrcSpan {
                        start: 30,
                        end: 32,
                    },
                    name: "C".to_string(),
                    module_name: vec!["one".to_string(),],
                    value_constructors: vec![],
                }
            }),
        },

        // A custom type marked as opaque cannot have its fields accessed
        // from a different module
        Case {
            input: vec![
                Input {
                    origin: ModuleOrigin::Src,
                    path: PathBuf::from("/src/one.gleam"),
                    source_base_path: PathBuf::from("/src"),
                    src: "pub opaque type T { C(a: Int, b: Int) }".to_string(),
                },
                Input {
                    origin: ModuleOrigin::Src,
                    path: PathBuf::from("/src/two.gleam"),
                    source_base_path: PathBuf::from("/src"),
                    src: "import one
fn test(t: one.T) { t.a }"
                        .to_string(),
                },
            ],
            expected: Err(Error::Type {
                path: PathBuf::from("/src/two.gleam"),
                src: "import one\nfn test(t: one.T) { t.a }".to_string(),
                error: crate::type_::Error::UnknownRecordField {
                    location: SrcSpan {
                        start: 31,
                        end: 34,
                    },
                    typ: Arc::new(crate::type_::Type::App {
                        public: true,
                        module: vec!["one".to_string(),],
                        name: "T".to_string(),
                        args: vec![],
                    }),
                    label: "a".to_string(),
                    fields: vec![],
                }
            }),
        },

        // Can import qualified and unqualified module constants
        Case {
            input: vec![
                Input {
                    origin: ModuleOrigin::Src,
                    path: PathBuf::from("/src/one.gleam"),
                    source_base_path: PathBuf::from("/src"),
                    src: "pub const const_string = \"hello!\"".to_string(),
                },
                Input {
                    origin: ModuleOrigin::Src,
                    path: PathBuf::from("/src/two.gleam"),
                    source_base_path: PathBuf::from("/src"),
                    src: "pub const cool_number = 4
pub const cool_number2 = 3.14".to_string(),
                },
                Input {
                    origin: ModuleOrigin::Src,
                    path: PathBuf::from("/src/three.gleam"),
                    source_base_path: PathBuf::from("/src"),
                    src: "import one
import two.{cool_number, cool_number2 as pi}
pub fn test() { one.const_string pi cool_number }"
                        .to_string(),
                },
            ],
            expected: Ok(vec![
                OutputFile {
                    path: PathBuf::from("/gen/src/two.erl"),
                    text: "-module(two).\n"
                        .to_string(),
                },
                OutputFile {
                    path: PathBuf::from("/gen/src/one.erl"),
                    text: "-module(one).\n"
                        .to_string(),
                },
                OutputFile {
                    path: PathBuf::from("/gen/src/three.erl"),
                    text: "-module(three).
-compile(no_auto_import).

-export([test/0]).

-spec test() -> integer().\ntest() ->
    <<\"hello!\"/utf8>>,
    3.14,
    4.
".to_string(),
                },
            ]),
        },

        // Can use module constants in case guards
        Case {
            input: vec![
                Input {
                    origin: ModuleOrigin::Src,
                    path: PathBuf::from("/src/one.gleam"),
                    source_base_path: PathBuf::from("/src"),
                    src: "pub const string_value = \"constant value\"
pub const float_value = 3.14
pub const int_value = 42
                    ".to_string(),
                },
                Input {
                    origin: ModuleOrigin::Src,
                    path: PathBuf::from("/src/two.gleam"),
                    source_base_path: PathBuf::from("/src"),
                    src: "import one.{string_value, float_value, int_value}
pub fn main(arg1, arg2, arg3) {
  case #(arg1, arg2, arg3) {
    #(x, y, z,) if x == string_value && y >. float_value && z == int_value -> 1
    _ -> 0
  }
}"
                        .to_string(),
                },
            ],
            expected: Ok(vec![
                OutputFile {
                    path: PathBuf::from("/gen/src/one.erl"),
                    text: "-module(one).\n"
                        .to_string(),
                },
                OutputFile {
                    path: PathBuf::from("/gen/src/two.erl"),
                    text: "-module(two).
-compile(no_auto_import).

-export([main/3]).

-spec main(binary(), float(), integer()) -> integer().
main(Arg1, Arg2, Arg3) ->
    case {Arg1, Arg2, Arg3} of
        {X, Y, Z} when ((X =:= <<\"constant value\"/utf8>>) andalso (Y > 3.14)) andalso (Z =:= 42) ->
            1;

        _@1 ->
            0
    end.
".to_string(),
                },
            ]),
        },

        // Bug: https://github.com/gleam-lang/gleam/issues/752
        Case {
            input: vec![
                Input {
                    origin: ModuleOrigin::Src,
                    path: PathBuf::from("/src/one.gleam"),
                    source_base_path: PathBuf::from("/src"),
                    src: "pub type One(a) { One(a) }".to_string(),
                },
                Input {
                    origin: ModuleOrigin::Src,
                    path: PathBuf::from("/src/two.gleam"),
                    source_base_path: PathBuf::from("/src"),
                    src: "import one.{One} pub type Two(b) { Two(thing: One(Int)) }"
                        .to_string(),
                },
            ],
            expected: Ok(vec![
                OutputFile {
                    path: PathBuf::from("/gen/src/one.erl"),
                    text: "-module(one).
-compile(no_auto_import).

-export_type([one/1]).

-type one(I) :: {one, I}.


".to_string(),
                },
                OutputFile {
                    path: PathBuf::from("/gen/src/two_Two.hrl"),
                    text: "-record(two, {thing :: one:one(integer())}).\n"
                        .to_string(),
                },
                OutputFile {
                    path: PathBuf::from("/gen/src/two.erl"),
                    text: "-module(two).
-compile(no_auto_import).

-export_type([two/1]).

-type two(K) :: {two, one:one(integer())} | {gleam_phantom, K}.


".to_string(),
                },
            ]),
        },
    ];

    for Case { input, expected } in cases {
        let actual = analysed(input).map(|analysed| crate::erlang::generate_erlang(&analysed));
        assert_eq!(expected, actual);
    }
}
