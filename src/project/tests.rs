use super::*;
use crate::{erl, file::OutputFile};
use std::sync::Arc;

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
                    text: "-module(two).\n-compile(no_auto_import).\n\n\n".to_string(),
                },
                OutputFile {
                    path: PathBuf::from("/gen/src/one.erl"),
                    text: "-module(one).\n-compile(no_auto_import).\n\n\n".to_string(),
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
                text: "-module(one).\n-compile(no_auto_import).\n\n\n".to_string(),
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
                location: crate::ast::SrcSpan { start: 7, end: 10 },
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
                    text: "-module(two).\n-compile(no_auto_import).\n\n\n".to_string(),
                },
                OutputFile {
                    path: PathBuf::from("/gen/src/one.erl"),
                    text: "-module(one).\n-compile(no_auto_import).\n\n\n".to_string(),
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
                    text: "-module(one).\n-compile(no_auto_import).\n\n\n".to_string(),
                },
                OutputFile {
                    path: PathBuf::from("/gen/src/two.erl"),
                    text: "-module(two).\n-compile(no_auto_import).\n\n\n".to_string(),
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
                    text: "-module(one).\n-compile(no_auto_import).\n\n\n".to_string(),
                },
                OutputFile {
                    path: PathBuf::from("/gen/src/two.erl"),
                    text: "-module(two).\n-compile(no_auto_import).\n\n-export([unbox/1]).\n
unbox(X) ->\n    {box, I} = X,\n    I.\n"
                        .to_string(),
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
                    text: "-module(one).\n-compile(no_auto_import).\n\n\n".to_string(),
                },
                OutputFile {
                    path: PathBuf::from("/gen/src/two.erl"),
                    text: "-module(two).\n-compile(no_auto_import).\n\n-export([box/1]).\n
box(X) ->\n    {box, X}.\n"
                        .to_string(),
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
                text: "-module(one@two).\n-compile(no_auto_import).\n\n\n".to_string(),
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
                    text: "-module(one).\n-compile(no_auto_import).\n\n\n".to_string(),
                },
                OutputFile {
                    path: PathBuf::from("/gen/src/two.erl"),
                    text: "-module(two).\n-compile(no_auto_import).\n\n-export([box/0]).\n
box() ->\n    box.\n"
                        .to_string(),
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
                    text: "-module(one).\n-compile(no_auto_import).\n\n-export([go/0]).\n
go() ->
    1.\n"
                        .to_string(),
                },
                OutputFile {
                    path: PathBuf::from("/gen/src/two.erl"),
                    text: "-module(two).\n-compile(no_auto_import).\n\n-export([call/0]).\n
call() ->
    one:go().\n"
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
                    text: "-module(nested@one).\n-compile(no_auto_import).\n\n\n".to_string(),
                },
                OutputFile {
                    path: PathBuf::from("/gen/src/two.erl"),
                    text: "-module(two).\n-compile(no_auto_import).\n\n-export([go/1]).
\ngo(X) ->\n    {box, Y} = X,\n    Y.\n"
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
                    text: "-module(nested@one).\n-compile(no_auto_import).\n\n\n".to_string(),
                },
                OutputFile {
                    path: PathBuf::from("/gen/src/two.erl"),
                    text: "-module(two).\n-compile(no_auto_import).\n\n-export([go/1]).
\ngo(X) ->\n    {box, Y} = X,\n    Y.\n"
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
                    text: "-module(nested@one).\n-compile(no_auto_import).\n\n-export([go/0]).\n
go() ->\n    1.\n"
                        .to_string(),
                },
                OutputFile {
                    path: PathBuf::from("/gen/src/two.erl"),
                    text: "-module(two).\n-compile(no_auto_import).\n\n-export([go/0, thing/0, call_thing/0]).\n
go() ->\n    nested@one:go().\n
thing() ->\n    thing:new().\n
call_thing() ->\n    thing:new().\n"
                        .to_string(),
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
                OutputFile {
                    path: PathBuf::from("/gen/src/one_Point.hrl"),
                    text: "-record(point, {x, y}).\n".to_string(),
                },
                OutputFile {
                    path: PathBuf::from("/gen/src/one.erl"),
                    text: "-module(one).\n-compile(no_auto_import).\n\n\n".to_string(),
                },
                OutputFile {
                    path: PathBuf::from("/gen/src/two.erl"),
                    text: "-module(two).\n-compile(no_auto_import).\n
make() ->\n    {point, 1, 4}.\n
x(P) ->\n    {point, X, _} = P,\n    X.\n"
                        .to_string(),
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
                OutputFile {
                    path: PathBuf::from("/gen/src/one.erl"),
                    text: "-module(one).\n-compile(no_auto_import).\n\n-export([\'div\'/2]).\n
'div'(X, Y) ->\n    X div Y.\n"
                        .to_string(),
                },
                OutputFile {
                    path: PathBuf::from("/gen/src/two.erl"),
                    text: "-module(two).\n-compile(no_auto_import).\n
run() ->\n    one:'div'(2, one:'div'(2, 4)).\n"
                        .to_string(),
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
                OutputFile {
                    path: PathBuf::from("/gen/src/one.erl"),
                    text: "-module(one).\n-compile(no_auto_import).\n\n\n".to_string(),
                },
                OutputFile {
                    path: PathBuf::from("/gen/src/two.erl"),
                    text: "-module(two).\n-compile(no_auto_import).\n
make() ->\n    empty.\n"
                        .to_string(),
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
                OutputFile {
                    path: PathBuf::from("/gen/src/one.erl"),
                    text: "-module(one).\n-compile(no_auto_import).\n\n-export([id/1]).\n
id(X) ->\n    X.\n"
                        .to_string(),
                },
                OutputFile {
                    path: PathBuf::from("/gen/src/two.erl"),
                    text: "-module(two).\n-compile(no_auto_import).\n
make() ->
    one:id(empty).\n"
                        .to_string(),
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
                OutputFile {
                    path: PathBuf::from("/gen/src/one.erl"),
                    text: "-module(one).\n-compile(no_auto_import).\n
-export([id/1]).\n
id(X) ->\n    X.\n"
                        .to_string(),
                },
                OutputFile {
                    path: PathBuf::from("/gen/src/two.erl"),
                    text: "-module(two).\n-compile(no_auto_import).\n
make() ->\n    one:id(empty).\n"
                        .to_string(),
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
                OutputFile {
                    path: PathBuf::from("/gen/src/one.erl"),
                    text: "-module(one).\n-compile(no_auto_import).\n
-export(['receive'/0]).\n
'receive'() ->\n    1.\n"
                        .to_string(),
                },
                OutputFile {
                    path: PathBuf::from("/gen/src/two.erl"),
                    text: "-module(two).\n-compile(no_auto_import).\n
funky() ->
    fun one:'receive'/0.\n"
                        .to_string(),
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
                OutputFile {
                    path: PathBuf::from("/gen/src/one.erl"),
                    text: "-module(one).\n-compile(no_auto_import).\n
-export(['receive'/0]).\n
'receive'() ->\n    1.\n"
                        .to_string(),
                },
                OutputFile {
                    path: PathBuf::from("/gen/src/two.erl"),
                    text: "-module(two).\n-compile(no_auto_import).\n\nfunky() ->
    fun one:'receive'/0.\n"
                        .to_string(),
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
                OutputFile {
                    path: PathBuf::from("/gen/src/one.erl"),
                    text: "-module(one).\n-compile(no_auto_import).\n
-export(['receive'/1]).\n
'receive'(X) ->\n    X.\n"
                        .to_string(),
                },
                OutputFile {
                    path: PathBuf::from("/gen/src/two.erl"),
                    text: "-module(two).\n-compile(no_auto_import).\n\nfunky() ->
    one:'receive'(1).\n"
                        .to_string(),
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
                    text: "-record(person, {name, age}).\n".to_string(),
                },
                OutputFile {
                    path: PathBuf::from("/gen/src/one.erl"),
                    text: "-module(one).\n-compile(no_auto_import).\n\n\n".to_string(),
                },
                OutputFile {
                    path: PathBuf::from("/gen/src/two.erl"),
                    text: "-module(two).\n-compile(no_auto_import).\n
-export([get_age/1, get_name/1]).

get_age(Person) ->
    erlang:element(3, Person).

get_name(Person) ->
    erlang:element(2, Person).\n"
                        .to_string(),
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
                    text: "-record(person, {name, age}).\n".to_string(),
                },
                OutputFile {
                    path: PathBuf::from("/gen/src/one.erl"),
                    text: "-module(one).\n-compile(no_auto_import).\n\n\n".to_string(),
                },
                OutputFile {
                    path: PathBuf::from("/gen/src/two.erl"),
                    text: "-module(two).\n-compile(no_auto_import).\n\n\n".to_string(),
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
                    text: "-record(person, {name, age}).\n".to_string(),
                },
                OutputFile {
                    path: PathBuf::from("/gen/src/one.erl"),
                    text: "-module(one).\n-compile(no_auto_import).\n\n\n".to_string(),
                },
                OutputFile {
                    path: PathBuf::from("/gen/src/two.erl"),
                    text: "-module(two).\n-compile(no_auto_import).\n\n\n".to_string(),
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
fn main() { C }"
                        .to_string(),
                },
            ],
            expected: Ok(vec![
                OutputFile {
                    path: PathBuf::from("/gen/src/one_C.hrl"),
                    text: "-record(c, {a, b}).\n".to_string(),
                },
                OutputFile {
                    path: PathBuf::from("/gen/src/one.erl"),
                    text: "-module(one).\n-compile(no_auto_import).\n\n\n".to_string(),
                },
                OutputFile {
                    path: PathBuf::from("/gen/src/two.erl"),
                    text: "-module(two).\n-compile(no_auto_import).\n\nmain() ->\n    fun(A, B) -> {c, A, B} end.\n".to_string(),
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
                    src: "import one.{X as e, id as i} fn make() { i(e) }".to_string(),
                },
            ],
            expected: Ok(vec![
                OutputFile {
                    path: PathBuf::from("/gen/src/one_X.hrl"),
                    text: "-record(x, {x}).\n".to_string(),

                },
                OutputFile {
                    path: PathBuf::from("/gen/src/one.erl"),
                    text: "-module(one).\n-compile(no_auto_import).\n
-export([id/1]).\n
id(X) ->\n    X.\n"
                        .to_string(),
                },
                OutputFile {
                    path: PathBuf::from("/gen/src/two.erl"),
                    text: "-module(two).\n-compile(no_auto_import).\n
make() ->\n    one:id(fun(A) -> {x, A} end).\n"
                        .to_string(),
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
fn main() { one.C }"
                        .to_string(),
                },
            ],
            expected: Ok(vec![
                OutputFile {
                    path: PathBuf::from("/gen/src/one_C.hrl"),
                    text: "-record(c, {a, b}).\n".to_string(),
                },
                OutputFile {
                    path: PathBuf::from("/gen/src/one.erl"),
                    text: "-module(one).\n-compile(no_auto_import).\n\n\n".to_string(),
                },
                OutputFile {
                    path: PathBuf::from("/gen/src/two.erl"),
                    text: "-module(two).\n-compile(no_auto_import).\n\nmain() ->\n    fun(A, B) -> {c, A, B} end.\n".to_string(),
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
fn main() { one.C }"
                        .to_string(),
                },
            ],
            expected: Err(Error::Type {
                path: PathBuf::from("/src/two.gleam"),
                src: "import one\nfn main() { one.C }".to_string(),
                error: crate::typ::Error::UnknownModuleValue {
                    location: crate::ast::SrcSpan {
                        start: 26,
                        end: 28,
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
                error: crate::typ::Error::UnknownField {
                    location: crate::ast::SrcSpan {
                        start: 32,
                        end: 34,
                    },
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
        },
    ];

    for Case { input, expected } in cases.into_iter() {
        let actual = analysed(input).map(|analysed| erl::generate_erlang(analysed.as_slice()));
        assert_eq!(expected, actual);
    }
}
