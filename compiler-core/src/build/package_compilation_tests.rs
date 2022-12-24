use super::*;
use crate::{
    ast::SrcSpan,
    build::{
        package_compiler::{PackageCompiler, Source},
        Origin, Target,
    },
    codegen,
    config::{Docs, ErlangConfig, JavaScriptConfig, PackageConfig, Repository},
    erlang,
    io::Content,
    javascript,
    type_::{self, FieldAccessUsage},
    Result,
};
use std::{collections::HashSet, fmt::Write, path::PathBuf, sync::Arc};

use hexpm::version::{Range, Version};
use pretty_assertions::assert_eq;

macro_rules! assert_erlang_compile {
    ($sources:expr, $expected_output:expr  $(,)?) => {
        let outputs = compile_test_project(
            $sources,
            &TargetCodegenConfiguration::Erlang { app_file: None },
            None,
        );
        let expected: Result<Vec<OutputFile>, Error> = $expected_output;
        let expected = expected.map(|out| {
            out.into_iter()
                .map(|output| (output.path, output.content))
                .collect::<HashMap<_, _>>()
        });
        assert_eq!(expected, outputs.map(|o| o.files));
    };
}

// TODO: move this to a test helper crate
#[derive(Debug)]
pub struct TestCompileOutput {
    files: HashMap<PathBuf, Content>,
    warnings: Vec<crate::Warning>,
}

fn compile_test_project(
    sources: Vec<Source>,
    target: &TargetCodegenConfiguration,
    config: Option<PackageConfig>,
) -> Result<TestCompileOutput> {
    let ids = crate::uid::UniqueIdGenerator::new();
    let mut modules = im::HashMap::new();
    let mut warnings = Vec::new();
    let config = config.unwrap_or_else(|| PackageConfig {
        name: "the_package".to_string(),
        version: Version::new(1, 0, 0),
        licences: vec![],
        description: "The description".into(),
        documentation: Docs { pages: vec![] },
        dependencies: [].into(),
        dev_dependencies: [].into(),
        repository: Repository::None,
        links: vec![],
        erlang: ErlangConfig {
            application_start_module: None,
            extra_applications: vec![],
        },
        javascript: JavaScriptConfig {
            typescript_declarations: false,
        },
        target: Target::Erlang,
    });
    let filesystem = crate::io::memory::InMemoryFileSystem::new();
    let root = PathBuf::from("some/build/path/root");
    let out = PathBuf::from("_build/default/lib/the_package");
    let lib = PathBuf::from("_build/default/lib");
    let mut build_journal = HashSet::new();
    let mut compiler = PackageCompiler::new(
        &config,
        &root,
        &out,
        &lib,
        target,
        ids,
        filesystem.clone(),
        Some(&mut build_journal),
    );
    compiler.write_entrypoint = false;
    compiler.write_metadata = false;
    compiler.compile_beam_bytecode = false;
    compiler.copy_native_files = false;
    compiler.sources = sources;
    let files = compiler
        .compile(&mut warnings, &mut modules, &mut im::HashMap::new())
        .map(|_| filesystem.into_contents())
        .map_err(|e| normalise_error(e))?;
    Ok(TestCompileOutput { files, warnings })
}

#[test]
fn package_compiler_test() {
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
                path: PathBuf::from("_build/default/lib/the_package/_gleam_artefacts/one.erl"),
                content: Content::Text(
                    "-module(one).
-compile(no_auto_import).

-export_type([box/0]).

-type box() :: box.


"
                    .to_string()
                ),
            },
            OutputFile {
                path: PathBuf::from("_build/default/lib/the_package/_gleam_artefacts/two.erl"),
                content: Content::Text(
                    "-module(two).
-compile(no_auto_import).

-export([box/0]).

-spec box() -> one:box().
box() ->
    box.
"
                    .to_string()
                ),
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
                path: PathBuf::from("_build/default/lib/the_package/_gleam_artefacts/one.erl"),
                content: Content::Text(
                    "-module(one).
-compile(no_auto_import).

-export([go/0]).

-spec go() -> integer().
go() ->
    1.
"
                    .to_string()
                ),
            },
            OutputFile {
                path: PathBuf::from("_build/default/lib/the_package/_gleam_artefacts/two.erl"),
                content: Content::Text(
                    "-module(two).
-compile(no_auto_import).

-export([call/0]).

-spec call() -> integer().
call() ->
    one:go().
"
                    .to_string()
                ),
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
                path: PathBuf::from(
                    "_build/default/lib/the_package/_gleam_artefacts/nested@one.erl"
                ),
                content: Content::Text(
                    "-module(nested@one).
-compile(no_auto_import).

-export_type([box/0]).

-type box() :: {box, integer()}.


"
                    .to_string()
                ),
            },
            OutputFile {
                path: PathBuf::from("_build/default/lib/the_package/_gleam_artefacts/two.erl"),
                content: Content::Text(
                    "-module(two).
-compile(no_auto_import).

-export([go/1]).

-spec go(nested@one:box()) -> integer().
go(X) ->
    {box, Y} = X,
    Y.
"
                    .to_string()
                ),
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
                path: PathBuf::from(
                    "_build/default/lib/the_package/_gleam_artefacts/nested@one.erl"
                ),
                content: Content::Text(
                    "-module(nested@one).
-compile(no_auto_import).

-export_type([box/0]).

-type box() :: {box, integer()}.


"
                    .to_string()
                ),
            },
            OutputFile {
                path: PathBuf::from("_build/default/lib/the_package/_gleam_artefacts/two.erl"),
                content: Content::Text(
                    "-module(two).
-compile(no_auto_import).

-export([go/1]).

-spec go(nested@one:box()) -> integer().
go(X) ->
    {box, Y} = X,
    Y.
"
                    .to_string()
                ),
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
                path: PathBuf::from(
                    "_build/default/lib/the_package/_gleam_artefacts/nested@one.erl"
                ),
                content: Content::Text(
                    "-module(nested@one).
-compile(no_auto_import).

-export([go/0]).
-export_type([thing/0]).

-type thing() :: any().

-spec go() -> integer().
go() ->
    1.
"
                    .to_string()
                ),
            },
            OutputFile {
                path: PathBuf::from("_build/default/lib/the_package/_gleam_artefacts/two.erl"),
                content: Content::Text(
                    "-module(two).
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
                    .to_string()
                ),
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
pub fn make() { one.Point(1, 4) }
pub fn x(p) { let one.Point(x, _) = p x }"
                    .to_string(),
            },
        ],
        Ok(vec![
            OutputFile {
                path: PathBuf::from("_build/default/lib/the_package/_gleam_artefacts/one.erl"),
                content: Content::Text(
                    "-module(one).
-compile(no_auto_import).

-export_type([point/0]).

-type point() :: {point, integer(), integer()}.


"
                    .to_string()
                ),
            },
            OutputFile {
                path: PathBuf::from("_build/default/lib/the_package/include/one_Point.hrl"),
                content: Content::Text(
                    "-record(point, {x :: integer(), y :: integer()}).
"
                    .to_string()
                ),
            },
            OutputFile {
                path: PathBuf::from("_build/default/lib/the_package/_gleam_artefacts/two.erl"),
                content: Content::Text(
                    "-module(two).
-compile(no_auto_import).

-export([make/0, x/1]).

-spec make() -> one:point().
make() ->
    {point, 1, 4}.

-spec x(one:point()) -> integer().
x(P) ->
    {point, X, _@1} = P,
    X.
"
                    .to_string()
                ),
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
                       pub fn run() { 2 |> div(top: _, bottom: 4) |> div(2, bottom: _) }"
                    .to_string(),
            },
        ],
        Ok(vec![
            OutputFile {
                path: PathBuf::from("_build/default/lib/the_package/_gleam_artefacts/one.erl"),
                content: Content::Text(
                    "-module(one).
-compile(no_auto_import).

-export([\'div\'/2]).

-spec \'div\'(integer(), integer()) -> integer().
'div'(X, Y) ->
    case Y of
        0 -> 0;
        Gleam@denominator -> X rem Gleam@denominator
    end.
"
                    .to_string()
                ),
            },
            OutputFile {
                path: PathBuf::from("_build/default/lib/the_package/_gleam_artefacts/two.erl"),
                content: Content::Text(
                    "-module(two).
-compile(no_auto_import).

-export([run/0]).

-spec run() -> integer().
run() ->
    _pipe = 2,
    _pipe@1 = one:'div'(_pipe, 4),
    one:'div'(2, _pipe@1).
"
                    .to_string()
                ),
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
pub fn make() { one.Empty }"
                    .to_string(),
            },
        ],
        Ok(vec![
            OutputFile {
                path: PathBuf::from("_build/default/lib/the_package/_gleam_artefacts/one.erl"),
                content: Content::Text(
                    "-module(one).
-compile(no_auto_import).

-export_type([empty/0]).

-type empty() :: empty.


"
                    .to_string()
                ),
            },
            OutputFile {
                path: PathBuf::from("_build/default/lib/the_package/_gleam_artefacts/two.erl"),
                content: Content::Text(
                    "-module(two).
-compile(no_auto_import).

-export([make/0]).

-spec make() -> one:empty().
make() ->
    empty.
"
                    .to_string()
                ),
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
                code: "import one.{Empty, id} pub fn make() { id(Empty) }".to_string(),
            },
        ],
        Ok(vec![
            OutputFile {
                path: PathBuf::from("_build/default/lib/the_package/_gleam_artefacts/one.erl"),
                content: Content::Text(
                    "-module(one).
-compile(no_auto_import).

-export([id/1]).
-export_type([empty/0]).

-type empty() :: empty.

-spec id(I) -> I.
id(X) ->
    X.
"
                    .to_string()
                ),
            },
            OutputFile {
                path: PathBuf::from("_build/default/lib/the_package/_gleam_artefacts/two.erl"),
                content: Content::Text(
                    "-module(two).
-compile(no_auto_import).

-export([make/0]).

-spec make() -> one:empty().
make() ->
    one:id(empty).
"
                    .to_string()
                ),
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
                path: PathBuf::from("_build/default/lib/the_package/_gleam_artefacts/one.erl"),
                content: Content::Text(
                    "-module(one).
-compile(no_auto_import).

-export_type([person/0]).

-type person() :: {person, binary(), integer()}.


"
                    .to_string()
                ),
            },
            OutputFile {
                path: PathBuf::from("_build/default/lib/the_package/include/one_Person.hrl"),
                content: Content::Text(
                    "-record(person, {name :: binary(), age :: integer()}).
"
                    .to_string()
                ),
            },
            OutputFile {
                path: PathBuf::from("_build/default/lib/the_package/_gleam_artefacts/two.erl"),
                content: Content::Text(
                    "-module(two).
-compile(no_auto_import).

-export([get_age/1, get_name/1]).

-spec get_age(one:person()) -> integer().
get_age(Person) ->
    erlang:element(3, Person).

-spec get_name(one:person()) -> binary().
get_name(Person) ->
    erlang:element(2, Person).
"
                    .to_string()
                ),
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
                path: PathBuf::from("_build/default/lib/the_package/_gleam_artefacts/one.erl"),
                content: Content::Text(
                    "-module(one).
-compile(no_auto_import).

-export_type([person/0]).

-type person() :: {person, binary(), integer()}.


"
                    .to_string()
                ),
            },
            OutputFile {
                path: PathBuf::from("_build/default/lib/the_package/include/one_Person.hrl"),
                content: Content::Text(
                    "-record(person, {name :: binary(), age :: integer()}).
"
                    .to_string()
                ),
            },
            OutputFile {
                path: PathBuf::from("_build/default/lib/the_package/_gleam_artefacts/two.erl"),
                content: Content::Text("-module(two).\n".to_string()),
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
                path: PathBuf::from("_build/default/lib/the_package/_gleam_artefacts/one.erl"),
                content: Content::Text(
                    "-module(one).
-compile(no_auto_import).

-export_type([person/0]).\n\n-type person() :: {person, binary(), integer()}.


"
                    .to_string()
                ),
            },
            OutputFile {
                path: PathBuf::from("_build/default/lib/the_package/include/one_Person.hrl"),
                content: Content::Text(
                    "-record(person, {name :: binary(), age :: integer()}).
"
                    .to_string()
                ),
            },
            OutputFile {
                path: PathBuf::from("_build/default/lib/the_package/_gleam_artefacts/two.erl"),
                content: Content::Text("-module(two).\n".to_string()),
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
                code: "import one.{C} pub fn main() { C }".to_string(),
            },
        ],
        Ok(vec![
            OutputFile {
                path: PathBuf::from("_build/default/lib/the_package/_gleam_artefacts/one.erl"),
                content: Content::Text(
                    "-module(one).
-compile(no_auto_import).

-export_type([t/1]).\n\n-type t(I) :: {c, integer(), integer()} | {gleam_phantom, I}.


"
                    .to_string()
                ),
            },
            OutputFile {
                path: PathBuf::from("_build/default/lib/the_package/include/one_C.hrl"),
                content: Content::Text(
                    "-record(c, {a :: integer(), b :: integer()}).
"
                    .to_string()
                ),
            },
            OutputFile {
                path: PathBuf::from("_build/default/lib/the_package/_gleam_artefacts/two.erl"),
                content: Content::Text(
                    "-module(two).
-compile(no_auto_import).

-export([main/0]).

-spec main() -> fun((integer(), integer()) -> one:t(any())).
main() ->
    fun(Field@0, Field@1) -> {c, Field@0, Field@1} end.
"
                    .to_string()
                ),
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
                code: "import one.{X as E, id as i} pub fn make() { i(E) }".to_string(),
            },
        ],
        Ok(vec![
            OutputFile {
                path: PathBuf::from("_build/default/lib/the_package/_gleam_artefacts/one.erl"),
                content: Content::Text(
                    "-module(one).
-compile(no_auto_import).

-export([id/1]).
-export_type([t/0]).

-type t() :: {x, integer()}.

-spec id(I) -> I.
id(X) ->
    X.
"
                    .to_string()
                ),
            },
            OutputFile {
                path: PathBuf::from("_build/default/lib/the_package/include/one_X.hrl"),
                content: Content::Text(
                    "-record(x, {x :: integer()}).
"
                    .to_string()
                ),
            },
            OutputFile {
                path: PathBuf::from("_build/default/lib/the_package/_gleam_artefacts/two.erl"),
                content: Content::Text(
                    "-module(two).
-compile(no_auto_import).

-export([make/0]).

-spec make() -> fun((integer()) -> one:t()).
make() ->
    one:id(fun(Field@0) -> {x, Field@0} end).
"
                    .to_string()
                ),
            },
        ]),
    );

    // Custom type constructors can be aliased when imported
    assert_erlang_compile!(
        vec![
            Source {
                origin: Origin::Src,
                path: PathBuf::from("/src/one.gleam"),
                name: "one".to_string(),
                code: "pub type Headers = List(String)".to_string(),
            },
            Source {
                origin: Origin::Src,
                path: PathBuf::from("/src/two.gleam"),
                name: "two".to_string(),
                code: r#"import one.{Headers as StringList} pub fn make_list() -> StringList { ["aliased", "type", "constructor"] }"#.to_string(),
            },
        ],
        Ok(vec![
            OutputFile {
                path: PathBuf::from("_build/default/lib/the_package/_gleam_artefacts/one.erl"),
                content: Content::Text("-module(one).\n".to_string()),
            },
            OutputFile {
                path: PathBuf::from("_build/default/lib/the_package/_gleam_artefacts/two.erl"),
                content: Content::Text(r#"-module(two).
-compile(no_auto_import).

-export([make_list/0]).

-spec make_list() -> list(binary()).
make_list() ->
    [<<"aliased"/utf8>>, <<"type"/utf8>>, <<"constructor"/utf8>>].
"#
                                .to_string()),
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
                code: "import one pub fn main() { one.C }".to_string(),
            },
        ],
        Ok(vec![
            OutputFile {
                path: PathBuf::from("_build/default/lib/the_package/_gleam_artefacts/one.erl"),
                content: Content::Text(
                    "-module(one).
-compile(no_auto_import).

-export_type([t/1]).

-type t(I) :: {c, integer(), integer()} | {gleam_phantom, I}.


"
                    .to_string()
                ),
            },
            OutputFile {
                path: PathBuf::from("_build/default/lib/the_package/include/one_C.hrl"),
                content: Content::Text(
                    "-record(c, {a :: integer(), b :: integer()}).
"
                    .to_string()
                ),
            },
            OutputFile {
                path: PathBuf::from("_build/default/lib/the_package/_gleam_artefacts/two.erl"),
                content: Content::Text(
                    "-module(two).
-compile(no_auto_import).

-export([main/0]).

-spec main() -> fun((integer(), integer()) -> one:t(any())).
main() ->
    fun(Field@0, Field@1) -> {c, Field@0, Field@1} end.
"
                    .to_string()
                ),
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
            error: crate::type_::Error::UnknownModuleValue {
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
            error: crate::type_::Error::UnknownRecordField {
                usage: FieldAccessUsage::Other,
                location: crate::ast::SrcSpan { start: 31, end: 34 },
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
pub fn x() { test }"
                    .to_string(),
            },
        ],
        Ok(vec![
            OutputFile {
                path: PathBuf::from("_build/default/lib/the_package/_gleam_artefacts/one.erl"),
                content: Content::Text(
                    "-module(one).
-compile(no_auto_import).

-export_type([test/0]).

-type test() :: a.


"
                    .to_string()
                ),
            },
            OutputFile {
                path: PathBuf::from("_build/default/lib/the_package/_gleam_artefacts/two.erl"),
                content: Content::Text(
                    "-module(two).
-compile(no_auto_import).

-export([x/0]).

-spec x() -> one:test().
x() ->
    a.
"
                    .to_string()
                ),
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
pub fn x() { test }"
                    .to_string(),
            },
        ],
        Ok(vec![
            OutputFile {
                path: PathBuf::from("_build/default/lib/the_package/_gleam_artefacts/one.erl"),
                content: Content::Text(
                    "-module(one).
-compile(no_auto_import).

-export_type([a/0, b/0]).

-type a() :: a.

-type b() :: {b, a()}.


"
                    .to_string()
                ),
            },
            OutputFile {
                path: PathBuf::from("_build/default/lib/the_package/_gleam_artefacts/two.erl"),
                content: Content::Text(
                    "-module(two).
-compile(no_auto_import).

-export([x/0]).

-spec x() -> one:b().
x() ->
    {b, a}.
"
                    .to_string()
                ),
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
            error: type_::Error::UnknownModuleValue {
                location: SrcSpan { start: 28, end: 33 },
                module_name: vec!["one".to_string()],
                name: "A".to_string(),
                value_constructors: vec![]
            }
        })
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
