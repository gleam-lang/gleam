use super::*;
use crate::{
    ast::SrcSpan,
    build::{
        package_compiler::{PackageCompiler, Source},
        Origin, Target,
    },
    codegen,
    config::{Docs, ErlangConfig, PackageConfig, Repository},
    erlang,
    io::test::FilesChannel,
    javascript, type_,
};
use std::{collections::HashSet, path::PathBuf, sync::Arc};

use hexpm::version::{Range, Version};
use pretty_assertions::assert_eq;

macro_rules! assert_erlang_compile {
    ($sources:expr, $expected_output:expr  $(,)?) => {
        let ids = crate::uid::UniqueIdGenerator::new();
        let mut modules = im::HashMap::new();
        let config = PackageConfig {
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
            target: Target::Erlang,
        };
        let (file_writer, file_receiver) = FilesChannel::new();
        let root = PathBuf::from("some/build/path/root");
        let out = PathBuf::from("_build/default/lib/the_package");
        let lib = PathBuf::from("_build/default/lib");
        let mut build_journal = HashSet::new();
        let mut compiler = PackageCompiler::new(
            &config,
            &root,
            &out,
            &lib,
            Target::Erlang,
            ids,
            file_writer,
            Some(&mut build_journal),
        );
        compiler.write_entrypoint = false;
        compiler.write_metadata = false;
        compiler.compile_beam_bytecode = false;
        compiler.copy_native_files = false;
        compiler.sources = $sources;
        let outputs = compiler
            .compile(&mut vec![], &mut modules, &mut im::HashMap::new())
            .map(|_| {
                let mut outputs = FilesChannel::recv_utf8_files(&file_receiver).unwrap();
                outputs.sort_by(|a, b| a.path.partial_cmp(&b.path).unwrap());
                outputs
            })
            .map_err(|e| normalise_error(e));
        let expected = $expected_output.map(|mut outputs: Vec<OutputFile>| {
            outputs.sort_by(|a, b| a.path.partial_cmp(&b.path).unwrap());
            outputs
        });
        assert_eq!(expected, outputs);
    };
}

macro_rules! assert_javascript_compile {
    ($sources:expr, $expected_output:expr  $(,)?) => {
        let ids = crate::uid::UniqueIdGenerator::new();
        let mut modules = im::HashMap::new();
        let config = PackageConfig {
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
            target: Target::JavaScript,
        };
        let (file_writer, file_receiver) = FilesChannel::new();
        let root = PathBuf::from("some/build/path/root");
        let out = PathBuf::from("_build/default/lib/the_package");
        let lib = PathBuf::from("_build/default/lib");
        let mut build_journal = HashSet::new();
        let mut compiler = PackageCompiler::new(
            &config,
            &root,
            &out,
            &lib,
            Target::JavaScript,
            ids,
            file_writer,
            Some(&mut build_journal),
        );
        compiler.write_entrypoint = false;
        compiler.write_metadata = false;
        compiler.compile_beam_bytecode = false;
        compiler.copy_native_files = false;
        compiler.sources = $sources;
        let outputs = compiler
            .compile(&mut vec![], &mut modules, &mut im::HashMap::new())
            .map(|_| {
                let mut outputs = FilesChannel::recv_utf8_files(&file_receiver).unwrap();
                outputs.sort_by(|a, b| a.path.partial_cmp(&b.path).unwrap());
                outputs
            })
            .map_err(|e| normalise_error(e));
        let expected = $expected_output.map(|mut outputs: Vec<OutputFile>| {
            outputs.sort_by(|a, b| a.path.partial_cmp(&b.path).unwrap());
            outputs
        });
        assert_eq!(expected, outputs);
    };
}

macro_rules! assert_no_warnings {
    ($sources:expr $(,)?) => {
        let ids = crate::uid::UniqueIdGenerator::new();
        let mut modules = im::HashMap::new();
        let config = PackageConfig {
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
            target: Target::Erlang,
        };
        let mut warnings = vec![];
        let (file_writer, file_receiver) = FilesChannel::new();
        let root = PathBuf::from("some/build/path/root");
        let out = PathBuf::from("_build/default/lib/the_package");
        let lib = PathBuf::from("_build/default/lib");
        let mut build_journal = HashSet::new();
        let mut compiler = PackageCompiler::new(
            &config,
            &root,
            &out,
            &lib,
            Target::Erlang,
            ids,
            file_writer,
            Some(&mut build_journal),
        );
        compiler.write_entrypoint = false;
        compiler.write_metadata = false;
        compiler.compile_beam_bytecode = false;
        compiler.copy_native_files = false;
        compiler.sources = $sources;
        let outputs = compiler
            .compile(&mut warnings, &mut modules, &mut im::HashMap::new())
            .unwrap();
        assert_eq!(vec![] as Vec<crate::Warning>, warnings);
    };
}

#[test]
fn package_compiler_test() {
    assert_erlang_compile!(
        vec![],
        Ok(vec![OutputFile {
            path: PathBuf::from("_build/default/lib/the_package/ebin/the_package.app"),
            text: "{application, the_package, [
    {vsn, \"1.0.0\"},
    {applications, []},
    {description, \"The description\"},
    {modules, []},
    {registered, []}
]}.
"
            .into(),
        },])
    );

    assert_erlang_compile!(
        vec![Source {
            path: PathBuf::from("src/one.gleam"),
            name: "one".to_string(),
            code: "".to_string(),
            origin: Origin::Src,
        }],
        Ok(vec![
            OutputFile {
                path: PathBuf::from("_build/default/lib/the_package/ebin/the_package.app"),
                text: "{application, the_package, [
    {vsn, \"1.0.0\"},
    {applications, []},
    {description, \"The description\"},
    {modules, [one]},
    {registered, []}
]}.
"
                .into(),
            },
            OutputFile {
                text: "-module(one).\n".to_string(),
                path: PathBuf::from("_build/default/lib/the_package/build/one.erl"),
            },
        ])
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
                path: PathBuf::from("_build/default/lib/the_package/ebin/the_package.app"),
                text: "{application, the_package, [
    {vsn, \"1.0.0\"},
    {applications, []},
    {description, \"The description\"},
    {modules, [one,
               two]},
    {registered, []}
]}.
"
                .into(),
            },
            OutputFile {
                text: "-module(one).\n".to_string(),
                path: PathBuf::from("_build/default/lib/the_package/build/one.erl"),
            },
            OutputFile {
                text: "-module(two).\n".to_string(),
                path: PathBuf::from("_build/default/lib/the_package/build/two.erl"),
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
        Ok(vec![
            OutputFile {
                path: PathBuf::from("_build/default/lib/the_package/ebin/the_package.app"),
                text: "{application, the_package, [
    {vsn, \"1.0.0\"},
    {applications, []},
    {description, \"The description\"},
    {modules, [one]},
    {registered, []}
]}.
"
                .into(),
            },
            OutputFile {
                path: PathBuf::from("_build/default/lib/the_package/build/one.erl"),
                text: "-module(one).\n".to_string(),
            },
        ]),
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
                path: PathBuf::from("_build/default/lib/the_package/ebin/the_package.app"),
                text: "{application, the_package, [
    {vsn, \"1.0.0\"},
    {applications, []},
    {description, \"The description\"},
    {modules, [one,
               two]},
    {registered, []}
]}.
"
                .into(),
            },
            OutputFile {
                path: PathBuf::from("_build/default/lib/the_package/build/one.erl"),
                text: "-module(one).\n".to_string(),
            },
            OutputFile {
                path: PathBuf::from("_build/default/lib/the_package/build/two.erl"),
                text: "-module(two).\n".to_string(),
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
                path: PathBuf::from("_build/default/lib/the_package/ebin/the_package.app"),
                text: "{application, the_package, [
    {vsn, \"1.0.0\"},
    {applications, []},
    {description, \"The description\"},
    {modules, [one,
               two]},
    {registered, []}
]}.
"
                .into(),
            },
            OutputFile {
                path: PathBuf::from("_build/default/lib/the_package/build/one.erl"),
                text: "-module(one).\n".to_string(),
            },
            OutputFile {
                path: PathBuf::from("_build/default/lib/the_package/build/two.erl"),
                text: "-module(two).\n".to_string(),
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
                path: PathBuf::from("_build/default/lib/the_package/ebin/the_package.app"),
                text: "{application, the_package, [
    {vsn, \"1.0.0\"},
    {applications, []},
    {description, \"The description\"},
    {modules, [one,
               two]},
    {registered, []}
]}.
"
                .into(),
            },
            OutputFile {
                path: PathBuf::from("_build/default/lib/the_package/build/one.erl"),
                text: "-module(one).
-compile(no_auto_import).

-export_type([box/0]).

-type box() :: {box, integer()}.


"
                .to_string(),
            },
            OutputFile {
                path: PathBuf::from("_build/default/lib/the_package/build/two.erl"),
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
                path: PathBuf::from("_build/default/lib/the_package/ebin/the_package.app"),
                text: "{application, the_package, [
    {vsn, \"1.0.0\"},
    {applications, []},
    {description, \"The description\"},
    {modules, [one,
               two]},
    {registered, []}
]}.
"
                .into(),
            },
            OutputFile {
                path: PathBuf::from("_build/default/lib/the_package/build/one.erl"),
                text: "-module(one).
-compile(no_auto_import).

-export_type([box/0]).

-type box() :: {box, integer()}.


"
                .to_string(),
            },
            OutputFile {
                path: PathBuf::from("_build/default/lib/the_package/build/two.erl"),
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
        Ok(vec![
            OutputFile {
                path: PathBuf::from("_build/default/lib/the_package/ebin/the_package.app"),
                text: "{application, the_package, [
    {vsn, \"1.0.0\"},
    {applications, []},
    {description, \"The description\"},
    {modules, [one@two]},
    {registered, []}
]}.
"
                .into(),
            },
            OutputFile {
                path: PathBuf::from("_build/default/lib/the_package/build/one@two.erl"),
                text: "-module(one@two).
-compile(no_auto_import).

-export_type([box/0]).

-type box() :: box.


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
                path: PathBuf::from("_build/default/lib/the_package/ebin/the_package.app"),
                text: "{application, the_package, [
    {vsn, \"1.0.0\"},
    {applications, []},
    {description, \"The description\"},
    {modules, [one,
               two]},
    {registered, []}
]}.
"
                .into(),
            },
            OutputFile {
                path: PathBuf::from("_build/default/lib/the_package/build/one.erl"),
                text: "-module(one).
-compile(no_auto_import).

-export_type([box/0]).

-type box() :: box.


"
                .to_string(),
            },
            OutputFile {
                path: PathBuf::from("_build/default/lib/the_package/build/two.erl"),
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
                path: PathBuf::from("_build/default/lib/the_package/ebin/the_package.app"),
                text: "{application, the_package, [
    {vsn, \"1.0.0\"},
    {applications, []},
    {description, \"The description\"},
    {modules, [one,
               two]},
    {registered, []}
]}.
"
                .into(),
            },
            OutputFile {
                path: PathBuf::from("_build/default/lib/the_package/build/one.erl"),
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
                path: PathBuf::from("_build/default/lib/the_package/build/two.erl"),
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
                path: PathBuf::from("_build/default/lib/the_package/ebin/the_package.app"),
                text: "{application, the_package, [
    {vsn, \"1.0.0\"},
    {applications, []},
    {description, \"The description\"},
    {modules, [nested@one,
               two]},
    {registered, []}
]}.
"
                .into(),
            },
            OutputFile {
                path: PathBuf::from("_build/default/lib/the_package/build/nested@one.erl"),
                text: "-module(nested@one).
-compile(no_auto_import).

-export_type([box/0]).

-type box() :: {box, integer()}.


"
                .to_string(),
            },
            OutputFile {
                path: PathBuf::from("_build/default/lib/the_package/build/two.erl"),
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
                path: PathBuf::from("_build/default/lib/the_package/ebin/the_package.app"),
                text: "{application, the_package, [
    {vsn, \"1.0.0\"},
    {applications, []},
    {description, \"The description\"},
    {modules, [nested@one,
               two]},
    {registered, []}
]}.
"
                .into(),
            },
            OutputFile {
                path: PathBuf::from("_build/default/lib/the_package/build/nested@one.erl"),
                text: "-module(nested@one).
-compile(no_auto_import).

-export_type([box/0]).

-type box() :: {box, integer()}.


"
                .to_string(),
            },
            OutputFile {
                path: PathBuf::from("_build/default/lib/the_package/build/two.erl"),
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
                path: PathBuf::from("_build/default/lib/the_package/ebin/the_package.app"),
                text: "{application, the_package, [
    {vsn, \"1.0.0\"},
    {applications, []},
    {description, \"The description\"},
    {modules, [nested@one,
               two]},
    {registered, []}
]}.
"
                .into(),
            },
            OutputFile {
                path: PathBuf::from("_build/default/lib/the_package/build/nested@one.erl"),
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
                path: PathBuf::from("_build/default/lib/the_package/build/two.erl"),
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
pub fn make() { one.Point(1, 4) }
pub fn x(p) { let one.Point(x, _) = p x }"
                    .to_string(),
            },
        ],
        Ok(vec![
            OutputFile {
                path: PathBuf::from("_build/default/lib/the_package/ebin/the_package.app"),
                text: "{application, the_package, [
    {vsn, \"1.0.0\"},
    {applications, []},
    {description, \"The description\"},
    {modules, [one,
               two]},
    {registered, []}
]}.
"
                .into(),
            },
            OutputFile {
                path: PathBuf::from("_build/default/lib/the_package/build/one.erl"),
                text: "-module(one).
-compile(no_auto_import).

-export_type([point/0]).

-type point() :: {point, integer(), integer()}.


"
                .to_string(),
            },
            OutputFile {
                path: PathBuf::from("_build/default/lib/the_package/include/one_Point.hrl"),
                text: "-record(point, {x :: integer(), y :: integer()}).
"
                .to_string(),
            },
            OutputFile {
                path: PathBuf::from("_build/default/lib/the_package/build/two.erl"),
                text: "-module(two).
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
                       pub fn run() { 2 |> div(top: _, bottom: 4) |> div(2, bottom: _) }"
                    .to_string(),
            },
        ],
        Ok(vec![
            OutputFile {
                path: PathBuf::from("_build/default/lib/the_package/ebin/the_package.app"),
                text: "{application, the_package, [
    {vsn, \"1.0.0\"},
    {applications, []},
    {description, \"The description\"},
    {modules, [one,
               two]},
    {registered, []}
]}.
"
                .into(),
            },
            OutputFile {
                path: PathBuf::from("_build/default/lib/the_package/build/one.erl"),
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
                path: PathBuf::from("_build/default/lib/the_package/build/two.erl"),
                text: "-module(two).
-compile(no_auto_import).

-export([run/0]).

-spec run() -> integer().
run() ->
    _pipe = 2,
    _pipe@1 = one:'div'(_pipe, 4),
    one:'div'(2, _pipe@1).
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
pub fn make() { one.Empty }"
                    .to_string(),
            },
        ],
        Ok(vec![
            OutputFile {
                path: PathBuf::from("_build/default/lib/the_package/ebin/the_package.app"),
                text: "{application, the_package, [
    {vsn, \"1.0.0\"},
    {applications, []},
    {description, \"The description\"},
    {modules, [one,
               two]},
    {registered, []}
]}.
"
                .into(),
            },
            OutputFile {
                path: PathBuf::from("_build/default/lib/the_package/build/one.erl"),
                text: "-module(one).
-compile(no_auto_import).

-export_type([empty/0]).

-type empty() :: empty.


"
                .to_string(),
            },
            OutputFile {
                path: PathBuf::from("_build/default/lib/the_package/build/two.erl"),
                text: "-module(two).
-compile(no_auto_import).

-export([make/0]).

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
                code: "import one.{Empty, id} pub fn make() { id(Empty) }".to_string(),
            },
        ],
        Ok(vec![
            OutputFile {
                path: PathBuf::from("_build/default/lib/the_package/ebin/the_package.app"),
                text: "{application, the_package, [
    {vsn, \"1.0.0\"},
    {applications, []},
    {description, \"The description\"},
    {modules, [one,
               two]},
    {registered, []}
]}.
"
                .into(),
            },
            OutputFile {
                path: PathBuf::from("_build/default/lib/the_package/build/one.erl"),
                text: "-module(one).
-compile(no_auto_import).

-export([id/1]).
-export_type([empty/0]).

-type empty() :: empty.

-spec id(I) -> I.
id(X) ->
    X.
"
                .to_string(),
            },
            OutputFile {
                path: PathBuf::from("_build/default/lib/the_package/build/two.erl"),
                text: "-module(two).
-compile(no_auto_import).

-export([make/0]).

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
                code: "import one.{Empty as E, id as i} pub fn make() { i(E) }".to_string(),
            },
        ],
        Ok(vec![
            OutputFile {
                path: PathBuf::from("_build/default/lib/the_package/ebin/the_package.app"),
                text: "{application, the_package, [
    {vsn, \"1.0.0\"},
    {applications, []},
    {description, \"The description\"},
    {modules, [one,
               two]},
    {registered, []}
]}.
"
                .into(),
            },
            OutputFile {
                path: PathBuf::from("_build/default/lib/the_package/build/one.erl"),
                text: "-module(one).
-compile(no_auto_import).

-export([id/1]).
-export_type([empty/0]).

-type empty() :: empty.

-spec id(I) -> I.
id(X) ->
    X.
"
                .to_string(),
            },
            OutputFile {
                path: PathBuf::from("_build/default/lib/the_package/build/two.erl"),
                text: "-module(two).
-compile(no_auto_import).

-export([make/0]).

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
                code: "import one pub fn funky() { one.receive }".to_string(),
            },
        ],
        Ok(vec![
            OutputFile {
                path: PathBuf::from("_build/default/lib/the_package/ebin/the_package.app"),
                text: "{application, the_package, [
    {vsn, \"1.0.0\"},
    {applications, []},
    {description, \"The description\"},
    {modules, [one,
               two]},
    {registered, []}
]}.
"
                .into(),
            },
            OutputFile {
                path: PathBuf::from("_build/default/lib/the_package/build/one.erl"),
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
                path: PathBuf::from("_build/default/lib/the_package/build/two.erl"),
                text: "-module(two).
-compile(no_auto_import).

-export([funky/0]).

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
                code: "import one.{receive} pub fn funky() { receive }".to_string(),
            },
        ],
        Ok(vec![
            OutputFile {
                path: PathBuf::from("_build/default/lib/the_package/ebin/the_package.app"),
                text: "{application, the_package, [
    {vsn, \"1.0.0\"},
    {applications, []},
    {description, \"The description\"},
    {modules, [one,
               two]},
    {registered, []}
]}.
"
                .into(),
            },
            OutputFile {
                path: PathBuf::from("_build/default/lib/the_package/build/one.erl"),
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
                path: PathBuf::from("_build/default/lib/the_package/build/two.erl"),
                text: "-module(two).
-compile(no_auto_import).

-export([funky/0]).

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
                code: "import one pub fn funky() { one.receive(1) }".to_string(),
            },
        ],
        Ok(vec![
            OutputFile {
                path: PathBuf::from("_build/default/lib/the_package/ebin/the_package.app"),
                text: "{application, the_package, [
    {vsn, \"1.0.0\"},
    {applications, []},
    {description, \"The description\"},
    {modules, [one,
               two]},
    {registered, []}
]}.
"
                .into(),
            },
            OutputFile {
                path: PathBuf::from("_build/default/lib/the_package/build/one.erl"),
                text: "-module(one).
-compile(no_auto_import).

-export(['receive'/1]).

-spec \'receive\'(I) -> I.
'receive'(X) ->
    X.
"
                .to_string(),
            },
            OutputFile {
                path: PathBuf::from("_build/default/lib/the_package/build/two.erl"),
                text: "-module(two).
-compile(no_auto_import).

-export([funky/0]).

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
                path: PathBuf::from("_build/default/lib/the_package/ebin/the_package.app"),
                text: "{application, the_package, [
    {vsn, \"1.0.0\"},
    {applications, []},
    {description, \"The description\"},
    {modules, [one,
               two]},
    {registered, []}
]}.
"
                .into(),
            },
            OutputFile {
                path: PathBuf::from("_build/default/lib/the_package/build/one.erl"),
                text: "-module(one).
-compile(no_auto_import).

-export_type([person/0]).

-type person() :: {person, binary(), integer()}.


"
                .to_string(),
            },
            OutputFile {
                path: PathBuf::from("_build/default/lib/the_package/include/one_Person.hrl"),
                text: "-record(person, {name :: binary(), age :: integer()}).
"
                .to_string(),
            },
            OutputFile {
                path: PathBuf::from("_build/default/lib/the_package/build/two.erl"),
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
                path: PathBuf::from("_build/default/lib/the_package/ebin/the_package.app"),
                text: "{application, the_package, [
    {vsn, \"1.0.0\"},
    {applications, []},
    {description, \"The description\"},
    {modules, [one,
               two]},
    {registered, []}
]}.
"
                .into(),
            },
            OutputFile {
                path: PathBuf::from("_build/default/lib/the_package/build/one.erl"),
                text: "-module(one).
-compile(no_auto_import).

-export_type([person/0]).

-type person() :: {person, binary(), integer()}.


"
                .to_string(),
            },
            OutputFile {
                path: PathBuf::from("_build/default/lib/the_package/include/one_Person.hrl"),
                text: "-record(person, {name :: binary(), age :: integer()}).
"
                .to_string(),
            },
            OutputFile {
                path: PathBuf::from("_build/default/lib/the_package/build/two.erl"),
                text: "-module(two).\n".to_string(),
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
                path: PathBuf::from("_build/default/lib/the_package/ebin/the_package.app"),
                text: "{application, the_package, [
    {vsn, \"1.0.0\"},
    {applications, []},
    {description, \"The description\"},
    {modules, [one,
               two]},
    {registered, []}
]}.
"
                .into(),
            },
            OutputFile {
                path: PathBuf::from("_build/default/lib/the_package/build/one.erl"),
                text: "-module(one).
-compile(no_auto_import).

-export_type([person/0]).\n\n-type person() :: {person, binary(), integer()}.


"
                .to_string(),
            },
            OutputFile {
                path: PathBuf::from("_build/default/lib/the_package/include/one_Person.hrl"),
                text: "-record(person, {name :: binary(), age :: integer()}).
"
                .to_string(),
            },
            OutputFile {
                path: PathBuf::from("_build/default/lib/the_package/build/two.erl"),
                text: "-module(two).\n".to_string(),
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
                path: PathBuf::from("_build/default/lib/the_package/ebin/the_package.app"),
                text: "{application, the_package, [
    {vsn, \"1.0.0\"},
    {applications, []},
    {description, \"The description\"},
    {modules, [one,
               two]},
    {registered, []}
]}.
"
                .into(),
            },
            OutputFile {
                path: PathBuf::from("_build/default/lib/the_package/build/one.erl"),
                text: "-module(one).
-compile(no_auto_import).

-export_type([t/1]).\n\n-type t(I) :: {c, integer(), integer()} | {gleam_phantom, I}.


"
                .to_string(),
            },
            OutputFile {
                path: PathBuf::from("_build/default/lib/the_package/include/one_C.hrl"),
                text: "-record(c, {a :: integer(), b :: integer()}).
"
                .to_string(),
            },
            OutputFile {
                path: PathBuf::from("_build/default/lib/the_package/build/two.erl"),
                text: "-module(two).
-compile(no_auto_import).

-export([main/0]).

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
                code: "import one.{X as E, id as i} pub fn make() { i(E) }".to_string(),
            },
        ],
        Ok(vec![
            OutputFile {
                path: PathBuf::from("_build/default/lib/the_package/ebin/the_package.app"),
                text: "{application, the_package, [
    {vsn, \"1.0.0\"},
    {applications, []},
    {description, \"The description\"},
    {modules, [one,
               two]},
    {registered, []}
]}.
"
                .into(),
            },
            OutputFile {
                path: PathBuf::from("_build/default/lib/the_package/build/one.erl"),
                text: "-module(one).
-compile(no_auto_import).

-export([id/1]).
-export_type([t/0]).

-type t() :: {x, integer()}.

-spec id(I) -> I.
id(X) ->
    X.
"
                .to_string(),
            },
            OutputFile {
                path: PathBuf::from("_build/default/lib/the_package/include/one_X.hrl"),
                text: "-record(x, {x :: integer()}).
"
                .to_string(),
            },
            OutputFile {
                path: PathBuf::from("_build/default/lib/the_package/build/two.erl"),
                text: "-module(two).
-compile(no_auto_import).

-export([make/0]).

-spec make() -> fun((integer()) -> one:t()).
make() ->
    one:id(fun(A) -> {x, A} end).
"
                .to_string(),
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
                path: PathBuf::from("_build/default/lib/the_package/ebin/the_package.app"),
                text: "{application, the_package, [
    {vsn, \"1.0.0\"},
    {applications, []},
    {description, \"The description\"},
    {modules, [one,
               two]},
    {registered, []}
]}.
".into(),
            },
            OutputFile {
                path: PathBuf::from("_build/default/lib/the_package/build/one.erl"),
                text: "-module(one).\n"
                .to_string(),
            },
            OutputFile {
                path: PathBuf::from("_build/default/lib/the_package/build/two.erl"),
                text: r#"-module(two).
-compile(no_auto_import).

-export([make_list/0]).

-spec make_list() -> list(binary()).
make_list() ->
    [<<"aliased"/utf8>>, <<"type"/utf8>>, <<"constructor"/utf8>>].
"#
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
                code: "import one pub fn main() { one.C }".to_string(),
            },
        ],
        Ok(vec![
            OutputFile {
                path: PathBuf::from("_build/default/lib/the_package/ebin/the_package.app"),
                text: "{application, the_package, [
    {vsn, \"1.0.0\"},
    {applications, []},
    {description, \"The description\"},
    {modules, [one,
               two]},
    {registered, []}
]}.
"
                .into(),
            },
            OutputFile {
                path: PathBuf::from("_build/default/lib/the_package/build/one.erl"),
                text: "-module(one).
-compile(no_auto_import).

-export_type([t/1]).

-type t(I) :: {c, integer(), integer()} | {gleam_phantom, I}.


"
                .to_string(),
            },
            OutputFile {
                path: PathBuf::from("_build/default/lib/the_package/include/one_C.hrl"),
                text: "-record(c, {a :: integer(), b :: integer()}).
"
                .to_string(),
            },
            OutputFile {
                path: PathBuf::from("_build/default/lib/the_package/build/two.erl"),
                text: "-module(two).
-compile(no_auto_import).

-export([main/0]).

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
fn bug_752() {
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
                path: PathBuf::from("_build/default/lib/the_package/ebin/the_package.app"),
                text: "{application, the_package, [
    {vsn, \"1.0.0\"},
    {applications, []},
    {description, \"The description\"},
    {modules, [one,
               two]},
    {registered, []}
]}.
"
                .into(),
            },
            OutputFile {
                path: PathBuf::from("_build/default/lib/the_package/build/one.erl",),
                text: "-module(one).
-compile(no_auto_import).

-export_type([one/1]).

-type one(I) :: {one, I}.


"
                .to_string(),
            },
            OutputFile {
                path: PathBuf::from("_build/default/lib/the_package/build/two.erl",),
                text: "-module(two).
-compile(no_auto_import).

-export_type([two/1]).

-type two(K) :: {two, one:one(integer())} | {gleam_phantom, K}.


"
                .to_string(),
            },
            OutputFile {
                path: PathBuf::from("_build/default/lib/the_package/include/two_Two.hrl",),
                text: "-record(two, {thing :: one:one(integer())}).
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
                path: PathBuf::from("_build/default/lib/the_package/ebin/the_package.app"),
                text: "{application, the_package, [
    {vsn, \"1.0.0\"},
    {applications, []},
    {description, \"The description\"},
    {modules, [main,
               power]},
    {registered, []}
]}.
"
                .into(),
            },
            OutputFile {
                path: PathBuf::from("_build/default/lib/the_package/build/main.erl",),
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
                path: PathBuf::from("_build/default/lib/the_package/build/power.erl",),
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
                path: PathBuf::from("_build/default/lib/the_package/include/power_Power.hrl",),
                text: "-record(power, {value :: integer()}).
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
pub fn x() { test }"
                    .to_string(),
            },
        ],
        Ok(vec![
            OutputFile {
                path: PathBuf::from("_build/default/lib/the_package/ebin/the_package.app"),
                text: "{application, the_package, [
    {vsn, \"1.0.0\"},
    {applications, []},
    {description, \"The description\"},
    {modules, [one,
               two]},
    {registered, []}
]}.
"
                .into(),
            },
            OutputFile {
                path: PathBuf::from("_build/default/lib/the_package/build/one.erl"),
                text: "-module(one).
-compile(no_auto_import).

-export_type([test/0]).

-type test() :: a.


"
                .to_string(),
            },
            OutputFile {
                path: PathBuf::from("_build/default/lib/the_package/build/two.erl"),
                text: "-module(two).
-compile(no_auto_import).

-export([x/0]).

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
pub fn x() { test }"
                    .to_string(),
            },
        ],
        Ok(vec![
            OutputFile {
                path: PathBuf::from("_build/default/lib/the_package/ebin/the_package.app"),
                text: "{application, the_package, [
    {vsn, \"1.0.0\"},
    {applications, []},
    {description, \"The description\"},
    {modules, [one,
               two]},
    {registered, []}
]}.
"
                .into(),
            },
            OutputFile {
                path: PathBuf::from("_build/default/lib/the_package/build/one.erl"),
                text: "-module(one).
-compile(no_auto_import).

-export_type([a/0, b/0]).

-type a() :: a.

-type b() :: {b, a()}.


"
                .to_string(),
            },
            OutputFile {
                path: PathBuf::from("_build/default/lib/the_package/build/two.erl"),
                text: "-module(two).
-compile(no_auto_import).

-export([x/0]).

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
            error: type_::Error::UnknownModuleValue {
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
pub fn x() { one.A }"
                    .to_string(),
            },
        ],
        Ok(vec![
            OutputFile {
                path: PathBuf::from("_build/default/lib/the_package/ebin/the_package.app"),
                text: "{application, the_package, [
    {vsn, \"1.0.0\"},
    {applications, []},
    {description, \"The description\"},
    {modules, [one,
               two]},
    {registered, []}
]}.
"
                .into(),
            },
            OutputFile {
                path: PathBuf::from("_build/default/lib/the_package/build/one.erl"),
                text: "-module(one).
-compile(no_auto_import).

-export_type([a/0]).

-type a() :: {a, binary()}.


"
                .to_string(),
            },
            OutputFile {
                path: PathBuf::from("_build/default/lib/the_package/build/two.erl"),
                text: "-module(two).
-compile(no_auto_import).

-export([x/0]).

-spec x() -> fun((binary()) -> one:a()).
x() ->
    fun(A) -> {a, A} end.
"
                .to_string(),
            }
        ]),
    );
}

// https://github.com/gleam-lang/otp/pull/22
#[test]
fn import_shadowed_name_warnings() {
    assert_no_warnings!(vec![
        Source {
            origin: Origin::Src,
            path: PathBuf::from("/src/one.gleam"),
            name: "one".to_string(),
            code: "pub external type Port".to_string(),
        },
        Source {
            origin: Origin::Src,
            path: PathBuf::from("/src/two.gleam"),
            name: "two".to_string(),
            code: r#"import one.{Port}
type Shadowing { Port }

pub external fn use_type(Port) -> Nil =
  "" ""
"#
            .to_string(),
        },
    ]);

    assert_no_warnings!(vec![
        Source {
            origin: Origin::Src,
            path: PathBuf::from("/src/one.gleam"),
            name: "one".to_string(),
            code: "pub type Port { Port }".to_string(),
        },
        Source {
            origin: Origin::Src,
            path: PathBuf::from("/src/two.gleam"),
            name: "two".to_string(),
            code: r#"import one.{Port}
type Shadowing { Port }

pub external fn use_type(Port) -> Nil =
  "" ""
"#
            .to_string(),
        },
    ]);
}

#[test]
fn config_compilation_test() {
    macro_rules! assert_config_compile {
        ($config:expr, $sources:expr, $expected_output:expr $(,)?) => {
            let ids = crate::uid::UniqueIdGenerator::new();
            let config = $config;
            let mut modules = im::HashMap::new();
            let (file_writer, file_receiver) = FilesChannel::new();
            let root = PathBuf::from("some/build/path/root");
            let out = PathBuf::from("_build/default/lib/the_package");
            let lib = PathBuf::from("_build/default/lib");
            let mut build_journal = HashSet::new();
            let mut compiler = PackageCompiler::new(
                &config,
                &root,
                &out,
                &lib,
                Target::Erlang,
                ids,
                file_writer,
                Some(&mut build_journal),
            );
            compiler.write_entrypoint = false;
            compiler.write_metadata = false;
            compiler.compile_beam_bytecode = false;
            compiler.copy_native_files = false;
            compiler.sources = $sources;
            let compiled = compiler
                .compile(&mut vec![], &mut modules, &mut im::HashMap::new())
                .expect("Should compile OK");
            let mut outputs = FilesChannel::recv_utf8_files(&file_receiver).unwrap();
            outputs.sort_by(|a, b| a.path.partial_cmp(&b.path).unwrap());
            assert_eq!($expected_output, outputs);
        };
    };

    fn make_config() -> PackageConfig {
        PackageConfig {
            dependencies: HashMap::new(),
            dev_dependencies: HashMap::new(),
            description: "".to_string(),
            version: Version::parse("1.0.0").unwrap(),
            name: "the_package".to_string(),
            repository: Repository::None,
            documentation: Default::default(),
            licences: Default::default(),
            erlang: Default::default(),
            links: vec![],
            target: Target::Erlang,
        }
    }

    assert_config_compile!(
        {
            let mut config = make_config();
            config.erlang.application_start_module = Some("myapp/mymod".to_string());
            config
        },
        vec![],
        vec![OutputFile {
            text: r#"{application, the_package, [
    {mod, 'myapp@mymod'},
    {vsn, "1.0.0"},
    {applications, []},
    {description, ""},
    {modules, []},
    {registered, []}
]}.
"#
            .to_string(),
            path: PathBuf::from("_build/default/lib/the_package/ebin/the_package.app"),
        }]
    );

    assert_config_compile!(
        make_config(),
        vec![],
        vec![OutputFile {
            text: r#"{application, the_package, [
    {vsn, "1.0.0"},
    {applications, []},
    {description, ""},
    {modules, []},
    {registered, []}
]}.
"#
            .to_string(),
            path: PathBuf::from("_build/default/lib/the_package/ebin/the_package.app"),
        }]
    );

    // Version is included if given
    let mut config = make_config();
    config.version = Version::parse("1.3.5").unwrap();
    assert_config_compile!(
        config,
        vec![],
        vec![OutputFile {
            text: r#"{application, the_package, [
    {vsn, "1.3.5"},
    {applications, []},
    {description, ""},
    {modules, []},
    {registered, []}
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
    {registered, []}
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
    .map(|(a, b)| (a.to_string(), Range::new(b.to_string())))
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
    {registered, []}
]}.
"#
            .to_string(),
            path: PathBuf::from("_build/default/lib/the_package/ebin/the_package.app"),
        }]
    );

    // Dev deps applications are listed
    let mut config = make_config();
    config.dependencies = [("gleam_stdlib", "1.0.0"), ("gleam_otp", "1.0.0")]
        .into_iter()
        .map(|(a, b)| (a.to_string(), Range::new(b.to_string())))
        .collect();
    config.dev_dependencies = [("midas", "1.0.0"), ("simple_json", "1.0.0")]
        .into_iter()
        .map(|(a, b)| (a.to_string(), Range::new(b.to_string())))
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
    {registered, []}
]}.
"#
            .to_string(),
            path: PathBuf::from("_build/default/lib/the_package/ebin/the_package.app"),
        }]
    );

    // Extra applications are included
    let mut config = make_config();
    config.dependencies = [("gleam_stdlib", "1.0.0"), ("gleam_otp", "1.0.0")]
        .into_iter()
        .map(|(a, b)| (a.to_string(), Range::new(b.to_string())))
        .collect();
    config.dev_dependencies = [("midas", "1.0.0"), ("simple_json", "1.0.0")]
        .into_iter()
        .map(|(a, b)| (a.to_string(), Range::new(b.to_string())))
        .collect();
    config.erlang.extra_applications = vec!["inets".into(), "ssl".into()];
    assert_config_compile!(
        config,
        vec![],
        vec![OutputFile {
            text: r#"{application, the_package, [
    {vsn, "1.0.0"},
    {applications, [gleam_otp,
                    gleam_stdlib,
                    inets,
                    midas,
                    simple_json,
                    ssl]},
    {description, ""},
    {modules, []},
    {registered, []}
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

// https://github.com/gleam-lang/gleam/issues/922#issuecomment-803272624
#[test]
fn qualified_constant_with_nested_module() {
    assert_erlang_compile!(
        vec![
            Source {
                origin: Origin::Src,
                path: PathBuf::from("/src/one/two.gleam"),
                name: "one/two".to_string(),
                code: "pub type A { A }".to_string(),
            },
            Source {
                origin: Origin::Src,
                path: PathBuf::from("/src/two.gleam"),
                name: "two".to_string(),
                code: r#"import one/two
const x = two.A"#
                    .to_string(),
            },
        ],
        Ok(vec![
            OutputFile {
                path: PathBuf::from("_build/default/lib/the_package/ebin/the_package.app"),
                text: "{application, the_package, [
    {vsn, \"1.0.0\"},
    {applications, []},
    {description, \"The description\"},
    {modules, [one@two,
               two]},
    {registered, []}
]}.
"
                .into(),
            },
            OutputFile {
                path: PathBuf::from("_build/default/lib/the_package/build/one@two.erl"),
                text: "-module(one@two).
-compile(no_auto_import).

-export_type([a/0]).

-type a() :: a.


"
                .to_string(),
            },
            OutputFile {
                path: PathBuf::from("_build/default/lib/the_package/build/two.erl"),
                text: "-module(two).\n".to_string(),
            }
        ]),
    );
}

#[test]
fn javascript_package() {
    assert_javascript_compile!(
        vec![
            Source {
                origin: Origin::Src,
                path: PathBuf::from("/src/one/two.gleam"),
                name: "one/two".to_string(),
                code: "pub type A { A }".to_string(),
            },
            Source {
                origin: Origin::Src,
                path: PathBuf::from("/src/two.gleam"),
                name: "two".to_string(),
                code: r#"import one/two
const x = two.A"#
                    .to_string(),
            },
        ],
        Ok(vec![
            OutputFile {
                path: PathBuf::from("_build/default/lib/the_package/dist/gleam.mjs"),
                text: javascript::PRELUDE.to_string(),
            },
            OutputFile {
                path: PathBuf::from("_build/default/lib/the_package/dist/one/two.mjs"),
                text: "import { CustomType } from \"../gleam.mjs\";

export class A extends CustomType {}\n"
                    .to_string(),
            },
            OutputFile {
                path: PathBuf::from("_build/default/lib/the_package/dist/two.mjs"),
                text: r#"import * as $two from "./one/two.mjs";

const x = new $two.A();
"#
                .to_string(),
            }
        ]),
    );
}

// https://github.com/gleam-lang/gleam/issues/1495
#[test]
fn import_error() {
    assert_erlang_compile!(
        vec![
            Source {
                origin: Origin::Src,
                path: PathBuf::from("/src/one.gleam"),
                name: "one".to_string(),
                code: "pub type Error { MyError }".to_string(),
            },
            Source {
                origin: Origin::Src,
                path: PathBuf::from("/src/two.gleam"),
                name: "two".to_string(),
                code: r#"import one.{Error}"#.to_string(),
            },
        ],
        Ok(vec![
            OutputFile {
                path: PathBuf::from("_build/default/lib/the_package/ebin/the_package.app"),
                text: "{application, the_package, [
    {vsn, \"1.0.0\"},
    {applications, []},
    {description, \"The description\"},
    {modules, [one,
               two]},
    {registered, []}
]}.
"
                .into(),
            },
            OutputFile {
                path: PathBuf::from("_build/default/lib/the_package/build/one.erl"),
                text: "-module(one).
-compile(no_auto_import).

-export_type([error/0]).

-type error() :: my_error.


"
                .to_string(),
            },
            OutputFile {
                path: PathBuf::from("_build/default/lib/the_package/build/two.erl"),
                text: "-module(two).\n".to_string(),
            }
        ]),
    );
}
