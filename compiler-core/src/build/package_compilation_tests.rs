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
