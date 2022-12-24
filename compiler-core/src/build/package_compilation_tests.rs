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
