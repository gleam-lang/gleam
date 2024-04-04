use std::time::SystemTime;

use crate::{
    build::{Mode, NullTelemetry, PackageCompiler, StaleTracker, TargetCodegenConfiguration},
    config::PackageConfig,
    io::{memory::InMemoryFileSystem, FileSystemWriter},
    paths::ProjectPaths,
    uid::UniqueIdGenerator,
    version::COMPILER_VERSION,
    warning::WarningEmitter,
};
use camino::Utf8PathBuf;
use ecow::EcoString;
use itertools::Itertools;

#[test]
fn hello_docs() {
    let config = PackageConfig::default();
    let modules = vec![(
        "app.gleam",
        r#"
/// Here is some documentation
pub fn one() {
  1
}
"#,
    )];
    insta::assert_snapshot!(compile(config, modules));
}

// https://github.com/gleam-lang/gleam/issues/2347

#[test]
fn tables() {
    let config = PackageConfig::default();
    let modules = vec![(
        "app.gleam",
        r#"
/// | heading 1    | heading 2    |
/// |--------------|--------------|
/// | row 1 cell 1 | row 1 cell 2 |
/// | row 2 cell 1 | row 2 cell 2 |
///
pub fn one() {
  1
}
"#,
    )];
    insta::assert_snapshot!(compile(config, modules));
}

// https://github.com/gleam-lang/gleam/issues/2202
#[test]
fn long_function_wrapping() {
    let config = PackageConfig::default();
    let modules = vec![(
        "app.gleam",
        r#"
pub type Option(t) {
  Some(t)
  None
}

/// Returns the first value if it is `Some`, otherwise evaluates the given
/// function for a fallback value.
///
pub fn lazy_or(first: Option(a), second: fn() -> Option(a)) -> Option(a) {
  case first {
    Some(_) -> first
    None -> second()
  }
}
"#,
    )];

    insta::assert_snapshot!(compile(config, modules));
}

fn compile(config: PackageConfig, modules: Vec<(&str, &str)>) -> EcoString {
    let fs = InMemoryFileSystem::new();
    for (name, src) in modules {
        fs.write(&Utf8PathBuf::from(format!("/src/{}", name)), src)
            .unwrap();
    }

    let ids = UniqueIdGenerator::new();
    let mut type_manifests = im::HashMap::new();
    let mut defined_modules = im::HashMap::new();
    let warnings = WarningEmitter::null();
    let target = TargetCodegenConfiguration::Erlang { app_file: None };

    let root = Utf8PathBuf::from("/");
    let build = root.join("build");
    let lib = root.join("lib");
    let paths = ProjectPaths::new(root.clone());
    let mut compiler =
        PackageCompiler::new(&config, Mode::Dev, &root, &build, &lib, &target, ids, fs);
    compiler.write_entrypoint = false;
    compiler.write_metadata = false;
    compiler.compile_beam_bytecode = true;
    let modules = compiler
        .compile(
            &warnings,
            &mut type_manifests,
            &mut defined_modules,
            &mut StaleTracker::default(),
            &NullTelemetry,
        )
        .unwrap();

    super::generate_html(&paths, &config, &modules, &[], SystemTime::UNIX_EPOCH)
        .into_iter()
        .filter(|file| file.path.extension() == Some("html"))
        .sorted_by(|a, b| a.path.cmp(&b.path))
        .flat_map(|file| {
            Some(format!(
                "//// {}\n\n{}\n\n",
                file.path.as_str(),
                file.content
                    .text()?
                    .replace(COMPILER_VERSION, "GLEAM_VERSION_HERE")
            ))
        })
        .collect::<String>()
        .chars()
        .collect()
}

// https://github.com/gleam-lang/gleam/issues/2561
#[test]
fn discarded_arguments_are_not_shown() {
    let config = PackageConfig::default();
    let modules = vec![("app.gleam", "pub fn discard(_discarded: a) -> Int { 1 }")];
    insta::assert_snapshot!(compile(config, modules));
}
