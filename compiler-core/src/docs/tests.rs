use std::{collections::HashSet, time::SystemTime};

use crate::{
    build::{Mode, NullTelemetry, PackageCompiler, StaleTracker, TargetCodegenConfiguration},
    config::{DocsPage, PackageConfig, Repository},
    docs::DocContext,
    io::{memory::InMemoryFileSystem, FileSystemWriter},
    paths::ProjectPaths,
    uid::UniqueIdGenerator,
    version::COMPILER_VERSION,
    warning::WarningEmitter,
};
use camino::Utf8PathBuf;
use ecow::EcoString;
use itertools::Itertools;

#[derive(Default)]
struct CompileWithMarkdownPagesOpts {
    hex_publish: Option<DocContext>,
}

fn compile_with_markdown_pages(
    config: PackageConfig,
    modules: Vec<(&str, &str)>,
    markdown_pages: Vec<(&str, &str)>,
    opts: CompileWithMarkdownPagesOpts,
) -> EcoString {
    let fs = InMemoryFileSystem::new();
    for (name, src) in modules {
        fs.write(&Utf8PathBuf::from(format!("/src/{name}")), src)
            .unwrap();
    }

    // We're saving the pages under a different `InMemoryFileSystem` for these
    // tests so we don't have to juggle with borrows and lifetimes.
    // The package compiler is going to take ownership of `fs` but later
    // `generate_html` also needs a `FileSystemReader` to go and read the
    // markdown pages' content.
    let pages_fs = InMemoryFileSystem::new();
    for (title, src) in markdown_pages.iter() {
        pages_fs
            .write(&Utf8PathBuf::from(format!("{title}.md")), src)
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
    let mut modules = compiler
        .compile(
            &warnings,
            &mut type_manifests,
            &mut defined_modules,
            &mut StaleTracker::default(),
            &mut HashSet::new(),
            &NullTelemetry,
        )
        .unwrap();

    for module in &mut modules {
        module.attach_doc_and_module_comments();
    }

    let docs_pages = markdown_pages
        .into_iter()
        .map(|(title, _)| DocsPage {
            title: (*title).into(),
            path: format!("{title}.html"),
            source: format!("{title}.md").into(),
        })
        .collect_vec();

    super::generate_html(
        &paths,
        &config,
        &modules,
        &docs_pages,
        pages_fs,
        SystemTime::UNIX_EPOCH,
        if let Some(doc_context) = opts.hex_publish {
            doc_context
        } else {
            DocContext::HexPublish
        },
    )
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

pub fn compile(config: PackageConfig, modules: Vec<(&str, &str)>) -> EcoString {
    compile_with_markdown_pages(
        config,
        modules,
        vec![],
        CompileWithMarkdownPagesOpts::default(),
    )
}

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

#[test]
fn internal_definitions_are_not_included() {
    let config = PackageConfig::default();
    let modules = vec![(
        "app.gleam",
        r#"
@internal
pub const wibble = 1

@internal
pub type Wibble = Int

@internal
pub type Wobble { Wobble }

@internal
pub fn one() { 1 }
"#,
    )];
    insta::assert_snapshot!(compile(config, modules));
}

// https://github.com/gleam-lang/gleam/issues/2561
#[test]
fn discarded_arguments_are_not_shown() {
    let config = PackageConfig::default();
    let modules = vec![("app.gleam", "pub fn discard(_discarded: a) -> Int { 1 }")];
    insta::assert_snapshot!(compile(config, modules));
}

// https://github.com/gleam-lang/gleam/issues/2631
#[test]
fn docs_of_a_type_constructor_are_not_used_by_the_following_function() {
    let mut config = PackageConfig::default();
    config.name = EcoString::from("test_project_name");
    let modules = vec![(
        "app.gleam",
        r#"
pub type Wibble {
  Wobble(
    /// Documentation!!
    wabble: Int,
  )
}

pub fn main() { todo }
"#,
    )];
    insta::assert_snapshot!(compile(config, modules));
}

#[test]
fn markdown_code_from_standalone_pages_is_not_trimmed() {
    let mut config = PackageConfig::default();
    config.name = EcoString::from("test_project_name");
    let pages = vec![(
        "one",
        "
This is an example code snippet that should be indented
```gleam
pub fn indentation_test() {
  todo as \"This line should be indented by two spaces\"
}
```",
    )];
    insta::assert_snapshot!(compile_with_markdown_pages(
        config,
        vec![],
        pages,
        CompileWithMarkdownPagesOpts::default()
    ));
}

#[test]
fn markdown_code_from_function_comment_is_trimmed() {
    let config = PackageConfig::default();
    let modules = vec![(
        "app.gleam",
        "
/// Here's an example code snippet:
/// ```
/// wibble
///   |> wobble
/// ```
///
pub fn indentation_test() {
  todo
}
",
    )];
    insta::assert_snapshot!(compile(config, modules));
}

#[test]
fn markdown_code_from_module_comment_is_trimmed() {
    let mut config = PackageConfig::default();
    config.name = EcoString::from("test_project_name");
    let modules = vec![(
        "app.gleam",
        "
//// Here's an example code snippet:
//// ```
//// wibble
////   |> wobble
//// ```
////
",
    )];
    insta::assert_snapshot!(compile(config, modules));
}

#[test]
fn doc_for_commented_definitions_is_not_included_in_next_constant() {
    let config = PackageConfig::default();
    let modules = vec![(
        "app.gleam",
        "
/// Not included!
// pub fn wibble() {}

/// Included!
pub const wobble = 1
",
    )];
    assert!(!compile(config, modules).contains("Not included!"));
}

#[test]
fn doc_for_commented_definitions_is_not_included_in_next_type() {
    let mut config = PackageConfig::default();
    config.name = EcoString::from("test_project_name");
    let modules = vec![(
        "app.gleam",
        "
/// Not included!
// pub fn wibble() {}

/// Included!
pub type Wibble {
  /// Wobble!
  Wobble
}
",
    )];
    assert!(!compile(config, modules).contains("Not included!"));
}

#[test]
fn doc_for_commented_definitions_is_not_included_in_next_function() {
    let config = PackageConfig::default();
    let modules = vec![(
        "app.gleam",
        "
/// Not included!
// pub fn wibble() {}

/// Included!
pub fn wobble(arg) {}
",
    )];
    assert!(!compile(config, modules).contains("Not included!"));
}

#[test]
fn doc_for_commented_definitions_is_not_included_in_next_type_alias() {
    let mut config = PackageConfig::default();
    config.name = EcoString::from("test_project_name");
    let modules = vec![(
        "app.gleam",
        "
/// Not included!
// pub fn wibble() {}

/// Included!
pub type Wibble = Int
",
    )];
    assert!(!compile(config, modules).contains("Not included!"));
}

#[test]
fn source_link_for_github_repository() {
    let mut config = PackageConfig::default();
    config.repository = Repository::GitHub {
        user: "wibble".to_string(),
        repo: "wobble".to_string(),
        path: None,
    };

    let modules = vec![("app.gleam", "pub type Wibble = Int")];
    assert!(compile(config, modules)
        .contains("https://github.com/wibble/wobble/blob/v0.1.0/src/app.gleam#L1-L1"));
}

#[test]
fn source_link_for_github_repository_with_path() {
    let mut config = PackageConfig::default();
    config.name = EcoString::from("test_project_name");
    config.repository = Repository::GitHub {
        user: "wibble".to_string(),
        repo: "wobble".to_string(),
        path: Some("path/to/package".to_string()),
    };

    let modules = vec![("app.gleam", "pub type Wibble = Int")];
    assert!(compile(config, modules).contains(
        "https://github.com/wibble/wobble/blob/v0.1.0/path/to/package/src/app.gleam#L1-L1"
    ));
}

#[test]
fn canonical_link() {
    let mut config = PackageConfig::default();
    config.name = EcoString::from("test_project_name");
    let modules = vec![
        (
            "app.gleam",
            r#"
/// Here is some documentation
pub fn one() {
  1
}
"#,
        ),
        (
            "gleam/otp/actor.gleam",
            r#"
/// Here is some documentation
pub fn one() {
  1
}
"#,
        ),
    ];

    let pages = vec![(
        "LICENSE",
        r#"
# LICENSE
    "#,
    )];
    insta::assert_snapshot!(compile_with_markdown_pages(
        config,
        modules,
        pages,
        CompileWithMarkdownPagesOpts::default()
    ));
}

#[test]
fn no_hex_publish() {
    let mut config = PackageConfig::default();
    config.name = EcoString::from("test_project_name");
    let modules = vec![
        (
            "app.gleam",
            r#"
/// Here is some documentation
pub fn one() {
  1
}
"#,
        ),
        (
            "gleam/otp/actor.gleam",
            r#"
/// Here is some documentation
pub fn one() {
  1
}
"#,
        ),
    ];

    let pages = vec![(
        "LICENSE",
        r#"
# LICENSE
    "#,
    )];
    insta::assert_snapshot!(compile_with_markdown_pages(
        config,
        modules,
        pages,
        CompileWithMarkdownPagesOpts {
            hex_publish: Some(DocContext::Build)
        }
    ));
}
