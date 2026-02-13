use std::{
    collections::{HashMap, HashSet},
    time::SystemTime,
};

use super::{
    Dependency, DependencyKind, DocumentationConfig, SearchData, SearchItem, SearchItemType,
    SearchProgrammingLanguage,
    printer::{PrintOptions, Printer},
    source_links::SourceLinker,
};
use crate::{
    build::{
        self, Mode, NullTelemetry, Origin, PackageCompiler, StaleTracker,
        TargetCodegenConfiguration,
    },
    config::{DocsPage, PackageConfig, Repository},
    docs::{DocContext, search_item_for_module, search_item_for_type, search_item_for_value},
    io::{FileSystemWriter, memory::InMemoryFileSystem},
    paths::ProjectPaths,
    type_,
    uid::UniqueIdGenerator,
    version::COMPILER_VERSION,
    warning::WarningEmitter,
};
use camino::Utf8PathBuf;
use ecow::{EcoString, eco_format};
use hexpm::version::Version;
use http::Uri;
use itertools::Itertools;
use serde_json::to_string as serde_to_string;

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
        .unwrap()
        .modules;

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
        DocumentationConfig {
            package_config: &config,
            dependencies: HashMap::new(),
            analysed: &modules,
            docs_pages: &docs_pages,
            rendering_timestamp: SystemTime::UNIX_EPOCH,
            context: if let Some(doc_context) = opts.hex_publish {
                doc_context
            } else {
                DocContext::HexPublish
            },
        },
        pages_fs,
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

fn compile_documentation(
    module_name: &str,
    module_src: &str,
    modules: Vec<(&str, &str, &str)>,
    dependency_kind: DependencyKind,
    options: PrintOptions,
) -> EcoString {
    let module = type_::tests::compile_module(module_name, module_src, None, modules.clone())
        .expect("Module should compile successfully");

    let mut config = PackageConfig::default();
    config.name = "thepackage".into();
    let paths = ProjectPaths::new("/".into());
    let build_module = build::Module {
        name: "main".into(),
        code: module_src.into(),
        mtime: SystemTime::now(),
        input_path: "/".into(),
        origin: Origin::Src,
        ast: module,
        extra: Default::default(),
        dependencies: Default::default(),
    };

    let source_links = SourceLinker::new(&paths, &config, &build_module);

    let module = build_module.ast;
    let dependencies = modules
        .iter()
        .map(|(package, _, _)| {
            (
                EcoString::from(*package),
                Dependency {
                    version: Version::new(1, 0, 0),
                    kind: dependency_kind,
                },
            )
        })
        .collect();

    let mut printer = Printer::new(
        module.type_info.package.clone(),
        module.name.clone(),
        &module.names,
        &dependencies,
    );
    printer.set_options(options);

    let types = printer.type_definitions(&source_links, &module.definitions);
    let values = printer.value_definitions(&source_links, &module.definitions);

    let mut output = EcoString::new();

    output.push_str("---- SOURCE CODE\n");
    for (_package, name, src) in modules {
        output.push_str(&format!("-- {name}.gleam\n{src}\n\n"));
    }
    output.push_str("-- ");
    output.push_str(module_name);
    output.push_str(".gleam\n");
    output.push_str(module_src);

    if !types.is_empty() {
        output.push_str("\n\n---- TYPES");
    }
    for type_ in types {
        output.push_str("\n\n--- ");
        output.push_str(type_.name);
        if !type_.documentation.is_empty() {
            output.push('\n');
            output.push_str(&type_.documentation);
        }
        output.push_str("\n<pre><code>");
        output.push_str(&type_.definition);
        output.push_str("</code></pre>");

        if !type_.constructors.is_empty() {
            output.push_str("\n\n-- CONSTRUCTORS");
        }
        for constructor in type_.constructors {
            output.push_str("\n\n");
            if !constructor.documentation.is_empty() {
                output.push_str(&constructor.documentation);
                output.push('\n');
            }
            output.push_str("<pre><code>");
            output.push_str(&constructor.definition);
            output.push_str("</code></pre>");
        }
    }

    if !values.is_empty() {
        output.push_str("\n\n---- VALUES");
    }
    for value in values {
        output.push_str("\n\n--- ");
        output.push_str(value.name);
        if !value.documentation.is_empty() {
            output.push('\n');
            output.push_str(&value.documentation);
        }
        output.push_str("\n<pre><code>");
        output.push_str(&value.definition);
        output.push_str("</code></pre>");
    }

    output
}

macro_rules! assert_documentation {
    ($src:literal $(,)?) => {
        assert_documentation!($src, PrintOptions::all());
    };

    ($src:literal, $options:expr $(,)?) => {
        let output = compile_documentation("main", $src, Vec::new(), DependencyKind::Hex, $options);
        insta::assert_snapshot!(output);
    };

    ($(($name:expr, $module_src:literal)),+, $src:literal $(,)?) => {
        let output = compile_documentation(
            "main",
            $src,
            vec![$(("thepackage", $name, $module_src)),*],
            DependencyKind::Hex,
            PrintOptions::all(),
        );
        insta::assert_snapshot!(output);
    };

    ($(($name:expr, $module_src:literal)),+, $src:literal, $options:expr $(,)?) => {
        let output = compile_documentation(
            "main",
            $src,
            vec![$(("thepackage", $name, $module_src)),*],
            DependencyKind::Hex,
            $options,
        );
        insta::assert_snapshot!(output);
    };

    ($(($name:expr, $module_src:literal)),+, $main_module:literal, $src:literal, $options:expr $(,)?) => {
        let output = compile_documentation(
            $main_module,
            $src,
            vec![$(("thepackage", $name, $module_src)),*],
            DependencyKind::Hex,
            $options,
        );
        insta::assert_snapshot!(output);
    };

    ($(($package:expr, $name:expr, $module_src:literal)),+, $src:literal $(,)?) => {
        let output = compile_documentation(
            "main",
            $src,
            vec![$(($package, $name, $module_src)),*],
            DependencyKind::Hex,
            PrintOptions::all(),
        );
        insta::assert_snapshot!(output);
    };

    ($(($package:expr, $name:expr, $module_src:literal)),+, $src:literal, $options:expr $(,)?) => {
        let output = compile_documentation(
            "main",
            $src,
            vec![$(($package, $name, $module_src)),*],
            DependencyKind::Hex,
            $options,
        );
        insta::assert_snapshot!(output);
    };

    (git: $(($package:expr, $name:expr, $module_src:literal)),+, $src:literal, $options:expr $(,)?) => {
        let output = compile_documentation(
            "main",
            $src,
            vec![$(($package, $name, $module_src)),*],
            DependencyKind::Git,
            $options,
        );
        insta::assert_snapshot!(output);
    };

    (path: $(($package:expr, $name:expr, $module_src:literal)),+, $src:literal, $options:expr $(,)?) => {
        let output = compile_documentation(
            "main",
            $src,
            vec![$(($package, $name, $module_src)),*],
            DependencyKind::Path,
            $options,
        );
        insta::assert_snapshot!(output);
    };
}

#[test]
fn hello_docs() {
    let mut config = PackageConfig::default();
    config.name = EcoString::from("test_project_name");
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

#[test]
fn ignored_argument_is_called_arg() {
    let mut config = PackageConfig::default();
    config.name = EcoString::from("test_project_name");
    let modules = vec![("app.gleam", "pub fn one(_) { 1 }")];
    insta::assert_snapshot!(compile(config, modules));
}

// https://github.com/gleam-lang/gleam/issues/2347
#[test]
fn tables() {
    let mut config = PackageConfig::default();
    config.name = EcoString::from("test_project_name");
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
    let mut config = PackageConfig::default();
    config.name = EcoString::from("test_project_name");
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
    let mut config = PackageConfig::default();
    config.name = EcoString::from("test_project_name");
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
    let mut config = PackageConfig::default();
    config.name = EcoString::from("test_project_name");
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
    let mut config = PackageConfig::default();
    config.name = EcoString::from("test_project_name");
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
    let mut config = PackageConfig::default();
    config.name = EcoString::from("test_project_name");
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
    let mut config = PackageConfig::default();
    config.name = EcoString::from("test_project_name");
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
    config.name = EcoString::from("test_project_name");
    config.repository = Some(Repository::GitHub {
        user: "wibble".to_string(),
        repo: "wobble".to_string(),
        path: None,
        tag_prefix: None,
    });

    let modules = vec![("app.gleam", "pub type Wibble = Int")];
    assert!(
        compile(config, modules)
            .contains("https://github.com/wibble/wobble/blob/v0.1.0/src/app.gleam#L1")
    );
}

#[test]
fn source_link_for_github_repository_with_path_and_tag_prefix() {
    let mut config = PackageConfig::default();
    config.name = EcoString::from("test_project_name");
    config.repository = Some(Repository::GitHub {
        user: "wibble".to_string(),
        repo: "wobble".to_string(),
        path: Some("path/to/package".to_string()),
        tag_prefix: Some("subdir-".into()),
    });

    let modules = vec![("app.gleam", "pub type Wibble = Int")];
    assert!(compile(config, modules).contains(
        "https://github.com/wibble/wobble/blob/subdir-v0.1.0/path/to/package/src/app.gleam#L1"
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

fn create_sample_search_data() -> SearchData {
    SearchData {
        items: vec![
            SearchItem {
                type_: SearchItemType::Module,
                parent_title: "gleam/option".to_string(),
                title: "gleam/option".to_string(),
                content: "".to_string(),
                reference: "gleam/option.html".to_string(),
            },
            SearchItem {
                type_: SearchItemType::Type,
                parent_title: "gleam/option".to_string(),
                title: "Option".to_string(),
                content: "`Option` represents a value that may be present or not. `Some` means the value is present, `None` means the value is not.".to_string(),
                reference: "gleam/option.html#Option".to_string(),
            },
            SearchItem {
                type_: SearchItemType::Value,
                parent_title: "gleam/option".to_string(),
                title: "unwrap".to_string(),
                content: "Extracts the value from an `Option`, returning a default value if there is none.".to_string(),
                reference: "gleam/option.html#unwrap".to_string(),
            },
            SearchItem {
                type_: SearchItemType::Value,
                parent_title: "gleam/dynamic/decode".to_string(),
                title: "bool".to_string(),
                content: "A decoder that decodes `Bool` values.\n\n # Examples\n\n \n let result = decode.run(dynamic.from(True), decode.bool)\n assert result == Ok(True)\n \n".to_string(),
                reference: "gleam/dynamic/decode.html#bool".to_string(),
            },
        ],
        programming_language: SearchProgrammingLanguage::Gleam,
    }
}

#[test]
fn ensure_search_data_matches_exdocs_search_data_model_specification() {
    let data = create_sample_search_data();
    let json = serde_to_string(&data).unwrap();

    let parsed: serde_json::Value = serde_json::from_str(&json).unwrap();

    // Ensure output of SearchData matches specification
    assert!(parsed.is_object());
    let obj = parsed.as_object().unwrap();
    assert!(obj.contains_key("items"));
    assert!(obj.contains_key("proglang"));

    // Ensure output of SearchItem matches specification
    let items = obj.get("items").unwrap().as_array().unwrap();
    for item in items {
        let item = item.as_object().unwrap();
        assert!(item.contains_key("type"));
        assert!(item.contains_key("parentTitle"));
        assert!(item.contains_key("title"));
        assert!(item.contains_key("doc"));
        assert!(item.contains_key("ref"));
    }
}

#[test]
fn output_of_search_data_json() {
    let data = create_sample_search_data();
    let json = serde_to_string(&data).unwrap();
    insta::assert_snapshot!(json);
}

const ONLY_LINKS: PrintOptions = PrintOptions {
    print_highlighting: false,
    print_html: true,
};
const NONE: PrintOptions = PrintOptions {
    print_highlighting: false,
    print_html: false,
};

#[test]
fn highlight_function_definition() {
    assert_documentation!(
        "
pub fn wibble(list: List(Int), generic: a, function: fn(a) -> b) -> #(a, b) { todo }
"
    );
}

#[test]
fn highlight_constant_definition() {
    assert_documentation!(
        "
pub const x = 22
"
    );
}

#[test]
fn highlight_type_alias() {
    assert_documentation!(
        "
pub type Option(a) = Result(a, Nil)
"
    );
}

#[test]
fn highlight_custom_type() {
    assert_documentation!(
        "
pub type Wibble(a, b) {
  Wibble(a, i: Int)
  Wobble(b: b, c: String)
}
"
    );
}

#[test]
fn highlight_opaque_custom_type() {
    assert_documentation!(
        "
pub opaque type Wibble(a, b) {
  Wibble(a, i: Int)
  Wobble(b: b, c: String)
}
"
    );
}

// https://github.com/gleam-lang/gleam/issues/2629
#[test]
fn print_type_variables_in_function_signatures() {
    assert_documentation!(
        "
pub type Dict(key, value)

pub fn insert(dict: Dict(key, value), key: key, value: value) -> Dict(key, value) {
  dict
}
",
        NONE
    );
}

// https://github.com/gleam-lang/gleam/issues/828
#[test]
fn print_qualified_names_from_other_modules() {
    assert_documentation!(
        (
            "gleam/option",
            "
pub type Option(t) {
  Some(t)
  None
}
"
        ),
        "
import gleam/option.{type Option, Some, None}

pub fn from_option(o: Option(t), e: e) -> Result(t, e) {
  case o {
    Some(t) -> Ok(t)
    None -> Error(e)
  }
}
",
        NONE
    );
}

// https://github.com/gleam-lang/gleam/issues/3461
#[test]
fn link_to_type_in_same_module() {
    assert_documentation!(
        "
pub type Dict(a, b)

pub fn new() -> Dict(a, b) { todo }
",
        ONLY_LINKS
    );
}

// https://github.com/gleam-lang/gleam/issues/3461
#[test]
fn link_to_type_in_different_module() {
    assert_documentation!(
        ("gleam/dict", "pub type Dict(a, b)"),
        "
import gleam/dict

pub fn make_dict() -> dict.Dict(a, b) { todo }
",
        ONLY_LINKS
    );
}

#[test]
fn link_to_type_in_different_module_from_nested_module() {
    assert_documentation!(
        ("gleam/dict", "pub type Dict(a, b)"),
        "gleam/dynamic/decode",
        "
import gleam/dict

pub fn decode_dict() -> dict.Dict(a, b) { todo }
",
        ONLY_LINKS
    );
}

#[test]
fn link_to_type_in_different_module_from_nested_module_with_shared_path() {
    assert_documentation!(
        ("gleam/dynamic", "pub type Dynamic"),
        "gleam/dynamic/decode",
        "
import gleam/dynamic

pub type Dynamic = dynamic.Dynamic
",
        ONLY_LINKS
    );
}

// https://github.com/gleam-lang/gleam/issues/3461
#[test]
fn link_to_type_in_different_package() {
    assert_documentation!(
        ("gleam_stdlib", "gleam/dict", "pub type Dict(a, b)"),
        "
import gleam/dict

pub fn make_dict() -> dict.Dict(a, b) { todo }
",
        ONLY_LINKS
    );
}

#[test]
fn no_link_to_type_in_git_dependency() {
    assert_documentation!(
        git: ("gleam_stdlib", "gleam/dict", "pub type Dict(a, b)"),
        "
import gleam/dict

pub fn make_dict() -> dict.Dict(a, b) { todo }
",
        ONLY_LINKS
    );
}

#[test]
fn no_link_to_type_in_path_dependency() {
    assert_documentation!(
        path: ("gleam_stdlib", "gleam/dict", "pub type Dict(a, b)"),
        "
import gleam/dict

pub fn make_dict() -> dict.Dict(a, b) { todo }
",
        ONLY_LINKS
    );
}

#[test]
fn no_links_to_prelude_types() {
    assert_documentation!(
        "
pub fn int_to_string(i: Int) -> String { todo }
",
        ONLY_LINKS
    );
}

#[test]
fn generated_type_variables() {
    assert_documentation!(
        "
pub fn wibble(_a, _b, _c, _d) {
  todo
}
",
        NONE
    );
}

#[test]
fn generated_type_variables_mixed_with_existing_variables() {
    assert_documentation!(
        "
pub fn wibble(_a: b, _b: a, _c, _d) {
  todo
}
",
        NONE
    );
}

#[test]
fn generated_type_variables_with_existing_variables_coming_afterwards() {
    assert_documentation!(
        "
pub fn wibble(_a, _b, _c: b, _d: a) {
  todo
}
",
        NONE
    );
}

#[test]
fn generated_type_variables_do_not_take_into_account_other_definitions() {
    assert_documentation!(
        "
pub fn wibble(_a: a, _b: b, _c: c) -> d {
  todo
}

pub fn identity(x) { x }
",
        NONE
    );
}

#[test]
fn internal_type_reexport_in_same_module_as_parameter() {
    assert_documentation!(
        "
@internal
pub type Internal

pub type External =
  List(Internal)
",
        ONLY_LINKS
    );
}

#[test]
fn internal_type_reexport_in_same_module_as_parameter_colours() {
    assert_documentation!(
        "
@internal
pub type Internal

pub type External =
  List(Internal)
",
    );
}

#[test]
fn internal_type_reexport_in_same_module() {
    assert_documentation!(
        "
@internal
pub type Internal

pub type External =
  Internal
",
        ONLY_LINKS
    );
}

#[test]
fn internal_type_reexport_in_different_module() {
    assert_documentation!(
        ("other", "@internal pub type Internal"),
        "
import other

pub type External =
  other.Internal
",
        ONLY_LINKS
    );
}

#[test]
fn public_type_reexport_in_different_internal_module() {
    assert_documentation!(
        ("thepackage/internal/other", "pub type Internal"),
        "
import thepackage/internal/other

pub type External =
  other.Internal
",
        ONLY_LINKS
    );
}

#[test]
fn use_reexport_from_other_package() {
    assert_documentation!(
        ("some_package", "some_package/internal", "pub type Internal"),
        (
            "some_package",
            "some_package/api",
            "
import some_package/internal
pub type External = internal.Internal
"
        ),
        "
import some_package/api

pub fn do_thing(value: api.External) {
  value
}
",
        ONLY_LINKS
    );
}

#[test]
fn function_uses_reexport_of_internal_type() {
    assert_documentation!(
        ("thepackage/internal", "pub type Internal"),
        "
import thepackage/internal

pub type External = internal.Internal

pub fn do_thing(value: internal.Internal) -> External {
  value
}
",
        ONLY_LINKS
    );
}

#[test]
fn function_uses_reexport_of_internal_type_in_other_module() {
    assert_documentation!(
        ("thepackage/internal", "pub type Internal"),
        (
            "thepackage/something",
            "
import thepackage/internal

pub type External = internal.Internal
"
        ),
        "
import thepackage/something

pub fn do_thing(value: something.External) {
  value
}
",
        ONLY_LINKS
    );
}

#[test]
fn constructor_with_long_types_and_many_fields() {
    assert_documentation!(
        ("option", "pub type Option(a)"),
        "
import option

pub type Uri {
    Uri(
        scheme: option.Option(String),
        userinfo: option.Option(String),
        host: option.Option(String),
        port: option.Option(Int),
        path: String,
        query: option.Option(String),
        fragment: option.Option(String)
    )
}
",
        NONE
    );
}

#[test]
fn constructor_with_long_types_and_many_fields_that_need_splitting() {
    assert_documentation!(
        ("option", "pub type Option(a)"),
        "
import option

pub type TypeWithAVeryLoooooooooooooooooooongName

pub type Wibble {
    Wibble(
        wibble: #(TypeWithAVeryLoooooooooooooooooooongName, TypeWithAVeryLoooooooooooooooooooongName),
        wobble: option.Option(String),
    )
}
",
        NONE
    );
}

#[test]
fn gitea_repository_url_has_no_double_slash() {
    let repo = Repository::Forgejo {
        host: "https://code.example.org/".parse::<Uri>().unwrap(),
        user: "person".into(),
        repo: "forgejo_bug".into(),
        path: None,
        tag_prefix: None,
    };

    assert_eq!(repo.url(), "https://code.example.org/person/forgejo_bug");
}

#[test]
fn long_function_with_no_arguments_parentheses_are_not_split() {
    assert_documentation!(
        "
pub fn aaaaaaaaaaaaaaaaaaaaaaaaaaaa() -> aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa {
  todo
}
",
        NONE
    );
}

#[test]
fn forgejo_single_line_definition() {
    let mut config = PackageConfig::default();
    let repo = Repository::Forgejo {
        host: "https://code.example.org/".parse::<Uri>().unwrap(),
        user: "wibble".into(),
        repo: "wobble".into(),
        path: None,
        tag_prefix: None,
    };

    config.name = EcoString::from("test_project_name");
    config.repository = Some(repo);

    let modules = vec![("app.gleam", "pub type Wibble = Int")];
    let html = compile(config, modules);

    assert!(
        html.contains("https://code.example.org/wibble/wobble/src/tag/v0.1.0/src/app.gleam#L1")
    );
}

#[test]
fn forgejo_multiple_line_definition() {
    let mut config = PackageConfig::default();
    let repo = Repository::Forgejo {
        host: "https://code.example.org/".parse::<Uri>().unwrap(),
        user: "wibble".into(),
        repo: "wobble".into(),
        path: None,
        tag_prefix: None,
    };

    config.name = EcoString::from("test_project_name");
    config.repository = Some(repo);

    let modules = vec![("app.gleam", "pub type Wibble \n\n= Int")];
    let html = compile(config, modules);

    assert!(
        html.contains("https://code.example.org/wibble/wobble/src/tag/v0.1.0/src/app.gleam#L1-L3")
    );
}

fn generate_search_data(module_name: &str, module_src: &str) -> EcoString {
    let module = type_::tests::compile_module(module_name, module_src, None, Vec::new())
        .expect("Module should compile successfully");

    let mut config = PackageConfig::default();
    config.name = "thepackage".into();
    let paths = ProjectPaths::new("/".into());
    let build_module = build::Module {
        name: "main".into(),
        code: module_src.into(),
        mtime: SystemTime::now(),
        input_path: "/".into(),
        origin: Origin::Src,
        ast: module,
        extra: Default::default(),
        dependencies: Default::default(),
    };

    let source_links = SourceLinker::new(&paths, &config, &build_module);

    let module = &build_module.ast;

    let dependencies = HashMap::new();
    let mut printer = Printer::new(
        module.type_info.package.clone(),
        module.name.clone(),
        &module.names,
        &dependencies,
    );

    let mut search_items = Vec::new();

    let types = printer.type_definitions(&source_links, &module.definitions);
    let values = printer.value_definitions(&source_links, &module.definitions);

    search_items.push(search_item_for_module(&build_module));

    for type_ in types {
        search_items.push(search_item_for_type(module_name, &type_));
    }
    for value in values {
        search_items.push(search_item_for_value(module_name, &value));
    }

    let mut output = EcoString::new();

    output.push_str("------ SOURCE CODE\n");
    output.push_str(module_src);
    output.push_str("\n------------------------------------\n\n");

    for item in search_items {
        let SearchItem {
            type_,
            parent_title,
            title,
            content,
            reference,
        } = item;

        let type_ = match type_ {
            SearchItemType::Value => "Value",
            SearchItemType::Module => "Module",
            SearchItemType::Page => "Page",
            SearchItemType::Type => "Type",
        };

        output.push_str(&eco_format!(
            "TITLE:         {title}
PARENT TITLE:  {parent_title}
TYPE:          {type_}
REFERENCE:     {reference}
CONTENT:
{content}
------------------------------------
"
        ));
    }

    output
}

#[test]
fn search_item_for_custom_type() {
    let output = generate_search_data(
        "module",
        "
/// # The `Option` type
/// Represents an optional value, either `Some` or `None`.
/// If it is None, the value is absent
/// [Read more](https://example.com)
pub type Option(inner) {
  /// Here is some
  /// documentation for the `Some` constructor
  Some(
    /// And even documentation on a **field**!
    value: inner
  )
  None
}
",
    );
    insta::assert_snapshot!(output);
}

#[test]
fn search_item_for_type_alias() {
    let output = generate_search_data(
        "module",
        "
/// This is a type alias to a list
/// of integer values.
/// ## Examples
/// ```
/// // Examples
/// ```
pub type IntList = List(Int)
",
    );
    insta::assert_snapshot!(output);
}

#[test]
fn search_item_for_function() {
    let output = generate_search_data(
        "module",
        "
/// Pi is the ration between a circle's **radius** and its
/// *circumference*. Pretty cool!
pub const pi = 3.14
",
    );
    insta::assert_snapshot!(output);
}

#[test]
fn search_item_for_constant() {
    let output = generate_search_data(
        "module",
        "
/// Reverses a `List`, and returns the list in reverse.
/// ```
/// reverse([1, 2, 3])
/// // [3, 2, 1]
/// ```
pub fn reverse(list: List(a), out: List(a)) -> List(a) {
  case list {
    [] -> out
    [first, ..rest] -> reverse(rest, [first, ..out])
  }
}
",
    );
    insta::assert_snapshot!(output);
}
