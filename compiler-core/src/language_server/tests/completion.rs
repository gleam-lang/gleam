use itertools::Itertools;
use lsp_types::{
    CompletionItem, CompletionItemKind, Documentation, MarkupContent, MarkupKind, Position,
    TextDocumentIdentifier, TextDocumentPositionParams, Url,
};

use super::*;

struct Completions<'a> {
    src: &'a str,
    root_package_modules: Vec<(&'a str, &'a str)>,
    dependency_modules: Vec<(&'a str, &'a str)>,
}

impl<'a> Completions<'a> {
    pub fn for_source(src: &'a str) -> Self {
        Completions {
            src,
            root_package_modules: vec![],
            dependency_modules: vec![],
        }
    }

    pub fn add_module(self, name: &'a str, src: &'a str) -> Self {
        let mut root_package_modules = self.root_package_modules;
        root_package_modules.push((name, src));
        Completions {
            root_package_modules,
            ..self
        }
    }

    pub fn add_dep_module(self, name: &'a str, src: &'a str) -> Self {
        let mut dependency_modules = self.dependency_modules;
        dependency_modules.push((name, src));
        Completions {
            dependency_modules,
            ..self
        }
    }

    pub fn at(self, position: Position) -> Vec<CompletionItem> {
        let io = LanguageServerTestIO::new();
        let mut engine = setup_engine(&io);

        // Add an external dependency and all its modules
        add_path_dep(&mut engine, "dep");
        self.dependency_modules.iter().for_each(|(name, code)| {
            let _ = io.path_dep_module("dep", name, code);
        });

        // Add all the modules belonging to the root package
        self.root_package_modules.iter().for_each(|(name, code)| {
            let _ = io.src_module(name, code);
        });

        // Add the final module we're going to be positioning the cursor in.
        _ = io.src_module("app", self.src);

        let response = engine.compile_please();
        assert!(response.result.is_ok());

        let path = Utf8PathBuf::from(if cfg!(target_family = "windows") {
            r"\\?\C:\src\app.gleam"
        } else {
            "/src/app.gleam"
        });

        let url = Url::from_file_path(path).unwrap();

        let response = engine.completion(TextDocumentPositionParams::new(
            TextDocumentIdentifier::new(url),
            position,
        ));

        let mut completions = response.result.unwrap().unwrap_or_default();
        completions.sort_by(|a, b| a.label.cmp(&b.label));
        completions
    }

    pub fn at_default_position(self) -> Vec<CompletionItem> {
        let src = &format!("fn typing_in_here() {{\n  0\n}}\n {}", self.src);
        let completions = Completions { src, ..self };
        completions
            .at(Position::new(1, 0))
            .into_iter()
            .filter(|c| c.label != "typing_in_here")
            .collect_vec()
    }
}

fn add_path_dep<B>(engine: &mut LanguageServerEngine<LanguageServerTestIO, B>, name: &str) {
    let path = engine.paths.root().join(name);
    let compiler = &mut engine.compiler.project_compiler;
    _ = compiler
        .config
        .dependencies
        .insert(name.into(), Requirement::Path { path: path.clone() });
    _ = compiler.packages.insert(
        name.into(),
        ManifestPackage {
            name: name.into(),
            version: Version::new(1, 0, 0),
            build_tools: vec!["gleam".into()],
            otp_app: None,
            requirements: vec![],
            source: ManifestPackageSource::Local { path: path.clone() },
        },
    );
    let toml = format!(
        r#"name = "{name}"
version = "1.0.0""#
    );
    _ = compiler.io.write(&path.join("gleam.toml"), &toml);
}

fn prelude_type_completions() -> Vec<CompletionItem> {
    vec![
        CompletionItem {
            label: "BitArray".into(),
            kind: Some(CompletionItemKind::CLASS),
            detail: Some("Type".into()),
            documentation: None,
            ..Default::default()
        },
        CompletionItem {
            label: "Bool".into(),
            kind: Some(CompletionItemKind::CLASS),
            detail: Some("Type".into()),
            documentation: None,
            ..Default::default()
        },
        CompletionItem {
            label: "Float".into(),
            kind: Some(CompletionItemKind::CLASS),
            detail: Some("Type".into()),
            documentation: None,
            ..Default::default()
        },
        CompletionItem {
            label: "Int".into(),
            kind: Some(CompletionItemKind::CLASS),
            detail: Some("Type".into()),
            documentation: None,
            ..Default::default()
        },
        CompletionItem {
            label: "List".into(),
            kind: Some(CompletionItemKind::CLASS),
            detail: Some("Type".into()),
            documentation: None,
            ..Default::default()
        },
        CompletionItem {
            label: "Nil".into(),
            kind: Some(CompletionItemKind::CLASS),
            detail: Some("Type".into()),
            documentation: None,
            ..Default::default()
        },
        CompletionItem {
            label: "Result".into(),
            kind: Some(CompletionItemKind::CLASS),
            detail: Some("Type".into()),
            documentation: None,
            ..Default::default()
        },
        CompletionItem {
            label: "String".into(),
            kind: Some(CompletionItemKind::CLASS),
            detail: Some("Type".into()),
            documentation: None,
            ..Default::default()
        },
        CompletionItem {
            label: "UtfCodepoint".into(),
            kind: Some(CompletionItemKind::CLASS),
            detail: Some("Type".into()),
            documentation: None,
            ..Default::default()
        },
    ]
}

#[test]
fn completions_for_outside_a_function() {
    let code = "

pub fn main() {
  0
}";

    assert_eq!(
        Completions::for_source(code).at(Position::new(0, 0)),
        vec![]
    );
}

#[test]
fn local_public_function() {
    let code = "
pub fn main() {
  0
}";

    assert_eq!(
        Completions::for_source(code).at_default_position(),
        vec![CompletionItem {
            label: "main".into(),
            kind: Some(CompletionItemKind::FUNCTION),
            detail: Some("fn() -> Int".into()),
            documentation: None,
            ..Default::default()
        }]
    );
}

#[test]
fn local_public_function_with_documentation() {
    let code = "
/// Hello
pub fn main() {
  0
}";

    assert_eq!(
        Completions::for_source(code).at_default_position(),
        vec![CompletionItem {
            label: "main".into(),
            kind: Some(CompletionItemKind::FUNCTION),
            detail: Some("fn() -> Int".into()),
            documentation: Some(Documentation::MarkupContent(MarkupContent {
                kind: MarkupKind::Markdown,
                value: " Hello\n".into(),
            })),
            ..Default::default()
        }]
    );
}

#[test]
fn local_public_enum() {
    let code = "
pub type Direction {
  Left
  Right
}
";

    assert_eq!(
        Completions::for_source(code).at_default_position(),
        vec![
            CompletionItem {
                label: "Left".into(),
                kind: Some(CompletionItemKind::ENUM_MEMBER),
                detail: Some("Direction".into()),
                documentation: None,
                ..Default::default()
            },
            CompletionItem {
                label: "Right".into(),
                kind: Some(CompletionItemKind::ENUM_MEMBER),
                detail: Some("Direction".into()),
                documentation: None,
                ..Default::default()
            }
        ]
    );
}

#[test]
fn local_public_record() {
    let code = "
pub type Box {
/// Hello
  Box(Int, Int, Float)
}
";

    assert_eq!(
        Completions::for_source(code).at_default_position(),
        vec![CompletionItem {
            label: "Box".into(),
            kind: Some(CompletionItemKind::CONSTRUCTOR),
            detail: Some("fn(Int, Int, Float) -> Box".into()),
            documentation: Some(Documentation::MarkupContent(MarkupContent {
                kind: MarkupKind::Markdown,
                value: " Hello\n".into(),
            })),
            ..Default::default()
        }]
    );
}

#[test]
fn local_public_enum_with_documentation() {
    let code = "
pub type Direction {
  /// Hello
  Left
  /// Goodbye
  Right
}
";

    assert_eq!(
        Completions::for_source(code).at_default_position(),
        vec![
            CompletionItem {
                label: "Left".into(),
                kind: Some(CompletionItemKind::ENUM_MEMBER),
                detail: Some("Direction".into()),
                documentation: Some(Documentation::MarkupContent(MarkupContent {
                    kind: MarkupKind::Markdown,
                    value: " Hello\n".into(),
                })),
                ..Default::default()
            },
            CompletionItem {
                label: "Right".into(),
                kind: Some(CompletionItemKind::ENUM_MEMBER),
                detail: Some("Direction".into()),
                documentation: Some(Documentation::MarkupContent(MarkupContent {
                    kind: MarkupKind::Markdown,
                    value: " Goodbye\n".into(),
                })),
                ..Default::default()
            }
        ]
    );
}

#[test]
fn local_public_record_with_documentation() {
    let code = "
pub type Box {
  Box(Int, Int, Float)
}
";

    assert_eq!(
        Completions::for_source(code).at_default_position(),
        vec![CompletionItem {
            label: "Box".into(),
            kind: Some(CompletionItemKind::CONSTRUCTOR),
            detail: Some("fn(Int, Int, Float) -> Box".into()),
            documentation: None,
            ..Default::default()
        }]
    );
}

#[test]
fn imported_module_function() {
    let code = "
import dep
";
    let dep = "
pub fn wobble() {
  Nil
}
";

    assert_eq!(
        Completions::for_source(code)
            .add_module("dep", dep)
            .at_default_position(),
        vec![CompletionItem {
            label: "dep.wobble".into(),
            kind: Some(CompletionItemKind::FUNCTION),
            detail: Some("fn() -> Nil".into()),
            documentation: None,
            ..Default::default()
        }]
    );
}

#[test]
fn imported_public_enum() {
    let code = "
import dep
";
    let dep = "
pub type Direction {
  Left
  Right
}
";

    assert_eq!(
        Completions::for_source(code)
            .add_module("dep", dep)
            .at_default_position(),
        vec![
            CompletionItem {
                label: "dep.Left".into(),
                kind: Some(CompletionItemKind::ENUM_MEMBER),
                detail: Some("Direction".into()),
                documentation: None,
                ..Default::default()
            },
            CompletionItem {
                label: "dep.Right".into(),
                kind: Some(CompletionItemKind::ENUM_MEMBER),
                detail: Some("Direction".into()),
                documentation: None,
                ..Default::default()
            }
        ]
    );
}

#[test]
fn imported_public_record() {
    let code = "
import dep
";
    let dep = "
pub type Box {
  Box(Int)
}
";

    assert_eq!(
        Completions::for_source(code)
            .add_module("dep", dep)
            .at_default_position(),
        vec![CompletionItem {
            label: "dep.Box".into(),
            kind: Some(CompletionItemKind::CONSTRUCTOR),
            detail: Some("fn(Int) -> Box".into()),
            documentation: None,
            ..Default::default()
        }]
    );
}

#[test]
fn imported_unqualified_module_function() {
    let code = "
import dep.{wobble}
";
    let dep = "
pub fn wobble() {
  Nil
}
";

    assert_eq!(
        Completions::for_source(code)
            .add_module("dep", dep)
            .at_default_position(),
        vec![
            CompletionItem {
                label: "dep.wobble".into(),
                kind: Some(CompletionItemKind::FUNCTION),
                detail: Some("fn() -> Nil".into()),
                documentation: None,
                ..Default::default()
            },
            CompletionItem {
                label: "wobble".into(),
                kind: Some(CompletionItemKind::FUNCTION),
                detail: Some("fn() -> Nil".into()),
                documentation: None,
                ..Default::default()
            },
        ]
    );
}

#[test]
fn imported_unqualified_public_enum() {
    let code = "
import dep.{Left}
";
    let dep = "
pub type Direction {
  Left
  Right
}
";

    assert_eq!(
        Completions::for_source(code)
            .add_module("dep", dep)
            .at_default_position(),
        vec![
            CompletionItem {
                label: "Left".into(),
                kind: Some(CompletionItemKind::ENUM_MEMBER),
                detail: Some("Direction".into()),
                documentation: None,
                ..Default::default()
            },
            CompletionItem {
                label: "dep.Left".into(),
                kind: Some(CompletionItemKind::ENUM_MEMBER),
                detail: Some("Direction".into()),
                documentation: None,
                ..Default::default()
            },
            CompletionItem {
                label: "dep.Right".into(),
                kind: Some(CompletionItemKind::ENUM_MEMBER),
                detail: Some("Direction".into()),
                documentation: None,
                ..Default::default()
            },
        ]
    );
}

#[test]
fn imported_unqualified_public_record() {
    let code = "
import dep.{Box}
";
    let dep = "
pub type Box {
  Box(Int)
}
";

    assert_eq!(
        Completions::for_source(code)
            .add_module("dep", dep)
            .at_default_position(),
        vec![
            CompletionItem {
                label: "Box".into(),
                kind: Some(CompletionItemKind::CONSTRUCTOR),
                detail: Some("fn(Int) -> Box".into()),
                documentation: None,
                ..Default::default()
            },
            CompletionItem {
                label: "dep.Box".into(),
                kind: Some(CompletionItemKind::CONSTRUCTOR),
                detail: Some("fn(Int) -> Box".into()),
                documentation: None,
                ..Default::default()
            },
        ]
    );
}

#[test]
fn private_function() {
    let code = "
fn private() {
  1
}
";
    let dep = "";

    assert_eq!(
        Completions::for_source(code)
            .add_module("dep", dep)
            .at_default_position(),
        vec![CompletionItem {
            label: "private".into(),
            kind: Some(CompletionItemKind::FUNCTION),
            detail: Some("fn() -> Int".into()),
            documentation: None,
            ..Default::default()
        },]
    );
}

#[test]
fn private_type() {
    let code = "
type Wibble {
  Wobble
}
";
    let dep = "";

    assert_eq!(
        Completions::for_source(code)
            .add_module("dep", dep)
            .at_default_position(),
        vec![CompletionItem {
            label: "Wobble".into(),
            kind: Some(CompletionItemKind::ENUM_MEMBER),
            detail: Some("Wibble".into()),
            documentation: None,
            ..Default::default()
        },]
    );
}

#[test]
fn opaque_type() {
    let code = "
pub opaque type Wibble {
  Wobble
}
";
    let dep = "";

    assert_eq!(
        Completions::for_source(code)
            .add_module("dep", dep)
            .at_default_position(),
        vec![CompletionItem {
            label: "Wobble".into(),
            kind: Some(CompletionItemKind::ENUM_MEMBER),
            detail: Some("Wibble".into()),
            documentation: None,
            ..Default::default()
        },]
    );
}

#[test]
fn private_function_in_dep() {
    let code = "import dep";
    let dep = "
fn private() {
  1
}
";

    assert_eq!(
        Completions::for_source(code)
            .add_module("dep", dep)
            .at_default_position(),
        vec![]
    );
}

#[test]
fn private_type_in_dep() {
    let code = "import dep";
    let dep = "
type Wibble {
  Wobble
}
";

    assert_eq!(
        Completions::for_source(code)
            .add_module("dep", dep)
            .at_default_position(),
        vec![]
    );
}

#[test]
fn in_custom_type_definition() {
    let code = "import dep";
    let dep = "
type Wibble {
  Wobble
}
";

    assert_eq!(
        Completions::for_source(code)
            .add_module("dep", dep)
            .at_default_position(),
        vec![]
    );
}

#[test]
fn for_custom_type_definition() {
    let code = "
pub type Wibble {
  Wobble
}";

    assert_eq!(
        Completions::for_source(code).at(Position::new(2, 0)),
        [
            prelude_type_completions(),
            vec![CompletionItem {
                label: "Wibble".into(),
                kind: Some(CompletionItemKind::CLASS),
                detail: Some("Type".into()),
                documentation: None,
                ..Default::default()
            },]
        ]
        .concat()
    );
}

#[test]
fn for_type_alias() {
    let code = "
pub type Wibble = Result(
  String,
  String
)
";

    assert_eq!(
        Completions::for_source(code).at(Position::new(2, 0)),
        [
            prelude_type_completions(),
            vec![CompletionItem {
                label: "Wibble".into(),
                kind: Some(CompletionItemKind::CLASS),
                detail: Some("Type".into()),
                documentation: None,
                ..Default::default()
            },]
        ]
        .concat()
    );
}

#[test]
fn for_function_arguments() {
    let code = "
pub fn wibble(
  _: String,
) -> Nil {
  Nil
}
";

    assert_eq!(
        Completions::for_source(code).at(Position::new(2, 0)),
        prelude_type_completions(),
    );
}

#[test]
fn imported_type() {
    let dep = "
pub type Zoo = List(String)
type Private = List(String)
";
    let code = "import dep

pub fn wibble(
  _: String,
) -> Nil {
  Nil
}
";

    assert_eq!(
        Completions::for_source(code)
            .add_module("dep", dep)
            .at(Position::new(3, 0)),
        [
            prelude_type_completions(),
            vec![CompletionItem {
                label: "dep.Zoo".into(),
                kind: Some(CompletionItemKind::CLASS),
                detail: Some("Type".into()),
                documentation: None,
                ..Default::default()
            },]
        ]
        .concat()
    );
}

#[test]
fn unqualified_imported_type() {
    let dep = "
pub type Zoo = List(String)
type Private = List(String)
";
    let code = "import dep.{type Zoo}

pub fn wibble(
  _: String,
) -> Nil {
  Nil
}
";

    assert_eq!(
        Completions::for_source(code)
            .add_module("dep", dep)
            .at(Position::new(3, 0)),
        [
            prelude_type_completions(),
            vec![
                CompletionItem {
                    label: "Zoo".into(),
                    kind: Some(CompletionItemKind::CLASS),
                    detail: Some("Type".into()),
                    documentation: None,
                    ..Default::default()
                },
                CompletionItem {
                    label: "dep.Zoo".into(),
                    kind: Some(CompletionItemKind::CLASS),
                    detail: Some("Type".into()),
                    documentation: None,
                    ..Default::default()
                },
            ]
        ]
        .concat()
    );
}

#[test]
fn local_private_type() {
    let code = "
type Zoo = Int

pub fn wibble(
  x: String,
) -> String {
  \"ok\"
}
";

    assert_eq!(
        Completions::for_source(code).at(Position::new(4, 0)),
        [
            prelude_type_completions(),
            vec![CompletionItem {
                label: "Zoo".into(),
                kind: Some(CompletionItemKind::CLASS),
                detail: Some("Type".into()),
                documentation: None,
                ..Default::default()
            }],
        ]
        .concat()
    );
}

#[test]
fn internal_values_from_root_package_are_in_the_completions() {
    let dep = r#"
@external(erlang, "rand", "uniform")
@internal pub fn random_float() -> Float
@internal pub fn main() { 0 }
@internal pub type Foo { Bar }
@internal pub const foo = 1
"#;

    assert_eq!(
        Completions::for_source("import dep")
            .add_module("dep", dep)
            .at_default_position(),
        vec![
            CompletionItem {
                label: "dep.Bar".into(),
                label_details: None,
                kind: Some(CompletionItemKind::ENUM_MEMBER),
                detail: Some("Foo".into()),
                documentation: None,
                ..Default::default()
            },
            CompletionItem {
                label: "dep.foo".into(),
                label_details: None,
                kind: Some(CompletionItemKind::CONSTANT),
                detail: Some("Int".into()),
                documentation: None,
                ..Default::default()
            },
            CompletionItem {
                label: "dep.main".into(),
                label_details: None,
                kind: Some(CompletionItemKind::FUNCTION),
                detail: Some("fn() -> Int".into()),
                documentation: None,
                ..Default::default()
            },
            CompletionItem {
                label: "dep.random_float".into(),
                label_details: None,
                kind: Some(CompletionItemKind::FUNCTION),
                detail: Some("fn() -> Float".into()),
                documentation: None,
                ..Default::default()
            },
        ]
    );
}

#[test]
fn internal_types_from_root_package_are_in_the_completions() {
    let code = "import dep

pub fn wibble(
    _: String,
) -> Nil {
    Nil
}";

    let dep = r#"
@internal pub type Alias = Int
@internal pub type AnotherType { Constructor }
"#;
    let mut expected_completions = prelude_type_completions();
    expected_completions.append(&mut vec![
        CompletionItem {
            label: "dep.Alias".into(),
            label_details: None,
            kind: Some(CompletionItemKind::CLASS),
            detail: Some("Type".into()),
            ..Default::default()
        },
        CompletionItem {
            label: "dep.AnotherType".into(),
            label_details: None,
            kind: Some(CompletionItemKind::CLASS),
            detail: Some("Type".into()),
            ..Default::default()
        },
    ]);

    assert_eq!(
        Completions::for_source(code)
            .add_module("dep", dep)
            .at(Position::new(3, 0)),
        expected_completions,
    );
}

#[test]
fn internal_values_from_the_same_module_are_in_the_completions() {
    let code = r#"
@external(erlang, "rand", "uniform")
@internal pub fn random_float() -> Float
@internal pub fn main() { 0 }
@internal pub type Foo { Bar }
@internal pub const foo = 1
"#;

    assert_eq!(
        Completions::for_source(code).at_default_position(),
        vec![
            CompletionItem {
                label: "Bar".into(),
                label_details: None,
                kind: Some(CompletionItemKind::ENUM_MEMBER),
                detail: Some("Foo".into()),
                documentation: None,
                ..Default::default()
            },
            CompletionItem {
                label: "foo".into(),
                label_details: None,
                kind: Some(CompletionItemKind::CONSTANT),
                detail: Some("Int".into()),
                documentation: None,
                ..Default::default()
            },
            CompletionItem {
                label: "main".into(),
                label_details: None,
                kind: Some(CompletionItemKind::FUNCTION),
                detail: Some("fn() -> Int".into()),
                documentation: None,
                ..Default::default()
            },
            CompletionItem {
                label: "random_float".into(),
                label_details: None,
                kind: Some(CompletionItemKind::FUNCTION),
                detail: Some("fn() -> Float".into()),
                documentation: None,
                ..Default::default()
            },
        ]
    );
}

#[test]
fn internal_types_from_the_same_module_are_in_the_completions() {
    let code = "
@internal pub type Alias = Result(Int, String)
@internal pub type AnotherType {
  Bar
}
";

    assert_eq!(
        Completions::for_source(code).at(Position::new(3, 0)),
        [
            vec![
                CompletionItem {
                    label: "Alias".into(),
                    kind: Some(CompletionItemKind::CLASS),
                    detail: Some("Type".into()),
                    documentation: None,
                    ..Default::default()
                },
                CompletionItem {
                    label: "AnotherType".into(),
                    kind: Some(CompletionItemKind::CLASS),
                    detail: Some("Type".into()),
                    documentation: None,
                    ..Default::default()
                },
            ],
            prelude_type_completions(),
        ]
        .concat()
    );
}

#[test]
fn internal_types_from_a_dependency_are_ignored() {
    let code = "import dep

pub fn wibble(
    _: String,
) -> Nil {
    Nil
}";

    let dep = r#"
@internal pub type Alias = Int
@internal pub type AnotherType { Constructor }
"#;

    assert_eq!(
        Completions::for_source(code)
            .add_dep_module("dep", dep)
            .at(Position::new(3, 0)),
        prelude_type_completions(),
    );
}

#[test]
fn internal_values_from_a_dependency_are_ignored() {
    let dep = r#"
@external(erlang, "rand", "uniform")
@internal pub fn random_float() -> Float
@internal pub fn main() { 0 }
@internal pub type Foo { Bar }
@internal pub const foo = 1
"#;

    assert_eq!(
        Completions::for_source("import dep")
            .add_dep_module("dep", dep)
            .at_default_position(),
        vec![]
    );
}
