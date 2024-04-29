use itertools::Itertools;
use lsp_types::{
    CompletionItem, CompletionItemKind, CompletionTextEdit, Documentation, MarkupContent,
    MarkupKind, Position, Range, TextEdit,
};

use super::*;

fn completion(tester: TestProject<'_>, position: Position) -> Vec<CompletionItem> {
    tester.at(position, |engine, param, src| {
        let response = engine.completion(param, src);

        let mut completions = response.result.unwrap().unwrap_or_default();
        completions.sort_by(|a, b| a.label.cmp(&b.label));
        completions
    })
}

fn completion_at_default_position(tester: TestProject<'_>) -> Vec<CompletionItem> {
    let src = &format!("fn typing_in_here() {{\n  0\n}}\n {}", tester.src);
    let tester = TestProject { src, ..tester };
    completion(tester, Position::new(1, 0))
        .into_iter()
        .filter(|c| c.label != "typing_in_here")
        .collect_vec()
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
        completion(TestProject::for_source(code), Position::new(0, 0)),
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
        completion_at_default_position(TestProject::for_source(code)),
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
        completion_at_default_position(TestProject::for_source(code)),
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
        completion_at_default_position(TestProject::for_source(code)),
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
        completion_at_default_position(TestProject::for_source(code)),
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
        completion_at_default_position(TestProject::for_source(code)),
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
        completion_at_default_position(TestProject::for_source(code)),
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
        completion_at_default_position(TestProject::for_source(code).add_module("dep", dep)),
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
        completion_at_default_position(TestProject::for_source(code).add_module("dep", dep)),
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
        completion_at_default_position(TestProject::for_source(code).add_module("dep", dep)),
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
        completion_at_default_position(TestProject::for_source(code).add_module("dep", dep)),
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
        completion_at_default_position(TestProject::for_source(code).add_module("dep", dep)),
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
        completion_at_default_position(TestProject::for_source(code).add_module("dep", dep)),
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
        completion_at_default_position(TestProject::for_source(code).add_module("dep", dep)),
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
        completion_at_default_position(TestProject::for_source(code).add_module("dep", dep)),
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
        completion_at_default_position(TestProject::for_source(code).add_module("dep", dep)),
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
        completion_at_default_position(TestProject::for_source(code).add_module("dep", dep)),
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
        completion_at_default_position(TestProject::for_source(code).add_module("dep", dep)),
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
        completion_at_default_position(TestProject::for_source(code).add_module("dep", dep)),
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
        completion(TestProject::for_source(code), Position::new(2, 0)),
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
        completion(TestProject::for_source(code), Position::new(2, 0)),
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
        completion(TestProject::for_source(code), Position::new(2, 0)),
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
        completion(
            TestProject::for_source(code).add_module("dep", dep),
            Position::new(3, 0)
        ),
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
        completion(
            TestProject::for_source(code).add_module("dep", dep),
            Position::new(3, 0)
        ),
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
        completion(TestProject::for_source(code), Position::new(4, 0)),
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
        completion_at_default_position(
            TestProject::for_source("import dep").add_module("dep", dep)
        ),
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
        completion(
            TestProject::for_source(code).add_module("dep", dep),
            Position::new(3, 0)
        ),
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
        completion_at_default_position(TestProject::for_source(code)),
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
        completion(TestProject::for_source(code), Position::new(3, 0)),
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
        completion(
            TestProject::for_source(code).add_dep_module("dep", dep),
            Position::new(3, 0)
        ),
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
        completion_at_default_position(
            TestProject::for_source("import dep").add_dep_module("dep", dep)
        ),
        vec![]
    );
}

#[test]
fn completions_for_an_import() {
    let code = "import dep

pub fn main() {
  0
}";
    let dep = "";

    assert_eq!(
        completion(
            TestProject::for_source(code).add_module("dep", dep),
            Position::new(0, 10)
        ),
        vec![CompletionItem {
            label: "gleam".into(),
            kind: Some(CompletionItemKind::MODULE),
            text_edit: Some(CompletionTextEdit::Edit(TextEdit {
                range: Range {
                    start: Position {
                        line: 0,
                        character: 7
                    },
                    end: Position {
                        line: 0,
                        character: 10
                    }
                },
                new_text: "gleam".into()
            })),
            ..Default::default()
        }]
    );
}

#[test]
fn completions_for_an_import_no_test() {
    let code = "import gleam

pub fn main() {
  0
}";
    let test = "
import gleam

pub fn main() {
  0
}
";

    assert_eq!(
        completion(
            TestProject::for_source(code).add_test_module("my_tests", test),
            Position::new(0, 10)
        ),
        vec![]
    );
}

#[test]
fn completions_for_an_import_while_in_test() {
    let code = "import gleam

pub fn main() {
  0
}";
    let test = "
import gleam

pub fn main() {
  0
}
";
    let test_helper = "
pub fn test_helper() {
  0
}
";

    let (mut engine, position_param) = TestProject::for_source(code)
        .add_test_module("my_test", test)
        .add_test_module("test_helper", test_helper)
        .positioned_with_io_in_test(Position::new(0, 10), "my_test");

    let response = engine.completion(position_param, code.into());

    let mut completions = response.result.unwrap().unwrap_or_default();
    completions.sort_by(|a, b| a.label.cmp(&b.label));

    assert_eq!(
        completions,
        vec![
            CompletionItem {
                label: "app".into(),
                kind: Some(CompletionItemKind::MODULE),
                text_edit: Some(CompletionTextEdit::Edit(TextEdit {
                    range: Range {
                        start: Position {
                            line: 0,
                            character: 7
                        },
                        end: Position {
                            line: 0,
                            character: 12
                        }
                    },
                    new_text: "app".into()
                })),
                ..Default::default()
            },
            CompletionItem {
                label: "test_helper".into(),
                kind: Some(CompletionItemKind::MODULE),
                text_edit: Some(CompletionTextEdit::Edit(TextEdit {
                    range: Range {
                        start: Position {
                            line: 0,
                            character: 7
                        },
                        end: Position {
                            line: 0,
                            character: 12
                        }
                    },
                    new_text: "test_helper".into()
                })),
                ..Default::default()
            }
        ]
    );
}

#[test]
fn completions_for_an_import_with_docs() {
    let code = "import gleam

pub fn main() {
  0
}";
    let dep = "//// Some package
//// documentation!

pub fn main() { 1 }
    ";

    assert_eq!(
        completion(
            TestProject::for_source(code).add_dep_module("dep", dep),
            Position::new(0, 10)
        ),
        vec![CompletionItem {
            label: "dep".into(),
            kind: Some(CompletionItemKind::MODULE),
            text_edit: Some(CompletionTextEdit::Edit(TextEdit {
                range: Range {
                    start: Position {
                        line: 0,
                        character: 7
                    },
                    end: Position {
                        line: 0,
                        character: 12
                    }
                },
                new_text: "dep".into()
            })),
            ..Default::default()
        }]
    );
}

#[test]
fn completions_for_an_import_from_dependency() {
    let code = "import gleam

pub fn main() {
  0
}";
    let dep = "";

    assert_eq!(
        completion(
            TestProject::for_source(code).add_hex_module("example_module", dep),
            Position::new(0, 10)
        ),
        vec![CompletionItem {
            label: "example_module".into(),
            kind: Some(CompletionItemKind::MODULE),
            text_edit: Some(CompletionTextEdit::Edit(TextEdit {
                range: Range {
                    start: Position {
                        line: 0,
                        character: 7
                    },
                    end: Position {
                        line: 0,
                        character: 12
                    }
                },
                new_text: "example_module".into()
            })),
            ..Default::default()
        }]
    );
}

#[test]
fn completions_for_an_import_not_from_indirect_dependency() {
    let code = "import gleam

pub fn main() {
  0
}";
    let dep = "";

    assert_eq!(
        completion(
            TestProject::for_source(code)
                .add_hex_module("example_module", dep)
                .add_indirect_hex_module("indirect_module", ""),
            Position::new(0, 10)
        ),
        vec![CompletionItem {
            label: "example_module".into(),
            kind: Some(CompletionItemKind::MODULE),
            text_edit: Some(CompletionTextEdit::Edit(TextEdit {
                range: Range {
                    start: Position {
                        line: 0,
                        character: 7
                    },
                    end: Position {
                        line: 0,
                        character: 12
                    }
                },
                new_text: "example_module".into()
            })),
            ..Default::default()
        }]
    );
}

#[test]
fn completions_for_an_import_not_from_dev_dependency() {
    let code = "import gleam

pub fn main() {
  0
}";
    let dep = "";

    assert_eq!(
        completion(
            TestProject::for_source(code)
                .add_hex_module("example_module", dep)
                .add_dev_hex_module("indirect_module", ""),
            Position::new(0, 10)
        ),
        vec![CompletionItem {
            label: "example_module".into(),
            kind: Some(CompletionItemKind::MODULE),
            text_edit: Some(CompletionTextEdit::Edit(TextEdit {
                range: Range {
                    start: Position {
                        line: 0,
                        character: 7
                    },
                    end: Position {
                        line: 0,
                        character: 12
                    }
                },
                new_text: "example_module".into()
            })),
            ..Default::default()
        }]
    );
}

#[test]
fn completions_for_an_import_not_from_dev_dependency_in_test() {
    let code = "import gleam

pub fn main() {
  0
}";
    let test = "import gleam

pub fn main() {
  0
}
";
    let dep = "";

    let (mut engine, position_param) = TestProject::for_source(code)
        .add_test_module("my_test", test)
        .add_hex_module("example_module", dep)
        .add_dev_hex_module("indirect_module", "")
        .positioned_with_io_in_test(Position::new(0, 10), "my_test");

    let response = engine.completion(position_param, code.into());

    let mut completions = response.result.unwrap().unwrap_or_default();
    completions.sort_by(|a, b| a.label.cmp(&b.label));

    assert_eq!(
        completions,
        vec![
            CompletionItem {
                label: "app".into(),
                kind: Some(CompletionItemKind::MODULE),
                text_edit: Some(CompletionTextEdit::Edit(TextEdit {
                    range: Range {
                        start: Position {
                            line: 0,
                            character: 7
                        },
                        end: Position {
                            line: 0,
                            character: 12
                        }
                    },
                    new_text: "app".into()
                })),
                ..Default::default()
            },
            CompletionItem {
                label: "example_module".into(),
                kind: Some(CompletionItemKind::MODULE),
                text_edit: Some(CompletionTextEdit::Edit(TextEdit {
                    range: Range {
                        start: Position {
                            line: 0,
                            character: 7
                        },
                        end: Position {
                            line: 0,
                            character: 12
                        }
                    },
                    new_text: "example_module".into()
                })),
                ..Default::default()
            },
            CompletionItem {
                label: "indirect_module".into(),
                kind: Some(CompletionItemKind::MODULE),
                text_edit: Some(CompletionTextEdit::Edit(TextEdit {
                    range: Range {
                        start: Position {
                            line: 0,
                            character: 7
                        },
                        end: Position {
                            line: 0,
                            character: 12
                        }
                    },
                    new_text: "indirect_module".into()
                })),
                ..Default::default()
            }
        ]
    );
}

#[test]
fn completions_for_an_import_from_dependency_with_docs() {
    let code = "//// Main package
//// documentation!

import gleam

pub fn main() {
  0
}";
    let dep = "//// Some package
//// documentation!

pub fn main() { 1 }
    ";

    assert_eq!(
        completion(
            TestProject::for_source(code).add_hex_module("example_module", dep),
            Position::new(3, 10)
        ),
        vec![CompletionItem {
            label: "example_module".into(),
            kind: Some(CompletionItemKind::MODULE),
            text_edit: Some(CompletionTextEdit::Edit(TextEdit {
                range: Range {
                    start: Position {
                        line: 3,
                        character: 7
                    },
                    end: Position {
                        line: 3,
                        character: 12
                    }
                },
                new_text: "example_module".into()
            })),
            ..Default::default()
        }]
    );
}

#[test]
fn completions_for_an_import_start() {
    let code = "import gleam

pub fn main() {
  0
}";
    let dep = "";

    assert_eq!(
        completion(
            TestProject::for_source(code).add_dep_module("dep", dep),
            Position::new(0, 0)
        ),
        vec![CompletionItem {
            label: "dep".into(),
            kind: Some(CompletionItemKind::MODULE),
            text_edit: Some(CompletionTextEdit::Edit(TextEdit {
                range: Range {
                    start: Position {
                        line: 0,
                        character: 7
                    },
                    end: Position {
                        line: 0,
                        character: 12
                    }
                },
                new_text: "dep".into()
            })),
            ..Default::default()
        }]
    );
}

#[test]
fn completions_for_an_import_preceeding_whitespace() {
    let code = " import gleam

pub fn main() {
  0
}";
    let dep = "";

    assert_eq!(
        completion(
            TestProject::for_source(code).add_dep_module("dep", dep),
            Position::new(0, 2)
        ),
        vec![CompletionItem {
            label: "dep".into(),
            kind: Some(CompletionItemKind::MODULE),
            text_edit: Some(CompletionTextEdit::Edit(TextEdit {
                range: Range {
                    start: Position {
                        line: 0,
                        character: 7
                    },
                    end: Position {
                        line: 0,
                        character: 13
                    }
                },
                new_text: "dep".into()
            })),
            ..Default::default()
        }]
    );
}

#[test]
fn internal_modules_from_same_package_are_included() {
    let code = "import gleam

pub fn main() {
  0
}";
    let internal_name = format!("{}/internal", LSP_TEST_ROOT_PACKAGE_NAME);

    assert_eq!(
        completion(
            TestProject::for_source(code)
                // Not included
                .add_dep_module("dep/internal", "")
                // Included
                .add_module(&internal_name, ""),
            Position::new(0, 0)
        ),
        vec![CompletionItem {
            label: internal_name.clone(),
            kind: Some(CompletionItemKind::MODULE),
            text_edit: Some(CompletionTextEdit::Edit(TextEdit {
                range: Range {
                    start: Position {
                        line: 0,
                        character: 7
                    },
                    end: Position {
                        line: 0,
                        character: 12
                    }
                },
                new_text: internal_name.clone(),
            })),
            ..Default::default()
        },]
    );
}

#[test]
fn completions_for_an_unqualified_import() {
    let code = "
import dep.{}

pub fn main() {
  0
}";
    let dep = "pub const wibble = \"wibble\"
const wobble = \"wobble\"
@internal
pub const wabble = \"wabble\"

pub fn myfun() {
    0
}

pub type Wibble = String
";

    assert_eq!(
        completion(
            TestProject::for_source(code).add_module("dep", dep),
            Position::new(1, 12)
        ),
        vec![
            CompletionItem {
                label: "Wibble".into(),
                kind: Some(CompletionItemKind::CLASS),
                detail: Some("Type".into()),
                insert_text: Some("type Wibble".into()),
                ..Default::default()
            },
            CompletionItem {
                label: "myfun".into(),
                kind: Some(CompletionItemKind::FUNCTION),
                detail: Some("fn() -> Int".into()),
                ..Default::default()
            },
            CompletionItem {
                label: "wabble".into(),
                kind: Some(CompletionItemKind::CONSTANT),
                detail: Some("String".into()),
                ..Default::default()
            },
            CompletionItem {
                label: "wibble".into(),
                kind: Some(CompletionItemKind::CONSTANT),
                detail: Some("String".into()),
                ..Default::default()
            },
        ]
    );
}

#[test]
fn completions_for_an_unqualified_import_on_new_line() {
    let code = "
import dep.{
  wibble,

}

pub fn main() {
  0
}";
    let dep = "pub const wibble = \"wibble\"

pub fn myfun() {
    0
}

pub type Wibble = String
";

    assert_eq!(
        completion(
            TestProject::for_source(code).add_module("dep", dep),
            // putting cursor at beginning of line because some formatters
            // remove the empty whitespace in the test code.
            // Does also work with (3, 2) when empty spaces are not removed.
            Position::new(3, 0)
        ),
        vec![
            CompletionItem {
                label: "Wibble".into(),
                kind: Some(CompletionItemKind::CLASS),
                detail: Some("Type".into()),
                insert_text: Some("type Wibble".into()),
                ..Default::default()
            },
            CompletionItem {
                label: "myfun".into(),
                kind: Some(CompletionItemKind::FUNCTION),
                detail: Some("fn() -> Int".into()),
                ..Default::default()
            },
        ]
    );
}

#[test]
fn completions_for_an_unqualified_import_already_imported() {
    let code = "
import dep.{wibble,wabble,type Wibble}

pub fn main() {
  0
}";
    let dep = "pub const wibble = \"wibble\"
const wobble = \"wobble\"
@internal
pub const wabble = \"wabble\"

pub fn myfun() {
    0
}

pub type Wibble = String
";

    assert_eq!(
        completion(
            TestProject::for_source(code).add_module("dep", dep),
            Position::new(1, 12)
        ),
        vec![CompletionItem {
            label: "myfun".into(),
            kind: Some(CompletionItemKind::FUNCTION),
            detail: Some("fn() -> Int".into()),
            ..Default::default()
        },]
    );
}

#[test]
fn completions_for_a_function_arg_annotation() {
    let code = "
pub fn wibble(
  _: String,
) -> Nil {
  Nil
}
";

    assert_eq!(
        completion(TestProject::for_source(code), Position::new(2, 11)),
        prelude_type_completions(),
    );
}

#[test]
fn completions_for_a_function_return_annotation() {
    let code = "
pub fn wibble(
  _: String,
) -> Nil {
  Nil
}
";

    assert_eq!(
        completion(TestProject::for_source(code), Position::new(3, 7)),
        prelude_type_completions(),
    );
}

#[test]
fn completions_for_a_var_annotation() {
    let code = "
pub fn main() {
  let wibble: Int = 7
}
";

    assert_eq!(
        completion(TestProject::for_source(code), Position::new(2, 16)),
        prelude_type_completions(),
    );
}

#[test]
fn completions_for_a_const_annotation() {
    let code = "

const wibble: Int = 7

pub fn main() {
  let wibble: Int = 7
}
";

    assert_eq!(
        completion(TestProject::for_source(code), Position::new(2, 16)),
        prelude_type_completions(),
    );
}
