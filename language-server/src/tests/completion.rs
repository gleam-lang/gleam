use insta::assert_debug_snapshot;
use itertools::Itertools;
use lsp_types::{CompletionItem, Position};

use super::*;

pub fn show_complete(code: &str, position: Position) -> String {
    let mut str: String = "".into();
    for (line_number, line) in code.lines().enumerate() {
        let same_line = line_number as u32 == position.line;
        if !same_line {
            str.push_str(line);
        } else {
            str.push_str(&line[0..position.character as usize]);
            str.push('|');
            str.push_str(&line[position.character as usize..]);
        }

        str.push('\n');
    }

    str
}

fn apply_completion(src: &str, completions: Vec<CompletionItem>, value: &str) -> String {
    let completion = completions
        .iter()
        .find(|c| c.label == value)
        .unwrap_or_else(|| panic!("no completion with value `{value}`"));

    let mut edits = vec![];
    if let Some(lsp_types::CompletionTextEdit::Edit(edit)) = &completion.text_edit {
        edits.push(edit.clone());
    }
    apply_code_edit(src, edits)
}

#[macro_export]
macro_rules! assert_apply_completion {
    ($project:expr, $name:literal, $position:expr) => {
        let src = $project.src;
        let completions = completion($project, $position);
        let output = format!(
            "{}\n\n----- After applying completion -----\n{}",
            show_complete(src, $position),
            apply_completion(src, completions, $name)
        );
        insta::assert_snapshot!(insta::internals::AutoName, output, src);
    };
}

#[macro_export]
macro_rules! assert_completion {
    ($project:expr) => {
        let src = $project.src;
        let result = completion_with_prefix($project, "");
        let output = format!(
            "{}\n\n----- Completion content -----\n{}",
            show_complete(src, Position::new(0, 0)),
            format_completion_results(result)
        );
        insta::assert_snapshot!(insta::internals::AutoName, output, src);
    };
    ($project:expr, $position:expr) => {
        let src = $project.src;
        let result = completion($project, $position);
        let output = format!(
            "{}\n\n----- Completion content -----\n{}",
            show_complete(src, $position),
            format_completion_results(result)
        );
        insta::assert_snapshot!(insta::internals::AutoName, output, src);
    };
}

#[macro_export]
macro_rules! assert_completion_with_prefix {
    ($project:expr, $prefix:expr) => {
        let src = $project.src;
        let result = completion_with_prefix($project, $prefix);
        let line = 1 + $prefix.lines().count();
        let output = format!(
            "{}\n\n----- Completion content -----\n{}",
            show_complete(src, Position::new(line as u32, 0)),
            format_completion_results(result)
        );
        insta::assert_snapshot!(insta::internals::AutoName, output, src);
    };
}

fn format_completion_results(completions: Vec<CompletionItem>) -> EcoString {
    use std::fmt::Write;
    let mut buffer: EcoString = "".into();

    for CompletionItem {
        label,
        label_details,
        kind,
        detail,
        documentation,
        deprecated,
        preselect,
        sort_text,
        filter_text,
        insert_text,
        insert_text_format,
        insert_text_mode,
        text_edit,
        additional_text_edits,
        command,
        commit_characters,
        data,
        tags,
    } in completions
    {
        assert!(deprecated.is_none());
        assert!(preselect.is_none());
        assert!(filter_text.is_none());
        assert!(insert_text.is_none());
        assert!(insert_text_format.is_none());
        assert!(insert_text_mode.is_none());
        assert!(command.is_none());
        assert!(commit_characters.is_none());
        assert!(data.is_none());
        assert!(tags.is_none());

        buffer.push_str(&label);

        if let Some(kind) = kind {
            write!(buffer, "\n  kind:   {kind:?}").unwrap();
        }

        if let Some(detail) = detail {
            write!(buffer, "\n  detail: {detail}").unwrap();
        }

        if let Some(sort_text) = sort_text {
            write!(buffer, "\n  sort:   {sort_text}").unwrap();
        }

        if let Some(label_details) = label_details {
            assert!(label_details.detail.is_none());
            if let Some(desc) = label_details.description {
                write!(buffer, "\n  desc:   {desc}").unwrap();
            }
        }

        if let Some(documentation) = documentation {
            let lsp_types::Documentation::MarkupContent(m) = documentation else {
                panic!("unexpected docs in test {documentation:?}");
            };
            match m.kind {
                lsp_types::MarkupKind::Markdown => (),
                lsp_types::MarkupKind::PlainText => {
                    panic!("unexpected docs markup kind {:?}", m.kind)
                }
            };
            write!(buffer, "\n  docs:   {:?}", m.value).unwrap();
        }

        let edit = |buffer: &mut EcoString, e: lsp_types::TextEdit| {
            let a = e.range.start.line;
            let b = e.range.start.character;
            let c = e.range.end.line;
            let d = e.range.end.character;
            write!(buffer, "\n    [{a}:{b}-{c}:{d}]: {:?}", e.new_text).unwrap();
        };

        if let Some(text_edit) = text_edit {
            let lsp_types::CompletionTextEdit::Edit(e) = text_edit else {
                panic!("unexpected text edit in test {text_edit:?}");
            };
            buffer.push_str("\n  edits:");
            edit(&mut buffer, e);
        }

        for e in additional_text_edits.unwrap_or_default() {
            edit(&mut buffer, e);
        }

        buffer.push('\n');
    }

    buffer
}

fn completion(tester: TestProject<'_>, position: Position) -> Vec<CompletionItem> {
    tester.at(position, |engine, param, src| {
        let response = engine.completion(param, src);

        let mut completions = response.result.unwrap().unwrap_or_default();
        completions.sort_by(|a, b| a.label.cmp(&b.label));
        completions
    })
}

fn completion_with_prefix(tester: TestProject<'_>, prefix: &str) -> Vec<CompletionItem> {
    let src = &format!("{}fn typing_in_here() {{\n  0\n}}\n {}", prefix, tester.src);
    let tester = TestProject { src, ..tester };
    // Put the cursor inside the "typing_in_here" fn body.
    let line = 1 + prefix.lines().count();
    completion(tester, Position::new(line as u32, 0))
        .into_iter()
        .filter(|c| c.label != "typing_in_here")
        .collect_vec()
}

#[test]
fn completions_for_outside_a_function() {
    let code = "

pub fn main() {
  0
}";

    assert_completion!(TestProject::for_source(code), Position::new(0, 0));
}

#[test]
fn local_public_function() {
    let code = "
pub fn main() {
  0
}";

    assert_completion!(TestProject::for_source(code));
}

#[test]
fn local_public_function_with_documentation() {
    let code = "
/// Hello
pub fn main() {
  0
}";

    assert_completion!(TestProject::for_source(code));
}

#[test]
fn local_public_enum() {
    let code = "
pub type Direction {
  Left
  Right
}
";

    assert_completion!(TestProject::for_source(code));
}

#[test]
fn local_public_record() {
    let code = "
pub type Box {
/// Hello
  Box(Int, Int, Float)
}
";

    assert_completion!(TestProject::for_source(code));
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

    assert_completion!(TestProject::for_source(code));
}

#[test]
fn local_public_record_with_documentation() {
    let code = "
pub type Box {
  Box(Int, Int, Float)
}
";

    assert_completion!(TestProject::for_source(code));
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

    assert_completion!(TestProject::for_source(code).add_module("dep", dep));
}

#[test]
fn importable_module_function() {
    let code = "
";
    let dep = "
pub fn wobble() {
  Nil
}
";

    assert_completion!(TestProject::for_source(code).add_module("dep", dep));
}

#[test]
fn importable_module_function_with_existing_imports() {
    let code = "
//// Some module comments
// Some other whitespace

import dep2
";
    let dep = "
pub fn wobble() {
  Nil
}
";
    let dep2 = "
pub fn wobble() {
  Nil
}
";

    assert_completion!(
        TestProject::for_source(code)
            .add_module("dep", dep)
            .add_module("dep2", dep2)
    );
}

#[test]
fn importable_module_function_from_deep_module() {
    let code = "
";
    let dep = "
pub fn wobble() {
  Nil
}
";

    assert_completion!(TestProject::for_source(code).add_module("a/b/dep", dep));
}

#[test]
fn completions_for_type_import_completions_without_brackets() {
    let src = "import dep.";
    let dep = "
pub opaque type Wibble {
  Wibble(wibble: String, wobble: Int)
}
";
    let position = Position::new(0, 11);
    let tester = TestProject::for_source("import dep").add_module("dep", dep);
    let mut io = LanguageServerTestIO::new();
    let mut engine = tester.build_engine(&mut io);
    // pass a valid src to compile once
    _ = io.src_module("app", tester.src);
    let _ = engine.compile_please();
    // update src to the one we want to test
    _ = io.src_module("app", src);
    let param = tester.build_path(position);
    let response = engine.completion(param, src.into());

    let mut completions = response.result.unwrap().unwrap_or_default();
    completions.sort_by(|a, b| a.label.cmp(&b.label));
    let output = format!(
        "{}\n\n----- Completion content -----\n{}",
        show_complete(src, position),
        format_completion_results(completions)
    );
    insta::assert_snapshot!(insta::internals::AutoName, output, src);
}

#[test]
fn importable_adds_extra_new_line_if_no_imports() {
    let dep = "pub fn wobble() {\nNil\n}";
    let code = "";

    assert_completion!(TestProject::for_source(code).add_module("dep", dep));
}

#[test]
fn importable_adds_extra_new_line_if_import_exists_below_other_definitions() {
    let dep = "pub fn wobble() {\nNil\n}";
    let code = "\nimport dep2\n"; // "code" goes after "fn typing_in_here() {}".

    assert_completion!(
        TestProject::for_source(code)
            .add_module("dep", dep)
            .add_module("dep2", "")
    );
}

#[test]
fn importable_does_not_add_extra_new_line_if_imports_exist() {
    let dep = "pub fn wobble() {\nNil\n}";
    let prefix = "import wibble\n\n";
    let code = "";

    assert_completion_with_prefix!(
        TestProject::for_source(code)
            .add_module("dep", dep)
            .add_module("wibble", ""),
        prefix
    );
}

#[test]
fn importable_does_not_add_extra_new_line_if_newline_exists() {
    let dep = "pub fn wobble() {\nNil\n}";
    let prefix = "\n";
    let code = "";

    assert_completion_with_prefix!(
        TestProject::for_source(code)
            .add_module("dep", dep)
            .add_module("wibble", ""),
        prefix
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

    assert_completion!(TestProject::for_source(code).add_module("dep", dep));
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

    assert_completion!(TestProject::for_source(code).add_module("dep", dep));
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

    assert_completion!(TestProject::for_source(code).add_module("dep", dep));
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

    assert_completion!(TestProject::for_source(code).add_module("dep", dep));
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

    assert_completion!(TestProject::for_source(code).add_module("dep", dep));
}

#[test]
fn private_function() {
    let code = "
fn private() {
  1
}
";
    let dep = "";

    assert_completion!(TestProject::for_source(code).add_module("dep", dep));
}

#[test]
fn private_type() {
    let code = "
type Wibble {
  Wobble
}
";
    let dep = "";

    assert_completion!(TestProject::for_source(code).add_module("dep", dep));
}

#[test]
fn opaque_type() {
    let code = "
pub opaque type Wibble {
  Wobble
}
";
    let dep = "";

    assert_completion!(TestProject::for_source(code).add_module("dep", dep));
}

#[test]
fn private_function_in_dep() {
    let code = "import dep";
    let dep = "
fn private() {
  1
}
";

    assert_completion!(TestProject::for_source(code).add_module("dep", dep));
}

#[test]
fn private_type_in_dep() {
    let code = "import dep";
    let dep = "
type Wibble {
  Wobble
}
";

    assert_completion!(TestProject::for_source(code).add_module("dep", dep));
}

#[test]
fn in_custom_type_definition() {
    let code = "import dep";
    let dep = "
type Wibble {
  Wobble
}
";

    assert_completion!(TestProject::for_source(code).add_module("dep", dep));
}

#[test]
fn for_custom_type_definition() {
    let code = "
pub type Wibble {
  Wobble
}";

    assert_completion!(TestProject::for_source(code), Position::new(2, 0));
}

#[test]
fn for_type_alias() {
    let code = "
pub type Wibble = Result(
  String,
  String
)
";

    assert_completion!(TestProject::for_source(code), Position::new(2, 0));
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

    assert_completion!(TestProject::for_source(code), Position::new(2, 0));
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

    assert_completion!(
        TestProject::for_source(code).add_module("dep", dep),
        Position::new(3, 0)
    );
}

#[test]
fn imported_type_cursor_after_dot() {
    let dep = "
pub type Zoo = List(String)
type Private = List(String)
";
    let code = "import dep

pub fn wibble(
  _: dep.Zoo,
) -> Nil {
  Nil
}
";

    assert_completion!(
        TestProject::for_source(code).add_module("dep", dep),
        Position::new(3, 12)
    );
}

#[test]
fn imported_type_cursor_after_dot_other_matching_modules() {
    let dep = "
pub type Zoo = List(String)
type Private = List(String)
";
    let dep2 = "
pub type Zoo = List(String)
type Private = List(String)
";
    let code = "import dep
import dep2

pub fn wibble(
  _: dep.Zoo,
) -> Nil {
  Nil
}
";

    assert_completion!(
        TestProject::for_source(code)
            .add_module("dep", dep)
            .add_module("dep2", dep2),
        Position::new(4, 12)
    );
}

#[test]
fn imported_type_cursor_after_dot_other_modules() {
    let dep = "
pub type Zoo = List(String)
type Private = List(String)
";
    let other = "
pub type Zoo = List(String)
type Private = List(String)
";
    let code = "import dep

pub fn wibble(
  _: dep.Zoo,
) -> Nil {
  Nil
}
";

    assert_completion!(
        TestProject::for_source(code)
            .add_module("dep", dep)
            .add_module("other", other),
        Position::new(3, 12)
    );
}

#[test]
fn imported_type_cursor_mid_phrase_other_modules() {
    let dep = "
pub type Zoo = List(String)
type Private = List(String)
";
    let other = "
pub type Zoo = List(String)
type Private = List(String)
";
    let code = "import dep

pub fn wibble(
  _: dep.Zoo,
) -> Nil {
  Nil
}
";

    assert_completion!(
        TestProject::for_source(code)
            .add_module("dep", dep)
            .add_module("other", other),
        Position::new(3, 8)
    );
}

#[test]
fn importable_type() {
    let dep = "
pub type Zoo = List(String)
type Private = List(String)
";
    let code = "

pub fn wibble(
  _: String,
) -> Nil {
  Nil
}
";

    assert_completion!(
        TestProject::for_source(code).add_module("dep", dep),
        Position::new(3, 0)
    );
}

#[test]
fn importable_type_with_existing_imports_at_top() {
    let dep = "
pub type Zoo = List(String)
type Private = List(String)
";
    let dep2 = "
pub type Zoo = List(String)
type Private = List(String)
";
    let code = "import dep2

pub fn wibble(
  _: String,
) -> Nil {
  Nil
}
";

    assert_completion!(
        TestProject::for_source(code)
            .add_module("dep", dep)
            .add_module("dep2", dep2),
        Position::new(3, 0)
    );
}

#[test]
fn importable_type_with_existing_imports() {
    let dep = "
pub type Zoo = List(String)
type Private = List(String)
";
    let dep2 = "
pub type Zoo = List(String)
type Private = List(String)
";
    let code = "
//// Some module comments
// Some other whitespace

import dep2

pub fn wibble(
  _: String,
) -> Nil {
  Nil
}
";

    assert_completion!(
        TestProject::for_source(code)
            .add_module("dep", dep)
            .add_module("dep2", dep2),
        Position::new(7, 0)
    );
}

#[test]
fn importable_type_from_deep_module() {
    let dep = "
pub type Zoo = List(String)
type Private = List(String)
";
    let code = "

pub fn wibble(
  _: String,
) -> Nil {
  Nil
}
";

    assert_completion!(
        TestProject::for_source(code).add_module("a/b/dep", dep),
        Position::new(3, 0)
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

    assert_completion!(
        TestProject::for_source(code).add_module("dep", dep),
        Position::new(3, 0)
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

    assert_completion!(TestProject::for_source(code), Position::new(4, 0));
}

#[test]
fn local_variable() {
    let code = "
pub fn main(wibble: Int) {
  let wobble = 1
  w

  let wabble = 2
}
";

    assert_completion!(TestProject::for_source(code), Position::new(3, 3));
}

#[test]
fn local_variable_anonymous_function() {
    let code = "
pub fn main() {
  let add_one = fn(wibble: Int) { wibble + 1 }
  add_one(1)
}
";

    assert_completion!(TestProject::for_source(code), Position::new(2, 40));
}

#[test]
fn local_variable_nested_anonymous_function() {
    let code = "
pub fn main() {
  let add_one = fn(wibble: Int) {
    let wabble = 1
    let add_two = fn(wobble: Int) { wobble + 2 }
    wibble + add_two(1)
  }
  add_one(1)
}
";

    assert_completion!(TestProject::for_source(code), Position::new(4, 42));
}

#[test]
fn local_variable_ignore_anonymous_function_args() {
    let code = "
pub fn main() {
  let add_one = fn(wibble: Int) { wibble + 1 }
  let wobble = 1
  w
}
";

    assert_completion!(TestProject::for_source(code), Position::new(4, 3));
}

#[test]
fn local_variable_ignore_anonymous_function_args_nested() {
    let code = "
pub fn main() {
  let add_one = fn(wibble: Int) {
    let wabble = 1
    let add_two = fn(wobble: Int) { wobble + 2 }
    wibble + add_two(1)
  }
  add_one(1)
}
";

    assert_completion!(TestProject::for_source(code), Position::new(5, 10));
}

#[test]
fn local_variable_ignore_anonymous_function_returned() {
    let code = "
pub fn main() {
  fn(wibble: Int) {
    let wabble = 1
    let add_two = fn(wobble: Int) { wobble + 2 }
    wibble + add_two(1)
  }
}
";

    assert_completion!(TestProject::for_source(code), Position::new(5, 10));
}

#[test]
fn local_variable_case_expression() {
    let code = "
pub fn main() {
  case True {
    True as wibble -> { todo }
    False -> { todo }
  }
}
";

    assert_completion!(TestProject::for_source(code), Position::new(3, 25));
}

#[test]
fn local_variable_inside_nested_exprs() {
    let code = r#"
type Wibble { Wobble(List(#(Bool))) }
fn wibble() {
  Wobble([#(!{
    let wibble = True
    wibble
  })])
  todo
}
"#;

    assert_completion!(TestProject::for_source(code), Position::new(5, 7));
}

#[test]
fn local_variable_pipe() {
    let code = "
pub fn main() {
  let add_one = fn(wibble: Int) { wibble + 1 }
  let wobble = 1
  wobble |> add_one
}
";

    assert_completion!(TestProject::for_source(code), Position::new(4, 19));
}

#[test]
fn local_variable_pipe_with_args() {
    let code = "
pub fn main() {
  let add_one = fn(wibble: Int, wobble: Int) { wibble + wobble }
  let wobble = 1
  let wibble = 2
  wobble |> add_one(1, wibble)
}
";

    assert_completion!(TestProject::for_source(code), Position::new(5, 29));
}

#[test]
fn local_variable_function_call() {
    let code = "
fn add_one(wibble: Int) -> Int {
  wibble + 1
}

pub fn main() {
  let wobble = 1
  add_one(wobble)
}
";

    assert_completion!(TestProject::for_source(code), Position::new(7, 16));
}

#[test]
fn local_variable_ignored() {
    let code = "
fn wibble() {
  let a = 1
  let _b = 2

}
";
    assert_completion!(TestProject::for_source(code), Position::new(4, 0));
}

#[test]
fn local_variable_as() {
    let code = "
fn wibble() {
  let b as c = 5

}
";
    assert_completion!(TestProject::for_source(code), Position::new(3, 0));
}

#[test]
fn local_variable_tuple() {
    let code = "
fn wibble() {
  let assert #([d, e] as f, g) = #([0, 1], 2)

}
";
    assert_completion!(TestProject::for_source(code), Position::new(3, 0));
}

#[test]
fn local_variable_bit_array() {
    let code = "
fn wibble() {
  let assert <<h:1>> as i = <<1:1>>

}
";
    assert_completion!(TestProject::for_source(code), Position::new(3, 0));
}

#[test]
fn local_variable_string() {
    let code = r#"
fn wibble() {
  let assert "a" <> j = "ab"

}
"#;
    assert_completion!(TestProject::for_source(code), Position::new(3, 0));
}

#[test]
fn local_variable_ignore_within_function() {
    let code = "
fn main(a, b, z) {
    Nil
}
";
    assert_completion!(TestProject::for_source(code), Position::new(1, 14));
}

#[test]
fn internal_values_from_root_package_are_in_the_completions() {
    let dep = r#"
@external(erlang, "rand", "uniform")
@internal pub fn random_float() -> Float
@internal pub fn main() { 0 }
@internal pub type Wibble { Wobble }
@internal pub const wibble = 1
"#;

    assert_completion!(TestProject::for_source("import dep").add_module("dep", dep));
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
    assert_completion!(
        TestProject::for_source(code).add_module("dep", dep),
        Position::new(3, 0)
    );
}

#[test]
fn internal_values_from_the_same_module_are_in_the_completions() {
    let code = r#"
@external(erlang, "rand", "uniform")
@internal pub fn random_float() -> Float
@internal pub fn main() { 0 }
@internal pub type Wibble { Wobble }
@internal pub const wibble = 1
"#;

    assert_completion!(TestProject::for_source(code));
}

#[test]
fn internal_types_from_the_same_module_are_in_the_completions() {
    let code = "
@internal pub type Alias = Result(Int, String)
@internal pub type AnotherType {
  Wibble
}
";

    assert_completion!(TestProject::for_source(code), Position::new(3, 0));
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

    assert_completion!(
        TestProject::for_source(code).add_dep_module("dep", dep),
        Position::new(3, 0)
    );
}

#[test]
fn internal_values_from_a_dependency_are_ignored() {
    let dep = r#"
@external(erlang, "rand", "uniform")
@internal pub fn random_float() -> Float
@internal pub fn main() { 0 }
@internal pub type Wibble { Wobble }
@internal pub const wibble = 1
"#;

    assert_completion!(TestProject::for_source("import dep").add_dep_module("dep", dep));
}

#[test]
fn completions_for_an_import() {
    let code = "import dep

pub fn main() {
  0
}";
    let dep = "";

    assert_completion!(
        TestProject::for_source(code).add_module("dep", dep),
        Position::new(0, 10)
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

    assert_completion!(
        TestProject::for_source(code).add_test_module("my_tests", test),
        Position::new(0, 10)
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

    assert_debug_snapshot!(completions,);
}

#[test]
fn completions_for_an_import_while_in_dev() {
    let code = "import gleam

pub fn main() {
  0
}";
    let dev_helper = "
pub fn dev_helper() {
  0
}
";

    let position = Position::new(0, 12);
    let (mut engine, position_param) = TestProject::for_source(code)
        .add_test_module("my_test", code)
        .add_dev_module("my_dev_code", code)
        .add_dev_module("dev_helper", dev_helper)
        .positioned_with_io_in_dev(position, "my_dev_code");

    let response = engine.completion(position_param, code.into());

    let mut completions = response.result.unwrap().unwrap_or_default();
    completions.sort_by(|a, b| a.label.cmp(&b.label));

    let output = format!(
        "{}\n\n----- Completion content -----\n{}",
        show_complete(code, position),
        format_completion_results(completions)
    );
    insta::assert_snapshot!(insta::internals::AutoName, output, code);
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

    assert_completion!(
        TestProject::for_source(code).add_dep_module("dep", dep),
        Position::new(0, 10)
    );
}

#[test]
fn completions_for_an_import_from_dependency() {
    let code = "import gleam

pub fn main() {
  0
}";
    let dep = "";

    assert_completion!(
        TestProject::for_source(code).add_hex_module("example_module", dep),
        Position::new(0, 10)
    );
}

#[test]
fn completions_for_an_import_not_from_indirect_dependency() {
    let code = "import gleam

pub fn main() {
  0
}";
    let dep = "";

    assert_completion!(
        TestProject::for_source(code)
            .add_hex_module("example_module", dep)
            .add_indirect_hex_module("indirect_module", ""),
        Position::new(0, 10)
    );
}

#[test]
fn completions_for_an_import_not_from_dev_dependency() {
    let code = "import gleam

pub fn main() {
  0
}";
    let dep = "";

    assert_completion!(
        TestProject::for_source(code)
            .add_hex_module("example_module", dep)
            .add_dev_hex_module("indirect_module", ""),
        Position::new(0, 10)
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

    assert_debug_snapshot!(completions,);
}

#[test]
fn completions_for_an_import_not_from_dev_dependency_in_dev() {
    let code = "import gleam

pub fn main() {
  0
}";
    let dev = "import gleam

pub fn main() {
  0
}
";
    let dep = "";

    let position = Position::new(0, 10);
    let (mut engine, position_param) = TestProject::for_source(code)
        .add_dev_module("my_dev_module", dev)
        .add_hex_module("example_module", dep)
        .add_dev_hex_module("indirect_module", "")
        .positioned_with_io_in_dev(position, "my_dev_module");

    let response = engine.completion(position_param, code.into());

    let mut completions = response.result.unwrap().unwrap_or_default();
    completions.sort_by(|a, b| a.label.cmp(&b.label));

    let output = format!(
        "{}\n\n----- Completion content -----\n{}",
        show_complete(dev, position),
        format_completion_results(completions)
    );
    insta::assert_snapshot!(insta::internals::AutoName, output, dev);
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

    assert_completion!(
        TestProject::for_source(code).add_hex_module("example_module", dep),
        Position::new(3, 10)
    );
}

#[test]
fn completions_for_an_import_start() {
    let code = "import gleam

pub fn main() {
  0
}";
    let dep = "";

    assert_completion!(
        TestProject::for_source(code).add_dep_module("dep", dep),
        Position::new(0, 0)
    );
}

#[test]
fn completions_for_an_import_preceeding_whitespace() {
    let code = " import gleam

pub fn main() {
  0
}";
    let dep = "";

    assert_completion!(
        TestProject::for_source(code).add_dep_module("dep", dep),
        Position::new(0, 2)
    );
}

#[test]
fn internal_modules_from_same_package_are_included() {
    let code = "import gleam

pub fn main() {
  0
}";
    let internal_name = format!("{LSP_TEST_ROOT_PACKAGE_NAME}/internal");

    assert_completion!(
        TestProject::for_source(code)
            // Not included
            .add_dep_module("dep/internal", "")
            // Included
            .add_module(&internal_name, ""),
        Position::new(0, 0)
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

    assert_completion!(
        TestProject::for_source(code).add_module("dep", dep),
        Position::new(1, 12)
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

    assert_completion!(
        TestProject::for_source(code).add_module("dep", dep),
        // putting cursor at beginning of line because some formatters
        // remove the empty whitespace in the test code.
        // Does also work with (3, 2) when empty spaces are not removed.
        Position::new(3, 0)
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

    assert_completion!(
        TestProject::for_source(code).add_module("dep", dep),
        Position::new(1, 12)
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

    assert_completion!(TestProject::for_source(code), Position::new(2, 11));
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

    assert_completion!(TestProject::for_source(code), Position::new(3, 7));
}

#[test]
fn completions_for_a_var_annotation() {
    let code = "
pub fn main() {
  let wibble: Int = 7
}
";

    assert_completion!(TestProject::for_source(code), Position::new(2, 16));
}

#[test]
fn completions_for_a_const_annotation() {
    let code = "

const wibble: Int = 7

pub fn main() {
  let wibble: Int = 7
}
";

    assert_completion!(TestProject::for_source(code), Position::new(2, 16));
}

#[test]
fn ignore_completions_in_empty_comment() {
    // Reproducing issue #2161
    let code = "
pub fn main() {
  case 0 {
    //
    _ -> 1
  }
}
";

    // End of the comment (right after the last `/`)
    assert_eq!(
        completion(TestProject::for_source(code), Position::new(3, 6)),
        vec![],
    );
}

#[test]
fn ignore_completions_in_middle_of_comment() {
    // Reproducing issue #2161
    let code = "
pub fn main() {
  case 0 {
    // comment
    _ -> 1
  }
}
";

    // At `c`
    assert_eq!(
        completion(TestProject::for_source(code), Position::new(3, 7)),
        vec![],
    );
}

#[test]
fn ignore_completions_in_end_of_comment() {
    // Reproducing issue #2161
    let code = "
pub fn main() {
  case 0 {
    // comment
    _ -> 1
  }
}
";

    // End of the comment (after `t`)
    assert_eq!(
        completion(TestProject::for_source(code), Position::new(3, 14)),
        vec![],
    );
}

#[test]
fn ignore_completions_inside_empty_string() {
    let code = "
pub fn main() {
  \"\"
}
";

    assert_eq!(
        completion(TestProject::for_source(code), Position::new(2, 2)),
        vec![],
    );
}

#[test]
fn ignore_completions_inside_string() {
    let code = "
pub fn main() {
  \"Ok()\"
}
";

    assert_eq!(
        completion(TestProject::for_source(code), Position::new(2, 5)),
        vec![],
    );
}

#[test]
fn completions_for_record_access() {
    let code = "
pub type Wibble {
  Wibble(wibble: Int, wobble: Int)
  Wobble(wabble: Int, wobble: Int)
}

fn fun() {
  let wibble = Wibble(1, 2)
  wibble.wobble
}
";

    assert_completion!(TestProject::for_source(code), Position::new(8, 15));
}

#[test]
fn completions_for_private_record_access() {
    let code = "
type Wibble {
  Wibble(wibble: Int, wobble: Int)
  Wobble(wabble: Int, wobble: Int)
}

fn fun() {
  let wibble = Wibble(1, 2)
  wibble.wobble
}
";

    assert_completion!(TestProject::for_source(code), Position::new(8, 15));
}

#[test]
fn completions_for_record_access_known_variant() {
    let code = "
type Wibble {
  Wibble(a: Int, b: Int, c: Int, d: Int)
  Wobble(z: Bool)
}

fn fun(some_wibble: Wibble) {
  case some_wibble {
    Wibble(..) as w -> w.a
    Wobble(..) -> panic
  }
}
";

    assert_completion!(TestProject::for_source(code), Position::new(8, 26));
}

#[test]
fn completions_for_record_access_unknown_variant() {
    let code = "
type Wibble {
  Wibble(a: Int, b: Int, c: Int, d: Int)
  Wobble(a: Int, z: Bool)
}

fn fun(some_wibble: Wibble) {
  some_wibble.a
}
";

    assert_completion!(TestProject::for_source(code), Position::new(7, 15));
}

#[test]
fn completions_for_record_labels() {
    let code = "
pub type Wibble {
  Wibble(wibble: String, wobble: Int)
}

fn fun() { // completion inside parens below includes labels
  let wibble = Wibble()
}
";

    assert_completion!(TestProject::for_source(code), Position::new(6, 22));
}

#[test]
fn completions_for_imported_record_labels() {
    let code = "
import dep

fn fun() { // completion inside parens below includes labels
  let wibble = dep.Wibble()
}
";
    let dep = "
pub type Wibble {
  Wibble(wibble: String, wobble: Int)
}
";

    assert_completion!(
        TestProject::for_source(code).add_dep_module("dep", dep),
        Position::new(4, 26)
    );
}

#[test]
fn completions_for_function_labels() {
    let code = "
fn wibble(wibble arg1: String, wobble arg2: String) {
  arg1 <> arg2
}

fn fun() { // completion inside parens below includes labels
  let wibble = wibble()
}
";

    assert_completion!(TestProject::for_source(code), Position::new(6, 22));
}

#[test]
fn completions_for_imported_function_labels() {
    let code = "
import dep

fn fun() { // completion inside parens below includes labels
  let wibble = dep.wibble()
}
";
    let dep = "
pub fn wibble(wibble arg1: String, wobble arg2: String) {
  arg1 <> arg2
}
";

    assert_completion!(
        TestProject::for_source(code).add_dep_module("dep", dep),
        Position::new(4, 26)
    );
}

#[test]
fn no_completion_inside_comment_that_is_more_than_three_lines() {
    let code = "import list

// list.
// list.
fn fun() {
  // list.
  todo
}
";

    let dep = "pub fn map() {}";
    let completions = completion(
        TestProject::for_source(code).add_dep_module("list", dep),
        Position::new(5, 10),
    );
    assert_eq!(completions, vec![],);
}

#[test]
fn completions_for_prelude_values() {
    let code = "
pub fn main() {
  let my_bool = T
}
";

    assert_completion!(TestProject::for_source(code), Position::new(2, 17));
}

#[test]
fn variable_shadowing() {
    let code = "
pub fn main() {
  let x = 1
  let x = [1, 2]

}
";

    assert_completion!(TestProject::for_source(code), Position::new(4, 0));
}

#[test]
fn argument_shadowing() {
    let code = "
pub fn main(x: Int) {
  fn(x: Float) {

  }
}
";

    assert_completion!(TestProject::for_source(code), Position::new(3, 0));
}

#[test]
fn argument_variable_shadowing() {
    let code = "
pub fn main(x: Int) {
  let x = [1, 2]

}
";

    assert_completion!(TestProject::for_source(code), Position::new(3, 0));
}

// https://github.com/gleam-lang/gleam/issues/3833
#[test]
fn autocomplete_doesnt_delete_the_piece_of_code_that_comes_after() {
    let code = "
import list
pub fn main(x: Int) {
  list.list.filter([1, 2, 3], todo)
}
";
    let list = "
pub fn filter(_, _) { [] }
pub fn map(_, _) { [] }
";

    assert_apply_completion!(
        TestProject::for_source(code).add_dep_module("list", list),
        "list.map",
        Position::new(3, 7)
    );
}

#[test]
fn autocomplete_doesnt_delete_the_piece_of_code_that_comes_after_2() {
    let code = "
import list
pub fn main(x: Int) {
  list.mlist.filter([1, 2, 3], todo)
}
";
    let list = "
pub fn filter(_, _) { [] }
pub fn map(_, _) { [] }
";

    assert_apply_completion!(
        TestProject::for_source(code).add_dep_module("list", list),
        "list.map",
        Position::new(3, 8)
    );
}

#[test]
fn case_subject() {
    let code = "
pub fn main(something: Bool) {
  case so
}
";

    assert_apply_completion!(
        TestProject::for_source(code),
        "something",
        Position::new(2, 9)
    );
}

#[test]
fn constant() {
    let code = "
const hello = 10
const world = he
";

    assert_completion!(TestProject::for_source(code), Position::new(2, 16));
}

#[test]
fn constant_with_many_options() {
    let code = "
import wibble.{Wobble}

type Wibble {
  Wibble
}

const pi = 3.14159

fn some_function() {
  todo
}

const my_constant = a
";

    assert_completion!(
        TestProject::for_source(code).add_hex_module("wibble", "pub type Wibble { Wobble Wubble }"),
        Position::new(13, 21)
    );
}

#[test]
fn constant_with_module_select() {
    let code = "
import wibble

type Wibble {
  Wibble
}

const pi = 3.14159

fn some_function() {
  todo
}

const my_constant = wibble.W
";

    assert_completion!(
        TestProject::for_source(code).add_hex_module(
            "wibble",
            "
pub type Wibble { Wobble Wubble }
pub const some_constant = 1
pub fn some_function() { todo }
"
        ),
        Position::new(13, 28)
    );
}

#[test]
fn labelled_arguments() {
    let code = "
pub type Wibble {
  Wibble(wibble: Int, wobble: Float)
}

pub fn main() {
  Wibble(w)
}
";

    assert_completion!(TestProject::for_source(code), Position::new(6, 10));
}

#[test]
fn labelled_arguments_with_existing_label() {
    // This should only suggest the `wobble:` label
    let code = "
pub type Wibble {
  Wibble(wibble: Int, wobble: Float)
}

pub fn main() {
  Wibble(wibble: 10, w)
}
";

    assert_completion!(TestProject::for_source(code), Position::new(6, 22));
}

#[test]
fn labelled_arguments_after_label() {
    // This should not suggest any labels, as `wibble: wibble:` is not valid syntax.
    let code = "
pub type Wibble {
  Wibble(wibble: Int, wobble: Float)
}

pub fn main() {
  Wibble(wibble: w)
}
";

    assert_completion!(TestProject::for_source(code), Position::new(6, 18));
}

#[test]
fn labelled_arguments_function_call() {
    let code = "
pub fn divide(x: Int, by y: Int) { x / y }

pub fn main() {
  divide(10, b)
}
";

    assert_completion!(TestProject::for_source(code), Position::new(4, 14));
}

#[test]
fn labelled_arguments_from_different_module() {
    let code = "
import wibble

pub fn main() {
  wibble.divide(10, b)
}
";

    assert_completion!(
        TestProject::for_source(code)
            .add_hex_module("wibble", "pub fn divide(x: Int, by y: Int) { x / y }"),
        Position::new(4, 21)
    );
}

#[test]
fn no_label_completions_in_nested_expression() {
    // Since we are completing inside a list, labels are no longer available
    let code = "
pub type Wibble {
  Wibble(wibble: Int, wobble: Float)
}

pub fn main() {
  Wibble([w])
}
";

    assert_completion!(TestProject::for_source(code), Position::new(6, 11));
}

// https://github.com/gleam-lang/gleam/issues/4625
#[test]
fn no_variable_completions_before_declaration_in_block() {
    let code = "
pub fn main() {
  {
    s
    let something = 10
  }
}
";

    assert_completion!(TestProject::for_source(code), Position::new(3, 5));
}

// https://github.com/gleam-lang/gleam/issues/4625
#[test]
fn no_variable_completions_before_declaration_in_anonymous_function() {
    let code = "
pub fn main() {
  fn() {
    s
    let something = 10
  }
}
";

    assert_completion!(TestProject::for_source(code), Position::new(3, 5));
}

// https://github.com/gleam-lang/gleam/issues/4625
#[test]
fn no_variable_completions_after_block_scope() {
    let code = "
pub fn main() {
  {
    let something = 10
  }
  s
}
";

    assert_completion!(TestProject::for_source(code), Position::new(5, 3));
}

// https://github.com/gleam-lang/gleam/issues/4625
#[test]
fn no_variable_completions_after_anonymous_function_scope() {
    let code = "
pub fn main() {
  fn() {
    let something = 10
  }
  s
}
";

    assert_completion!(TestProject::for_source(code), Position::new(5, 3));
}

// https://github.com/gleam-lang/gleam/issues/4625
#[test]
fn no_variable_completions_after_case_scope() {
    let code = "
pub fn main() {
  case todo {
    something -> Nil
  }
  s
}
";

    assert_completion!(TestProject::for_source(code), Position::new(5, 3));
}

// https://github.com/gleam-lang/gleam/issues/4625
#[test]
fn no_variable_completions_after_case_clause_scope() {
    let code = "
pub fn main() {
  case todo {
    something -> Nil
    something_else -> s
  }
  s
}
";

    assert_completion!(TestProject::for_source(code), Position::new(4, 23));
}

// https://github.com/gleam-lang/gleam/issues/4625
#[test]
fn no_variable_completions_before_case_clause() {
    let code = "
pub fn main() {
  case todo {
    something -> s
    something_else -> Nil
  }
  s
}
";

    assert_completion!(TestProject::for_source(code), Position::new(3, 18));
}

// https://github.com/gleam-lang/gleam/issues/4652
#[test]
fn no_completions_in_constant_string() {
    let code = r#"
const x = "io."
"#;

    let completions = completion(
        TestProject::for_source(code).add_hex_module("gleam/io", "pub fn println() {todo}"),
        Position::new(1, 14),
    );
    assert_eq!(completions, vec![],);
}

#[test]
fn prefer_values_matching_expected_type() {
    let code = "
pub fn main() -> Bool {
  let wibble = 123
  let wubble = True
  let Wobble = 1.5
  w
}
";

    assert_completion!(TestProject::for_source(code), Position::new(5, 3));
}

#[test]
fn prefer_function_which_returns_expected_type() {
    let code = "
pub fn main() -> Int {
  a
}

fn add(a, b) { a + b }
fn sub(a, b) { a - b }
fn addf(a, b) { a +. b }
";

    assert_completion!(TestProject::for_source(code), Position::new(2, 3));
}

#[test]
fn prefer_function_which_returns_expected_generic_type() {
    let code = "
pub fn main() -> Result(Int, Nil) {
  let result = Ok(12)
  let result2 = Error(True)
  r
}
";

    assert_completion!(TestProject::for_source(code), Position::new(4, 3));
}

#[test]
fn completion_for_type() {
    let dep = "pub type Wibble";
    let code = "pub fn new() -> wibble.Wibble {}";
    assert_completion!(
        TestProject::for_source(code).add_dep_module("wibble", dep),
        Position::new(0, 23)
    );
}

#[test]
fn completion_for_partially_correct_existing_module_select() {
    let dep = "pub fn filter() {}";
    let code = "
import gleam/list

pub fn new() {
  list.fer
//      ^ cursor right after the 'f'
}
";
    assert_completion!(
        TestProject::for_source(code).add_dep_module("gleam/list", dep),
        Position::new(4, 8)
    );
}

#[test]
fn complete_keyword_being_typed() {
    assert_apply_completion!(
        TestProject::for_source("pub fn main() { t }"),
        "todo",
        Position::new(0, 17)
    );
}

#[test]
fn complete_echo_keyword() {
    assert_apply_completion!(
        TestProject::for_source("pub fn main() { e wibble }"),
        "echo",
        Position::new(0, 17)
    );
}

#[test]
fn complete_panic_keyword() {
    assert_apply_completion!(
        TestProject::for_source("pub fn main() { wibble(p) }"),
        "panic",
        Position::new(0, 24)
    );
}

#[test]
fn do_not_show_completions_when_typing_a_number() {
    assert_completion!(
        TestProject::for_source(
            "
pub fn main() { 2 }
pub fn window_by_2() {}
pub fn to_base_32() {}
"
        ),
        Position::new(1, 17)
    );
}
