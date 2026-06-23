// SPDX-License-Identifier: Apache-2.0
// SPDX-FileCopyrightText: 2026 The Gleam contributors

use lsp_types::{
    FoldingRange, FoldingRangeKind, FoldingRangeParams, PartialResultParams, WorkDoneProgressParams,
};

use super::*;

fn folding_ranges(tester: TestProject<'_>) -> Vec<FoldingRange> {
    tester.at(Position::default(), |engine, param, _| {
        let params = FoldingRangeParams {
            text_document: param.text_document,
            work_done_progress_params: WorkDoneProgressParams::default(),
            partial_result_params: PartialResultParams::default(),
        };
        let response = engine.folding_range(params);

        response.result.unwrap()
    })
}

fn kind_name(kind: &Option<FoldingRangeKind>) -> &str {
    match kind {
        Some(FoldingRangeKind::Imports) => "imports",
        Some(FoldingRangeKind::Comment) => "comment",
        Some(FoldingRangeKind::Region) => "region",
        Some(FoldingRangeKind::Custom(any)) => &any,
        None => "none",
    }
}

fn apply_fold(code: &str, range: &FoldingRange) -> String {
    let start = range.start_line as usize;
    let end = range.end_line as usize;
    let lines: Vec<&str> = code.lines().collect();

    let mut output = vec![];
    for (index, line) in lines.iter().enumerate() {
        if index < start || index > end {
            output.push((*line).to_string());
        } else if index == start {
            output.push(format!("{line} ..."));
        }
    }

    output.join("\n")
}

fn pretty_folding_output(code: &str, ranges: &[FoldingRange]) -> String {
    let mut output = format!("----- Code -----\n{code}\n\n----- Ranges -----\n");

    if ranges.is_empty() {
        output.push_str("(none)\n");
        return output;
    }

    for (index, range) in ranges.iter().enumerate() {
        let line = format!(
            "{}. lines {}..{} kind: {}\n",
            index + 1,
            range.start_line,
            range.end_line,
            kind_name(&range.kind)
        );
        output.push_str(&line);
    }

    for (index, range) in ranges.iter().enumerate() {
        let fold = apply_fold(code, range);
        let section = format!(
            "\n----- Fold {} (lines {}..{}) -----\n{fold}\n",
            index + 1,
            range.start_line,
            range.end_line
        );
        output.push_str(&section);
    }

    output
}

fn folding_snapshot_output(project: TestProject<'_>) -> String {
    let src = project.src;
    let ranges = folding_ranges(project);
    pretty_folding_output(src, &ranges)
}

macro_rules! assert_folding {
    ($src:literal $(,)?) => {
        assert_folding!(TestProject::for_source($src));
    };
    ($project:expr $(,)?) => {{
        let project = $project;
        let src = project.src;
        let output = folding_snapshot_output(project);
        insta::assert_snapshot!(insta::internals::AutoName, output, src);
    }};
}

macro_rules! assert_no_folding {
    ($src:literal $(,)?) => {
        assert_no_folding!(TestProject::for_source($src));
    };
    ($project:expr $(,)?) => {{
        let ranges = folding_ranges($project);
        assert!(
            ranges.is_empty(),
            "Expected no folding ranges, got: {ranges:#?}"
        );
    }};
}

#[test]
fn folds_multiline_function_body() {
    assert_folding!(
        "pub fn main() {
  let x = 1
  x
}"
    );
}

#[test]
fn does_not_fold_single_line_function_body() {
    assert_no_folding!("pub fn main() { 1 }");
}

#[test]
fn folds_import_block() {
    let code = "import one
import two
import three

pub fn main() { 1 }";

    assert_folding!(
        TestProject::for_source(code)
            .add_dep_module("one", "pub fn value() { 1 }")
            .add_dep_module("two", "pub fn value() { 2 }")
            .add_dep_module("three", "pub fn value() { 3 }"),
    );
}

#[test]
fn folds_separated_import_blocks() {
    let code = "import one
import two

import three
import four

pub fn main() { 1 }";

    assert_folding!(
        TestProject::for_source(code)
            .add_dep_module("one", "pub fn value() { 1 }")
            .add_dep_module("two", "pub fn value() { 2 }")
            .add_dep_module("three", "pub fn value() { 3 }")
            .add_dep_module("four", "pub fn value() { 4 }"),
    );
}

#[test]
fn folds_multiline_custom_type() {
    assert_folding!(
        "pub type W {
  W(Int)
}"
    );
}

#[test]
fn folds_multiline_constant() {
    assert_folding!(
        "pub const xs = [
  1,
  2,
]"
    );
}

#[test]
fn folds_multiline_type_alias() {
    assert_folding!(
        "pub type Pair =
  #(Int, Int)"
    );
}

#[test]
fn does_not_fold_single_line_custom_type() {
    assert_no_folding!("pub type W { W(Int) }");
}

#[test]
fn does_not_fold_single_line_constant() {
    assert_no_folding!("pub const x = 1");
}

#[test]
fn does_not_fold_single_line_type_alias() {
    assert_no_folding!("pub type Pair = #(Int, Int)");
}

#[test]
fn folds_mixed_definitions_in_source_order() {
    let code = "import one
import two

pub type W {
  W(Int)
}

pub const xs = [
  1,
  2,
]

pub fn main() {
  1
}";

    assert_folding!(
        TestProject::for_source(code)
            .add_dep_module("one", "pub fn value() { 1 }")
            .add_dep_module("two", "pub fn value() { 2 }"),
    );
}

#[test]
fn folds_only_multiline_functions_in_source_order() {
    assert_folding!(
        "pub fn one() {
  1
}

pub fn two() { 2 }

pub fn three() {
  3
}"
    );
}

#[test]
fn folds_for_comments() {
    assert_folding!(
        "
//// This is very useful module.
//// It can be used to wibble, wobble, and to wubble

/// Function to wibble.
///
/// Not that it wobbles when wubble is true
fn wibble() {
  // We use this to wibble.
  // Actually we could use wobble, would probably
  // use wubble some time later
  todo
}
"
    );
}

#[test]
fn folds_for_comments_breaked_with_code() {
    assert_folding!(
        "
fn wibble() {
  // Wibble comment
  // Wobble comment
  todo // ToDo comment

  // Wubble comment
}
"
    );
}

#[test]
fn doesnt_fold_single_line_comments() {
    assert_folding!(
        "
//// This is very useful module

/// This is function for wibbling
fn wibble() {
  // Useful comment
  todo
}
"
    );
}

#[test]
fn doesnt_fold_separated_comment_blocks() {
    assert_folding!(
        "
fn wibble() {
  // Useful comment
  // Another useful comment

  // Yet another useful comment
  // Last useful comment

  todo
}
"
    );
}
