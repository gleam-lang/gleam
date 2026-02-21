use lsp_types::{FoldingRange, FoldingRangeKind, FoldingRangeParams};

use super::*;

fn folding_ranges(tester: TestProject<'_>) -> Vec<FoldingRange> {
    tester.at(Position::default(), |engine, param, _| {
        let params = FoldingRangeParams {
            text_document: param.text_document,
            work_done_progress_params: Default::default(),
            partial_result_params: Default::default(),
        };
        let response = engine.folding_range(params);

        response.result.unwrap()
    })
}

fn kind_name(kind: &Option<FoldingRangeKind>) -> &'static str {
    match kind {
        Some(kind) if *kind == FoldingRangeKind::Imports => "imports",
        Some(kind) if *kind == FoldingRangeKind::Comment => "comment",
        Some(kind) if *kind == FoldingRangeKind::Region => "region",
        Some(_) => "other",
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

#[test]
fn folds_multiline_function_body() {
    let code = "pub fn main() {
  let x = 1
  x
}";

    let output = folding_snapshot_output(TestProject::for_source(code));
    insta::assert_snapshot!(insta::internals::AutoName, output, code);
}

#[test]
fn does_not_fold_single_line_function_body() {
    let code = "pub fn main() { 1 }";

    let output = folding_snapshot_output(TestProject::for_source(code));
    insta::assert_snapshot!(insta::internals::AutoName, output, code);
}

#[test]
fn folds_import_block() {
    let code = "import one
import two
import three

pub fn main() { 1 }";

    let output = folding_snapshot_output(
        TestProject::for_source(code)
            .add_dep_module("one", "pub fn value() { 1 }")
            .add_dep_module("two", "pub fn value() { 2 }")
            .add_dep_module("three", "pub fn value() { 3 }"),
    );
    insta::assert_snapshot!(insta::internals::AutoName, output, code);
}

#[test]
fn folds_separated_import_blocks() {
    let code = "import one
import two

import three
import four

pub fn main() { 1 }";

    let output = folding_snapshot_output(
        TestProject::for_source(code)
            .add_dep_module("one", "pub fn value() { 1 }")
            .add_dep_module("two", "pub fn value() { 2 }")
            .add_dep_module("three", "pub fn value() { 3 }")
            .add_dep_module("four", "pub fn value() { 4 }"),
    );
    insta::assert_snapshot!(insta::internals::AutoName, output, code);
}

#[test]
fn folds_multiline_custom_type() {
    let code = "pub type W {
  W(Int)
}";

    let output = folding_snapshot_output(TestProject::for_source(code));
    insta::assert_snapshot!(insta::internals::AutoName, output, code);
}

#[test]
fn folds_multiline_constant() {
    let code = "pub const xs = [
  1,
  2,
]";

    let output = folding_snapshot_output(TestProject::for_source(code));
    insta::assert_snapshot!(insta::internals::AutoName, output, code);
}

#[test]
fn folds_multiline_type_alias() {
    let code = "pub type Pair =
  #(Int, Int)";

    let output = folding_snapshot_output(TestProject::for_source(code));
    insta::assert_snapshot!(insta::internals::AutoName, output, code);
}

#[test]
fn does_not_fold_single_line_top_level_definitions() {
    let code = "pub type W { W(Int) }
pub const x = 1
pub type Pair = #(Int, Int)
pub fn main() { 1 }";

    let output = folding_snapshot_output(TestProject::for_source(code));
    insta::assert_snapshot!(insta::internals::AutoName, output, code);
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

    let output = folding_snapshot_output(
        TestProject::for_source(code)
            .add_dep_module("one", "pub fn value() { 1 }")
            .add_dep_module("two", "pub fn value() { 2 }"),
    );
    insta::assert_snapshot!(insta::internals::AutoName, output, code);
}

#[test]
fn folds_only_multiline_functions_in_source_order() {
    let code = "pub fn one() {
  1
}

pub fn two() { 2 }

pub fn three() {
  3
}";

    let output = folding_snapshot_output(TestProject::for_source(code));
    insta::assert_snapshot!(insta::internals::AutoName, output, code);
}
