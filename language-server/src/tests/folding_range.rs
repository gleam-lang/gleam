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

fn range(start_line: u32, end_line: u32, kind: Option<FoldingRangeKind>) -> FoldingRange {
    FoldingRange {
        start_line,
        start_character: None,
        end_line,
        end_character: None,
        kind,
        collapsed_text: None,
    }
}

#[test]
fn folds_multiline_function_body() {
    let code = "pub fn main() {
  let x = 1
  x
}";

    assert_eq!(
        folding_ranges(TestProject::for_source(code)),
        vec![range(0, 3, None)]
    );
}

#[test]
fn does_not_fold_single_line_function_body() {
    let code = "pub fn main() { 1 }";

    assert_eq!(folding_ranges(TestProject::for_source(code)), vec![]);
}

#[test]
fn folds_import_block() {
    let code = "import one
import two
import three

pub fn main() { 1 }";

    assert_eq!(
        folding_ranges(
            TestProject::for_source(code)
                .add_dep_module("one", "pub fn value() { 1 }")
                .add_dep_module("two", "pub fn value() { 2 }")
                .add_dep_module("three", "pub fn value() { 3 }"),
        ),
        vec![range(0, 2, Some(FoldingRangeKind::Imports))]
    );
}

#[test]
fn folds_separated_import_blocks() {
    let code = "import one
import two

import three
import four

pub fn main() { 1 }";

    assert_eq!(
        folding_ranges(
            TestProject::for_source(code)
                .add_dep_module("one", "pub fn value() { 1 }")
                .add_dep_module("two", "pub fn value() { 2 }")
                .add_dep_module("three", "pub fn value() { 3 }")
                .add_dep_module("four", "pub fn value() { 4 }"),
        ),
        vec![
            range(0, 1, Some(FoldingRangeKind::Imports)),
            range(3, 4, Some(FoldingRangeKind::Imports)),
        ]
    );
}

#[test]
fn folds_multiline_custom_type() {
    let code = "pub type W {
  W(Int)
}";

    assert_eq!(
        folding_ranges(TestProject::for_source(code)),
        vec![range(0, 2, None)]
    );
}

#[test]
fn folds_multiline_constant() {
    let code = "pub const xs = [
  1,
  2,
]";

    assert_eq!(
        folding_ranges(TestProject::for_source(code)),
        vec![range(0, 3, None)]
    );
}

#[test]
fn folds_multiline_type_alias() {
    let code = "pub type Pair =
  #(Int, Int)";

    assert_eq!(
        folding_ranges(TestProject::for_source(code)),
        vec![range(0, 1, None)]
    );
}

#[test]
fn does_not_fold_single_line_top_level_definitions() {
    let code = "pub type W { W(Int) }
pub const x = 1
pub type Pair = #(Int, Int)
pub fn main() { 1 }";

    assert_eq!(folding_ranges(TestProject::for_source(code)), vec![]);
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

    assert_eq!(
        folding_ranges(
            TestProject::for_source(code)
                .add_dep_module("one", "pub fn value() { 1 }")
                .add_dep_module("two", "pub fn value() { 2 }"),
        ),
        vec![
            range(0, 1, Some(FoldingRangeKind::Imports)),
            range(3, 5, None),
            range(7, 10, None),
            range(12, 14, None),
        ]
    );
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

    assert_eq!(
        folding_ranges(TestProject::for_source(code)),
        vec![range(0, 2, None), range(6, 8, None)]
    );
}
