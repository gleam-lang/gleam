use insta::assert_debug_snapshot;
use lsp_types::{DocumentSymbol, DocumentSymbolParams};

use super::*;

fn doc_symbols(tester: TestProject<'_>) -> Vec<DocumentSymbol> {
    tester.at(Position::default(), |engine, param, _| {
        let params = DocumentSymbolParams {
            text_document: param.text_document,
            work_done_progress_params: Default::default(),
            partial_result_params: Default::default(),
        };
        let response = engine.document_symbol(params);

        response.result.unwrap()
    })
}

#[test]
fn doc_symbols_type_no_constructors() {
    let code = "
pub type A";

    assert_debug_snapshot!(doc_symbols(TestProject::for_source(code)))
}

#[test]
fn doc_symbols_type_no_constructors_starting_at_documentation() {
    let code = "
/// My type
pub type A";

    assert_debug_snapshot!(doc_symbols(TestProject::for_source(code)))
}

#[test]
fn doc_symbols_type_no_constructors_starting_at_empty_doc() {
    let code = "
// Some prior code...

///
pub type A";

    assert_debug_snapshot!(doc_symbols(TestProject::for_source(code)))
}

#[test]
fn doc_symbols_type_constructor_no_args() {
    let code = "
pub type B {
    C
    D

    /// E
    E
}";

    assert_debug_snapshot!(doc_symbols(TestProject::for_source(code)))
}

#[test]
fn doc_symbols_type_constructor_pos_args() {
    let code = "
pub type B {
    C(Int)

    /// D
    D(List(Int))

    /// E
    E(
        Result(Int, Bool)
    )
}";

    assert_debug_snapshot!(doc_symbols(TestProject::for_source(code)))
}

#[test]
fn doc_symbols_type_constructor_labeled_args() {
    let code = "
pub type B {
    C(argc: Int)

    /// D
    D(argd: List(Int))

    /// E
    E(
        /// Arg
        arge: Result(Int, Bool)
    )
}";

    assert_debug_snapshot!(doc_symbols(TestProject::for_source(code)))
}

#[test]
fn doc_symbols_type_constructor_pos_and_labeled_args() {
    let code = "
pub type B {
    C(Int, argc: Int)

    /// D
    D(Int, argd: List(Int))

    /// E
    E(
        Int,

        /// Arg
        arge: Result(Int, Bool)
    )
}";

    assert_debug_snapshot!(doc_symbols(TestProject::for_source(code)))
}

#[test]
fn doc_symbols_type_alias() {
    let code = "
/// DOC
pub type FFF = Int

pub type FFFF = List(Int)";

    assert_debug_snapshot!(doc_symbols(TestProject::for_source(code)))
}

#[test]
fn doc_symbols_function() {
    let code = "
/// DOC
pub fn super_func(a: Int) -> List(Int) {
    [a + 5]
}

pub fn super_func2(a: Int) -> List(Int) {
    [a + 5]
}";

    assert_debug_snapshot!(doc_symbols(TestProject::for_source(code)))
}

#[test]
fn doc_symbols_constant() {
    let code = "
/// DOC
pub const my_const = 5

pub const my_const2 = [25]";

    assert_debug_snapshot!(doc_symbols(TestProject::for_source(code)))
}
