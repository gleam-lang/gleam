use lsp_types::{Location, Range, ReferenceContext, ReferenceParams};

use super::*;

fn references(tester: TestProject<'_>, position: Position) -> Option<Vec<Location>> {
    tester.at(position, |engine, param, _| {
        let params = ReferenceParams {
            text_document_position: param,
            context: ReferenceContext {
                include_declaration: true,
            },
            work_done_progress_params: Default::default(),
            partial_result_params: Default::default(),
        };
        let response = engine.references(params);

        response.result.expect("ok result")
    })
}

fn test_uri(paths: &[&str]) -> Url {
    let mut path = Utf8PathBuf::from(if cfg!(target_family = "windows") {
        r"\\?\C:\"
    } else {
        "/"
    });

    path.extend(paths);
    Url::from_file_path(path).expect("valid uri")
}

#[test]
fn references_module_select_with_module_fn_constructor() {
    let code = "
import foo
fn main() { foo.bar() } // <- this line
";

    let project = TestProject::for_source(code).add_module("foo", "pub fn bar() { 0 }");
    let position = Position::new(2, 16);
    let app_uri = test_uri(&["src", "app.gleam"]);
    let foo_uri = test_uri(&["src", "foo.gleam"]);

    assert_eq!(
        references(project, position),
        Some(vec![
            Location {
                uri: app_uri,
                range: Range {
                    start: Position {
                        line: 2,
                        character: 15,
                    },
                    end: Position {
                        line: 2,
                        character: 19,
                    }
                }
            },
            Location {
                uri: foo_uri,
                range: Range {
                    start: Position {
                        line: 0,
                        character: 0,
                    },
                    end: Position {
                        line: 0,
                        character: 12,
                    }
                }
            },
        ])
    );
}

#[test]
fn references_var_with_module_fn_constructor() {
    let code = "
fn foo() { 0 }
fn main() { foo() } // <- this line
";

    let project = TestProject::for_source(code);
    let position = Position::new(2, 12);
    let app_uri = test_uri(&["src", "app.gleam"]);

    assert_eq!(
        references(project, position),
        Some(vec![
            Location {
                uri: app_uri.clone(),
                range: Range {
                    start: Position {
                        line: 1,
                        character: 0,
                    },
                    end: Position {
                        line: 1,
                        character: 8,
                    }
                }
            },
            Location {
                uri: app_uri,
                range: Range {
                    start: Position {
                        line: 2,
                        character: 12,
                    },
                    end: Position {
                        line: 2,
                        character: 15,
                    }
                }
            },
        ])
    );
}

#[test]
fn references_function_definition() {
    let code = "
fn foo() { 0 } // <- this line
fn main() { foo() }
";

    let project = TestProject::for_source(code);
    let position = Position::new(1, 3);
    let app_uri = test_uri(&["src", "app.gleam"]);

    assert_eq!(
        references(project, position),
        Some(vec![
            Location {
                uri: app_uri.clone(),
                range: Range {
                    start: Position {
                        line: 1,
                        character: 0,
                    },
                    end: Position {
                        line: 1,
                        character: 8,
                    }
                }
            },
            Location {
                uri: app_uri,
                range: Range {
                    start: Position {
                        line: 2,
                        character: 12,
                    },
                    end: Position {
                        line: 2,
                        character: 15,
                    }
                }
            },
        ])
    );
}
