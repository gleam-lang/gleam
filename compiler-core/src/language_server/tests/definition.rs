use lsp_types::{GotoDefinitionParams, Location, Position, Range, Url};

use super::*;

fn definition(tester: TestProject<'_>, position: Position) -> Option<Location> {
    tester.at(position, |engine, param, _| {
        let params = GotoDefinitionParams {
            text_document_position_params: param,
            work_done_progress_params: Default::default(),
            partial_result_params: Default::default(),
        };
        let response = engine.goto_definition(params);

        response.result.unwrap()
    })
}

#[test]
fn goto_definition_local_variable() {
    let code = "
pub fn main() {
  let x = 1
  x
}";

    assert_eq!(
        definition(TestProject::for_source(code), Position::new(3, 2)),
        Some(Location {
            uri: Url::from_file_path(Utf8PathBuf::from(if cfg!(target_family = "windows") {
                r"\\?\C:\src\app.gleam"
            } else {
                "/src/app.gleam"
            }))
            .unwrap(),
            range: Range {
                start: Position {
                    line: 2,
                    character: 6
                },
                end: Position {
                    line: 2,
                    character: 7
                }
            }
        })
    )
}

#[test]
fn goto_definition_same_module_constants() {
    let code = "
const x = 1

pub fn main() {
  x
}";

    assert_eq!(
        definition(TestProject::for_source(code), Position::new(4, 2)),
        Some(Location {
            uri: Url::from_file_path(Utf8PathBuf::from(if cfg!(target_family = "windows") {
                r"\\?\C:\src\app.gleam"
            } else {
                "/src/app.gleam"
            }))
            .unwrap(),
            range: Range {
                start: Position {
                    line: 1,
                    character: 6
                },
                end: Position {
                    line: 1,
                    character: 7
                }
            }
        })
    )
}

#[test]
fn goto_definition_same_module_functions() {
    let code = "
fn add_2(x) {
  x + 2
}

pub fn main() {
  add_2(1)
}";

    assert_eq!(
        definition(TestProject::for_source(code), Position::new(6, 3)),
        Some(Location {
            uri: Url::from_file_path(Utf8PathBuf::from(if cfg!(target_family = "windows") {
                r"\\?\C:\src\app.gleam"
            } else {
                "/src/app.gleam"
            }))
            .unwrap(),
            range: Range {
                start: Position {
                    line: 1,
                    character: 0
                },
                end: Position {
                    line: 1,
                    character: 11
                }
            }
        })
    )
}

#[test]
fn goto_definition_same_module_records() {
    let code = "
pub type Rec {
  Var1(Int)
  Var2(Int, Int)
}

pub fn main() {
  let a = Var1(1)
  let b = Var2(2, 3)
}";

    assert_eq!(
        definition(TestProject::for_source(code), Position::new(7, 11)),
        Some(Location {
            uri: Url::from_file_path(Utf8PathBuf::from(if cfg!(target_family = "windows") {
                r"\\?\C:\src\app.gleam"
            } else {
                "/src/app.gleam"
            }))
            .unwrap(),
            range: Range {
                start: Position {
                    line: 2,
                    character: 2
                },
                end: Position {
                    line: 2,
                    character: 11
                }
            }
        })
    )
}

#[test]
fn goto_definition_imported_module_constants() {
    let code = "
import example_module
fn main() {
  example_module.my_num
}
";

    assert_eq!(
        definition(
            TestProject::for_source(code).add_module("example_module", "pub const my_num = 1"),
            Position::new(3, 19)
        ),
        Some(Location {
            uri: Url::from_file_path(Utf8PathBuf::from(if cfg!(target_family = "windows") {
                r"\\?\C:\src\example_module.gleam"
            } else {
                "/src/example_module.gleam"
            }))
            .unwrap(),
            range: Range {
                start: Position {
                    line: 0,
                    character: 10
                },
                end: Position {
                    line: 0,
                    character: 16
                }
            }
        })
    )
}

#[test]
fn goto_definition_unqualified_imported_module_constants() {
    let code = "
import example_module.{my_num}
fn main() {
  my_num
}
";

    assert_eq!(
        definition(
            TestProject::for_source(code).add_module("example_module", "pub const my_num = 1"),
            Position::new(3, 3)
        ),
        Some(Location {
            uri: Url::from_file_path(Utf8PathBuf::from(if cfg!(target_family = "windows") {
                r"\\?\C:\src\example_module.gleam"
            } else {
                "/src/example_module.gleam"
            }))
            .unwrap(),
            range: Range {
                start: Position {
                    line: 0,
                    character: 10
                },
                end: Position {
                    line: 0,
                    character: 16
                }
            }
        })
    )
}

#[test]
fn goto_definition_module_function_calls() {
    let code = "
import example_module
fn main() {
  example_module.my_fn
}
";

    assert_eq!(
        definition(
            TestProject::for_source(code).add_module("example_module", "pub fn my_fn() { Nil }"),
            Position::new(3, 19)
        ),
        Some(Location {
            uri: Url::from_file_path(Utf8PathBuf::from(if cfg!(target_family = "windows") {
                r"\\?\C:\src\example_module.gleam"
            } else {
                "/src/example_module.gleam"
            }))
            .unwrap(),
            range: Range {
                start: Position {
                    line: 0,
                    character: 0
                },
                end: Position {
                    line: 0,
                    character: 14
                }
            }
        })
    )
}

#[test]
fn goto_definition_imported_module_records() {
    let dep_src = "
pub type Rec {
  Var1(Int)
  Var2(Int, Int)
}";

    let code = "
import example_module
fn main() {
  example_module.Var1(1)
}
";

    assert_eq!(
        definition(
            TestProject::for_source(code).add_module("example_module", dep_src),
            Position::new(3, 20)
        ),
        Some(Location {
            uri: Url::from_file_path(Utf8PathBuf::from(if cfg!(target_family = "windows") {
                r"\\?\C:\src\example_module.gleam"
            } else {
                "/src/example_module.gleam"
            }))
            .unwrap(),
            range: Range {
                start: Position {
                    line: 2,
                    character: 2
                },
                end: Position {
                    line: 2,
                    character: 11
                }
            }
        })
    )
}

#[test]
fn goto_definition_unqualified_imported_module_records() {
    let dep_src = "
pub type Rec {
  Var1(Int)
  Var2(Int, Int)
}";

    let code = "
import example_module.{Var1}
fn main() {
  Var1(1)
}
";

    assert_eq!(
        definition(
            TestProject::for_source(code).add_module("example_module", dep_src),
            Position::new(3, 3)
        ),
        Some(Location {
            uri: Url::from_file_path(Utf8PathBuf::from(if cfg!(target_family = "windows") {
                r"\\?\C:\src\example_module.gleam"
            } else {
                "/src/example_module.gleam"
            }))
            .unwrap(),
            range: Range {
                start: Position {
                    line: 2,
                    character: 2
                },
                end: Position {
                    line: 2,
                    character: 11
                }
            }
        })
    )
}

#[test]
fn goto_definition_external_module_constants() {
    let code = "
import example_module
fn main() {
  example_module.my_num
}
";

    assert_eq!(
        definition(
            TestProject::for_source(code).add_hex_module("example_module", "pub const my_num = 1"),
            Position::new(3, 20)
        ),
        Some(Location {
            uri: Url::from_file_path(Utf8PathBuf::from(if cfg!(target_family = "windows") {
                r"\\?\C:\build\packages\hex\src\example_module.gleam"
            } else {
                "/build/packages/hex/src/example_module.gleam"
            }))
            .unwrap(),
            range: Range {
                start: Position {
                    line: 0,
                    character: 10
                },
                end: Position {
                    line: 0,
                    character: 16
                }
            }
        })
    )
}

#[test]
fn goto_definition_external_module_function_calls() {
    let code = "
import example_module
fn main() {
  example_module.my_fn
}
";

    assert_eq!(
        definition(
            TestProject::for_source(code)
                .add_hex_module("example_module", "pub fn my_fn() { Nil }"),
            Position::new(3, 20)
        ),
        Some(Location {
            uri: Url::from_file_path(Utf8PathBuf::from(if cfg!(target_family = "windows") {
                r"\\?\C:\build\packages\hex\src\example_module.gleam"
            } else {
                "/build/packages/hex/src/example_module.gleam"
            }))
            .unwrap(),
            range: Range {
                start: Position {
                    line: 0,
                    character: 0
                },
                end: Position {
                    line: 0,
                    character: 14
                }
            }
        })
    )
}

#[test]
fn goto_definition_external_module_function_calls_with_multiple_compiles() {
    let dep = "pub fn my_fn() { Nil }";
    let code = "
import example_module
fn main() {
  example_module.my_fn
}
";

    let (mut engine, position_param) = TestProject::for_source(code)
        .add_hex_module("example_module", dep)
        .positioned_with_io(Position::new(3, 20));

    let params = GotoDefinitionParams {
        text_document_position_params: position_param.clone(),
        work_done_progress_params: Default::default(),
        partial_result_params: Default::default(),
    };
    let response = engine.goto_definition(params.clone());
    let response = response.result.unwrap();

    assert_eq!(
        response,
        Some(Location {
            uri: Url::from_file_path(Utf8PathBuf::from(if cfg!(target_family = "windows") {
                r"\\?\C:\build\packages\hex\src\example_module.gleam"
            } else {
                "/build/packages/hex/src/example_module.gleam"
            }))
            .unwrap(),
            range: Range {
                start: Position {
                    line: 0,
                    character: 0
                },
                end: Position {
                    line: 0,
                    character: 14
                }
            }
        })
    );

    engine.compiler.sources.clear();
    let response = engine.compile_please();
    assert!(response.result.is_ok());

    let response = engine.goto_definition(params.clone());
    let response = response.result.unwrap();

    assert_eq!(
        response,
        Some(Location {
            uri: Url::from_file_path(Utf8PathBuf::from(if cfg!(target_family = "windows") {
                r"\\?\C:\build\packages\hex\src\example_module.gleam"
            } else {
                "/build/packages/hex/src/example_module.gleam"
            }))
            .unwrap(),
            range: Range {
                start: Position {
                    line: 0,
                    character: 0
                },
                end: Position {
                    line: 0,
                    character: 14
                }
            }
        })
    )
}

#[test]
fn goto_definition_path_module_function_calls_with_multiple_compiles() {
    let dep = "pub fn my_fn() { Nil }";
    let code = "
import example_module
fn main() {
  example_module.my_fn
}
";

    let (mut engine, position_param) = TestProject::for_source(code)
        .add_dep_module("example_module", dep)
        .positioned_with_io(Position::new(3, 20));

    let params = GotoDefinitionParams {
        text_document_position_params: position_param.clone(),
        work_done_progress_params: Default::default(),
        partial_result_params: Default::default(),
    };

    let response = engine.goto_definition(params.clone());
    let response = response.result.unwrap();

    assert_eq!(
        response,
        Some(Location {
            uri: Url::from_file_path(Utf8PathBuf::from(if cfg!(target_family = "windows") {
                r"\\?\C:\dep\src\example_module.gleam"
            } else {
                "/dep/src/example_module.gleam"
            }))
            .unwrap(),
            range: Range {
                start: Position {
                    line: 0,
                    character: 0
                },
                end: Position {
                    line: 0,
                    character: 14
                }
            }
        })
    );

    engine.compiler.sources.clear();
    let response = engine.compile_please();
    assert!(response.result.is_ok());

    let response = engine.goto_definition(params.clone());
    let response = response.result.unwrap();

    assert_eq!(
        response,
        Some(Location {
            uri: Url::from_file_path(Utf8PathBuf::from(if cfg!(target_family = "windows") {
                r"\\?\C:\dep\src\example_module.gleam"
            } else {
                "/dep/src/example_module.gleam"
            }))
            .unwrap(),
            range: Range {
                start: Position {
                    line: 0,
                    character: 0
                },
                end: Position {
                    line: 0,
                    character: 14
                }
            }
        })
    )
}

#[test]
fn goto_definition_external_module_records() {
    let hex_src = "
pub type Rec {
  Var1(Int)
  Var2(Int, Int)
}
";

    let code = "
import example_module
fn main() {
  example_module.Var1(1)
}
";

    assert_eq!(
        definition(
            TestProject::for_source(code).add_hex_module("example_module", hex_src),
            Position::new(3, 20)
        ),
        Some(Location {
            uri: Url::from_file_path(Utf8PathBuf::from(if cfg!(target_family = "windows") {
                r"\\?\C:\build\packages\hex\src\example_module.gleam"
            } else {
                "/build/packages/hex/src/example_module.gleam"
            }))
            .unwrap(),
            range: Range {
                start: Position {
                    line: 2,
                    character: 2
                },
                end: Position {
                    line: 2,
                    character: 11
                }
            }
        })
    )
}

#[test]
fn goto_definition_path_module_function_calls() {
    let code = "
import example_module
fn main() {
  example_module.my_fn
}
";

    assert_eq!(
        definition(
            TestProject::for_source(code)
                .add_dep_module("example_module", "pub fn my_fn() { Nil }"),
            Position::new(3, 20)
        ),
        Some(Location {
            uri: Url::from_file_path(Utf8PathBuf::from(if cfg!(target_family = "windows") {
                r"\\?\C:\dep\src\example_module.gleam"
            } else {
                "/dep/src/example_module.gleam"
            }))
            .unwrap(),
            range: Range {
                start: Position {
                    line: 0,
                    character: 0
                },
                end: Position {
                    line: 0,
                    character: 14
                }
            }
        })
    )
}

#[test]
fn goto_definition_type() {
    let code = "
pub type Rec {
  Var1(Int)
  Var2(Int, Int)
}

pub fn make_var() -> Rec {
  Var1(1)
}";

    assert_eq!(
        definition(TestProject::for_source(code), Position::new(6, 22)),
        Some(Location {
            uri: Url::from_file_path(Utf8PathBuf::from(if cfg!(target_family = "windows") {
                r"\\?\C:\src\app.gleam"
            } else {
                "/src/app.gleam"
            }))
            .unwrap(),
            range: Range {
                start: Position {
                    line: 1,
                    character: 0
                },
                end: Position {
                    line: 1,
                    character: 12
                }
            }
        })
    )
}

#[test]
fn goto_definition_type_in_module() {
    let hex_src = "
pub type Rec {
  Var1(Int)
  Var2(Int, Int)
}
";

    let code = "
import example_module
fn make_var() -> example_module.Rec {
  example_module.Var1(1)
}
";

    assert_eq!(
        definition(
            TestProject::for_source(code).add_hex_module("example_module", hex_src),
            Position::new(2, 33)
        ),
        Some(Location {
            uri: Url::from_file_path(Utf8PathBuf::from(if cfg!(target_family = "windows") {
                r"\\?\C:\build\packages\hex\src\example_module.gleam"
            } else {
                "/build/packages/hex/src/example_module.gleam"
            }))
            .unwrap(),
            range: Range {
                start: Position {
                    line: 1,
                    character: 0
                },
                end: Position {
                    line: 1,
                    character: 12
                }
            }
        })
    )
}

#[test]
fn goto_definition_type_in_path_dep() {
    let dep = "
pub type Rec {
  Var1(Int)
  Var2(Int, Int)
}
";

    let code = "
import example_module
fn make_var() -> example_module.Rec {
  example_module.Var1(1)
}
";

    assert_eq!(
        definition(
            TestProject::for_source(code).add_dep_module("example_module", dep),
            Position::new(2, 33)
        ),
        Some(Location {
            uri: Url::from_file_path(Utf8PathBuf::from(if cfg!(target_family = "windows") {
                r"\\?\C:\dep\src\example_module.gleam"
            } else {
                "/dep/src/example_module.gleam"
            }))
            .unwrap(),
            range: Range {
                start: Position {
                    line: 1,
                    character: 0
                },
                end: Position {
                    line: 1,
                    character: 12
                }
            }
        })
    )
}

#[test]
fn goto_definition_deep_type_in_module() {
    let hex_src = "
pub type Wobble {
  Wobble(Int)
}

pub type Wibble(a) {
  Wibble(a)
}

pub type Wabble(a) {
  Wabble(a)
}
";

    let code = "
import example_module
fn make_var() -> example_module.Wabble(example_module.Wibble(example_module.Wobble)) {
  example_module.Wabble(example_module.Wibble(example_module.Wobble(1)))
}
";

    assert_eq!(
        definition(
            TestProject::for_source(code).add_hex_module("example_module", hex_src),
            Position::new(2, 80)
        ),
        Some(Location {
            uri: Url::from_file_path(Utf8PathBuf::from(if cfg!(target_family = "windows") {
                r"\\?\C:\build\packages\hex\src\example_module.gleam"
            } else {
                "/build/packages/hex/src/example_module.gleam"
            }))
            .unwrap(),
            range: Range {
                start: Position {
                    line: 1,
                    character: 0
                },
                end: Position {
                    line: 1,
                    character: 15
                }
            }
        })
    )
}

#[test]
fn goto_definition_import() {
    let code = "
import example_module
fn main() {
  example_module.my_num
}
";

    assert_eq!(
        definition(
            TestProject::for_source(code).add_module("example_module", "pub const my_num = 1"),
            Position::new(1, 13)
        ),
        Some(Location {
            uri: Url::from_file_path(Utf8PathBuf::from(if cfg!(target_family = "windows") {
                r"\\?\C:\src\example_module.gleam"
            } else {
                "/src/example_module.gleam"
            }))
            .unwrap(),
            range: Range {
                start: Position {
                    line: 0,
                    character: 0
                },
                end: Position {
                    line: 0,
                    character: 0
                }
            }
        })
    )
}

#[test]
fn goto_definition_import_aliased() {
    let code = "
import example_module as example
fn main() {
  example.my_num
}
";

    assert_eq!(
        definition(
            TestProject::for_source(code).add_module("example_module", "pub const my_num = 1"),
            Position::new(1, 29)
        ),
        Some(Location {
            uri: Url::from_file_path(Utf8PathBuf::from(if cfg!(target_family = "windows") {
                r"\\?\C:\src\example_module.gleam"
            } else {
                "/src/example_module.gleam"
            }))
            .unwrap(),
            range: Range {
                start: Position {
                    line: 0,
                    character: 0
                },
                end: Position {
                    line: 0,
                    character: 0
                }
            }
        })
    )
}

#[test]
fn goto_definition_import_unqualified_value() {
    let code = "
import example_module.{my_num}
fn main() {
  my_num
}
";

    assert_eq!(
        definition(
            TestProject::for_source(code).add_module("example_module", "pub const my_num = 1"),
            Position::new(1, 26)
        ),
        Some(Location {
            uri: Url::from_file_path(Utf8PathBuf::from(if cfg!(target_family = "windows") {
                r"\\?\C:\src\example_module.gleam"
            } else {
                "/src/example_module.gleam"
            }))
            .unwrap(),
            range: Range {
                start: Position {
                    line: 0,
                    character: 10
                },
                end: Position {
                    line: 0,
                    character: 16
                }
            }
        })
    )
}

#[test]
fn goto_definition_import_unqualified_type() {
    let code = "
import example_module.{type MyType}
fn main() -> MyType {
  0
}
";

    assert_eq!(
        definition(
            TestProject::for_source(code).add_module("example_module", "pub type MyType = Int"),
            Position::new(1, 33)
        ),
        Some(Location {
            uri: Url::from_file_path(Utf8PathBuf::from(if cfg!(target_family = "windows") {
                r"\\?\C:\src\example_module.gleam"
            } else {
                "/src/example_module.gleam"
            }))
            .unwrap(),
            range: Range {
                start: Position {
                    line: 0,
                    character: 0
                },
                end: Position {
                    line: 0,
                    character: 21
                }
            }
        })
    )
}
