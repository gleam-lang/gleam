use lsp_types::{GotoDefinitionParams, Location, Position, Range, Url};

use super::*;

fn positioned_with_io_definition(
    src: &str,
    position: Position,
    io: &LanguageServerTestIO,
) -> Option<Location> {
    let (mut engine, position_param) = positioned_with_io(src, position, io);

    let params = GotoDefinitionParams {
        text_document_position_params: position_param,
        work_done_progress_params: Default::default(),
        partial_result_params: Default::default(),
    };
    let response = engine.goto_definition(params);

    response.result.unwrap()
}

fn positioned_goto_definition(src: &str, position: Position) -> Option<Location> {
    positioned_with_io_definition(src, position, &LanguageServerTestIO::new())
}

#[test]
fn goto_definition_local_variable() {
    let code = "
pub fn main() {
  let x = 1
  x
}";

    assert_eq!(
        positioned_goto_definition(code, Position::new(3, 2)),
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
        positioned_goto_definition(code, Position::new(4, 2)),
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
        positioned_goto_definition(code, Position::new(6, 3)),
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
        positioned_goto_definition(code, Position::new(7, 11)),
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
    let io = LanguageServerTestIO::new();
    _ = io.src_module("example_module", "pub const my_num = 1");

    let code = "
import example_module
fn main() {
  example_module.my_num
}
";

    assert_eq!(
        positioned_with_io_definition(code, Position::new(3, 19), &io),
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
    let io = LanguageServerTestIO::new();
    _ = io.src_module("example_module", "pub const my_num = 1");

    let code = "
import example_module.{my_num}
fn main() {
  my_num
}
";

    assert_eq!(
        positioned_with_io_definition(code, Position::new(3, 3), &io),
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
    let io = LanguageServerTestIO::new();
    _ = io.src_module("example_module", "pub fn my_fn() { Nil }");

    let code = "
import example_module
fn main() {
  example_module.my_fn
}
";

    assert_eq!(
        positioned_with_io_definition(code, Position::new(3, 19), &io),
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
    let io = LanguageServerTestIO::new();
    _ = io.src_module(
        "example_module",
        "
pub type Rec {
  Var1(Int)
  Var2(Int, Int)
}",
    );

    let code = "
import example_module
fn main() {
  example_module.Var1(1)
}
";

    assert_eq!(
        positioned_with_io_definition(code, Position::new(3, 20), &io),
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
    let io = LanguageServerTestIO::new();
    _ = io.src_module(
        "example_module",
        "
pub type Rec {
  Var1(Int)
  Var2(Int, Int)
}",
    );

    let code = "
import example_module.{Var1}
fn main() {
  Var1(1)
}
";

    assert_eq!(
        positioned_with_io_definition(code, Position::new(3, 3), &io),
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
