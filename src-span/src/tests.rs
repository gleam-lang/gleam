// SPDX-License-Identifier: Apache-2.0
// SPDX-FileCopyrightText: 2026 The Gleam contributors

use lsp_types::Position;

use crate::{LineColumn, LineNumbers};

#[test]
fn byte_index() {
    let src = r#"import gleam/io

pub fn main() {
  io.println("Hello, world!")
}
"#;
    let line_numbers = LineNumbers::new(src);

    assert_eq!(
        line_numbers.byte_index(Position {
            line: 0,
            character: 0
        }),
        0
    );
    assert_eq!(
        line_numbers.byte_index(Position {
            line: 0,
            character: 4
        }),
        4
    );
    assert_eq!(
        line_numbers.byte_index(Position {
            line: 100,
            character: 0
        }),
        src.len() as u32
    );
    assert_eq!(
        line_numbers.byte_index(Position {
            line: 2,
            character: 1
        }),
        18
    );
}

// https://github.com/gleam-lang/gleam/issues/3628
#[test]
fn byte_index_with_multibyte_characters() {
    let src = r#"fn wibble(_a, _b, _c) {
  todo
}

pub fn main() {
  wibble("क्षि", 10, <<"abc">>)
}
"#;
    let line_numbers = LineNumbers::new(src);

    assert_eq!(
        line_numbers.byte_index(Position {
            line: 1,
            character: 6
        }),
        30
    );
    assert_eq!(
        line_numbers.byte_index(Position {
            line: 5,
            character: 2
        }),
        52
    );
    assert_eq!(
        line_numbers.byte_index(Position {
            line: 5,
            character: 17
        }),
        75
    );
    assert_eq!(
        line_numbers.byte_index(Position {
            line: 6,
            character: 1
        }),
        91
    );
}

// https://github.com/gleam-lang/gleam/issues/3628
#[test]
fn line_and_column_with_multibyte_characters() {
    let src = r#"fn wibble(_a, _b, _c) {
  todo
}

pub fn main() {
  wibble("क्षि", 10, <<"abc">>)
}
"#;
    let line_numbers = LineNumbers::new(src);

    assert_eq!(
        line_numbers.line_and_utf16_column_number(30),
        LineColumn { line: 2, column: 7 }
    );
    assert_eq!(
        line_numbers.line_and_utf16_column_number(52),
        LineColumn { line: 6, column: 3 }
    );
    assert_eq!(
        line_numbers.line_and_utf16_column_number(75),
        LineColumn {
            line: 6,
            column: 18
        }
    );
    assert_eq!(
        line_numbers.line_and_utf16_column_number(91),
        LineColumn { line: 7, column: 2 }
    );
}
