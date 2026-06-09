// SPDX-License-Identifier: Apache-2.0
// SPDX-FileCopyrightText: 2021 The Gleam contributors

use crate::assert_erl;

#[test]
fn qualified_prelude() {
    assert_erl!(
        "import gleam
pub type X { X(gleam.Int) }
"
    );

    assert_erl!(
        "import gleam
pub fn x() { gleam.Ok(1) }
"
    );
}

