// SPDX-License-Identifier: Apache-2.0
// SPDX-FileCopyrightText: 2021 The Gleam contributors

use crate::assert_erl;

#[test]
fn empty_list() {
    assert_erl!(
        r#"
pub fn main() {
  []
}
"#
    );
}

#[test]
fn single_item_list() {
    assert_erl!(
        r#"
pub fn main() {
  [1]
}
"#
    );
}

#[test]
fn list_with_multiple_items() {
    assert_erl!(
        r#"
pub fn main() {
  [1, 2, 3]
}
"#
    );
}

#[test]
fn list_with_spread() {
    assert_erl!(
        r#"
pub fn main() {
  let a = [3, 2, 1]
  [5, 4, ..a]
}
"#
    );
}
