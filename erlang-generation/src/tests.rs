// SPDX-License-Identifier: Apache-2.0
// SPDX-FileCopyrightText: 2026 The Gleam contributors

use crate::{ErlangBinaryBuilder, ErlangBuilder, ErlangBuiltInType};

#[test]
fn type_spec_with_custom_user_type() {
    let mut etf = ErlangBinaryBuilder::new(None);
    let type_spec = etf.start_type_spec(true, "wibble", []);
    let wibble = etf.start_custom_named_type("wibble");
    let int = etf.start_built_in_named_type(ErlangBuiltInType::Integer);
    etf.end_named_type(int);
    etf.end_named_type(wibble);
    etf.end_type_spec(type_spec);

    // Here we're asserting on the exact bytes produced by the code generator
    // producing a custom named type.
    // This is to make sure this won't change and regress in future:
    // https://github.com/gleam-lang/gleam/issues/6410
    assert_eq!(
        etf.into_output(),
        [
            131, 108, 0, 0, 0, 1, 104, 4, 119, 9, 97, 116, 116, 114, 105, 98, 117, 116, 101, 97, 0,
            119, 6, 111, 112, 97, 113, 117, 101, 104, 3, 119, 6, 119, 105, 98, 98, 108, 101, 104,
            4, 119, 9, 117, 115, 101, 114, 95, 116, 121, 112, 101, 97, 0, 119, 6, 119, 105, 98, 98,
            108, 101, 108, 0, 0, 0, 1, 104, 4, 119, 4, 116, 121, 112, 101, 97, 0, 119, 7, 105, 110,
            116, 101, 103, 101, 114, 108, 0, 0, 0, 0, 106, 106, 108, 0, 0, 0, 0, 106, 106
        ]
    );
}
