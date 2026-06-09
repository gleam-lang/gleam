// SPDX-License-Identifier: Apache-2.0
// SPDX-FileCopyrightText: 2022 The Gleam contributors

// This module uses imports in a way which previously failed to compile due a
// compiler bug.
//
// https://github.com/gleam-lang/gleam/issues/922#issuecomment-803272624
//
import one/two

pub const x = two.A
