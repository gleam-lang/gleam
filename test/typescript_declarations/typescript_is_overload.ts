// SPDX-License-Identifier: Apache-2.0
// SPDX-FileCopyrightText: 2026 The Gleam contributors

// These statements use the `satisfies` keyword to assert the types are
// what we expect.
// In particular this file tests that we are generating the correct `isX()`
// overloads to narrow a type into one of its variants with the correct
// generic arguments.

import * as $box from "./build/dev/javascript/typescript_declarations/typescript_is_overload.mjs";

const almost_empty = $box.Box$AlmostEmpty(1);
almost_empty satisfies $box.Box$<unknown, number, unknown>;
if ($box.Box$isAlmostEmpty(almost_empty)) {
  almost_empty satisfies $box.AlmostEmpty<number>;
}

const almost_full = $box.Box$AlmostFull("hello", 12);
almost_full satisfies $box.Box$<string, unknown, number>;
if ($box.Box$isAlmostFull(almost_full)) {
  almost_full satisfies $box.AlmostFull<string, number>;
}

const empty = $box.Box$Empty();
empty satisfies $box.Box$<unknown, unknown, unknown>;
if ($box.Box$isEmpty(empty)) {
  empty satisfies $box.Empty;
}
