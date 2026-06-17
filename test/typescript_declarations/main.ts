// SPDX-License-Identifier: Apache-2.0
// SPDX-FileCopyrightText: 2026 The Gleam contributors

import type { Option$ } from "./build/dev/javascript/gleam_stdlib/gleam/option.d.mts";
import type { List } from "./build/dev/javascript/typescript_declarations/gleam.d.mts";
import * as gleam from "./build/dev/javascript/typescript_declarations/typescript_declarations.mjs";

// These statements use the `satisfies` keyword to assert the types are
// what we expect.

gleam.const_int satisfies number
gleam.const_int_alias satisfies number
gleam.const_int_list satisfies List<number>
gleam.const_string_list satisfies List<string>
gleam.const_tuple satisfies [string, number]
gleam.either_int satisfies gleam.Either$<number, number>
gleam.function_int_int_returns_int_alias satisfies (a: number, b: number) => number
gleam.function_closure_returns_fn_int_which_returns_int_alias satisfies () => (a: number) => number

type GenericFn<T = any> = (a: T, fn: (a: T) => T) => T;
gleam.function_generic_fn_generic_which_returns_generic_returns_generic satisfies GenericFn

gleam.function_int_int_returns_int satisfies (a: number, b: number) => number
gleam.function_option satisfies () => Option$<number>
