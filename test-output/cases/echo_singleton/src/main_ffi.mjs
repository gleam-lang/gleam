// SPDX-License-Identifier: Apache-2.0
// SPDX-FileCopyrightText: 2025 The Gleam contributors

import { Thing } from "./thing.mjs";

const it = new Thing();

export function singleton() {
  return it;
}
