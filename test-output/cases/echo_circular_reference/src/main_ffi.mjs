// SPDX-License-Identifier: Apache-2.0
// SPDX-FileCopyrightText: 2025 The Gleam contributors

export function circular_reference() {
  const x = [];
  x.push(x);
  return x;
}
