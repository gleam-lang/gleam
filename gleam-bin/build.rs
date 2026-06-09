// SPDX-License-Identifier: Apache-2.0
// SPDX-FileCopyrightText: 2023 The Gleam contributors

fn main() {
    #[cfg(windows)]
    static_vcruntime::metabuild();
}
