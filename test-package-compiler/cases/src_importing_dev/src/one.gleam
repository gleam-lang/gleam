// SPDX-License-Identifier: Apache-2.0
// SPDX-FileCopyrightText: 2025 The Gleam contributors

// This import will fail because the `two` module is in the `dev` directory and
// as such isn't permitted to be imported into the `src` directory.
import two
