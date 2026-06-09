// SPDX-License-Identifier: Apache-2.0
// SPDX-FileCopyrightText: 2021 The Gleam contributors

import one
import two

pub fn test_() -> Bool {
  one.hello() == two.main()
}
