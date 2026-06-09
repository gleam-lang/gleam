// SPDX-License-Identifier: Apache-2.0
// SPDX-FileCopyrightText: 2023 The Gleam contributors

import project_d

pub type TypeC {
  ConstructorC(project_d.TypeD)
}

pub fn new(str) {
  ConstructorC(project_d.ConstructorD(str))
}
