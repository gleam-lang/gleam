// SPDX-License-Identifier: Apache-2.0
// SPDX-FileCopyrightText: 2023 The Gleam contributors

import project_d

pub type TypeB {
  ConstructorB(contained: project_d.TypeD, name: String)
}

pub fn new(contained, name) {
  ConstructorB(project_d.ConstructorD(contained), name)
}
