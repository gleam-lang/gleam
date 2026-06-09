// SPDX-License-Identifier: Apache-2.0
// SPDX-FileCopyrightText: 2022 The Gleam contributors

pub type A {
  A
}

pub type B {
  B(A, A)
}

pub type User {
  User(name: String, score: Int)
}
