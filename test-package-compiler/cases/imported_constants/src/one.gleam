pub type A {
  A
}

pub type B {
  B(A, A)
}

pub type User {
  User(name: String, score: Int)
}
