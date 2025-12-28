import one/one.{A, A as C, B, B as D, User, User as XUser}
import one/two as aliased

/// For these statements we use the records in a qualified fashion
pub const qualified_const_a = one.A

pub fn qualified_fn_a() {
  qualified_const_a
}

pub const qualified_const_b = one.B(one.A, one.A)

pub fn qualified_fn_b() {
  qualified_const_b
}

pub const qualified_aliased_const_a = aliased.A

pub fn qualified_aliased_fn_a() {
  qualified_aliased_const_a
}

pub const qualified_aliased_const_b = aliased.B(aliased.A, aliased.A)

pub fn qualified_aliased_fn_b() {
  qualified_aliased_const_b
}

/// For these statements we use the records in a unqualified fashion
pub const unqualified_const_a = A

pub fn unqualified_fn_a() {
  unqualified_const_a
}

pub const unqualified_const_b = B(A, A)

pub fn unqualified_fn_b() {
  unqualified_const_b
}

/// For these statements we use the records in a unqualified and also aliased
/// fashion
pub const aliased_const_a = C

pub fn aliased_fn_a() {
  aliased_const_a
}

pub const aliased_const_b = D(C, C)

pub fn aliased_fn_b() {
  aliased_const_b
}

/// For these statements we use the accessors for the record from the other
/// module
pub fn accessors(user: one.User) {
  let name = user.name
  let score = user.score
  #(name, score)
}

/// For these statements we use destructure the record
pub fn destructure_qualified(user) {
  let one.User(name: name, score: score) = user
  #(name, score)
}

pub fn destructure_qualified_aliased(user) {
  let aliased.User(name: name, score: score) = user
  #(name, score)
}

pub fn destructure_unqualified(user) {
  let User(name: name, score: score) = user
  #(name, score)
}

pub fn destructure_aliased(user) {
  let XUser(name: name, score: score) = user
  #(name, score)
}

/// For these statements we use update the record
pub fn update_qualified(user) {
  one.User(..user, name: "wibble")
}

pub fn update_qualified_aliased(user) {
  aliased.User(..user, name: "wibble")
}

pub fn update_unqualified(user) {
  User(..user, name: "wibble")
}

pub fn update_aliased(user) {
  XUser(..user, name: "wibble")
}
