import one.{User}

// This operation is not permitted because the type is opaque and this module
// did not define the custom type.

pub fn destructure(user: User) {
  let User(name: name, score: score) = user
  #(name, score)
}
