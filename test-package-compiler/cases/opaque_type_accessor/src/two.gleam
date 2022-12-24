import one.{User}

// This operation is not permitted because the type is opaque and this module
// did not define the custom type.

pub fn accessors(user: User) {
  let name = user.name
  let score = user.score
  #(name, score)
}
