import one.{User}

// This operation is not permitted because the type is opaque and this module
// did not define the custom type.

pub fn construct() {
  User(name: "Alim", score: 10)
}
