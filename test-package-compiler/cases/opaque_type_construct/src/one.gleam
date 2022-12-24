pub opaque type User {
  User(name: String, score: Int)
}

// These operations are permitted in this module as it is the one that defined
// the custom type

pub fn construct() {
  User(name: "Alim", score: 10)
}

pub fn accessors(user: User) {
  let name = user.name
  let score = user.score
  #(name, score)
}

pub fn destructure(user: User) {
  let User(name: name, score: score) = user
  #(name, score)
}
