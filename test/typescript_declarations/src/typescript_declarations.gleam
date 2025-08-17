//// Test TS declarartions

/// Answer for all questions in the world
pub const answer = 256

pub type User {
  // Registered user
  User(name: String, email: String)
  /// Unregistered guest
  Guest
}

/// Get name of user. Returns "Guest" if user is guest
pub fn get_name(user) {
  case user {
    User(name, ..) -> name
    Guest -> "Guest"
  }
}
