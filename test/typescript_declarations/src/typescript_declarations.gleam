import gleam/option.{type Option}
import gleam/result

/// Greeting for user
pub const greeting: String = "Hello, dear user!"

/// Add two numbers
pub fn add(first a: Int, second b: Int) -> Int {
  a + b
}

/// Returns true if number is divisible by another
pub fn is_divisible_by(number num: Int, divider div: Int) -> Bool {
  case num % div {
    0 -> True
    _ -> False
  }
}

/// ID of specific user
pub type UserId =
  Int

/// Alias to add function
pub const add_alias = add

/// Return function that will add one to given number
pub fn add_one() -> fn(Int) -> Int {
  let func = add(1, _)
  func
}

/// IDs of administrators
pub const admins: List(UserId) = [00_010, 00_000, 00_001, 00_002, 29_373]

/// IDs of moderators
pub const moders = [01_013, 36_371, 74_839, 18_930, 36_373]

/// Create alert on browser target
@external(javascript, "globalThis", "alert")
pub fn js_alert(text: String) -> Nil

/// Twice value via given function
pub fn twice(val: a, function: fn(a) -> a) -> a {
  function(function(val))
}

/// Recursively sum list of numbers
pub fn sum_list(list: List(Int), total: Int) -> Int {
  case list {
    [first, ..rest] -> sum_list(rest, total + first)
    [] -> total
  }
}

/// Returns true if result is ok
pub fn is_ok_result(res: Result(a, b)) -> Bool {
  result.is_ok(res)
}

/// First value is name and second is description
pub const name_description = #("MyApp", "My Awesome Application")

/// Role of user
pub type UserRole {
  /// Administrator
  Administrator
  /// Moderator
  Moderator
  /// Just a user
  PlainUser
}

/// Represents user
pub type User {
  /// Represents registered user
  User(username: String, id: UserId, role: UserRole)
  /// Represents guest without any data
  Guest
}

/// Get name of user, None if user is guest
pub fn user_name(user: User) -> Option(String) {
  case user {
    User(name, ..) -> option.Some(name)
    Guest -> option.None
  }
}

/// Format user role as string
pub fn role_string(role: UserRole) -> String {
  case role {
    PlainUser -> "user"
    Moderator -> "moderator"
    Administrator -> "admin"
  }
}

pub type Either(a, b) {
  Left(a)
  Right(b)
}
