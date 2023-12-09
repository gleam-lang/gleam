//// A module containing some unusual functions and types.

/// A type where the value can never be constructed.
/// Can you work out why?
pub type Never {
  Never(Never)
}

/// Call a function twice with an initial value.
///
pub fn twice(argument: value, function: fn(value) -> value) -> value {
  function(function(argument))
}

/// Call a function three times with an initial value.
///
pub fn thrice(argument: value, function: fn(value) -> value) -> value {
  function(function(function(argument)))
}
