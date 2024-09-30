// This function is only implemented for Erlang, so if we try and call it from
// JavaScript, or build this package for JavaScript, then the compiler will
// (should) emit an error.
@external(erlang, "external_only_erlang_ffi", "main")
pub fn main() -> Nil
