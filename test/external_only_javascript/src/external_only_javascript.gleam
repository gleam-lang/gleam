// This function is only implemented for JavaScript, so if we try and call it
// from Erlang, or build this package for Erlang, then the compiler will
// (should) emit an error.
@external(javascript, "./external_only_javascript_ffi.mjs", "main")
pub fn main() -> Nil
