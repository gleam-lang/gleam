// This function is only implemented for JavaScript, so if we try and call it
// from Erlang, or build this package for Erlang, then the compiler will
// (should) emit an error.
pub fn main() -> Nil {
  run()
}

// TODO: replace this with the `main` function being an external.
// Currently there is a bug in the compiler's target detection that causes
// functions with no body to not emit an error if they don't support the
// current target.
@external(javascript, "./external_only_javascript_ffi.mjs", "main")
pub fn run() -> Nil
