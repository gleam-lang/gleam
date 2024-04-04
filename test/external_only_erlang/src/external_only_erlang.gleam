// This function is only implemented for Erlang, so if we try and call it from
// JavaScript, or build this package for JavaScript, then the compiler will
// (should) emit an error.
pub fn main() -> Nil {
  run()
}

// TODO: replace this with the `main` function being an external.
// Currently there is a bug in the compiler's target detection that causes
// functions with no body to not emit an error if they don't support the
// current target.
@external(erlang, "external_only_erlang_ffi", "main")
pub fn run() -> Nil
