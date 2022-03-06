/// The current version of the gleam compiler. If this does not match what is
/// already in the build folder we will not reuse any cached artifacts and
/// instead build from scratch
pub const COMPILER_VERSION: &str = env!("CARGO_PKG_VERSION");
