use gleam_core::{
    build::{Mode, Options, Target},
    Result,
};

// TODO: copy files across
// TODO: generate start script
// TODO: start in embedded mode
// TODO: print help information
// TODO: test

/// Generate a directory of precompiled Erlang along with a start script.
/// Suitable for deployment to a server.
///
/// For each Erlang application (aka package) directory these directories are
/// copied across:
/// - ebin
/// - include
/// - priv
pub(crate) fn erlang_parcel() -> Result<()> {
    // Build project in production mode
    let _ = crate::build::main(Options {
        perform_codegen: true,
        mode: Mode::Prod,
        target: Some(Target::Erlang),
    })?;

    Ok(())
}
