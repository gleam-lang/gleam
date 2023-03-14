use gleam_core::{
    build::{Codegen, Mode, Options, Target},
    Result,
};

// TODO: start in embedded mode
// TODO: test

/// Generate a directory of precompiled Erlang along with a start script.
/// Suitable for deployment to a server.
///
/// For each Erlang application (aka package) directory these directories are
/// copied across:
/// - ebin
/// - include
/// - priv
pub(crate) fn erlang_shipment() -> Result<()> {
    let paths = crate::project_paths_at_current_directory();
    let target = Target::Erlang;
    let mode = Mode::Prod;
    let build = paths.build_directory_for_target(mode, target);
    let out = paths.erlang_shipment_directory();

    crate::fs::mkdir(&out)?;

    // Reset the directories to ensure we have a clean slate and no old code
    crate::fs::delete_dir(&build)?;
    crate::fs::delete_dir(&out)?;

    // Build project in production mode
    let (_, package) = crate::build::main(Options {
        warnings_as_errors: false,
        codegen: Codegen::All,
        mode,
        target: Some(target),
    })?;

    for entry in crate::fs::read_dir(&build)?.filter_map(Result::ok) {
        let path = entry.path();

        // We are only interested in package directories
        if !path.is_dir() {
            continue;
        }

        let name = path.file_name().expect("Directory name").to_string_lossy();
        let build = build.join(name.as_ref());
        let out = out.join(name.as_ref());
        crate::fs::mkdir(&out)?;

        // Copy desired package subdirectories
        for subdirectory in ["ebin", "priv", "include"] {
            let source = build.join(subdirectory);
            if source.is_dir() {
                let source = crate::fs::canonicalise(&source)?;
                crate::fs::copy_dir(source, &out)?;
            }
        }
    }

    // Write entrypoint script
    let entrypoint = out.join("entrypoint.sh");
    let text = include_str!("../templates/erlang-shipment-entrypoint.sh")
        .replace("$PACKAGE_NAME_FROM_GLEAM", &package.config.name);
    crate::fs::write(&entrypoint, &text)?;
    crate::fs::make_executable(&entrypoint)?;

    crate::cli::print_exported(&package.config.name);

    println!(
        "
Your Erlang shipment has been generated to {path}.

It can be copied to a compatible server with Erlang installed and run with
the entrypoint.sh script.

    {entrypoint}
",
        path = out.to_string_lossy(),
        entrypoint = entrypoint.to_string_lossy(),
    );

    Ok(())
}

pub fn hex_tarball() -> Result<()> {
    let paths = crate::project_paths_at_current_directory();
    let config = crate::config::root_config()?;
    let data: Vec<u8> = crate::publish::build_hex_tarball(&paths, &config)?;

    let path = paths.build_export_hex_tarball(&config.name, &config.version.to_string());
    crate::fs::write_bytes(&path, &data)?;
    println!(
        "
Your hex tarball has been generated in {}.
",
        &path.display()
    );
    Ok(())
}
