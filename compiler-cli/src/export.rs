use std::sync::Arc;

use camino::Utf8PathBuf;
use gleam_core::{
    analyse::TargetSupport,
    build::{Codegen, Mode, Options, Target},
    Result,
};

#[cfg(target_os = "windows")]
static ENTRYPOINT_FILENAME: &str = "entrypoint.ps1";
#[cfg(not(target_os = "windows"))]
static ENTRYPOINT_FILENAME: &str = "entrypoint.sh";

#[cfg(target_os = "windows")]
static ENTRYPOINT_TEMPLATE: &str = include_str!("../templates/erlang-shipment-entrypoint.ps1");
#[cfg(not(target_os = "windows"))]
static ENTRYPOINT_TEMPLATE: &str = include_str!("../templates/erlang-shipment-entrypoint.sh");

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
    let paths = crate::find_project_paths()?;
    let target = Target::Erlang;
    let mode = Mode::Prod;
    let build = paths.build_directory_for_target(mode, target);
    let out = paths.erlang_shipment_directory();

    crate::fs::mkdir(&out)?;

    // Reset the directories to ensure we have a clean slate and no old code
    crate::fs::delete_directory(&build)?;
    crate::fs::delete_directory(&out)?;

    // Build project in production mode
    let built = crate::build::main(
        Options {
            root_target_support: TargetSupport::Enforced,
            warnings_as_errors: false,
            codegen: Codegen::All,
            mode,
            target: Some(target),
        },
        crate::build::download_dependencies()?,
        Arc::new(crate::cli::Reporter::new()),
    )?;

    for entry in crate::fs::read_dir(&build)?.filter_map(Result::ok) {
        let path = entry.path();

        // We are only interested in package directories
        if !path.is_dir() {
            continue;
        }

        let name = path.file_name().expect("Directory name");
        let build = build.join(name);
        let out = out.join(name);
        crate::fs::mkdir(&out)?;

        // Copy desired package subdirectories
        for subdirectory in ["ebin", "priv", "include"] {
            let source = build.join(subdirectory);
            if source.is_dir() {
                let source = crate::fs::canonicalise(&source)?;
                let out = out.join(subdirectory);
                crate::fs::copy_dir(source, &out)?;
            }
        }
    }

    // Write entrypoint script
    let entrypoint = out.join(ENTRYPOINT_FILENAME);
    let text =
        ENTRYPOINT_TEMPLATE.replace("$PACKAGE_NAME_FROM_GLEAM", &built.root_package.config.name);
    crate::fs::write(&entrypoint, &text)?;
    crate::fs::make_executable(&entrypoint)?;

    crate::cli::print_exported(&built.root_package.config.name);

    println!(
        "
Your Erlang shipment has been generated to {path}.

It can be copied to a compatible server with Erlang installed and run with
the {file} script.

    {entrypoint}
",
        path = out,
        file = ENTRYPOINT_FILENAME,
        entrypoint = entrypoint,
    );

    Ok(())
}

pub fn hex_tarball() -> Result<()> {
    let paths = crate::find_project_paths()?;
    let config = crate::config::root_config()?;
    let data: Vec<u8> = crate::publish::build_hex_tarball(&paths, &config)?;

    let path = paths.build_export_hex_tarball(&config.name, &config.version.to_string());
    crate::fs::write_bytes(&path, &data)?;
    println!(
        "
Your hex tarball has been generated in {}.
",
        &path
    );
    Ok(())
}

pub fn javascript_prelude() -> Result<()> {
    print!("{}", gleam_core::javascript::PRELUDE);
    Ok(())
}

pub fn typescript_prelude() -> Result<()> {
    print!("{}", gleam_core::javascript::PRELUDE_TS_DEF);
    Ok(())
}

pub fn package_interface(path: Utf8PathBuf) -> Result<()> {
    // Build the project
    let mut built = crate::build::main(
        Options {
            mode: Mode::Prod,
            target: None,
            codegen: Codegen::All,
            warnings_as_errors: false,
            root_target_support: TargetSupport::Enforced,
        },
        crate::build::download_dependencies()?,
        Arc::new(crate::cli::Reporter::new()),
    )?;
    built.root_package.attach_doc_and_module_comments();

    let out = gleam_core::docs::generate_json_package_interface(path, &built.root_package);
    crate::fs::write_outputs_under(&[out], crate::find_project_paths()?.root())?;
    Ok(())
}
