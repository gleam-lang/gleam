use camino::Utf8PathBuf;
use gleam_core::{
    Result,
    analyse::TargetSupport,
    build::{Codegen, Compile, Mode, Options, Target},
    paths::ProjectPaths,
};
use itertools::Itertools;
use std::io::{self, Write};
use std::{fs::File, io::Cursor};
use zip::ZipWriter;

use crate::fs;

static ENTRYPOINT_FILENAME_POWERSHELL: &str = "entrypoint.ps1";
static ENTRYPOINT_FILENAME_POSIX_SHELL: &str = "entrypoint.sh";

static ENTRYPOINT_TEMPLATE_POWERSHELL: &str =
    include_str!("../templates/erlang-shipment-entrypoint.ps1");
static ENTRYPOINT_TEMPLATE_POSIX_SHELL: &str =
    include_str!("../templates/erlang-shipment-entrypoint.sh");

/// Generate a single file of precompiled Erlang, suitable for CLIs.
pub fn escript(paths: &ProjectPaths) -> Result<()> {
    // TODO: Add new entrypoint shim module
    //
    // ```erlang
    // -module(gleescript_main_shim).
    //
    // -export([main/1]).
    //
    // main(_) ->
    //     io:setopts(standard_io, [binary, {encoding, utf8}]),
    //     io:setopts(standard_error, [{encoding, utf8}]),
    //     ApplicationModule = INLINE_THE_MODULE_HERE,
    //     {ok, _} = application:ensure_all_started(ApplicationModule),
    //     ApplicationModule:main().
    // ```
    //
    // TODO: Compile project (including new shim)
    //
    // TODO: Build zip archive of all the files
    //
    // TODO: Make a file with
    //   1. shebang
    //   2. emulator flags including `-escript main gleescript_main_shim`
    //   3. the zip file
    //   4. write to disc
    let target = Target::Erlang;
    let mode = Mode::Prod;
    let build = paths.build_directory_for_target(mode, target);
    let out = paths.erlang_shipment_directory();

    crate::fs::mkdir(&out)?;

    // Reset the directories to ensure we have a clean slate and no old code
    crate::fs::delete_directory(&build)?;
    crate::fs::delete_directory(&out)?;

    let manifest = crate::build::download_dependencies(paths, crate::cli::Reporter::new())?;
    let packages = manifest
        .packages
        .iter()
        .map(|package| package.name.clone())
        .collect_vec();

    // Build project in production mode
    let build_options = Options {
        root_target_support: TargetSupport::Enforced,
        warnings_as_errors: false,
        codegen: Codegen::All,
        compile: Compile::All,
        mode,
        target: Some(target),
        no_print_progress: false,
    };
    let built = crate::build::main(paths, build_options, manifest)?;

    // Create a zip file to write to

    let mut zip = ZipWriter::new(Cursor::new(Vec::new()));
    let options = zip::write::SimpleFileOptions::default();

    for package in packages {
        let directory = format!("{package}/ebin");
        zip.add_directory(directory, options).map_err(|_| todo!())?;

        let zip_path = format!("{package}/ebin/{package}.app");
        zip.start_file(&zip_path, options).map_err(|_| todo!())?;
        // TODO: move to fs
        let input = paths.build_package_app_file(mode, target, &package);

        // TODO: replace this with only iterating over the production packages
        if !input.exists() {
            continue;
        }

        let mut file = std::fs::File::open(dbg!(input)).unwrap();
        let _: u64 = std::io::copy(&mut file, &mut zip).map_err(|_| todo!())?;

        // let name = path.file_name().expect("Directory name");
        // let build = build.join(name);
        // let out = out.join(name);
        // crate::fs::mkdir(&out)?;
        //
        // // Copy desired package subdirectories
        // for subdirectory in ["ebin", "priv", "include"] {
        //     let source = build.join(subdirectory);
        //     if source.is_dir() {
        //         let source = crate::fs::canonicalise(&source)?;
        //         let out = out.join(subdirectory);
        //         crate::fs::copy_dir(source, &out)?;
        //     }
        // }
    }

    let result = zip.finish().map_err(|_| todo!())?;
    let bytes: Vec<u8> = result.into_inner();
    fs::write_bytes(&Utf8PathBuf::from("stuff.zip"), &bytes)?;

    Ok(())
}

/// Generate a directory of precompiled Erlang along with a start script.
/// Suitable for deployment to a server.
///
/// For each Erlang application (aka package) directory these directories are
/// copied across:
/// - ebin
/// - include
/// - priv
pub(crate) fn erlang_shipment(paths: &ProjectPaths) -> Result<()> {
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
        paths,
        Options {
            root_target_support: TargetSupport::Enforced,
            warnings_as_errors: false,
            codegen: Codegen::All,
            compile: Compile::All,
            mode,
            target: Some(target),
            no_print_progress: false,
        },
        crate::build::download_dependencies(paths, crate::cli::Reporter::new())?,
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

    // PowerShell entry point script.
    write_entrypoint_script(
        &out.join(ENTRYPOINT_FILENAME_POWERSHELL),
        ENTRYPOINT_TEMPLATE_POWERSHELL,
        &built.root_package.config.name,
    )?;

    // POSIX Shell entry point script.
    write_entrypoint_script(
        &out.join(ENTRYPOINT_FILENAME_POSIX_SHELL),
        ENTRYPOINT_TEMPLATE_POSIX_SHELL,
        &built.root_package.config.name,
    )?;

    crate::cli::print_exported(&built.root_package.config.name);

    println!(
        "
Your Erlang shipment has been generated to {out}.

It can be copied to a compatible server with Erlang installed and run with
one of the following scripts:
    - {ENTRYPOINT_FILENAME_POWERSHELL} (PowerShell script)
    - {ENTRYPOINT_FILENAME_POSIX_SHELL} (POSIX Shell script)
",
    );

    Ok(())
}

fn write_entrypoint_script(
    entrypoint_output_path: &Utf8PathBuf,
    entrypoint_template_path: &str,
    package_name: &str,
) -> Result<()> {
    let text = entrypoint_template_path.replace("$PACKAGE_NAME_FROM_GLEAM", package_name);
    crate::fs::write(entrypoint_output_path, &text)?;
    crate::fs::make_executable(entrypoint_output_path)?;
    Ok(())
}

pub fn hex_tarball(paths: &ProjectPaths) -> Result<()> {
    let mut config = crate::config::root_config(paths)?;
    let data: Vec<u8> = crate::publish::build_hex_tarball(paths, &mut config)?;

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

pub fn package_interface(paths: &ProjectPaths, out: Utf8PathBuf) -> Result<()> {
    // Build the project
    let mut built = crate::build::main(
        paths,
        Options {
            mode: Mode::Prod,
            target: None,
            codegen: Codegen::None,
            compile: Compile::All,
            warnings_as_errors: false,
            root_target_support: TargetSupport::Enforced,
            no_print_progress: false,
        },
        crate::build::download_dependencies(paths, crate::cli::Reporter::new())?,
    )?;
    built.root_package.attach_doc_and_module_comments();

    let out = gleam_core::docs::generate_json_package_interface(
        out,
        &built.root_package,
        &built.module_interfaces,
    );
    crate::fs::write_outputs_under(&[out], paths.root())?;
    Ok(())
}

pub fn package_information(paths: &ProjectPaths, out: Utf8PathBuf) -> Result<()> {
    let config = crate::config::root_config(paths)?;
    let out = gleam_core::docs::generate_json_package_information(out, config);
    crate::fs::write_outputs_under(&[out], paths.root())?;
    Ok(())
}
