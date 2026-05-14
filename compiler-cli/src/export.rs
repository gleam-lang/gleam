use camino::Utf8PathBuf;
use gleam_core::{
    Result,
    analyse::TargetSupport,
    build::{Codegen, Compile, Mode, Options, Target},
    paths::ProjectPaths,
};
use im::HashSet;
use itertools::Itertools;
use std::io::{self, Write};
use std::{fs::File, io::Cursor};

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
    let mut packages = HashSet::new();

    // Create a zip file to write to
    let mut zip = ZipArchive::new(Cursor::new(Vec::new()));

    for module in built.module_interfaces.values() {
        // Skip the prelude module, it doesn't exist at runtime.
        if module.is_prelude() {
            continue;
        }

        let package = &module.package;

        // If we have not seen this package before then create its directory and
        // configuration in the archive
        if packages.insert(module.package.clone()).is_none() {
            zip.create_directory(format!("{package}/ebin"))?;
            let app_disc_path = paths.build_package_dot_app(mode, target, &package);
            let app_zip_path = format!("{package}/ebin/{package}.app");
            zip.add_file_from_disc(app_disc_path, app_zip_path)?;
        }

        let beam_disc_path = paths.build_package_beam(mode, target, &package, &module.name);
        let beam_zip_path = format!(
            "{package}/ebin/{name}.beam",
            name = module.name.replace("/", "@")
        );
        zip.add_file_from_disc(beam_disc_path, beam_zip_path)?;
    }

    let result = zip.finish().map_err(|_| todo!())?;
    let bytes: Vec<u8> = result.into_inner();
    crate::fs::write_bytes(&Utf8PathBuf::from("stuff.zip"), &bytes)?;

    Ok(())
}

pub struct ZipArchive<W: std::io::Write + std::io::Seek> {
    zip: zip::ZipWriter<W>,
}

impl<W: std::io::Write + std::io::Seek> ZipArchive<W> {
    pub fn new(writer: W) -> Self {
        Self {
            zip: zip::ZipWriter::new(writer),
        }
    }

    pub fn finish(self) -> Result<W> {
        self.zip.finish().map_err(|_| todo!("implement error"))
    }

    pub fn create_directory(&mut self, path: impl Into<String>) -> Result<()> {
        self.zip
            .add_directory(path, self.options())
            .map_err(|_| todo!())
    }

    pub fn add_file_from_disc(
        &mut self,
        disc_path: impl AsRef<std::path::Path>,
        zip_path: impl Into<String>,
    ) -> Result<()> {
        self.zip
            .start_file(zip_path.into(), self.options())
            .map_err(|_| todo!("implement error"))?;
        // TODO: move this file open code to the fs module
        let mut file = std::fs::File::open(disc_path).unwrap();
        let _: u64 = std::io::copy(&mut file, &mut self.zip).unwrap();
        Ok(())
    }

    fn options(&self) -> zip::write::FileOptions<'static, ()> {
        zip::write::SimpleFileOptions::default()
    }
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
