use std::io::Cursor;

use crate::fs;
use camino::Utf8PathBuf;
use gleam_core::{
    Result,
    analyse::TargetSupport,
    build::{Codegen, Compile, Mode, Options, Target},
    paths::ProjectPaths,
};

static ENTRYPOINT_FILENAME_POWERSHELL: &str = "entrypoint.ps1";
static ENTRYPOINT_FILENAME_POSIX_SHELL: &str = "entrypoint.sh";

static ENTRYPOINT_TEMPLATE_POWERSHELL: &str =
    include_str!("../templates/erlang-shipment-entrypoint.ps1");
static ENTRYPOINT_TEMPLATE_POSIX_SHELL: &str =
    include_str!("../templates/erlang-shipment-entrypoint.sh");

/// Generate a single file of precompiled Erlang, suitable for CLIs.
pub fn escript(paths: &ProjectPaths) -> Result<()> {
    use std::io::Write;

    // TODO: ensure that the main function exists

    let target = Target::Erlang;
    let mode = Mode::Prod;
    let build = paths.build_directory_for_target(mode, target);

    // Reset the directories to ensure we have a clean slate and no old code
    fs::delete_directory(&build)?;

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

    // Create the zip archive for the code
    let mut zip = ZipArchive::new(Cursor::new(Vec::new()));

    for entry in fs::read_dir(&build)? {
        let entry = entry.map_err(|e| todo!())?;
        let ebin = entry.path().join("ebin");

        // We want the ebin code directories for each package
        if !ebin.is_dir() {
            continue;
        }

        for entry in fs::read_dir(&ebin)? {
            let entry = entry.map_err(|e| todo!())?;
            let path = entry.path();
            let extension = path.extension().unwrap_or_default();

            let Some(name) = path.file_name() else {
                continue;
            };

            if !path.is_file() {
                continue;
            }

            // We want to copy compiled BEAM bytecode and app configuration files
            if extension != "beam" && extension != "app" {
                continue;
            }

            zip.add_file_from_disc(path, name)?;
        }
    }

    let zip = zip.finish()?.into_inner();

    let package_name = Utf8PathBuf::from(&built.root_package.config.name);
    let mut file = std::fs::File::create(&package_name).map_err(|_| todo!())?;
    let header = format!(
        "#!/usr/bin/env escript
%%
%%!-escript main {package_name}@@main
"
    );

    file.write_all(header.as_bytes()).unwrap();
    file.write_all(&zip).unwrap();

    fs::make_executable(&package_name)?;

    println!(
        "
Your escript has been generated to ./{package_name}.
",
    );

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

    fs::mkdir(&out)?;

    // Reset the directories to ensure we have a clean slate and no old code
    fs::delete_directory(&build)?;
    fs::delete_directory(&out)?;

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

    for entry in fs::read_dir(&build)?.filter_map(Result::ok) {
        let path = entry.path();

        // We are only interested in package directories
        if !path.is_dir() {
            continue;
        }

        let name = path.file_name().expect("Directory name");
        let build = build.join(name);
        let out = out.join(name);
        fs::mkdir(&out)?;

        // Copy desired package subdirectories
        for subdirectory in ["ebin", "priv", "include"] {
            let source = build.join(subdirectory);
            if source.is_dir() {
                let source = fs::canonicalise(&source)?;
                let out = out.join(subdirectory);
                fs::copy_dir(source, &out)?;
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
    fs::write(entrypoint_output_path, &text)?;
    fs::make_executable(entrypoint_output_path)?;
    Ok(())
}

pub fn hex_tarball(paths: &ProjectPaths) -> Result<()> {
    let mut config = crate::config::root_config(paths)?;
    let data: Vec<u8> = crate::publish::build_hex_tarball(paths, &mut config)?;

    let path = paths.build_export_hex_tarball(&config.name, &config.version.to_string());
    fs::write_bytes(&path, &data)?;
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
    fs::write_outputs_under(&[out], paths.root())?;
    Ok(())
}

pub fn package_information(paths: &ProjectPaths, out: Utf8PathBuf) -> Result<()> {
    let config = crate::config::root_config(paths)?;
    let out = gleam_core::docs::generate_json_package_information(out, config);
    fs::write_outputs_under(&[out], paths.root())?;
    Ok(())
}
