use std::{
    collections::HashMap,
    time::{Instant, SystemTime},
};

use camino::{Utf8Path, Utf8PathBuf};
use ecow::EcoString;

use crate::{cli, fs::ProjectIO, http::HttpClient};
use gleam_core::{
    Result,
    analyse::TargetSupport,
    build::{Codegen, Compile, Mode, Options, Package, Target},
    config::{DocsPage, PackageConfig},
    docs::{Dependency, DependencyKind, DocContext},
    error::Error,
    hex,
    io::HttpClient as _,
    manifest::ManifestPackageSource,
    paths::ProjectPaths,
    type_,
};

pub fn remove(package: String, version: String) -> Result<()> {
    let runtime = tokio::runtime::Runtime::new().expect("Unable to start Tokio async runtime");
    let hex_config = hexpm::Config::new();
    let api_key =
        crate::hex::HexAuthentication::new(&runtime, hex_config.clone()).get_or_create_api_key()?;
    let http = HttpClient::new();

    // Remove docs from API
    let request = hexpm::api_remove_docs_request(&package, &version, &api_key, &hex_config)
        .map_err(Error::hex)?;
    let response = runtime.block_on(http.send(request))?;
    hexpm::api_remove_docs_response(response).map_err(Error::hex)?;

    // Done!
    println!("The docs for {package} {version} have been removed from HexDocs");
    Ok(())
}

#[derive(Debug)]
pub struct BuildOptions {
    /// Whether to open the docs after building.
    pub open: bool,
    pub target: Option<Target>,
}

pub fn build(paths: &ProjectPaths, options: BuildOptions) -> Result<()> {
    let config = crate::config::root_config(paths)?;

    // Reset the build directory so we know the state of the project
    crate::fs::delete_directory(&paths.build_directory_for_target(Mode::Prod, config.target))?;

    let out = paths.build_documentation_directory(&config.name);

    let manifest = crate::build::download_dependencies(paths, cli::Reporter::new())?;
    let dependencies = manifest
        .packages
        .iter()
        .map(|package| {
            (
                package.name.clone(),
                Dependency {
                    version: package.version.clone(),
                    kind: match &package.source {
                        ManifestPackageSource::Hex { .. } => DependencyKind::Hex,
                        ManifestPackageSource::Git { .. } => DependencyKind::Git,
                        ManifestPackageSource::Local { .. } => DependencyKind::Path,
                    },
                },
            )
        })
        .collect();

    let mut built = crate::build::main(
        paths,
        Options {
            mode: Mode::Prod,
            target: options.target,
            codegen: Codegen::All,
            compile: Compile::All,
            warnings_as_errors: false,
            root_target_support: TargetSupport::Enforced,
            no_print_progress: false,
        },
        manifest,
    )?;
    let outputs = build_documentation(
        paths,
        &config,
        dependencies,
        &mut built.root_package,
        DocContext::Build,
        &built.module_interfaces,
    )?;

    // Write
    crate::fs::delete_directory(&out)?;
    crate::fs::write_outputs_under(&outputs, &out)?;

    let index_html = out.join("index.html");

    println!(
        "\nThe documentation for {package} has been rendered to \n{index_html}",
        package = config.name,
        index_html = index_html
    );

    if options.open {
        open_docs(&index_html)?;
    }

    // We're done!
    Ok(())
}

/// Opens the indicated path in the default program configured by the system.
///
/// For the docs this will generally be a browser (unless some other program is
/// configured as the default for `.html` files).
fn open_docs(path: &Utf8Path) -> Result<()> {
    opener::open(path).map_err(|error| Error::FailedToOpenDocs {
        path: path.to_path_buf(),
        error: error.to_string(),
    })?;

    Ok(())
}

pub(crate) fn build_documentation(
    paths: &ProjectPaths,
    config: &PackageConfig,
    dependencies: HashMap<EcoString, Dependency>,
    compiled: &mut Package,
    is_hex_publish: DocContext,
    cached_modules: &im::HashMap<EcoString, type_::ModuleInterface>,
) -> Result<Vec<gleam_core::io::OutputFile>, Error> {
    compiled.attach_doc_and_module_comments();
    cli::print_generating_documentation();
    let mut pages = vec![DocsPage {
        title: "README".into(),
        path: "index.html".into(),
        source: paths.readme(), // TODO: support non markdown READMEs. Or a default if there is none.
    }];
    pages.extend(config.documentation.pages.iter().cloned());
    let mut outputs = gleam_core::docs::generate_html(
        paths,
        gleam_core::docs::DocumentationConfig {
            package_config: config,
            dependencies,
            analysed: compiled.modules.as_slice(),
            docs_pages: &pages,
            rendering_timestamp: SystemTime::now(),
            context: is_hex_publish,
        },
        ProjectIO::new(),
    );

    outputs.push(gleam_core::docs::generate_json_package_interface(
        Utf8PathBuf::from("package-interface.json"),
        compiled,
        cached_modules,
    ));
    Ok(outputs)
}

pub fn publish(paths: &ProjectPaths) -> Result<()> {
    let config = crate::config::root_config(paths)?;

    let runtime = tokio::runtime::Runtime::new().expect("Unable to start Tokio async runtime");
    let hex_config = hexpm::Config::new();
    let api_key =
        crate::hex::HexAuthentication::new(&runtime, hex_config.clone()).get_or_create_api_key()?;

    // Reset the build directory so we know the state of the project
    crate::fs::delete_directory(&paths.build_directory_for_target(Mode::Prod, config.target))?;

    let manifest = crate::build::download_dependencies(paths, cli::Reporter::new())?;
    let dependencies = manifest
        .packages
        .iter()
        .map(|package| {
            (
                package.name.clone(),
                Dependency {
                    version: package.version.clone(),
                    kind: match &package.source {
                        ManifestPackageSource::Hex { .. } => DependencyKind::Hex,
                        ManifestPackageSource::Git { .. } => DependencyKind::Git,
                        ManifestPackageSource::Local { .. } => DependencyKind::Path,
                    },
                },
            )
        })
        .collect();

    let mut built = crate::build::main(
        paths,
        Options {
            root_target_support: TargetSupport::Enforced,
            warnings_as_errors: false,
            codegen: Codegen::All,
            compile: Compile::All,
            mode: Mode::Prod,
            target: None,
            no_print_progress: false,
        },
        manifest,
    )?;
    let outputs = build_documentation(
        paths,
        &config,
        dependencies,
        &mut built.root_package,
        DocContext::HexPublish,
        &built.module_interfaces,
    )?;
    let archive = crate::fs::create_tar_archive(outputs)?;

    let start = Instant::now();
    cli::print_publishing_documentation();
    runtime.block_on(hex::publish_documentation(
        &config.name,
        &config.version,
        archive,
        &api_key,
        &hex_config,
        &HttpClient::new(),
    ))?;
    cli::print_published(start.elapsed());
    Ok(())
}
