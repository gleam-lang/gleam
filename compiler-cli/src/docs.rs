use std::{collections::HashMap, time::SystemTime};

use camino::{Utf8Component, Utf8Path, Utf8PathBuf};
use ecow::EcoString;

use crate::{cli, fs::ProjectIO, http::HttpClient};
use gleam_core::{
    Result,
    analyse::TargetSupport,
    build::{Codegen, Compile, Mode, Options, Package, Target},
    config::{DocsPage, PackageConfig},
    docs::{Dependency, DependencyKind, DocContext},
    error::{Error, FileIoAction, FileKind},
    hex,
    io::HttpClient as _,
    manifest::ManifestPackageSource,
    paths::ProjectPaths,
    type_,
};

pub fn remove(package: String, version: String) -> Result<()> {
    let runtime = tokio::runtime::Runtime::new().expect("Unable to start Tokio async runtime");
    let http = HttpClient::new();
    let hex_config = hexpm::Config::new();
    let credentials = crate::hex::HexAuthentication::new(&runtime, &http, hex_config.clone())
        .get_or_create_api_credentials()?;

    // Remove docs from API
    let request = hexpm::api_remove_docs_request(
        &package,
        &version,
        &crate::hex::write_credentials(&credentials)?,
        &hex_config,
    )
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

    // Reset the build directory for the root package so that's recompiled and
    // the docs can be up to date for all modules.
    // Note how this doesn't delete the entire build directory, this way we can
    // avoid recompiling all the dependencies every time we're building the
    // documentation for our package.
    crate::fs::delete_directory(&paths.build_directory_for_package(
        Mode::Prod,
        options.target.unwrap_or(Target::Erlang),
        &config.name,
    ))?;

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
            // Code generation is not needed when building docs, this can speed
            // up the docs building process quite drastically, especially on the
            // Erlang target where we can avoid calling erlc.
            codegen: Codegen::None,
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
    let pages = documentation_pages(paths, config)?;
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

fn documentation_pages(
    paths: &ProjectPaths,
    config: &PackageConfig,
) -> Result<Vec<DocsPage>, Error> {
    let mut pages = vec![DocsPage {
        title: "README".into(),
        path: "index.html".into(),
        source: paths.readme(), // TODO: support non markdown READMEs. Or a default if there is none.
    }];

    for page in &config.documentation.pages {
        let path = validate_docs_page_path(paths, config, page)?;
        let source = validate_docs_page_source(paths, page)?;
        pages.push(DocsPage {
            title: page.title.clone(),
            path: path.into_string(),
            source,
        });
    }

    Ok(pages)
}

fn validate_docs_page_path(
    paths: &ProjectPaths,
    config: &PackageConfig,
    page: &DocsPage,
) -> Result<Utf8PathBuf, Error> {
    normalize_path_within_base(
        &paths.root_config(),
        "path",
        Utf8Path::new(&page.path),
        &page.path,
        &paths.build_documentation_directory(&config.name),
        "documentation output directory",
    )
}

fn validate_docs_page_source(paths: &ProjectPaths, page: &DocsPage) -> Result<Utf8PathBuf, Error> {
    let source = normalize_path_within_base(
        &paths.root_config(),
        "source",
        &page.source,
        &page.source,
        paths.root(),
        "project root",
    )?;
    Ok(paths.root().join(source))
}

fn normalize_path_within_base(
    config_path: &Utf8Path,
    field_name: &str,
    path: &Utf8Path,
    value: impl std::fmt::Display,
    base: &Utf8Path,
    boundary_name: &str,
) -> Result<Utf8PathBuf, Error> {
    debug_assert!(base.is_absolute());
    let base_parts = base
        .components()
        .filter_map(|component| match component {
            Utf8Component::Normal(component) => Some(component.to_string()),
            _ => None,
        })
        .collect::<Vec<_>>();
    let mut normalized_parts = base_parts.clone();
    let base_depth = base_parts.len();

    for component in path.components() {
        match component {
            Utf8Component::CurDir => (),
            Utf8Component::Normal(component) => normalized_parts.push(component.to_string()),
            Utf8Component::ParentDir => {
                if !normalized_parts.is_empty() {
                    let _ = normalized_parts.pop();
                }
            }
            Utf8Component::RootDir | Utf8Component::Prefix(_) => {
                return Err(Error::FileIo {
                    action: FileIoAction::Parse,
                    kind: FileKind::File,
                    path: config_path.to_path_buf(),
                    err: Some(format!(
                        "Invalid documentation page {field_name} `{value}`. It must stay within the {boundary_name}."
                    )),
                });
            }
        }
    }

    if normalized_parts.get(..base_depth) != Some(base_parts.as_slice()) {
        return Err(Error::FileIo {
            action: FileIoAction::Parse,
            kind: FileKind::File,
            path: config_path.to_path_buf(),
            err: Some(format!(
                "Invalid documentation page {field_name} `{value}`. It must stay within the {boundary_name}."
            )),
        });
    }

    let mut normalized = Utf8PathBuf::new();
    for component in normalized_parts.into_iter().skip(base_depth) {
        normalized.push(component);
    }

    Ok(normalized)
}

#[cfg(test)]
mod tests {
    use super::*;
    use gleam_core::config::PackageConfig;

    #[test]
    fn custom_docs_page_path_must_stay_within_project() {
        let paths = ProjectPaths::new(Utf8PathBuf::from("/tmp/project"));
        let mut config = PackageConfig::default();
        config.documentation.pages.push(DocsPage {
            title: "Escape".into(),
            path: "../../escape.html".into(),
            source: Utf8PathBuf::from("README.md"),
        });

        let error = documentation_pages(&paths, &config).expect_err("invalid docs page path");

        assert!(matches!(
            error,
            Error::FileIo {
                action: FileIoAction::Parse,
                ..
            }
        ));
    }

    #[test]
    fn custom_docs_page_absolute_path_is_rejected() {
        let paths = ProjectPaths::new(Utf8PathBuf::from("/tmp/project"));
        let mut config = PackageConfig::default();
        config.name = "project".into();
        config.documentation.pages.push(DocsPage {
            title: "Escape".into(),
            path: "/tmp/escape.html".into(),
            source: Utf8PathBuf::from("README.md"),
        });

        let error = documentation_pages(&paths, &config).expect_err("invalid docs page path");

        assert!(matches!(
            error,
            Error::FileIo {
                action: FileIoAction::Parse,
                ..
            }
        ));
    }

    #[test]
    fn custom_docs_pages_are_resolved_under_the_docs_output_directory() {
        let paths = ProjectPaths::new(Utf8PathBuf::from("/tmp/project"));
        let mut config = PackageConfig::default();
        config.name = "project".into();
        config.documentation.pages.push(DocsPage {
            title: "Guide".into(),
            path: "../project/guides/intro.html".into(),
            source: Utf8PathBuf::from("docs/intro.md"),
        });

        let pages = documentation_pages(&paths, &config).expect("valid docs pages");

        assert_eq!(pages[1].path, "guides/intro.html");
        assert_eq!(pages[1].source, Utf8PathBuf::from("/tmp/project/docs/intro.md"));
    }

    #[test]
    fn custom_docs_page_source_must_stay_within_project_root() {
        let paths = ProjectPaths::new(Utf8PathBuf::from("/tmp/project"));
        let mut config = PackageConfig::default();
        config.documentation.pages.push(DocsPage {
            title: "Leak".into(),
            path: "leak.html".into(),
            source: Utf8PathBuf::from("/etc/passwd"),
        });

        let error = documentation_pages(&paths, &config).expect_err("invalid docs page source");

        assert!(matches!(
            error,
            Error::FileIo {
                action: FileIoAction::Parse,
                ..
            }
        ));
    }

    #[test]
    fn custom_docs_page_source_can_leave_and_return_within_project_root() {
        let paths = ProjectPaths::new(Utf8PathBuf::from("/tmp/project"));
        let mut config = PackageConfig::default();
        config.documentation.pages.push(DocsPage {
            title: "Guide".into(),
            path: "guide.html".into(),
            source: Utf8PathBuf::from("docs/../README.md"),
        });

        let pages = documentation_pages(&paths, &config).expect("valid docs pages");

        assert_eq!(pages[1].source, Utf8PathBuf::from("/tmp/project/README.md"));
    }
}

pub fn publish(paths: &ProjectPaths) -> Result<()> {
    let config = crate::config::root_config(paths)?;
    let http = HttpClient::new();
    let runtime = tokio::runtime::Runtime::new().expect("Unable to start Tokio async runtime");
    let hex_config = hexpm::Config::new();
    let credentials = crate::hex::HexAuthentication::new(&runtime, &http, hex_config.clone())
        .get_or_create_api_credentials()?;

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

    cli::print_publishing_documentation();
    runtime.block_on(hex::publish_documentation(
        &config.name,
        &config.version,
        archive,
        &crate::hex::write_credentials(&credentials)?,
        &hex_config,
        &http,
    ))?;
    cli::print_published("documentation");
    Ok(())
}
