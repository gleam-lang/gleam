use crate::{cli, project};
use bytes::Bytes;
use gleam_core::{
    config::{DocsPage, PackageConfig},
    error::Error,
    io::OutputFile,
    project::ModuleOrigin,
};
use hexpm::Client;
use std::path::{Path, PathBuf};

static TOKEN_NAME: &str = concat!(env!("CARGO_PKG_NAME"), " (", env!("CARGO_PKG_VERSION"), ")");
static DOCS_DIR_NAME: &str = "docs";

pub fn remove(package: String, version: String) -> Result<(), Error> {
    // Start event loop so we can run async functions to call the Hex API
    let runtime = tokio::runtime::Runtime::new().expect("Unable to start Tokio async runtime");

    // Get login creds from user
    let username = cli::ask("https://hex.pm username")?;
    let password = cli::ask_password("https://hex.pm password")?;

    // Remove docs from API
    runtime.block_on(async {
        hexpm::UnauthenticatedClient::new()
            .authenticate(&username, &password, TOKEN_NAME)
            .await
            .map_err(|e| Error::Hex(e.to_string()))?
            .remove_docs(&package, &version)
            .await
            .map_err(|e| Error::Hex(e.to_string()))
    })?;

    // Done!
    println!(
        "The docs for {} {} have been removed from HexDocs",
        package, version
    );
    Ok(())
}

pub fn build(project_root: String, version: String, to: Option<String>) -> Result<(), Error> {
    let project_root = PathBuf::from(&project_root).canonicalize().map_err(|_| {
        Error::UnableToFindProjectRoot {
            path: project_root.clone(),
        }
    })?;

    let output_dir = to.map(PathBuf::from).unwrap_or_else(|| {
        project_root
            .join(project::OUTPUT_DIR_NAME)
            .join(DOCS_DIR_NAME)
    });

    // Build
    let (config, outputs) = build_project(&project_root, version, &output_dir)?;

    // Write
    crate::fs::delete_dir(&output_dir)?;
    crate::fs::write_outputs(&outputs)?;

    println!(
        "\nThe docs for {package} have been rendered to {output_dir}",
        package = config.name,
        output_dir = output_dir.to_string_lossy()
    );
    // We're done!
    Ok(())
}

pub fn publish(project_root: String, version: String) -> Result<(), Error> {
    let project_root = PathBuf::from(&project_root).canonicalize().map_err(|_| {
        Error::UnableToFindProjectRoot {
            path: project_root.clone(),
        }
    })?;

    let output_dir = PathBuf::new();

    // Build
    let (config, outputs) = build_project(&project_root, version.clone(), &output_dir)?;

    // Create gzipped tarball of docs
    let archive = crate::fs::create_tar_archive(outputs)?;

    // Start event loop so we can run async functions to call the Hex API
    let runtime = tokio::runtime::Runtime::new().expect("Unable to start Tokio async runtime");

    // Get login creds from user
    let username = cli::ask("https://hex.pm username")?;
    let password = cli::ask_password("https://hex.pm password")?;

    // Upload to hex
    runtime.block_on(async {
        hexpm::UnauthenticatedClient::new()
            .authenticate(&username, &password, TOKEN_NAME)
            .await
            .map_err(|e| Error::Hex(e.to_string()))?
            .publish_docs(&config.name, &version, Bytes::from(archive))
            .await
            .map_err(|e| Error::Hex(e.to_string()))
    })?;

    println!(
        "
The docs for {package} have been published to HexDocs:

    https://hexdocs.pm/{package}",
        package = config.name
    );

    // We're done!
    Ok(())
}

pub fn build_project(
    project_root: impl AsRef<Path>,
    version: String,
    output_dir: &Path,
) -> Result<(PackageConfig, Vec<OutputFile>), Error> {
    // Read and type check project
    let (mut config, analysed) = project::read_and_analyse(&project_root)?;
    config.version = version;
    check_app_file_version_matches(&project_root, &config)?;

    // Attach documentation to Src modules
    let analysed: Vec<_> = analysed
        .into_iter()
        .filter(|a| a.origin == ModuleOrigin::Src)
        .map(|mut a| {
            a.attach_doc_and_module_comments();
            a
        })
        .collect();

    // Initialize pages with the README
    let mut pages = vec![DocsPage {
        title: "README".to_string(),
        path: "index.html".to_string(),
        source: project_root.as_ref().join("README.md"),
    }];

    // Add any user-supplied pages
    pages.extend(config.docs.pages.iter().cloned());

    // Generate HTML
    let outputs =
        gleam_core::docs::generate_html(project_root, &config, &analysed, &pages, output_dir);
    Ok((config, outputs))
}

fn check_app_file_version_matches(
    root: impl AsRef<Path>,
    project_config: &PackageConfig,
) -> Result<(), Error> {
    let mut app_src_path = root.as_ref().to_path_buf();
    app_src_path.push("src");
    app_src_path.push(format!("{}.app.src", &project_config.name));

    let re =
        regex::Regex::new("\\{ *vsn *, *\"([^\"]*)\" *\\}").expect("Could not compile vsn regex");

    std::fs::read_to_string(&app_src_path)
        // Remove all new lines so we can regex easily across the content
        .map(|contents| contents.replace("\n", ""))
        .ok()
        .and_then(|contents| {
            // Extract the vsn if we can
            re.captures(&contents)
                .and_then(|captures| captures.get(1))
                .map(|capture| capture.as_str().to_string())
        })
        .map(|version| {
            if version == project_config.version {
                Ok(())
            } else {
                // Error if we've found the version and it doesn't match
                Err(Error::VersionDoesNotMatch {
                    toml_ver: project_config.version.clone(),
                    app_ver: version,
                })
            }
        })
        // Don't mind if we never found the version
        .unwrap_or(Ok(()))
}
