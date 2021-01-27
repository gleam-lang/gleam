use crate::{
    cli,
    error::{Error, GleamExpect},
    project,
};
use bytes::Bytes;
use hexpm::Client;
use std::path::PathBuf;

static TOKEN_NAME: &str = concat!(env!("CARGO_PKG_NAME"), " (", env!("CARGO_PKG_VERSION"), ")");
static DOCS_DIR_NAME: &str = "docs";

pub fn remove(package: String, version: String) -> Result<(), Error> {
    // Start event loop so we can run async functions to call the Hex API
    let runtime =
        tokio::runtime::Runtime::new().gleam_expect("Unable to start Tokio async runtime");

    // Get login creds from user
    let username = cli::ask("https://hex.pm username")?;
    let password = cli::ask_password("https://hex.pm password")?;

    // Remove docs from API
    runtime.block_on(async {
        hexpm::UnauthenticatedClient::new()
            .authenticate(username.as_str(), password.as_str(), TOKEN_NAME)
            .await
            .map_err(|e| Error::Hex(e.to_string()))?
            .remove_docs(package.as_str(), version.as_str())
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
    let (config, outputs) = super::build_project(&project_root, version, &output_dir)?;

    // Write
    crate::fs::delete_dir(&output_dir)?;
    crate::fs::write_outputs(outputs.as_slice())?;

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
    let (config, outputs) = super::build_project(&project_root, version.clone(), &output_dir)?;

    // Create gzipped tarball of docs
    let archive = crate::fs::create_tar_archive(outputs)?;

    // Start event loop so we can run async functions to call the Hex API
    let runtime =
        tokio::runtime::Runtime::new().gleam_expect("Unable to start Tokio async runtime");

    // Get login creds from user
    let username = cli::ask("https://hex.pm username")?;
    let password = cli::ask_password("https://hex.pm password")?;

    // Upload to hex
    runtime.block_on(async {
        hexpm::UnauthenticatedClient::new()
            .authenticate(username.as_str(), password.as_str(), TOKEN_NAME)
            .await
            .map_err(|e| Error::Hex(e.to_string()))?
            .publish_docs(config.name.as_str(), version.as_str(), Bytes::from(archive))
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
