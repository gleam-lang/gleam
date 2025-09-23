mod auth;

use crate::{cli, http::HttpClient};
use gleam_core::{
    Error, Result,
    hex::{self, RetirementReason},
    io::HttpClient as _,
    paths::ProjectPaths,
};

pub use auth::HexAuthentication;

pub fn retire(
    package: String,
    version: String,
    reason: RetirementReason,
    message: Option<String>,
) -> Result<()> {
    let runtime = tokio::runtime::Runtime::new().expect("Unable to start Tokio async runtime");
    let config = hexpm::Config::new();
    let api_key = HexAuthentication::new(&runtime, config.clone()).get_or_create_api_key()?;

    runtime.block_on(hex::retire_release(
        &package,
        &version,
        reason,
        message.as_deref(),
        &api_key,
        &config,
        &HttpClient::new(),
    ))?;
    cli::print_retired(&package, &version);
    Ok(())
}

pub fn unretire(package: String, version: String) -> Result<()> {
    let runtime = tokio::runtime::Runtime::new().expect("Unable to start Tokio async runtime");
    let config = hexpm::Config::new();
    let api_key = HexAuthentication::new(&runtime, config.clone()).get_or_create_api_key()?;

    runtime.block_on(hex::unretire_release(
        &package,
        &version,
        &api_key,
        &config,
        &HttpClient::new(),
    ))?;
    cli::print_unretired(&package, &version);
    Ok(())
}

pub fn revert(
    paths: &ProjectPaths,
    package: Option<String>,
    version: Option<String>,
) -> Result<()> {
    let (package, version) = match (package, version) {
        (Some(pkg), Some(ver)) => (pkg, ver),
        (None, Some(ver)) => (crate::config::root_config(paths)?.name.to_string(), ver),
        (Some(pkg), None) => {
            let query = format!("Which version of package {pkg} do you want to revert?");
            let ver = cli::ask(&query)?;
            (pkg, ver)
        }
        (None, None) => {
            // Only want to access root_config once rather than twice
            let config = crate::config::root_config(paths)?;
            (config.name.to_string(), config.version.to_string())
        }
    };

    let question = format!("Do you wish to revert {package} version {version}?");
    if !cli::confirm(&question)? {
        println!("Not reverting.");
        return Ok(());
    }

    let runtime = tokio::runtime::Runtime::new().expect("Unable to start Tokio async runtime");
    let hex_config = hexpm::Config::new();
    let api_key = HexAuthentication::new(&runtime, hex_config.clone()).get_or_create_api_key()?;
    let http = HttpClient::new();

    // Revert release from API
    let request = hexpm::api_revert_release_request(&package, &version, &api_key, &hex_config)
        .map_err(Error::hex)?;
    let response = runtime.block_on(http.send(request))?;
    hexpm::api_revert_release_response(response).map_err(Error::hex)?;

    // Done!
    println!("{package} {version} has been removed from Hex");
    Ok(())
}

pub(crate) fn authenticate() -> Result<()> {
    let runtime = tokio::runtime::Runtime::new().expect("Unable to start Tokio async runtime");
    let http = HttpClient::new();
    let config = hexpm::Config::new();
    let mut auth = HexAuthentication::new(&runtime, config.clone());
    let previous = auth.read_stored_api_key()?;

    if previous.is_some() {
        let question = "You already have a local Hex API token. Would you like to replace it
with a new one?";
        if !cli::confirm(question)? {
            return Ok(());
        }
    }

    let new_key = auth.create_and_store_api_key()?;

    if let Some(previous) = previous {
        println!("Deleting previous key `{}` from Hex", previous.name);
        runtime.block_on(hex::remove_api_key(
            &previous.name,
            &config,
            &new_key.unencrypted,
            &http,
        ))?;
    }
    Ok(())
}
