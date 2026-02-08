mod auth;

use crate::{cli, http::HttpClient};
use gleam_core::{
    Error, Result,
    hex::{self, RetirementReason},
    io::HttpClient as _,
    paths::ProjectPaths,
};

pub use auth::HexAuthentication;

/// Prepare credentials for user for write actions.
/// This will prompt for a one-time-password if needed.
pub fn write_credentials(
    credentials: &hexpm::Credentials,
) -> Result<hexpm::WriteActionCredentials> {
    match credentials {
        hexpm::Credentials::ApiKey(key) => Ok(hexpm::WriteActionCredentials::ApiKey(key.clone())),
        hexpm::Credentials::OAuthAccessToken(token) => {
            let one_time_password = cli::ask("Enter your MFA code")?.into();
            Ok(hexpm::WriteActionCredentials::OAuthAccessToken {
                access_token: token.clone(),
                one_time_password,
            })
        }
    }
}

pub fn retire(
    package: String,
    version: String,
    reason: RetirementReason,
    message: Option<String>,
) -> Result<()> {
    let runtime = tokio::runtime::Runtime::new().expect("Unable to start Tokio async runtime");
    let config = hexpm::Config::new();
    let http = HttpClient::new();
    let credentials =
        HexAuthentication::new(&runtime, &http, config.clone()).get_or_create_api_credentials()?;

    runtime.block_on(hex::retire_release(
        &package,
        &version,
        reason,
        message.as_deref(),
        &write_credentials(&credentials)?,
        &config,
        &http,
    ))?;
    cli::print_retired(&package, &version);
    Ok(())
}

pub fn unretire(package: String, version: String) -> Result<()> {
    let runtime = tokio::runtime::Runtime::new().expect("Unable to start Tokio async runtime");
    let http = HttpClient::new();
    let config = hexpm::Config::new();
    let credentials =
        HexAuthentication::new(&runtime, &http, config.clone()).get_or_create_api_credentials()?;

    runtime.block_on(hex::unretire_release(
        &package,
        &version,
        &write_credentials(&credentials)?,
        &config,
        &http,
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
    let http = HttpClient::new();
    let credentials = HexAuthentication::new(&runtime, &http, hex_config.clone())
        .get_or_create_api_credentials()?;

    // Revert release from API
    let request = hexpm::api_revert_release_request(
        &package,
        &version,
        &write_credentials(&credentials)?,
        &hex_config,
    )
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
    let credentials = HexAuthentication::new(&runtime, &http, config.clone())
        .create_and_store_new_credentials_via_oauth()?;

    let request = hexpm::get_me_request(&credentials, &config);
    let response = runtime.block_on(http.send(request))?;
    let me = hexpm::get_me_response(response).map_err(Error::hex)?;
    println!("\nSuccessfully logged in as {}", me.username);

    Ok(())
}
