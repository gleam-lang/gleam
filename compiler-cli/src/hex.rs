use gleam_core::{
    hex::{self, RetirementReason},
    Result,
};

use crate::{cli, http::HttpClient};

pub fn retire(
    package: String,
    version: String,
    reason: RetirementReason,
    message: Option<String>,
) -> Result<()> {
    let runtime = tokio::runtime::Runtime::new().expect("Unable to start Tokio async runtime");
    let hostname = crate::publish::get_hostname();
    let hex_config = hexpm::Config::new();
    let http = HttpClient::new();

    // Get login creds from user
    let username = cli::ask("https://hex.pm username")?;
    let password = cli::ask_password("https://hex.pm password")?;

    // Publish the package
    let result = runtime.block_on(perform_retire(
        &package,
        &version,
        reason,
        message.as_deref(),
        &hostname,
        &username,
        &password,
        &hex_config,
        &http,
    ));

    // The result from the operation is handled after key deletion to ensure that
    // we remove it even in the case of an error
    result?;

    cli::print_retired(&package, &version);

    Ok(())
}

async fn perform_retire(
    package: &str,
    version: &str,
    reason: RetirementReason,
    message: Option<&str>,
    hostname: &str,
    username: &str,
    password: &str,
    hex_config: &hexpm::Config,
    http: &HttpClient,
) -> Result<()> {
    let api_key =
        gleam_core::hex::create_api_key(&hostname, &username, &password, &hex_config, http).await?;

    // Perform the API operation but don't exit early if it fails, we want to always
    // remove the API key
    let result = hex::retire_release(
        package, version, reason, message, &api_key, hex_config, http,
    )
    .await;

    // Ensure to remove the API key
    gleam_core::hex::remove_api_key(&hostname, &hex_config, &api_key, http).await?;

    result
}

pub fn unretire(package: String, version: String) -> Result<()> {
    let runtime = tokio::runtime::Runtime::new().expect("Unable to start Tokio async runtime");
    let hostname = crate::publish::get_hostname();
    let hex_config = hexpm::Config::new();
    let http = HttpClient::new();

    // Get login creds from user
    let username = cli::ask("https://hex.pm username")?;
    let password = cli::ask_password("https://hex.pm password")?;

    // Publish the package
    let result = runtime.block_on(perform_unretire(
        &package,
        &version,
        &hostname,
        &username,
        &password,
        &hex_config,
        &http,
    ));

    // The result from the operation is handled after key deletion to ensure that
    // we remove it even in the case of an error
    result?;

    cli::print_unretired(&package, &version);

    Ok(())
}

async fn perform_unretire(
    package: &str,
    version: &str,
    hostname: &str,
    username: &str,
    password: &str,
    hex_config: &hexpm::Config,
    http: &HttpClient,
) -> Result<()> {
    let api_key =
        gleam_core::hex::create_api_key(&hostname, &username, &password, &hex_config, http).await?;

    // Perform the API operation but don't exit early if it fails, we want to always
    // remove the API key
    let result = hex::unretire_release(package, version, &api_key, hex_config, http).await;

    // Ensure to remove the API key
    gleam_core::hex::remove_api_key(&hostname, &hex_config, &api_key, http).await?;

    result
}
