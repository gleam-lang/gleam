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

pub(crate) fn oauth_authenticate() -> Result<()> {
    const HEX_OAUTH_CLIENT_ID: &str = "877731e8-cb88-45e1-9b84-9214de7da421";

    let config = hexpm::Config::new();
    let runtime = tokio::runtime::Runtime::new().expect("Unable to start Tokio async runtime");
    let http = HttpClient::new();

    // Create a device authorisation with HEx, starting the oauth flow.
    let request = hexpm::oauth_device_authorisation_request(HEX_OAUTH_CLIENT_ID, &config);
    let response = runtime.block_on(http.send(request))?;
    let mut device_authorisation =
        hexpm::oauth_device_authorisation_response(HEX_OAUTH_CLIENT_ID.to_string(), response)
            .map_err(Error::hex)?;

    // Show the user their code, and send them to Hex to log in.
    // We make them press Enter to open the browser instead of going there immediately,
    // to make sure they see the code before the browser window potentially hides the
    // terminal window.
    let uri = &device_authorisation.verification_uri;
    println!(
        "Your Hex verification code:

    {code}

Verify this code matches what is shown in your browser.

Press Enter to open {uri}",
        code = device_authorisation.user_code,
    );

    let _ = std::io::stdin().read_line(&mut String::new()).unwrap();
    if let Err(_) = opener::open_browser(uri) {
        println!("\nFailed to open the browser, please navigate to {uri}");
    }

    // The user has been sent to the Hex website to authenticate.
    // Poll the Hex API until they accept or reject the request, or it times out.
    let tokens = loop {
        let request = device_authorisation.poll_token_request(&config);
        let response = runtime.block_on(http.send(request))?;
        let next = device_authorisation
            .poll_token_response(response)
            .map_err(Error::hex)?;
        match next {
            hexpm::PollStep::Done(tokens) => break tokens,
            hexpm::PollStep::SleepThenPollAgain(duration) => std::thread::sleep(duration),
        }
    };

    dbg!(tokens);

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
