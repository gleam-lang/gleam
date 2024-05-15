use gleam_core::{
    hex::{self, RetirementReason},
    io::HttpClient as _,
    Error, Result,
};

use crate::{cli, http::HttpClient};

const USER_PROMPT: &str = "https://hex.pm username";
const USER_KEY: &str = "HEXPM_USER";
const PASS_PROMPT: &str = "https://hex.pm password";
const PASS_KEY: &str = "HEXPM_PASS";
const API_KEY: &str = "HEXPM_API_KEY";

/// A helper trait that handles the provisioning and destruction of a Hex API key.
pub trait ApiKeyCommand {
    fn with_api_key(
        &mut self,
        runtime: &tokio::runtime::Handle,
        hex_config: &hexpm::Config,
        api_key: &str,
    ) -> Result<()>;

    fn with_new_api_key(
        &mut self,
        runtime: &tokio::runtime::Runtime,
        hex_config: &hexpm::Config,
    ) -> Result<()> {
        let hostname = crate::publish::get_hostname();
        let http = HttpClient::new();

        // Get login creds from user
        let username = std::env::var(USER_KEY).or_else(|_| cli::ask(USER_PROMPT))?;
        let password = std::env::var(PASS_KEY).or_else(|_| cli::ask_password(PASS_PROMPT))?;

        // Get API key
        let api_key = runtime.block_on(hex::create_api_key(
            &hostname, &username, &password, hex_config, &http,
        ))?;

        // Perform the API operation but don't exit early if it fails, we want to always
        // remove the API key
        let result = self.with_api_key(runtime.handle(), hex_config, &api_key);

        // Ensure to remove the API key
        runtime.block_on(hex::remove_api_key(&hostname, hex_config, &api_key, &http))?;

        result
    }

    fn run(&mut self) -> Result<()> {
        let runtime = tokio::runtime::Runtime::new().expect("Unable to start Tokio async runtime");
        let hex_config = hexpm::Config::new();

        let api_key = std::env::var(API_KEY).unwrap_or_default().trim().to_owned();

        if api_key.is_empty() {
            self.with_new_api_key(&runtime, &hex_config)
        } else {
            self.with_api_key(runtime.handle(), &hex_config, &api_key)
        }
    }
}

pub struct RetireCommand {
    package: String,
    version: String,
    reason: RetirementReason,
    message: Option<String>,
}

impl RetireCommand {
    pub fn new(
        package: String,
        version: String,
        reason: RetirementReason,
        message: Option<String>,
    ) -> Self {
        Self {
            package,
            version,
            reason,
            message,
        }
    }
}

impl ApiKeyCommand for RetireCommand {
    fn with_api_key(
        &mut self,
        handle: &tokio::runtime::Handle,
        hex_config: &hexpm::Config,
        api_key: &str,
    ) -> Result<()> {
        handle.block_on(hex::retire_release(
            &self.package,
            &self.version,
            self.reason,
            self.message.as_deref(),
            api_key,
            hex_config,
            &HttpClient::new(),
        ))?;
        cli::print_retired(&self.package, &self.version);
        Ok(())
    }
}

pub struct UnretireCommand {
    package: String,
    version: String,
}

impl UnretireCommand {
    pub fn new(package: String, version: String) -> Self {
        Self { package, version }
    }
}

impl ApiKeyCommand for UnretireCommand {
    fn with_api_key(
        &mut self,
        handle: &tokio::runtime::Handle,
        hex_config: &hexpm::Config,
        api_key: &str,
    ) -> Result<()> {
        handle.block_on(hex::unretire_release(
            &self.package,
            &self.version,
            api_key,
            hex_config,
            &HttpClient::new(),
        ))?;
        cli::print_unretired(&self.package, &self.version);
        Ok(())
    }
}

pub struct RevertCommand {
    package: String,
    version: String,
}

pub fn revertcommand(package: Option<String>, version: Option<String>) -> Result<()> {
    RevertCommand::setup(package, version)?.run()?;

    Ok(())
}

impl RevertCommand {
    fn setup(package: Option<String>, version: Option<String>) -> Result<Self> {
        let (package, version): (String, String) = match (package, version) {
            (Some(pkg), Some(ver)) => (pkg, ver),
            (None, Some(ver)) => (crate::config::root_config()?.name.to_string(), ver),
            (Some(pkg), None) => {
                let query =
                    "Which version of package ".to_string() + &pkg + " do you want to revert?";
                let ver = cli::ask(&query)?;
                (pkg, ver)
            }
            (None, None) => {
                // Only want to access root_config once rather than twice
                let config = crate::config::root_config()?;

                (config.name.to_string(), config.version.to_string())
            }
        };

        let question =
            "Do you wish to revert ".to_string() + &package + " version " + &version + "?";
        let should_revert = cli::confirm(&question)?;
        if should_revert {
            Ok(Self { version, package })
        } else {
            println!("Not reverting.");
            std::process::exit(0);
        }
    }
}

impl ApiKeyCommand for RevertCommand {
    fn with_api_key(
        &mut self,
        handle: &tokio::runtime::Handle,
        hex_config: &hexpm::Config,
        api_key: &str,
    ) -> Result<()> {
        let http = HttpClient::new();

        // Revert release from API
        let request =
            hexpm::revert_release_request(&self.package, &self.version, api_key, hex_config)
                .map_err(Error::hex)?;
        let response = handle.block_on(http.send(request))?;
        hexpm::revert_release_response(response).map_err(Error::hex)?;

        // Done!
        println!(
            "{} {} has been removed from Hex",
            self.package, self.version
        );
        Ok(())
    }
}
