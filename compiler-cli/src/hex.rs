use gleam_core::{
    hex::{self, RetirementReason},
    Result,
};

use crate::{cli, http::HttpClient};

/// A helper trait that handles the provisioning and destruction of a Hex API key.
pub trait ApiKeyCommand {
    const USER_PROMPT: &'static str = "https://hex.pm username";
    const USER_KEY: &'static str = "HEXPM_USER";

    const PASS_PROMPT: &'static str = "https://hex.pm password";
    const PASS_KEY: &'static str = "HEXPM_PASS";

    fn with_api_key(
        &mut self,
        runtime: &tokio::runtime::Handle,
        hex_config: &hexpm::Config,
        api_key: &str,
    ) -> Result<()>;

    fn run(&mut self) -> Result<()> {
        let runtime = tokio::runtime::Runtime::new().expect("Unable to start Tokio async runtime");
        let hostname = crate::publish::get_hostname();
        let hex_config = hexpm::Config::new();
        let http = HttpClient::new();

        // Get login creds from user

        let username = std::env::var(Self::USER_KEY).or_else(|_| cli::ask(Self::USER_PROMPT))?;
        let password =
            std::env::var(Self::PASS_KEY).or_else(|_| cli::ask_password(Self::PASS_PROMPT))?;

        // Get API key
        let api_key = runtime.block_on(gleam_core::hex::create_api_key(
            &hostname,
            &username,
            &password,
            &hex_config,
            &http,
        ))?;

        // Perform the API operation but don't exit early if it fails, we want to always
        // remove the API key
        let result = self.with_api_key(runtime.handle(), &hex_config, &api_key);

        // Ensure to remove the API key
        runtime.block_on(gleam_core::hex::remove_api_key(
            &hostname,
            &hex_config,
            &api_key,
            &http,
        ))?;

        result
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
