use gleam_core::{
    hex::{self, RetirementReason},
    Result,
};

use crate::{cli, http::HttpClient};

const USER_PROMPT: &str = "https://hex.pm username";
const USER_KEY: &str = "HEXPM_USER";
const PASS_PROMPT: &str = "https://hex.pm password";
const PASS_KEY: &str = "HEXPM_PASS";
const API_KEY_PROMPT: &str = "https://hex.pm API key";
const API_KEY_KEY: &str = "HEXPM_API_KEY";

/// A helper trait that handles the provisioning and destruction of a Hex API key.
pub trait ApiKeyCommand {
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

        // Get login creds from user without prompting, to check
        // if we're using username and password, or an API key.
        let username = std::env::var(USER_KEY).unwrap_or("".into());
        let password = std::env::var(PASS_KEY).unwrap_or("".into());
        let api_key = std::env::var(API_KEY_KEY).unwrap_or("".into());

        let insufficient_creds_in_env_var = api_key.trim().is_empty() && (username.trim().is_empty() || password.trim().is_empty());

        // Get login creds from user, prompting this time if
        // insufficient creds could be sourced from the env vars.

        // Prefer the API key, which is necessarily empty, so just ask for it.
        let api_key = if insufficient_creds_in_env_var {
            cli::ask_password(API_KEY_PROMPT)?
        } else {
            api_key
        };
        let no_api_key_supplied = api_key.trim().is_empty();

        // If no creds were sourced from env vars or an API key was
        // not entered, then we prompt for the username and password
        let username = if insufficient_creds_in_env_var && no_api_key_supplied {
            std::env::var(USER_KEY).or_else(|_| cli::ask(USER_PROMPT))?
        } else {
            username
        };
        let password = if insufficient_creds_in_env_var && no_api_key_supplied {
            std::env::var(PASS_KEY).or_else(|_| cli::ask_password(PASS_PROMPT))?
        } else {
            password
        };

        let api_key = if no_api_key_supplied {
            // Create and manage a short lived API key
            runtime.block_on(gleam_core::hex::create_api_key(
                &hostname,
                &username,
                &password,
                &hex_config,
                &http,
            ))?
        } else {
            api_key
        };

        // Perform the API operation but don't exit early if it fails, we want to always
        // check if we should remove the API key
        let result = self.with_api_key(runtime.handle(), &hex_config, &api_key);

        if no_api_key_supplied {
            // Ensure to remove the API key
            runtime.block_on(gleam_core::hex::remove_api_key(
                &hostname,
                &hex_config,
                &api_key,
                &http,
            ))?;
        }

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
