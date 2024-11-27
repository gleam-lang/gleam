use crate::{cli, http::HttpClient};
use camino::Utf8PathBuf;
use gleam_core::{
    encryption,
    hex::{self, RetirementReason},
    io::HttpClient as _,
    paths::global_hexpm_credentials_path,
    Error, Result,
};
use std::time::SystemTime;

const USER_PROMPT: &str = "https://hex.pm username";
const USER_ENV_NAME: &str = "HEXPM_USER";
const PASS_PROMPT: &str = "https://hex.pm password";
const LOCAL_PASS_PROMPT: &str = "Local password";
const PASS_ENV_NAME: &str = "HEXPM_PASS";
const API_ENV_NAME: &str = "HEXPM_API_KEY";

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
        let hostname = generate_api_key_name();
        let http = HttpClient::new();

        // Get login creds from user
        let username = std::env::var(USER_ENV_NAME).or_else(|_| cli::ask(USER_PROMPT))?;
        let password = std::env::var(PASS_ENV_NAME).or_else(|_| cli::ask_password(PASS_PROMPT))?;

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

        let api_key = std::env::var(API_ENV_NAME)
            .unwrap_or_default()
            .trim()
            .to_owned();

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

pub(crate) fn authenticate() -> Result<()> {
    let mut auth = HexAuthentication::new();

    if auth.has_stored_api_key() {
        let question = "You already have a local Hex API token. Would you
like to replace it with a new one?";
        if !cli::confirm(question)? {
            return Ok(());
        }
    }

    _ = auth.create_and_store_api_key()?;
    Ok(())
}

#[derive(Debug)]
pub struct EncryptedApiKey {
    name: String,
    encrypted: String,
}

#[derive(Debug)]
pub struct UnencryptedApiKey {
    name: String,
    unencrypted: String,
}

pub struct HexAuthentication {
    runtime: tokio::runtime::Runtime,
    http: HttpClient,
    stored_api_key_path: Utf8PathBuf,
}

impl HexAuthentication {
    /// Reads the stored API key from disc, if it exists.
    ///
    pub fn new() -> Self {
        Self {
            runtime: tokio::runtime::Runtime::new().expect("Unable to start Tokio async runtime"),
            http: HttpClient::new(),
            stored_api_key_path: global_hexpm_credentials_path(),
        }
    }

    /// Create a new API key, removing the previous one if it already exists.
    ///
    pub fn create_and_store_api_key(&mut self) -> Result<UnencryptedApiKey> {
        if self.stored_api_key_path.exists() {
            self.remove_stored_api_key()?;
        }

        let name = generate_api_key_name();
        let hex_config = hexpm::Config::new();

        // Get login creds from user
        let username = ask_username()?;
        let password = ask_password()?;

        // Get API key
        let future = hex::create_api_key(&name, &username, &password, &hex_config, &self.http);
        let api_key = self.runtime.block_on(future)?;

        println!(
            "
Please enter a new unique password. This will be used to locally
encrypt your Hex API key.
"
        );
        let password = ask_local_password()?;
        let encrypted = encryption::encrypt_with_passphrase(api_key.as_bytes(), &password)?;

        crate::fs::write(&self.stored_api_key_path, &format!("{name}\n{encrypted}"))?;
        println!(
            "Encrypted Hex API key written to {path}",
            path = self.stored_api_key_path
        );

        Ok(UnencryptedApiKey {
            name,
            unencrypted: api_key,
        })
    }

    pub fn has_stored_api_key(&self) -> bool {
        self.stored_api_key_path.exists()
    }

    /// Get an API key from
    /// 1. the HEXPM_API_KEY env var
    /// 2. the file system (encrypted)
    /// 3. the Hex API
    pub fn get_or_create_api_key(&mut self) -> Result<String> {
        if let Some(key) = Self::load_env_api_key()? {
            return Ok(key);
        }

        if let Some(key) = Self::load_stored_api_key()? {
            return Ok(key);
        }

        Ok(self.create_and_store_api_key()?.unencrypted)
    }

    fn load_env_api_key() -> Result<Option<String>> {
        let api_key = std::env::var(API_ENV_NAME).unwrap_or_default();
        if api_key.trim().is_empty() {
            return Ok(None);
        } else {
            Ok(Some(api_key))
        }
    }

    fn load_stored_api_key() -> Result<Option<String>> {
        let Some(EncryptedApiKey { encrypted, .. }) = Self::read_stored_api_key()? else {
            return Ok(None);
        };
        let password = ask_local_password()?;
        let key = encryption::decrypt_with_passphrase(encrypted.as_bytes(), &password)?;
        Ok(Some(key))
    }

    fn read_stored_api_key() -> Result<Option<EncryptedApiKey>> {
        let path = global_hexpm_credentials_path();
        if !path.exists() {
            return Ok(None);
        }
        let text = crate::fs::read(&path)?;
        let mut chunks = text.splitn(2, '\n');
        let name = chunks.next().unwrap().to_string();
        let encrypted = chunks.next().unwrap().to_string();
        Ok(Some(EncryptedApiKey { name, encrypted }))
    }

    fn remove_stored_api_key(&mut self) -> Result<()> {
        todo!()
    }
}

fn ask_local_password() -> std::result::Result<String, Error> {
    std::env::var(PASS_ENV_NAME).or_else(|_| cli::ask_password(LOCAL_PASS_PROMPT))
}

fn ask_password() -> std::result::Result<String, Error> {
    std::env::var(PASS_ENV_NAME).or_else(|_| cli::ask_password(PASS_PROMPT))
}

fn ask_username() -> std::result::Result<String, Error> {
    std::env::var(USER_ENV_NAME).or_else(|_| cli::ask(USER_PROMPT))
}

// TODO: move to authenticator
pub fn generate_api_key_name() -> String {
    let timestamp = SystemTime::now()
        .duration_since(SystemTime::UNIX_EPOCH)
        .expect("This function must only be called after January 1, 1970. Sorry time traveller!")
        .as_secs();
    let name = hostname::get()
        .expect("Looking up hostname")
        .to_string_lossy()
        .to_string();
    format!("{name}-{timestamp}")
}
