use crate::{cli, http::HttpClient};
use gleam_core::{encryption, hex, paths::global_hexpm_credentials_path, Error, Result};
use std::time::SystemTime;

pub const USER_PROMPT: &str = "https://hex.pm username";
pub const USER_ENV_NAME: &str = "HEXPM_USER";
pub const PASS_PROMPT: &str = "https://hex.pm password";
pub const LOCAL_PASS_PROMPT: &str = "Local password";
pub const PASS_ENV_NAME: &str = "HEXPM_PASS";
pub const API_ENV_NAME: &str = "HEXPM_API_KEY";

#[derive(Debug)]
pub struct EncryptedApiKey {
    pub name: String,
    pub encrypted: String,
}

#[derive(Debug)]
pub struct UnencryptedApiKey {
    pub unencrypted: String,
}

pub struct HexAuthentication<'runtime> {
    runtime: &'runtime tokio::runtime::Runtime,
    http: HttpClient,
    local_password: Option<String>,
    hex_config: hexpm::Config,
}

impl<'runtime> HexAuthentication<'runtime> {
    /// Reads the stored API key from disc, if it exists.
    ///
    pub fn new(runtime: &'runtime tokio::runtime::Runtime, hex_config: hexpm::Config) -> Self {
        Self {
            runtime,
            http: HttpClient::new(),
            local_password: None,
            hex_config,
        }
    }

    /// Create a new API key, removing the previous one if it already exists.
    ///
    pub fn create_and_store_api_key(&mut self) -> Result<UnencryptedApiKey> {
        let name = generate_api_key_name();
        let path = global_hexpm_credentials_path();

        // Get login creds from user
        let username = ask_username()?;
        let password = ask_password()?;

        // Get API key
        let future = hex::create_api_key(&name, &username, &password, &self.hex_config, &self.http);
        let api_key = self.runtime.block_on(future)?;

        if self.local_password.is_none() {
            println!(
                "
Please enter a new unique password. This will be used to locally
encrypt your Hex API key.
"
            );
        }
        let password = self.ask_local_password()?;
        let encrypted = encryption::encrypt_with_passphrase(api_key.as_bytes(), &password)?;

        crate::fs::write(&path, &format!("{name}\n{encrypted}"))?;
        println!("Encrypted Hex API key written to {path}");

        Ok(UnencryptedApiKey {
            unencrypted: api_key,
        })
    }

    fn ask_local_password(&mut self) -> Result<String> {
        if let Some(pw) = self.local_password.as_ref() {
            return Ok(pw.clone());
        }
        let pw = ask_local_password()?;
        self.local_password = Some(pw.clone());
        Ok(pw)
    }

    /// Get an API key from
    /// 1. the HEXPM_API_KEY env var
    /// 2. the file system (encrypted)
    /// 3. the Hex API
    pub fn get_or_create_api_key(&mut self) -> Result<String> {
        if let Some(key) = Self::read_env_api_key()? {
            return Ok(key);
        }

        if let Some(key) = self.read_and_decrypt_stored_api_key()? {
            return Ok(key.unencrypted);
        }

        Ok(self.create_and_store_api_key()?.unencrypted)
    }

    fn read_env_api_key() -> Result<Option<String>> {
        let api_key = std::env::var(API_ENV_NAME).unwrap_or_default();
        if api_key.trim().is_empty() {
            Ok(None)
        } else {
            Ok(Some(api_key))
        }
    }

    fn read_and_decrypt_stored_api_key(&mut self) -> Result<Option<UnencryptedApiKey>> {
        let Some(EncryptedApiKey { encrypted, .. }) = self.read_stored_api_key()? else {
            return Ok(None);
        };
        let password = self.ask_local_password()?;
        let unencrypted = encryption::decrypt_with_passphrase(encrypted.as_bytes(), &password)?;
        Ok(Some(UnencryptedApiKey { unencrypted }))
    }

    pub fn read_stored_api_key(&self) -> Result<Option<EncryptedApiKey>> {
        let path = global_hexpm_credentials_path();
        if !path.exists() {
            return Ok(None);
        }
        let text = crate::fs::read(&path)?;
        let mut chunks = text.splitn(2, '\n');
        let Some(name) = chunks.next() else {
            return Ok(None);
        };
        let Some(encrypted) = chunks.next() else {
            return Ok(None);
        };
        Ok(Some(EncryptedApiKey {
            name: name.to_string(),
            encrypted: encrypted.to_string(),
        }))
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
