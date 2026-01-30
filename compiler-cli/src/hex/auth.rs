use crate::{cli, http::HttpClient};
use ecow::EcoString;
use gleam_core::{
    Error, Result, encryption,
    error::{FileIoAction, FileKind},
    io::HttpClient as _,
    paths,
};
use hexpm::OAuthTokens;

pub const HEX_OAUTH_CLIENT_ID: &str = "877731e8-cb88-45e1-9b84-9214de7da421";

pub const LOCAL_PASS_PROMPT: &str = "Local password";
pub const API_ENV_NAME: &str = "HEXPM_API_KEY";

#[derive(Debug)]
pub struct EncryptedApiKey {
    pub name: String,
    pub encrypted: String,
}

pub struct HexAuthentication<'runtime> {
    runtime: &'runtime tokio::runtime::Runtime,
    http: HttpClient,
    local_password: Option<EcoString>,
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

    /// Create new OAuth tokens by sending the user through the OAuth flow.
    ///
    /// Any already-existing tokens stored on the file system are revoked.
    ///
    pub fn create_and_store_new_oauth_tokens(&mut self) -> Result<OAuthTokens> {
        // TODO: revoke any prior oauth tokens
        // TODO: revoke any prior old api keys

        // Create a recognisable name for the client, so folks can more easily understand which
        // session is which in the Hex console.
        let client_name = format!(
            "Gleam ({})",
            hostname::get().expect("hostname").to_string_lossy()
        );

        // Create a device authorisation with HEx, starting the oauth flow.
        let request = hexpm::oauth_device_authorisation_request(
            HEX_OAUTH_CLIENT_ID,
            &client_name,
            &self.hex_config,
        );
        let response = self.runtime.block_on(self.http.send(request))?;
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
            let request = device_authorisation.poll_token_request(&self.hex_config);
            let response = self.runtime.block_on(self.http.send(request))?;
            let next = device_authorisation
                .poll_token_response(response)
                .map_err(Error::hex)?;
            match next {
                hexpm::PollStep::Done(tokens) => break tokens,
                hexpm::PollStep::SleepThenPollAgain(duration) => std::thread::sleep(duration),
            }
        };

        // We are creating a new API key, so we need a new local password
        // to encrypt it with.
        self.ask_for_new_local_password()?;
        self.encrypt_and_store_oauth_refresh_token(&tokens)?;

        Ok(tokens)
    }

    fn encrypt_and_store_oauth_refresh_token(&mut self, tokens: &OAuthTokens) -> Result<(), Error> {
        let path = paths::global_hexpm_oauth_credentials_path();
        let local_password = self.get_local_password()?;
        let encrypted_refresh_token =
            encryption::encrypt_with_passphrase(tokens.refresh_token.as_bytes(), &local_password)
                .map_err(|e| Error::FailedToEncryptLocalHexApiKey {
                detail: e.to_string(),
            })?;
        let credentials = StoredOAuthCredentials {
            hexpm: StoredOAuthRepoCredentials {
                api: self.hex_config.api_base.clone(),
                repository: self.hex_config.repository_base.clone(),
                refresh_token: encrypted_refresh_token,
            },
        };
        let toml = toml::to_string(&credentials).expect("OAuth credentials TOML encoding");
        crate::fs::write(&path, &toml)?;
        Ok(())
    }

    /// Create a new local password.
    ///
    /// The password must be long enough.
    ///
    /// The old password will be discarded, and the new one will be both
    /// returned and stored in `self.local_password`
    ///
    fn ask_for_new_local_password(&mut self) -> Result<()> {
        let required_length = 8;
        self.local_password = None;
        println!(
            "Please enter a new unique password, at least {required_length} characters long.
It will be used to locally encrypt your Hex API tokens.
"
        );

        loop {
            let password = cli::ask_password(LOCAL_PASS_PROMPT)?;
            if password.chars().count() < required_length {
                println!("\nPlease use a password at least {required_length} characters long.\n")
            } else {
                self.local_password = Some(password.clone());
                return Ok(());
            }
        }
    }

    fn get_local_password(&mut self) -> Result<EcoString> {
        if let Some(password) = self.local_password.as_ref() {
            return Ok(password.clone());
        }

        let password = cli::ask_password(LOCAL_PASS_PROMPT)?;
        self.local_password = Some(password.clone());
        return Ok(password);
    }

    /// Get a token that can be used to authenticate with the Hex API.
    /// In order, it will try these sources:
    ///
    /// 1. An API key from the HEXPM_API_KEY environment variable.
    /// 2. An OAuth refresh token from the file system, which is the exchanged for an access token.
    /// 3. The OAuth flow.
    pub fn get_or_create_api_access_token(&mut self) -> Result<String> {
        if let Some(key) = Self::read_env_api_key()? {
            return Ok(key);
        }

        if let Some(tokens) = self.read_and_decrypt_and_refresh_stored_tokens()? {
            return Ok(tokens.access_token);
        }

        Ok(self.create_and_store_new_oauth_tokens()?.access_token)
    }

    fn read_env_api_key() -> Result<Option<String>> {
        let api_key = std::env::var(API_ENV_NAME).unwrap_or_default();
        if api_key.trim().is_empty() {
            Ok(None)
        } else {
            Ok(Some(api_key))
        }
    }

    /// Read, decrypt, and refresh OAuth keys stored on the filesystem.
    ///
    /// The new refresh is encrypted and stored on the file system for next use.
    ///
    /// If there is no stored key then this return `Ok(None)`, and you'll
    /// need to create a new one.
    ///
    fn read_and_decrypt_and_refresh_stored_tokens(&mut self) -> Result<Option<OAuthTokens>> {
        let Some(EncryptedApiKey { encrypted, .. }) = self.read_stored_legacy_api_key()? else {
            // There was no prior token to use, return None.
            return Ok(None);
        };

        let local_password = self.get_local_password()?;
        let refresh_token =
            encryption::decrypt_with_passphrase(encrypted.as_bytes(), &local_password).map_err(
                |e| Error::FailedToDecryptLocalHexApiKey {
                    detail: e.to_string(),
                },
            )?;

        // Use a refresh token, consuming it to get a new set of tokens.
        let request = hexpm::oauth_refresh_token_request(
            HEX_OAUTH_CLIENT_ID,
            &refresh_token,
            &self.hex_config,
        );
        let response = self.runtime.block_on(self.http.send(request))?;
        let tokens = hexpm::oauth_refresh_token_response(response).map_err(Error::hex)?;

        // Store the refresh token for future use.
        self.encrypt_and_store_oauth_refresh_token(&tokens)?;

        Ok(Some(tokens))
    }

    pub fn read_stored_encrypted_oauth_refresh_token(&self) -> Result<Option<String>> {
        let path = paths::global_hexpm_legacy_credentials_path();
        if !path.exists() {
            return Ok(None);
        }
        let toml = crate::fs::read(&path)?;
        let credentials: StoredOAuthCredentials =
            toml::from_str(&toml).map_err(|e| Error::FileIo {
                action: FileIoAction::Parse,
                kind: FileKind::File,
                path,
                err: Some(e.to_string()),
            })?;
        Ok(Some(credentials.hexpm.refresh_token))
    }

    pub fn read_stored_legacy_api_key(&self) -> Result<Option<EncryptedApiKey>> {
        let path = paths::global_hexpm_legacy_credentials_path();
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

#[derive(Debug, serde::Serialize, serde::Deserialize)]
struct StoredOAuthRepoCredentials {
    #[serde(with = "http_serde::uri")]
    api: http::Uri,
    #[serde(with = "http_serde::uri")]
    repository: http::Uri,
    refresh_token: String,
}

#[derive(Debug, serde::Serialize, serde::Deserialize)]
struct StoredOAuthCredentials {
    hexpm: StoredOAuthRepoCredentials,
}
