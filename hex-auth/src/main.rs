// ## Auth flow
//
// - [ ] Revoke any already provisioned oauth tokens using the Hex API
// - [ ] Revoke any old format API keys
// - [x] Create a device authorization https://github.com/hexpm/hex/blob/main/src/mix_hex_api_oauth.erl#L52
// - [x] Take details from device response https://github.com/hexpm/hex/blob/main/lib/mix/tasks/hex.ex#L157
// - [x] Display the URL and the verification code to the user
// - [x] Open the URL in the browser (ask for confirmation first?)
// - [ ] Poll Hex for a token https://github.com/hexpm/hex/blob/main/lib/mix/tasks/hex.ex#L190
//   - [ ] 200 - OK got token
//   - [ ] Too much time: timed out, tell user to try again, exit. How long should timeout be?
//   - [ ] 400 {error: slow_down} - increase delay to `min(attempt_number * 2, 30)` seconds
//   - [ ] 400 {error: expired_token} - tell user, tell to try again, exit.
//   - [ ] 403 {error: access_denied} - tell user, tell to try again, exit.
//   - [ ] other error - tell user, tell to try again, exit.
// - [ ] Store the token. Mix stores it unencrypted https://github.com/hexpm/hex/blob/main/lib/mix/tasks/hex.ex#L460
//          $ cat .hex/hex.config
//          {'$repos',#{<<"hexpm">> =>
//                          #{auth_key => <<"REDACTED">>,
//                            url => <<"https://repo.hex.pm">>},
//                      <<"hexpm:gleam">> =>
//                          #{auth_key => <<"REDACTED">>}}}.
//          {'$oauth_token',#{<<"access_token">> =>
//                                <<"REDACTED">>,
//                            <<"expires_at">> => 1769254194,
//                            <<"refresh_token">> =>
//                                <<"REDACTED">>}}.
// - [ ] Get an initial oauth token to actually use it https://github.com/hexpm/hex/blob/main/lib/mix/tasks/hex.ex#L460
// - [ ] Something to do with one-time-passwords (enforced for write access)
//
// @doc false
// def auth_device(_opts \\ []) do
//   # Clean up any existing authentication
//   revoke_existing_oauth_tokens()
//   revoke_and_cleanup_old_api_keys()
//
//   name = get_hostname()
//
//   case Hex.API.OAuth.device_authorization("api repositories", name) do
//     {:ok, {200, _, device_response}} ->
//       perform_device_flow(device_response)
//
//     {:ok, {status, _, error}} ->
//       Hex.Shell.error("Device authorization failed (#{status}): #{inspect(error)}")
//       :error
//
//     {:error, reason} ->
//       Hex.Shell.error("Device authorization error: #{inspect(reason)}")
//       :error
//   end
// end

use std::time::{Duration, Instant};

use serde::Deserialize;
use serde_json::json;

static USER_AGENT: &str = concat!("Gleam build tool ", env!("CARGO_PKG_VERSION"));

pub fn main() {
    let http_client = reqwest::blocking::Client::new();
    let DeviceAuthorisation {
        poll_interval_seconds,
        device_code,
        user_code,
        verification_uri,
        verification_uri_complete,
    } = http_client
        .post("https://hex.pm/api/oauth/device_authorization")
        .header("user-agent", USER_AGENT)
        .json(&json!({
            "client_id": "877731e8-cb88-45e1-9b84-9214de7da421",
            "scope": "api:write",
            "name": "The Gleam build tool"
        }))
        .send()
        .unwrap()
        .error_for_status()
        .unwrap()
        .json()
        .unwrap();

    let uri = verification_uri_complete.unwrap_or(verification_uri);
    // TODO: format the user code nicely

    println!(
        "Your Hex verification code:

    {user_code}

Verify this code matches what is shown in your browser.

Press Enter to open {uri}",
    );

    let _ = std::io::stdin().read_line(&mut String::new()).unwrap();

    if let Err(_) = opener::open_browser(&uri) {
        println!("\nFailed to open the browser, please navigate to {uri}");
    }

    let mut attempt = 0;
    let mut interval = Duration::from_secs(poll_interval_seconds);
    let start = Instant::now();

    let (access_token, refresh_token) = loop {
        if start.elapsed() > Duration::from_mins(10) {
            // TODO: return error
            todo!("timeout")
        }

        std::thread::sleep(interval);
        attempt = attempt + 1;

        let text = http_client
            .post("https://hex.pm/api/oauth/token")
            .header("user-agent", USER_AGENT)
            .json(&json!({
                "client_id": "877731e8-cb88-45e1-9b84-9214de7da421",
                "grant_type": "urn:ietf:params:oauth:grant-type:device_code",
                "device_code": device_code,
            }))
            .send()
            .unwrap()
            .text()
            .unwrap();
        println!("{text}");

        let response: PollOutcome = http_client
            .post("https://hex.pm/api/oauth/token")
            .header("user-agent", USER_AGENT)
            .json(&json!({
                "client_id": "877731e8-cb88-45e1-9b84-9214de7da421",
                "grant_type": "urn:ietf:params:oauth:grant-type:device_code",
                "device_code": device_code,
            }))
            .send()
            .unwrap()
            .json()
            .unwrap();

        dbg!(&response);

        let error = match response {
            PollOutcome::Success {
                access_token,
                refresh_token,
            } => break (access_token, refresh_token),
            PollOutcome::Fail { error } => error,
        };

        match error {
            PollError::AuthorizationPending => (),
            PollError::SlowDown => {
                interval = interval.saturating_mul(2).min(Duration::from_secs(30));
            }
            // TODO: return error
            PollError::AccessDenied => todo!("access denied error"),
            // TODO: return error
            PollError::ExpiredToken => todo!("access denied error"),
        }
    };
}

#[derive(Debug, Deserialize)]
pub struct DeviceAuthorisation {
    #[serde(default = "default_poll_interval_seconds", rename = "interval")]
    poll_interval_seconds: u64,
    device_code: String,
    user_code: String,
    verification_uri: String,
    verification_uri_complete: Option<String>,
}

#[derive(Debug, Deserialize)]
#[serde(untagged)]
pub enum PollOutcome {
    Success {
        access_token: String,
        refresh_token: String,
    },
    Fail {
        error: PollError,
    },
}

#[derive(Debug, Deserialize)]
pub enum PollError {
    #[serde(rename = "authorization_pending")]
    AuthorizationPending,
    #[serde(rename = "slow_down")]
    SlowDown,
    #[serde(rename = "access_denied")]
    AccessDenied,
    #[serde(rename = "expired_token")]
    ExpiredToken,
}

fn default_poll_interval_seconds() -> u64 {
    5
}
