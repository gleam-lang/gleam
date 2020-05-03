use crate::error::{Error, GleamExpect, StandardIOAction};
use hexpm::Client;
use std::io::Write;

static TOKEN_NAME: &str = concat!(env!("CARGO_PKG_NAME"), " (", env!("CARGO_PKG_VERSION"), ")");

pub fn revoke(package: String, version: String) -> Result<(), Error> {
    let username = ask("https://hex.pm username")?;
    let password = ask_password("https://hex.pm password")?;

    let mut runtime =
        tokio::runtime::Runtime::new().gleam_expect("Unable to start Tokio async runtime");

    runtime.block_on(async {
        hexpm::UnauthenticatedClient::new()
            .authenticate(username.as_str(), password.as_str(), TOKEN_NAME)
            .await
            .map_err(|_| todo!())?
            .remove_docs(package.as_str(), version.as_str())
            .await
            .map_err(|_| todo!())
    })?;

    println!(
        "The docs for {} {} have been removed from HexDocs",
        package, version
    );
    Ok(())
}

pub fn ask(question: &str) -> Result<String, Error> {
    print!("{}: ", question);
    std::io::stdout().flush().unwrap();
    let mut answer = String::new();
    std::io::stdin()
        .read_line(&mut answer)
        .map_err(|e| Error::StandardIO {
            action: StandardIOAction::Read,
            err: Some(e.kind()),
        })?;
    Ok(answer.trim().to_string())
}

pub fn ask_password(question: &str) -> Result<String, Error> {
    let prompt = format!("{} (will not be printed as you type): ", question);
    rpassword::read_password_from_tty(Some(prompt.as_str()))
        .map_err(|e| Error::StandardIO {
            action: StandardIOAction::Read,
            err: Some(e.kind()),
        })
        .map(|s| s.trim().to_string())
}
