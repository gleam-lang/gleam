use crate::{
    error::{Error, GleamExpect, StandardIOAction},
    file,
};
use hexpm::Client;
use std::io::Write;
use std::path::PathBuf;

static TOKEN_NAME: &str = concat!(env!("CARGO_PKG_NAME"), " (", env!("CARGO_PKG_VERSION"), ")");

pub fn revoke(package: String, version: String) -> Result<(), Error> {
    // Get login creds from user
    let username = ask("https://hex.pm username")?;
    let password = ask_password("https://hex.pm password")?;

    // Start event loop so we can run async functions to call the Hex API
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

    // Done!
    println!(
        "The docs for {} {} have been removed from HexDocs",
        package, version
    );
    Ok(())
}

pub fn build(project_root: String, to: Option<String>) -> Result<(), Error> {
    let project_root = PathBuf::from(&project_root);
    let output_dir = to
        .map(PathBuf::from)
        .unwrap_or_else(|| project_root.join("gen").join("docs"));

    // Build
    let outputs = super::build_project(&project_root, &output_dir)?;

    // Write
    crate::file::delete_dir(&output_dir)?;
    file::write_outputs(outputs.as_slice())?;

    // We're done!
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
