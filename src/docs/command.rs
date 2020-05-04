use crate::{
    error::{Error, GleamExpect, StandardIOAction},
    project,
};
use hexpm::Client;
use std::io::Write;
use std::path::PathBuf;

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

pub fn build(project_root: String, to: Option<String>) -> Result<(), Error> {
    let root = PathBuf::from(&project_root);

    let output_dir = to
        .map(PathBuf::from)
        .unwrap_or_else(|| root.join("gen").join("docs"));

    // Read and type check project
    let (config, analysed) = project::read_and_analyse(&root)?;

    // Get README content
    let readme = std::fs::read_to_string(root.join("README.md")).unwrap_or_default();

    // Generate HTML
    let mut output_files = vec![];
    super::generate_html(
        &config,
        analysed.as_slice(),
        &mut output_files,
        readme.as_str(),
    );

    // Reset output directory
    crate::file::delete_dir(&output_dir)?;

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
