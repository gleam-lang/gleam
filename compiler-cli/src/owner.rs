use crate::{cli, http::HttpClient};
use gleam_core::{Result, hex};

pub fn transfer(package: String, new_owner_username_or_email: String) -> Result<()> {
    println!(
        "Transferring ownership of this package will remove all current owners and make
{new_owner_username_or_email} its new owner.
Do you wish to transfer ownership of `{package}` to {new_owner_username_or_email}?",
    );

    let should_transfer_ownership = cli::confirm_with_text(&package)?;
    if !should_transfer_ownership {
        println!("Not transferring ownership.");
        return Ok(());
    }

    let runtime = tokio::runtime::Runtime::new().expect("Unable to start Tokio async runtime");
    let http = HttpClient::new();
    let hex_config = hexpm::Config::new();
    let credentials = crate::hex::HexAuthentication::new(&runtime, &http, hex_config.clone())
        .get_or_create_api_credentials()?;

    cli::print_transferring_ownership();
    runtime.block_on(hex::transfer_owner(
        &crate::hex::write_credentials(&credentials)?,
        package,
        new_owner_username_or_email,
        &hex_config,
        &HttpClient::new(),
    ))?;
    cli::print_transferred_ownership();

    Ok(())
}
