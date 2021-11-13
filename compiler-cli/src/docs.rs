use std::time::Instant;

use crate::{cli, hex::ApiKeyCommand, http::HttpClient};
use gleam_core::{
    build::Package,
    config::{DocsPage, PackageConfig},
    error::Error,
    hex,
    io::HttpClient as _,
    paths, Result,
};

static TOKEN_NAME: &str = concat!(env!("CARGO_PKG_NAME"), " (", env!("CARGO_PKG_VERSION"), ")");

pub fn remove(package: String, version: String) -> Result<(), Error> {
    let config = hexpm::Config::new();
    let http = HttpClient::new();

    // Start event loop so we can run async functions to call the Hex API
    let runtime = tokio::runtime::Runtime::new().expect("Unable to start Tokio async runtime");

    // Get login creds from user
    let username = cli::ask("https://hex.pm username")?;
    let password = cli::ask_password("https://hex.pm password")?;

    // Authenticate with API
    let request = hexpm::create_api_key_request(&username, &password, TOKEN_NAME, &config);
    let response = runtime.block_on(http.send(request))?;
    let token = hexpm::create_api_key_response(response).map_err(Error::hex)?;

    // Remove docs from API
    let request =
        hexpm::remove_docs_request(&package, &version, &token, &config).map_err(Error::hex)?;
    let response = runtime.block_on(http.send(request))?;
    hexpm::remove_docs_response(response).map_err(Error::hex)?;

    // Done!
    println!(
        "The docs for {} {} have been removed from HexDocs",
        package, version
    );
    Ok(())
}

pub fn build() -> Result<()> {
    let config = crate::config::root_config()?;
    let out = paths::build_docs(&config.name);
    let mut compiled = crate::build::main()?;
    let outputs = build_documentation(&config, &mut compiled)?;

    // Write
    crate::fs::delete_dir(&out)?;
    crate::fs::write_outputs(&outputs)?;

    println!(
        "\nThe documentation for {package} has been rendered to \n./{out}/index.html",
        package = config.name,
        out = out.to_string_lossy()
    );

    // We're done!
    Ok(())
}

pub(crate) fn build_documentation(
    config: &PackageConfig,
    compiled: &mut Package,
) -> Result<Vec<gleam_core::io::OutputFile>, Error> {
    compiled.attach_doc_and_module_comments();
    cli::print_generating_documentation();
    let mut pages = vec![DocsPage {
        title: "README".to_string(),
        path: "index.html".to_string(),
        source: paths::readme(), // TODO: support non markdown READMEs. Or a default if there is none.
    }];
    pages.extend(config.docs.pages.iter().cloned());
    let outputs = gleam_core::docs::generate_html(config, compiled.modules.as_slice(), &pages);
    Ok(outputs)
}

pub struct PublishCommand {
    config: PackageConfig,
    archive: Vec<u8>,
}

impl PublishCommand {
    pub fn new() -> Result<Self> {
        let config = crate::config::root_config()?;
        let mut compiled = crate::build::main()?;
        let outputs = build_documentation(&config, &mut compiled)?;
        let archive = crate::fs::create_tar_archive(outputs)?;
        Ok(Self { config, archive })
    }

    pub fn publish() -> Result<()> {
        Self::new()?.run()
    }
}

impl ApiKeyCommand for PublishCommand {
    fn with_api_key(
        &mut self,
        handle: &tokio::runtime::Handle,
        hex_config: &hexpm::Config,
        api_key: &str,
    ) -> Result<()> {
        let start = Instant::now();
        cli::print_publishing_documentation();
        handle.block_on(hex::publish_documentation(
            &self.config.name,
            &self.config.version,
            std::mem::take(&mut self.archive),
            api_key,
            hex_config,
            &HttpClient::new(),
        ))?;
        cli::print_published(start.elapsed());
        Ok(())
    }
}
