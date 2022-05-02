use std::time::Instant;

use crate::{cli, hex::ApiKeyCommand, http::HttpClient};
use gleam_core::{
    build::{Mode, Options, Package},
    config::{DocsPage, PackageConfig},
    error::Error,
    hex,
    io::HttpClient as _,
    paths, Result,
};

pub fn remove(package: String, version: String) -> Result<()> {
    RemoveCommand::new(package, version).run()
}

struct RemoveCommand {
    package: String,
    version: String,
}

impl RemoveCommand {
    pub fn new(package: String, version: String) -> Self {
        Self { package, version }
    }
}

impl ApiKeyCommand for RemoveCommand {
    fn with_api_key(
        &mut self,
        handle: &tokio::runtime::Handle,
        hex_config: &hexpm::Config,
        api_key: &str,
    ) -> Result<()> {
        let http = HttpClient::new();

        // Remove docs from API
        let request = hexpm::remove_docs_request(&self.package, &self.version, api_key, hex_config)
            .map_err(Error::hex)?;
        let response = handle.block_on(http.send(request))?;
        hexpm::remove_docs_response(response).map_err(Error::hex)?;

        // Done!
        println!(
            "The docs for {} {} have been removed from HexDocs",
            self.package, self.version
        );
        Ok(())
    }
}

pub fn build() -> Result<()> {
    let config = crate::config::root_config()?;
    let out = paths::build_docs(&config.name);
    let mut compiled = crate::build::main(Options {
        mode: Mode::Prod,
        target: None,
        perform_codegen: true,
    })?;
    let outputs = build_documentation(&config, &mut compiled)?;

    // Write
    crate::fs::delete_dir(&out)?;
    crate::fs::write_outputs_under(&outputs, &out)?;

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
    pages.extend(config.documentation.pages.iter().cloned());
    let outputs = gleam_core::docs::generate_html(config, compiled.modules.as_slice(), &pages);
    Ok(outputs)
}

struct PublishCommand {
    config: PackageConfig,
    archive: Vec<u8>,
}

pub fn publish() -> Result<()> {
    PublishCommand::new()?.run()
}

impl PublishCommand {
    pub fn new() -> Result<Self> {
        let config = crate::config::root_config()?;
        let mut compiled = crate::build::main(Options {
            perform_codegen: true,
            mode: Mode::Dev,
            target: None,
        })?;
        let outputs = build_documentation(&config, &mut compiled)?;
        let archive = crate::fs::create_tar_archive(outputs)?;
        Ok(Self { config, archive })
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
