use std::sync::Arc;
use std::time::{Instant, SystemTime};

use camino::{Utf8Path, Utf8PathBuf};

use crate::{cli, fs::ProjectIO, hex::ApiKeyCommand, http::HttpClient};
use gleam_core::{
    analyse::TargetSupport,
    build::{Codegen, Mode, Options, Package, Target},
    config::{DocsPage, PackageConfig},
    docs::DocContext,
    error::Error,
    hex,
    io::HttpClient as _,
    Result,
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

#[derive(Debug)]
pub struct BuildOptions {
    /// Whether to open the docs after building.
    pub open: bool,
    pub target: Option<Target>,
}

pub fn build(options: BuildOptions) -> Result<()> {
    let paths = crate::find_project_paths()?;
    let config = crate::config::root_config()?;

    // Reset the build directory so we know the state of the project
    crate::fs::delete_directory(&paths.build_directory_for_target(Mode::Prod, config.target))?;

    let out = paths.build_documentation_directory(&config.name);
    let mut built = crate::build::main(
        Options {
            mode: Mode::Prod,
            target: options.target,
            codegen: Codegen::All,
            warnings_as_errors: false,
            root_target_support: TargetSupport::Enforced,
        },
        crate::build::download_dependencies()?,
        Arc::new(cli::Reporter::new()),
    )?;
    let outputs = build_documentation(&config, &mut built.root_package, DocContext::Build)?;

    // Write
    crate::fs::delete_directory(&out)?;
    crate::fs::write_outputs_under(&outputs, &out)?;

    let index_html = out.join("index.html");

    println!(
        "\nThe documentation for {package} has been rendered to \n{index_html}",
        package = config.name,
        index_html = index_html
    );

    if options.open {
        open_docs(&index_html)?;
    }

    // We're done!
    Ok(())
}

/// Opens the indicated path in the default program configured by the system.
///
/// For the docs this will generally be a browser (unless some other program is
/// configured as the default for `.html` files).
fn open_docs(path: &Utf8Path) -> Result<()> {
    opener::open(path).map_err(|error| Error::FailedToOpenDocs {
        path: path.to_path_buf(),
        error: error.to_string(),
    })?;

    Ok(())
}

pub(crate) fn build_documentation(
    config: &PackageConfig,
    compiled: &mut Package,
    is_hex_publish: DocContext,
) -> Result<Vec<gleam_core::io::OutputFile>, Error> {
    compiled.attach_doc_and_module_comments();
    cli::print_generating_documentation();
    let paths = crate::find_project_paths()?;
    let mut pages = vec![DocsPage {
        title: "README".into(),
        path: "index.html".into(),
        source: paths.readme(), // TODO: support non markdown READMEs. Or a default if there is none.
    }];
    pages.extend(config.documentation.pages.iter().cloned());
    let mut outputs = gleam_core::docs::generate_html(
        &paths,
        config,
        compiled.modules.as_slice(),
        &pages,
        ProjectIO::new(),
        SystemTime::now(),
        is_hex_publish,
    );

    outputs.push(gleam_core::docs::generate_json_package_interface(
        Utf8PathBuf::from("package-interface.json"),
        compiled,
    ));
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
        let paths = crate::find_project_paths()?;
        let config = crate::config::root_config()?;

        // Reset the build directory so we know the state of the project
        crate::fs::delete_directory(&paths.build_directory_for_target(Mode::Prod, config.target))?;

        let mut built = crate::build::main(
            Options {
                root_target_support: TargetSupport::Enforced,
                warnings_as_errors: false,
                codegen: Codegen::All,
                mode: Mode::Prod,
                target: None,
            },
            crate::build::download_dependencies()?,
            Arc::new(cli::Reporter::new()),
        )?;
        let outputs =
            build_documentation(&config, &mut built.root_package, DocContext::HexPublish)?;
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
