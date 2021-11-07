use crate::{cli, http::HttpClient};
use gleam_core::{config::DocsPage, error::Error, io::HttpClient as _, paths, Result};

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
    let compiled = crate::build::main()?;
    let out = paths::build_docs(&config.name);

    // Attach documentation to Src modules
    // let analysed: Vec<_> = compiled
    //     .modules
    //     .into_iter()
    //     .filter(|a| a.origin == Origin::Src)
    //     .map(|mut a| {
    //         a.attach_doc_and_module_comments();
    //         a
    //     })
    //     .collect();

    // Initialize pages with the README
    let mut pages = vec![DocsPage {
        title: "README".to_string(),
        path: "index.html".to_string(),
        source: paths::readme(), // TODO: support non markdown READMEs. Or a default if there is none.
    }];

    // Add any user-supplied pages
    pages.extend(config.docs.pages.iter().cloned());

    // Generate HTML
    let outputs = gleam_core::docs::generate_html(&config, compiled.modules.as_slice(), &pages);

    // Write
    crate::fs::delete_dir(&out)?;
    crate::fs::write_outputs(&outputs)?;

    println!(
        "\nThe docs for {package} have been rendered to {out}",
        package = config.name,
        out = out.to_string_lossy()
    );

    // We're done!
    Ok(())
}

// pub fn publish(project_root: String, version: String) -> Result<(), Error> {
//     let project_root = PathBuf::from(&project_root).canonicalize().map_err(|_| {
//         Error::UnableToFindProjectRoot {
//             path: project_root.clone(),
//         }
//     })?;

//     let output_dir = PathBuf::new();
//     let http = HttpClient::new();
//     let hex_config = hexpm::Config::new();

//     // Build
//     let (config, outputs) = build_project(&project_root, version.clone(), &output_dir)?;

//     // Create gzipped tarball of docs
//     let archive = crate::fs::create_tar_archive(outputs)?;

//     // Start event loop so we can run async functions to call the Hex API
//     let runtime = tokio::runtime::Runtime::new().expect("Unable to start Tokio async runtime");

//     // Get login creds from user
//     let username = cli::ask("https://hex.pm username")?;
//     let password = cli::ask_password("https://hex.pm password")?;

//     // Authenticate with API
//     let request = hexpm::create_api_key_request(&username, &password, TOKEN_NAME, &hex_config);
//     let response = runtime.block_on(http.send(request))?;
//     let token = hexpm::create_api_key_response(response).map_err(Error::hex)?;

//     // Upload to hex
//     let request = hexpm::publish_docs_request(&config.name, &version, archive, &token, &hex_config)
//         .map_err(Error::hex)?;
//     let response = runtime.block_on(http.send(request))?;
//     hexpm::publish_docs_response(response).map_err(Error::hex)?;

//     println!(
//         "
// The docs for {package} have been published to HexDocs:

//     https://hexdocs.pm/{package}",
//         package = config.name
//     );

//     // We're done!
//     Ok(())
// }

// pub fn build_project(
//     project_root: impl AsRef<Path>,
//     version: String,
//     output_dir: &Path,
// ) -> Result<(PackageConfig, Vec<OutputFile>), Error> {
//     // Read and type check project
//     let (mut config, analysed) = project::read_and_analyse(&project_root)?;
//     config.version = Version::parse(&version).map_err(|e| Error::InvalidVersionFormat {
//         input: version.to_string(),
//         error: e.to_string(),
//     })?;
//     check_app_file_version_matches(&project_root, &config)?;

//     // Attach documentation to Src modules
//     let analysed: Vec<_> = analysed
//         .into_iter()
//         .filter(|a| a.origin == ModuleOrigin::Src)
//         .map(|mut a| {
//             a.attach_doc_and_module_comments();
//             a
//         })
//         .collect();

//     // Initialize pages with the README
//     let mut pages = vec![DocsPage {
//         title: "README".to_string(),
//         path: "index.html".to_string(),
//         source: paths::readme(),
//     }];

//     // Add any user-supplied pages
//     pages.extend(config.docs.pages.iter().cloned());

//     // Generate HTML
//     let outputs = gleam_core::docs::generate_html(&config, &analysed, &pages, output_dir);
//     Ok((config, outputs))
// }
