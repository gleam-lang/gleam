use std::{
    io::Write,
    path::{Path, PathBuf},
};

use flate2::{write::GzEncoder, Compression};
use gleam_core::{hex::HEXPM_PUBLIC_KEY, io::HttpClient as _, Error, Result};
use itertools::Itertools;

use crate::{build, cli, fs, http::HttpClient};

pub fn command() -> Result<()> {
    tokio::runtime::Runtime::new()
        .expect("Unable to start Tokio async runtime")
        .block_on(perform_command())
}

pub async fn perform_command() -> Result<()> {
    let hostname = get_hostname();
    let config = crate::config::root_config()?;
    let hex_config = hexpm::Config::new();
    let http = HttpClient::new();

    // Build the project to check that it is valid
    let _ = build::main()?;

    // Get login creds from user
    let username = cli::ask("https://hex.pm username")?;
    let password = cli::ask_password("https://hex.pm password")?;

    // Create API token
    let key = gleam_core::hex::create_api_key(&hostname, &username, &password, &hex_config, &http)
        .await?;

    // TODO: Build HTML documentation

    tracing::info!("Creating release tarball");
    let files = project_files();
    let _tarball = contents_tarball(&files)?;
    let _version_int = package_version_int(&config.name).await?;

    // TODO: Build release tarball
    // https://github.com/hexpm/specifications/blob/master/package_tarball.md

    // TODO: Publish release to hexpm

    // TODO: Publish docs to hexpm for release

    // Delete API token
    gleam_core::hex::remove_api_key(&hostname, &hex_config, &key, &http).await?;

    Ok(())
}

/// Hex wants a second version number that is a single int. It's unclear why,
/// and we don't know what this number should be locally, so use the number of
/// releases as that int.
async fn package_version_int(name: &str) -> Result<usize> {
    let config = hexpm::Config::new();
    let request = hexpm::get_package_request(name, None, &config);
    let response = HttpClient::new().send(request).await?;
    let int = match hexpm::get_package_response(response, HEXPM_PUBLIC_KEY) {
        Ok(response) => response.releases.len(),
        Err(hexpm::ApiError::NotFound) => 0, // Not found
        Err(e) => return Err(Error::hex(e)),
    };
    tracing::debug!(version=%int, "Package int version fetched");
    Ok(int)
}

// TODO: return the list of file paths from here to use in the metadata
fn contents_tarball(files: &[PathBuf]) -> Result<Vec<u8>, Error> {
    let mut contents_tar_gz = Vec::new();
    {
        let mut tarball =
            tar::Builder::new(GzEncoder::new(&mut contents_tar_gz, Compression::default()));
        for path in files {
            add_to_tar(&mut tarball, path)?;
        }
        tarball.finish().map_err(Error::finish_tar)?;
    }
    Ok(contents_tar_gz)
}

fn project_files() -> Vec<PathBuf> {
    let mut files: Vec<PathBuf> = fs::gleam_files(&PathBuf::from("src")).collect();
    files.push(PathBuf::from("gleam.toml"));
    files
}

fn add_to_tar<P, W>(tarball: &mut tar::Builder<W>, path: P) -> Result<()>
where
    P: AsRef<Path>,
    W: Write,
{
    let path = path.as_ref();
    tracing::debug!(file=?path, "Adding file to tarball");
    tarball
        .append_path(path)
        .map_err(|e| Error::add_tar(path, e))
}

#[derive(Debug, Clone)]
pub struct ReleaseMetadata {
    name: String,
    version: String,
    // app: String,
    description: String,
    files: Vec<PathBuf>,
    licenses: Vec<String>, // TODO: use spdx licence type to ensure correct format
    links: Vec<(String, String)>, // TODO: use http::Uri type to ensure correct format
    requirements: Vec<ReleaseRequirement>,
    build_tools: Vec<String>,
    // What should this be? I can't find it in the API anywhere.
    // extra: (kvlist(string => kvlist(...))) (optional)
}

impl ReleaseMetadata {
    pub fn to_erlang(&self) -> String {
        fn link(link: &(String, String)) -> String {
            format!(
                "\n  {{<<\"{name}\">>, <<\"{url}\">>}}",
                name = link.0,
                url = link.1
            )
        }
        fn file(name: &PathBuf) -> String {
            format!("\n  <<\"{name}\">>", name = name.to_string_lossy())
        }

        format!(
            r#"{{<<"name">>, <<"{name}">>}}.
{{<<"app">>, <<"{name}">>}}.
{{<<"version">>, <<"{version}">>}}.
{{<<"licenses">>, [{licenses}]}}.
{{<<"build_tools">>, [{build_tools}]}}.
{{<<"links">>, [{links}
]}}.
{{<<"requirements">>, [{requirements}
]}}.
{{<<"files">>, [{files}
]}}.
"#,
            name = self.name,
            version = self.version,
            files = self.files.iter().map(file).join(","),
            links = self.links.iter().map(link).join(","),
            licenses = self.licenses.iter().map(quotes).join(", "),
            build_tools = self.build_tools.iter().map(quotes).join(", "),
            requirements = self
                .requirements
                .iter()
                .map(ReleaseRequirement::to_erlang)
                .join(",")
        )
    }
}

#[derive(Debug, Clone)]
struct ReleaseRequirement {
    app: String,
    // optional: bool,
    requirement: String,
    // Support alternate repositories at a later date.
    // repository: String,
}
impl ReleaseRequirement {
    pub fn to_erlang(&self) -> String {
        format!(
            r#"
  {{<<"{app}">>, [
    {{<<"app">>, <<"{app}">>}},
    {{<<"optional">>, false}},
    {{<<"requirement">>, <<"{requirement}">>}}
  ]}}"#,
            app = self.app,
            requirement = self.requirement,
        )
    }
}

#[test]
fn release_metadata_to_erlang() {
    let meta = ReleaseMetadata {
        name: "myapp".to_string(),
        version: "1.2.3".parse().unwrap(),
        description: "description goes here".to_string(),
        files: vec![
            PathBuf::from("gleam.toml"),
            PathBuf::from("src/thingy.gleam"),
            PathBuf::from("src/whatever.gleam"),
        ],
        licenses: vec!["MIT".to_string(), "MPL-2.0".to_string()],
        links: vec![
            ("homepage".to_string(), "https://gleam.run".to_string()),
            (
                "github".to_string(),
                "https://github.com/lpil/myapp".to_string(),
            ),
        ],
        requirements: vec![
            ReleaseRequirement {
                app: "wibble".to_string(),
                requirement: "~> 1.2.3 or >= 5.0.0".to_string(),
            },
            ReleaseRequirement {
                app: "wobble".to_string(),
                requirement: "~> 1.2".to_string(),
            },
        ],
        build_tools: vec!["gleam".to_string(), "rebar3".to_string()],
    };
    assert_eq!(
        meta.to_erlang(),
        r#"{<<"name">>, <<"myapp">>}.
{<<"app">>, <<"myapp">>}.
{<<"version">>, <<"1.2.3">>}.
{<<"licenses">>, [<<"MIT">>, <<"MPL-2.0">>]}.
{<<"build_tools">>, [<<"gleam">>, <<"rebar3">>]}.
{<<"links">>, [
  {<<"homepage">>, <<"https://gleam.run">>},
  {<<"github">>, <<"https://github.com/lpil/myapp">>}
]}.
{<<"requirements">>, [
  {<<"wibble">>, [
    {<<"app">>, <<"wibble">>},
    {<<"optional">>, false},
    {<<"requirement">>, <<"~> 1.2.3 or >= 5.0.0">>}
  ]},
  {<<"wobble">>, [
    {<<"app">>, <<"wobble">>},
    {<<"optional">>, false},
    {<<"requirement">>, <<"~> 1.2">>}
  ]}
]}.
{<<"files">>, [
  <<"gleam.toml">>,
  <<"src/thingy.gleam">>,
  <<"src/whatever.gleam">>
]}.
"#
        .to_string()
    );
}

fn get_hostname() -> String {
    hostname::get()
        .expect("Looking up hostname")
        .to_string_lossy()
        .to_string()
}

fn quotes(x: &String) -> String {
    format!(r#"<<"{}">>"#, x)
}
