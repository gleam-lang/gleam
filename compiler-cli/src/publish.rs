use std::{
    io::Write,
    path::{Path, PathBuf},
};

use flate2::{write::GzEncoder, Compression};
use gleam_core::{hex::HEXPM_PUBLIC_KEY, io::HttpClient as _, Error, Result};
use hexpm::version::{Range, Version};
use itertools::Itertools;
use sha2::Digest;

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

    // TODO: Build HTML documentation

    // Build the package release tarball
    let files = project_files();
    let contents_tar_gz = contents_tarball(&files)?;
    let version = package_version_int(&config.name).await?.to_string();
    let metadata = metadata_config(config, files);

    // Create inner checksum
    let mut hasher = sha2::Sha256::new();
    hasher.update(version.as_bytes());
    hasher.update(metadata.as_bytes());
    hasher.update(contents_tar_gz);
    let checksum = base16::encode_upper(&hasher.finalize());

    tracing::info!(checksum = %checksum, "Generated Hex package inner checksum");

    // Get login creds from user
    let username = cli::ask("https://hex.pm username")?;
    let password = cli::ask_password("https://hex.pm password")?;

    // Create API token
    let key = gleam_core::hex::create_api_key(&hostname, &username, &password, &hex_config, &http)
        .await?;

    // TODO: Build release tarball
    // https://github.com/hexpm/specifications/blob/master/package_tarball.md

    // TODO: Publish release to hexpm

    // TODO: Publish docs to hexpm for release

    // Delete API token
    gleam_core::hex::remove_api_key(&hostname, &hex_config, &key, &http).await?;

    Ok(())
}

fn metadata_config(config: gleam_core::config::PackageConfig, files: Vec<PathBuf>) -> String {
    let metadata = ReleaseMetadata {
        name: &config.name,
        version: &config.version,
        description: &config.description,
        files,
        licenses: vec![], // TODO: get from config, assert it exists
        links: config
            .docs
            .links
            .iter()
            .map(|l| (l.title.as_str(), l.href.as_str()))
            .collect(), // TODO: move links to the top level
        requirements: config
            .dependencies
            .iter()
            .map(|(name, requirement)| ReleaseRequirement {
                name: name.as_str(),
                requirement,
            })
            .collect(),
        build_tools: vec!["gleam"],
    }
    .as_erlang();
    tracing::info!(contents = ?metadata, "Generated Hex metadata.config");
    metadata
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
        Err(hexpm::ApiError::NotFound) => 0,
        Err(e) => return Err(Error::hex(e)),
    };
    tracing::info!(number=%int, "Package Hex internal version");
    Ok(int)
}

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
    tracing::info!("Generated contents.tar.gz");
    Ok(contents_tar_gz)
}

fn project_files() -> Vec<PathBuf> {
    let mut files: Vec<PathBuf> = fs::gleam_files(&PathBuf::from("src")).collect();
    let mut add = |path| {
        let path = PathBuf::from(path);
        if path.exists() {
            files.push(path);
        }
    };
    add("README.md");
    add("gleam.toml");
    add("LICENSE");
    add("LICENCE");
    files
}

fn add_to_tar<P, W>(tarball: &mut tar::Builder<W>, path: P) -> Result<()>
where
    P: AsRef<Path>,
    W: Write,
{
    let path = path.as_ref();
    tracing::info!(file=?path, "Adding file to tarball");
    tarball
        .append_path(path)
        .map_err(|e| Error::add_tar(path, e))
}

#[derive(Debug, Clone)]
pub struct ReleaseMetadata<'a> {
    name: &'a str,
    version: &'a Version,
    description: &'a str,
    files: Vec<PathBuf>,
    licenses: Vec<&'a str>, // TODO: use spdx licence type to ensure correct format
    links: Vec<(&'a str, &'a str)>, // TODO: use http::Uri type to ensure correct format
    requirements: Vec<ReleaseRequirement<'a>>,
    build_tools: Vec<&'a str>,
    // What should this be? I can't find it in the API anywhere.
    // extra: (kvlist(string => kvlist(...))) (optional)
}

impl<'a> ReleaseMetadata<'a> {
    pub fn as_erlang(&self) -> String {
        fn link(link: &(&str, &str)) -> String {
            format!(
                "\n  {{<<\"{name}\">>, <<\"{url}\">>}}",
                name = link.0,
                url = link.1
            )
        }
        fn file(name: impl AsRef<Path>) -> String {
            format!("\n  <<\"{name}\">>", name = name.as_ref().to_string_lossy())
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
                .map(ReleaseRequirement::as_erlang)
                .join(",")
        )
    }
}

#[derive(Debug, Clone)]
struct ReleaseRequirement<'a> {
    name: &'a str,
    // optional: bool,
    requirement: &'a Range,
    // Support alternate repositories at a later date.
    // repository: String,
}
impl<'a> ReleaseRequirement<'a> {
    pub fn as_erlang(&self) -> String {
        format!(
            r#"
  {{<<"{app}">>, [
    {{<<"app">>, <<"{app}">>}},
    {{<<"optional">>, false}},
    {{<<"requirement">>, <<"{requirement}">>}}
  ]}}"#,
            app = self.name,
            requirement = self.requirement,
        )
    }
}

#[test]
fn release_metadata_as_erlang() {
    let version = "1.2.3".try_into().unwrap();
    let req1 = Range::new("~> 1.2.3 or >= 5.0.0".to_string());
    let req2 = Range::new("~> 1.2".to_string());
    let meta = ReleaseMetadata {
        name: "myapp",
        version: &version,
        description: "description goes here",
        files: vec![
            PathBuf::from("gleam.toml"),
            PathBuf::from("src/thingy.gleam"),
            PathBuf::from("src/whatever.gleam"),
        ],
        licenses: vec!["MIT", "MPL-2.0"],
        links: vec![
            ("homepage", "https://gleam.run"),
            ("github", "https://github.com/lpil/myapp"),
        ],
        requirements: vec![
            ReleaseRequirement {
                name: "wibble",
                requirement: &req1,
            },
            ReleaseRequirement {
                name: "wobble",
                requirement: &req2,
            },
        ],
        build_tools: vec!["gleam", "rebar3"],
    };
    assert_eq!(
        meta.as_erlang(),
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

fn quotes(x: &&str) -> String {
    format!(r#"<<"{}">>"#, x)
}
