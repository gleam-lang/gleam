use std::{
    io::Write,
    path::{Path, PathBuf},
    time::Instant,
};

use flate2::{write::GzEncoder, Compression};
use gleam_core::{hex, Error, Result};
use hexpm::version::{Range, Version};
use itertools::Itertools;
use sha2::Digest;

use crate::{build, cli, fs, http::HttpClient};

pub fn command() -> Result<()> {
    let runtime = tokio::runtime::Runtime::new().expect("Unable to start Tokio async runtime");
    let hostname = get_hostname();
    let config = crate::config::root_config()?;
    let hex_config = hexpm::Config::new();
    let http = HttpClient::new();

    // TODO: validate that config.description is non-null

    // Build the project to check that it is valid
    let _ = build::main()?;

    // TODO: Build HTML documentation

    // Build the package release tarball
    let tarball = build_hex_tarball(&config)?;

    // Get login creds from user
    let username = cli::ask("https://hex.pm username")?;
    let password = cli::ask_password("https://hex.pm password")?;

    let start = Instant::now();
    // Publish the package
    let result = runtime.block_on(perform_publish(
        config, hostname, username, password, hex_config, http, tarball,
    ))?;

    // The result from publishing is handled after key deletion to ensure that
    // we remove it even in the case of an error
    result?;

    cli::print_published(start.elapsed());

    Ok(())
}

async fn perform_publish(
    config: gleam_core::config::PackageConfig,
    hostname: String,
    username: String,
    password: String,
    hex_config: hexpm::Config,
    http: HttpClient,
    tarball: Vec<u8>,
) -> Result<Result<(), Error>, Error> {
    cli::print_publishing(&config.name, &config.version);

    let key = gleam_core::hex::create_api_key(&hostname, &username, &password, &hex_config, &http)
        .await?;

    // Publish the package but don't exit early if it fails, we want to always
    // remove the API key
    let result = hex::publish_package(tarball, &key, &hex_config, &http).await;

    // Ensure to remove the API key
    gleam_core::hex::remove_api_key(&hostname, &hex_config, &key, &http).await?;

    Ok(result)
}

fn build_hex_tarball(config: &gleam_core::config::PackageConfig) -> Result<Vec<u8>> {
    let files = project_files()?;
    let contents_tar_gz = contents_tarball(&files)?;
    let version = "3";
    let metadata = metadata_config(config, files);

    // Calculate checksum
    let mut hasher = sha2::Sha256::new();
    hasher.update(version.as_bytes());
    hasher.update(metadata.as_bytes());
    hasher.update(contents_tar_gz.as_slice());
    let checksum = base16::encode_upper(&hasher.finalize());
    tracing::info!(checksum = %checksum, "Generated Hex package inner checksum");

    // Build tarball
    let mut tarball = Vec::new();
    {
        let mut tarball = tar::Builder::new(&mut tarball);
        add_to_tar(&mut tarball, "VERSION", version.as_bytes())?;
        add_to_tar(&mut tarball, "metadata.config", metadata.as_bytes())?;
        add_to_tar(&mut tarball, "contents.tar.gz", contents_tar_gz.as_slice())?;
        add_to_tar(&mut tarball, "CHECKSUM", checksum.as_bytes())?;
        tarball.finish().map_err(Error::finish_tar)?;
    }
    tracing::info!("Generated package Hex release tarball");
    Ok(tarball)
}

fn metadata_config(config: &gleam_core::config::PackageConfig, files: Vec<PathBuf>) -> String {
    let metadata = ReleaseMetadata {
        name: &config.name,
        version: &config.version,
        description: &config.description,
        files,
        licenses: &config.licences,
        links: config
            .links
            .iter()
            .map(|l| (l.title.as_str(), l.href.as_str()))
            .collect(),
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

fn contents_tarball(files: &[PathBuf]) -> Result<Vec<u8>, Error> {
    let mut contents_tar_gz = Vec::new();
    {
        let mut tarball =
            tar::Builder::new(GzEncoder::new(&mut contents_tar_gz, Compression::default()));
        for path in files {
            add_path_to_tar(&mut tarball, path)?;
        }
        tarball.finish().map_err(Error::finish_tar)?;
    }
    tracing::info!("Generated contents.tar.gz");
    Ok(contents_tar_gz)
}

// TODO: test
// TODO: Don't include git-ignored Erlang files
fn project_files() -> Result<Vec<PathBuf>> {
    let src = PathBuf::from("src");
    let mut files: Vec<PathBuf> = fs::gleam_files_excluding_gitignore(&src)
        .chain(fs::erlang_files(&src)?)
        .collect();
    let mut add = |path| {
        let path = PathBuf::from(path);
        if path.exists() {
            files.push(path);
        }
    };
    add("README");
    add("README.md");
    add("README.txt");
    add("gleam.toml");
    add("LICENSE");
    add("LICENCE");
    add("LICENSE.md");
    add("LICENCE.md");
    add("LICENSE.txt");
    add("LICENCE.txt");
    Ok(files)
}

fn add_to_tar<P, W>(tarball: &mut tar::Builder<W>, path: P, data: &[u8]) -> Result<()>
where
    P: AsRef<Path>,
    W: Write,
{
    let path = path.as_ref();
    tracing::info!(file=?path, "Adding file to tarball");
    let mut header = tar::Header::new_gnu();
    header.set_mode(0o600);
    header.set_size(data.len() as u64);
    header.set_cksum();
    tarball
        .append_data(&mut header, path, data)
        .map_err(|e| Error::add_tar(path, e))
}

fn add_path_to_tar<P, W>(tarball: &mut tar::Builder<W>, path: P) -> Result<()>
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
    licenses: &'a [String],
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
{{<<"description">>, <<"{description}">>}}.
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
            description = self.description,
            files = self.files.iter().map(file).join(","),
            links = self.links.iter().map(link).join(","),
            licenses = self.licenses.iter().map(|l| quotes(l.as_str())).join(", "),
            build_tools = self.build_tools.iter().map(|l| quotes(*l)).join(", "),
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
    let licences = vec!["MIT".to_string(), "MPL-2.0".to_string()];
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
        licenses: licences.as_slice(),
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
{<<"description">>, <<"description goes here">>}.
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

fn quotes(x: &str) -> String {
    format!(r#"<<"{}">>"#, x)
}
