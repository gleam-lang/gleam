use camino::{Utf8Path, Utf8PathBuf};
use flate2::{write::GzEncoder, Compression};
use gleam_core::{
    analyse::TargetSupport,
    build::{Codegen, Mode, Options, Package, Target},
    config::{PackageConfig, SpdxLicense},
    docs::DocContext,
    hex,
    paths::{self, ProjectPaths},
    requirement::Requirement,
    Error, Result,
};
use hexpm::version::{Range, Version};
use itertools::Itertools;
use sha2::Digest;
use std::{io::Write, path::PathBuf, time::Instant, sync::Arc};

use crate::{build, cli, docs, fs, hex::ApiKeyCommand, http::HttpClient};

pub fn command(replace: bool, yes: bool) -> Result<()> {
    let command = PublishCommand::setup(replace, yes)?;

    if let Some(mut command) = command {
        command.run()?;
    }
    Ok(())
}

pub struct PublishCommand {
    config: PackageConfig,
    package_tarball: Vec<u8>,
    docs_tarball: Vec<u8>,
    replace: bool,
}

impl PublishCommand {
    pub fn setup(replace: bool, i_am_sure: bool) -> Result<Option<Self>> {
        let paths = crate::find_project_paths()?;
        let config = crate::config::root_config()?;

        let should_publish = check_for_gleam_prefix(&config, i_am_sure)?
            && check_for_version_zero(&config, i_am_sure)?
            && check_repo_url(&config, i_am_sure)?;

        if !should_publish {
            println!("Not publishing.");
            std::process::exit(0);
        }

        let Tarball {
            mut compile_result,
            data: package_tarball,
            src_files_added,
            generated_files_added,
        } = do_build_hex_tarball(&paths, &config)?;

        check_for_name_squatting(&compile_result)?;

        // Build HTML documentation
        let docs_tarball = fs::create_tar_archive(docs::build_documentation(
            &config,
            &mut compile_result,
            DocContext::HexPublish,
        )?)?;

        // Ask user if this is correct
        if !generated_files_added.is_empty() {
            println!("\nGenerated files:");
            for file in generated_files_added.iter().sorted() {
                println!("  - {}", file.0);
            }
        }
        println!("\nSource files:");
        for file in src_files_added.iter().sorted() {
            println!("  - {}", file);
        }
        println!("\nName: {}", config.name);
        println!("Version: {}", config.version);

        let should_publish = i_am_sure || cli::confirm("\nDo you wish to publish this package?")?;
        if !should_publish {
            println!("Not publishing.");
            std::process::exit(0);
        }

        Ok(Some(Self {
            config,
            docs_tarball,
            package_tarball,
            replace,
        }))
    }
}

fn check_for_name_squatting(package: &Package) -> Result<(), Error> {
    if package.modules.len() > 1 {
        return Ok(());
    }

    let Some(module) = package.modules.first() else {
        return Err(Error::HexPackageSquatting);
    };

    if module.dependencies.len() > 1 {
        return Ok(());
    }

    let definitions = &module.ast.definitions;

    if definitions.len() > 2 {
        return Ok(());
    }

    let Some(main) = definitions.iter().find_map(|d| d.main_function()) else {
        return Ok(());
    };

    if main.body.first().is_println() {
        return Err(Error::HexPackageSquatting);
    }

    Ok(())
}

fn check_repo_url(config: &PackageConfig, i_am_sure: bool) -> Result<bool, Error> {
    let Some(url) = config.repository.url() else {
        return Ok(true);
    };

    let runtime = tokio::runtime::Runtime::new().expect("Unable to start Tokio async runtime");
    let response = runtime.block_on(reqwest::get(&url)).map_err(Error::http)?;

    if response.status().is_success() {
        return Ok(true);
    }

    println!(
        "The repository configuration in your `gleam.toml` file does not appear to be
valid, {} returned status {}",
        &url,
        response.status()
    );
    let should_publish = i_am_sure || cli::confirm("\nDo you wish to continue?")?;
    println!();
    Ok(should_publish)
}

/// Ask for confirmation if the package name if a v0.x.x version
fn check_for_version_zero(config: &PackageConfig, i_am_sure: bool) -> Result<bool, Error> {
    if config.version.major != 0 {
        return Ok(true);
    }

    println!(
        "You are about to publish a release that is below version 1.0.0.

Semantic versioning doesn't apply to version 0.x.x releases, so your
users will not be protected from breaking changes. This can result
in a poor user experience where packages can break unexpectedly with
updates that would normally be safe."
    );
    let should_publish = i_am_sure || cli::confirm("\nDo you wish to continue?")?;
    println!();
    Ok(should_publish)
}

/// Ask for confirmation if the package name if `gleam_*`
fn check_for_gleam_prefix(config: &PackageConfig, i_am_sure: bool) -> Result<bool, Error> {
    if !config.name.starts_with("gleam_") || config.name.starts_with("gleam_community_") {
        return Ok(true);
    }

    println!(
        "You are about to publish a package with a name that starts with
the prefix `gleam_`, which is for packages maintained by the Gleam
core team.",
    );
    let should_publish =
        i_am_sure || cli::confirm("\nAre you sure you want to use this package name?")?;
    println!();
    Ok(should_publish)
}

impl ApiKeyCommand for PublishCommand {
    fn with_api_key(
        &mut self,
        runtime: &tokio::runtime::Handle,
        hex_config: &hexpm::Config,
        api_key: &str,
    ) -> Result<()> {
        let start = Instant::now();
        cli::print_publishing(&self.config.name, &self.config.version);

        runtime.block_on(hex::publish_package(
            std::mem::take(&mut self.package_tarball),
            self.config.version.to_string(),
            api_key,
            hex_config,
            self.replace,
            &HttpClient::new(),
        ))?;

        cli::print_publishing_documentation();
        runtime.block_on(hex::publish_documentation(
            &self.config.name,
            &self.config.version,
            std::mem::take(&mut self.docs_tarball),
            api_key,
            hex_config,
            &HttpClient::new(),
        ))?;
        cli::print_published(start.elapsed());
        println!(
            "\nView your package at https://hex.pm/packages/{}",
            &self.config.name
        );

        // Prompt the user to make a git tag if they have not.
        let has_repo = self.config.repository.url().is_some();
        let git = PathBuf::from(".git");
        let version = format!("v{}", &self.config.version);
        let git_tag = git.join("refs").join("tags").join(&version);
        if has_repo && git.exists() && !git_tag.exists() {
            println!(
                "
Please push a git tag for this release so source code links in the
HTML documentation will work:

    git tag {version}
    git push origin {version}
"
            )
        }
        Ok(())
    }
}

struct Tarball {
    compile_result: Package,
    data: Vec<u8>,
    src_files_added: Vec<Utf8PathBuf>,
    generated_files_added: Vec<(Utf8PathBuf, String)>,
}

pub fn build_hex_tarball(paths: &ProjectPaths, config: &PackageConfig) -> Result<Vec<u8>> {
    let Tarball { data, .. } = do_build_hex_tarball(paths, config)?;
    Ok(data)
}

fn do_build_hex_tarball(paths: &ProjectPaths, config: &PackageConfig) -> Result<Tarball> {
    let target = config.target;
    check_config_for_publishing(config)?;

    // Reset the build directory so we know the state of the project
    fs::delete_directory(&paths.build_directory_for_target(Mode::Prod, target))?;

    // Build the project to check that it is valid
    let built = build::main(
        Options {
            root_target_support: TargetSupport::Enforced,
            warnings_as_errors: false,
            mode: Mode::Prod,
            target: Some(target),
            codegen: Codegen::All,
        },
        build::download_dependencies()?,
        Arc::new(cli::Reporter::new()),
    )?;

    // If any of the modules in the package contain a todo then refuse to
    // publish as the package is not yet finished.
    let unfinished = built
        .root_package
        .modules
        .iter()
        .filter(|module| module.ast.type_info.contains_todo())
        .map(|module| module.name.clone())
        .sorted()
        .collect_vec();
    if !unfinished.is_empty() {
        return Err(Error::CannotPublishTodo { unfinished });
    }

    // TODO: If any of the modules in the package contain a leaked internal type then
    // refuse to publish as the package is not yet finished.
    // We need to move aliases in to the type system first.
    // context: https://discord.com/channels/768594524158427167/768594524158427170/1227250677734969386

    // Collect all the files we want to include in the tarball
    let generated_files = match target {
        Target::Erlang => generated_erlang_files(paths, &built.root_package)?,
        Target::JavaScript => vec![],
    };
    let src_files = project_files()?;
    let contents_tar_gz = contents_tarball(&src_files, &generated_files)?;
    let version = "3";
    let metadata = metadata_config(&built.root_package.config, &src_files, &generated_files)?;

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
    Ok(Tarball {
        compile_result: built.root_package,
        data: tarball,
        src_files_added: src_files,
        generated_files_added: generated_files,
    })
}

fn check_config_for_publishing(config: &PackageConfig) -> Result<()> {
    // These fields are required to publish a Hex package. Hex will reject
    // packages without them.
    if config.description.is_empty() || config.licences.is_empty() {
        Err(Error::MissingHexPublishFields {
            description_missing: config.description.is_empty(),
            licence_missing: config.licences.is_empty(),
        })
    } else {
        Ok(())
    }
}

fn metadata_config<'a>(
    config: &'a PackageConfig,
    source_files: &[Utf8PathBuf],
    generated_files: &[(Utf8PathBuf, String)],
) -> Result<String> {
    let repo_url = http::Uri::try_from(config.repository.url().unwrap_or_default()).ok();
    let requirements: Result<Vec<ReleaseRequirement<'a>>> = config
        .dependencies
        .iter()
        .map(|(name, requirement)| match requirement {
            Requirement::Hex { version } => Ok(ReleaseRequirement {
                name,
                requirement: version,
            }),
            _ => Err(Error::PublishNonHexDependencies {
                package: name.to_string(),
            }),
        })
        .collect();
    let metadata = ReleaseMetadata {
        name: &config.name,
        version: &config.version,
        description: &config.description,
        source_files,
        generated_files,
        licenses: &config.licences,
        links: config
            .links
            .iter()
            .map(|l| (l.title.as_str(), l.href.clone()))
            .chain(repo_url.into_iter().map(|u| ("Repository", u)))
            .collect(),
        requirements: requirements?,
        build_tools: vec!["gleam"],
    }
    .as_erlang();
    tracing::info!(contents = ?metadata, "Generated Hex metadata.config");
    Ok(metadata)
}

fn contents_tarball(
    files: &[Utf8PathBuf],
    data_files: &[(Utf8PathBuf, String)],
) -> Result<Vec<u8>, Error> {
    let mut contents_tar_gz = Vec::new();
    {
        let mut tarball =
            tar::Builder::new(GzEncoder::new(&mut contents_tar_gz, Compression::default()));
        for path in files {
            add_path_to_tar(&mut tarball, path)?;
        }
        for (path, contents) in data_files {
            add_to_tar(&mut tarball, path, contents.as_bytes())?;
        }
        tarball.finish().map_err(Error::finish_tar)?;
    }
    tracing::info!("Generated contents.tar.gz");
    Ok(contents_tar_gz)
}

// TODO: test
// TODO: Don't include git-ignored native files
fn project_files() -> Result<Vec<Utf8PathBuf>> {
    let src = Utf8Path::new("src");
    let mut files: Vec<Utf8PathBuf> = fs::gleam_files_excluding_gitignore(src)
        .chain(fs::native_files(src)?)
        .collect();
    let private = Utf8Path::new("priv");
    let mut private_files: Vec<Utf8PathBuf> =
        fs::private_files_excluding_gitignore(private).collect();
    files.append(&mut private_files);
    let mut add = |path| {
        let path = Utf8PathBuf::from(path);
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
    add("NOTICE");
    add("NOTICE.md");
    add("NOTICE.txt");
    Ok(files)
}

// TODO: test
fn generated_erlang_files(
    paths: &ProjectPaths,
    package: &Package,
) -> Result<Vec<(Utf8PathBuf, String)>> {
    let mut files = vec![];

    let dir = paths.build_directory_for_package(Mode::Prod, Target::Erlang, &package.config.name);
    let ebin = dir.join("ebin");
    let build = dir.join(paths::ARTEFACT_DIRECTORY_NAME);
    let include = dir.join("include");

    let tar_src = Utf8Path::new("src");
    let tar_include = Utf8Path::new("include");

    // Erlang modules
    for module in &package.modules {
        if module.is_test() {
            continue;
        }
        let name = module.compiled_erlang_path();
        files.push((tar_src.join(&name), fs::read(build.join(name))?));
    }

    // Erlang headers
    if include.is_dir() {
        for file in fs::erlang_files(&include)? {
            let name = file.file_name().expect("generated_files include file name");
            files.push((tar_include.join(name), fs::read(file)?));
        }
    }

    // src/package.app.src file
    let app = format!("{}.app", &package.config.name);
    let appsrc = format!("{}.src", &app);
    files.push((tar_src.join(appsrc), fs::read(ebin.join(app))?));

    Ok(files)
}

fn add_to_tar<P, W>(tarball: &mut tar::Builder<W>, path: P, data: &[u8]) -> Result<()>
where
    P: AsRef<Utf8Path>,
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
    P: AsRef<Utf8Path>,
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
    source_files: &'a [Utf8PathBuf],
    generated_files: &'a [(Utf8PathBuf, String)],
    licenses: &'a Vec<SpdxLicense>,
    links: Vec<(&'a str, http::Uri)>,
    requirements: Vec<ReleaseRequirement<'a>>,
    build_tools: Vec<&'a str>,
    // What should this be? I can't find it in the API anywhere.
    // extra: (kvlist(string => kvlist(...))) (optional)
}

impl<'a> ReleaseMetadata<'a> {
    pub fn as_erlang(&self) -> String {
        fn link(link: &(&str, http::Uri)) -> String {
            format!(
                "\n  {{<<\"{name}\">>, <<\"{url}\">>}}",
                name = link.0,
                url = link.1
            )
        }
        fn file(name: impl AsRef<Utf8Path>) -> String {
            format!("\n  <<\"{name}\">>", name = name.as_ref())
        }

        format!(
            r#"{{<<"name">>, <<"{name}">>}}.
{{<<"app">>, <<"{name}">>}}.
{{<<"version">>, <<"{version}">>}}.
{{<<"description">>, <<"{description}"/utf8>>}}.
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
            files = self
                .source_files
                .iter()
                .chain(self.generated_files.iter().map(|(p, _)| p))
                .map(file)
                .sorted()
                .join(","),
            links = self.links.iter().map(link).join(","),
            licenses = self.licenses.iter().map(|l| quotes(l.as_ref())).join(", "),
            build_tools = self.build_tools.iter().map(|l| quotes(l)).join(", "),
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
    let licences = vec![
        SpdxLicense {
            licence: "MIT".into(),
        },
        SpdxLicense {
            licence: "MPL-2.0".into(),
        },
    ];
    let version = "1.2.3".try_into().unwrap();
    let homepage = "https://gleam.run".parse().unwrap();
    let github = "https://github.com/lpil/myapp".parse().unwrap();
    let req1 = Range::new("~> 1.2.3 or >= 5.0.0".into());
    let req2 = Range::new("~> 1.2".into());
    let meta = ReleaseMetadata {
        name: "myapp",
        version: &version,
        description: "description goes here 🌈",
        source_files: &[
            Utf8PathBuf::from("gleam.toml"),
            Utf8PathBuf::from("src/thingy.gleam"),
            Utf8PathBuf::from("src/whatever.gleam"),
        ],
        generated_files: &[
            (Utf8PathBuf::from("src/myapp.app"), "".into()),
            (Utf8PathBuf::from("src/thingy.erl"), "".into()),
            (Utf8PathBuf::from("src/whatever.erl"), "".into()),
        ],
        licenses: &licences,
        links: vec![("homepage", homepage), ("github", github)],
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
{<<"description">>, <<"description goes here 🌈"/utf8>>}.
{<<"licenses">>, [<<"MIT">>, <<"MPL-2.0">>]}.
{<<"build_tools">>, [<<"gleam">>, <<"rebar3">>]}.
{<<"links">>, [
  {<<"homepage">>, <<"https://gleam.run/">>},
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
  <<"src/myapp.app">>,
  <<"src/thingy.erl">>,
  <<"src/thingy.gleam">>,
  <<"src/whatever.erl">>,
  <<"src/whatever.gleam">>
]}.
"#
        .to_string()
    );
}

#[test]
fn prevent_publish_local_dependency() {
    let mut config = PackageConfig::default();
    config.dependencies = [("provided".into(), Requirement::path("./path/to/package"))].into();
    assert_eq!(
        metadata_config(&config, &[], &[]),
        Err(Error::PublishNonHexDependencies {
            package: "provided".into()
        })
    );
}

#[test]
fn prevent_publish_git_dependency() {
    let mut config = PackageConfig::default();
    config.dependencies = [(
        "provided".into(),
        Requirement::git("https://github.com/gleam-lang/gleam.git"),
    )]
    .into();
    assert_eq!(
        metadata_config(&config, &[], &[]),
        Err(Error::PublishNonHexDependencies {
            package: "provided".into()
        })
    );
}

pub fn get_hostname() -> String {
    hostname::get()
        .expect("Looking up hostname")
        .to_string_lossy()
        .to_string()
}

fn quotes(x: &str) -> String {
    format!(r#"<<"{x}">>"#)
}
