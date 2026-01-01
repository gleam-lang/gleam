use camino::{Utf8Path, Utf8PathBuf};
use ecow::EcoString;
use flate2::{Compression, write::GzEncoder};
use gleam_core::{
    Error, Result,
    analyse::TargetSupport,
    ast::{CallArg, Statement, TypedExpr, TypedFunction},
    build::{Codegen, Compile, Mode, Options, Package, Target},
    config::{GleamVersion, PackageConfig, SpdxLicense},
    docs::{Dependency, DependencyKind, DocContext},
    error::{SmallVersion, wrap},
    hex,
    manifest::ManifestPackageSource,
    paths::{self, ProjectPaths},
    requirement::Requirement,
    type_,
};
use hexpm::version::{Range, Version};
use itertools::Itertools;
use sha2::Digest;
use std::{collections::HashMap, io::Write, path::PathBuf, time::Instant};

use crate::{build, cli, docs, fs, http::HttpClient};

const CORE_TEAM_PUBLISH_PASSWORD: &str = "Trans rights are human rights";

pub fn command(paths: &ProjectPaths, replace: bool, i_am_sure: bool) -> Result<()> {
    let mut config = crate::config::root_config(paths)?;

    let should_publish = check_for_gleam_prefix(&config)?
        && check_for_version_zero(&config)?
        && check_repo_url(&config, i_am_sure)?;

    if !should_publish {
        println!("Not publishing.");
        return Ok(());
    }

    let Tarball {
        mut compile_result,
        cached_modules,
        data: package_tarball,
        src_files_added,
        generated_files_added,
        dependencies,
    } = do_build_hex_tarball(paths, &mut config)?;

    check_for_name_squatting(&compile_result)?;
    check_for_multiple_top_level_modules(&compile_result, i_am_sure)?;
    check_for_default_main(&compile_result)?;

    // Build HTML documentation
    let docs_tarball = fs::create_tar_archive(docs::build_documentation(
        paths,
        &config,
        dependencies,
        &mut compile_result,
        DocContext::HexPublish,
        &cached_modules,
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
        println!("  - {file}");
    }
    println!("\nName: {}", config.name);
    println!("Version: {}", config.version);

    let should_publish = i_am_sure || cli::confirm("\nDo you wish to publish this package?")?;
    if !should_publish {
        println!("Not publishing.");
        return Ok(());
    }

    let runtime = tokio::runtime::Runtime::new().expect("Unable to start Tokio async runtime");
    let hex_config = hexpm::Config::new();
    let api_key =
        crate::hex::HexAuthentication::new(&runtime, hex_config.clone()).get_or_create_api_key()?;
    let start = Instant::now();
    cli::print_publishing(&config.name, &config.version);

    runtime.block_on(hex::publish_package(
        package_tarball,
        config.version.to_string(),
        config.name.to_string(),
        &api_key,
        &hex_config,
        replace,
        &HttpClient::new(),
    ))?;

    cli::print_publishing_documentation();
    runtime.block_on(hex::publish_documentation(
        &config.name,
        &config.version,
        docs_tarball,
        &api_key,
        &hex_config,
        &HttpClient::new(),
    ))?;
    cli::print_published(start.elapsed());
    println!(
        "\nView your package at https://hex.pm/packages/{}",
        &config.name
    );

    // Prompt the user to make a git tag if they have not.
    let has_repo = config.repository.is_some();
    let git = PathBuf::from(".git");
    let tag_name = config.tag_for_version(&config.version);
    let git_tag = git.join("refs").join("tags").join(&tag_name);
    if has_repo && git.exists() && !git_tag.exists() {
        println!(
            "
Please push a git tag for this release so source code links in the
HTML documentation will work:

    git tag {tag_name}
    git push origin {tag_name}
"
        )
    }
    Ok(())
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

    if module.ast.definitions_len() > 2 {
        return Ok(());
    }

    let Some(main) = module
        .ast
        .definitions
        .functions
        .iter()
        .find_map(|function| function.main_function())
    else {
        return Ok(());
    };

    if let Some(first) = &main.body.first()
        && first.is_println()
    {
        return Err(Error::HexPackageSquatting);
    }

    Ok(())
}

/// Checks if publishing packages contain default main functions.
/// Main functions with documentation are considered intentional and allowed.
fn check_for_default_main(package: &Package) -> Result<(), Error> {
    let package_name = &package.config.name;

    let has_default_main = package
        .modules
        .iter()
        .flat_map(|module| module.ast.definitions.functions.iter())
        .filter_map(|function| function.main_function())
        .any(|main| main.documentation.is_none() && is_default_main(main, package_name));

    if has_default_main {
        return Err(Error::CannotPublishWithDefaultMain {
            package_name: package_name.clone(),
        });
    }

    Ok(())
}

fn is_default_main(main: &TypedFunction, package_name: &EcoString) -> bool {
    if main.body.len() != 1 {
        return false;
    }

    let Some(Statement::Expression(expression)) = main.body.first() else {
        return false;
    };

    if !expression.is_println() {
        return false;
    }

    match expression {
        TypedExpr::Call { arguments, .. } => {
            if arguments.len() != 1 {
                return false;
            }

            match arguments.first() {
                Some(CallArg {
                    value: TypedExpr::String { value, .. },
                    ..
                }) => {
                    let default_argument = format!("Hello from {}!", &package_name);
                    value == &default_argument
                }
                _ => false,
            }
        }
        _ => false,
    }
}

fn check_for_multiple_top_level_modules(package: &Package, i_am_sure: bool) -> Result<(), Error> {
    // Collect top-level module names
    let mut top_level_module_names = package
        .modules
        .iter()
        .filter_map(|module| {
            // Top-level modules are those that don't contain any path separators
            if module.name.contains('/') {
                None
            } else {
                Some(module.name.clone())
            }
        })
        .collect::<Vec<_>>();

    // Remove duplicates
    top_level_module_names.sort_unstable();
    top_level_module_names.dedup();

    // If more than one top-level module name is found, prompt for confirmation
    if top_level_module_names.len() > 1 {
        let text = wrap(&format!(
            "Your package defines multiple top-level modules: {}.

Defining multiple top-level modules can lead to namespace pollution \
and potential conflicts for consumers.

To fix this, move all your modules under a single top-level module of your choice.

For example:
  src/{1}.gleam
  src/{1}/module1.gleam
  src/{1}/module2.gleam",
            top_level_module_names.join(", "),
            package.config.name
        ));
        println!("{text}\n");

        let should_publish =
            i_am_sure || cli::confirm("\nDo you wish to continue publishing this package?")?;
        println!();

        if !should_publish {
            println!("Not publishing.");
            std::process::exit(0);
        }
    }

    Ok(())
}

fn check_repo_url(config: &PackageConfig, i_am_sure: bool) -> Result<bool, Error> {
    let Some(repo) = config.repository.as_ref() else {
        return Ok(true);
    };
    let url = repo.url();

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
fn check_for_version_zero(config: &PackageConfig) -> Result<bool, Error> {
    if config.version.major != 0 {
        return Ok(true);
    }

    println!(
        "You are about to publish a release that is below version 1.0.0.

Semantic versioning doesn't apply to version 0.x.x releases, so your
users will not be protected from breaking changes. This can result
in a poor user experience where packages can break unexpectedly with
updates that would normally be safe.

If your package is not ready to be used in production it should not
be published.
\n"
    );
    let should_publish = cli::confirm_with_text("I am not using semantic versioning")?;
    println!();
    Ok(should_publish)
}

/// Ask for confirmation if the package name if `gleam_*`
fn check_for_gleam_prefix(config: &PackageConfig) -> Result<bool, Error> {
    if !config.name.starts_with("gleam_") || config.name.starts_with("gleam_community_") {
        return Ok(true);
    }

    println!(
        "You are about to publish a package with a name that starts with
the prefix `gleam_`, which is preferred for packages maintained by the
Gleam core team.

Security: do not assume the owner of a package from the name, always check
the maintainers listed on https://hex.pm/.
\n",
    );
    let password = cli::ask_password("Please enter the core team password to continue")?;
    println!();
    Ok(password == CORE_TEAM_PUBLISH_PASSWORD)
}

struct Tarball {
    compile_result: Package,
    cached_modules: im::HashMap<EcoString, type_::ModuleInterface>,
    data: Vec<u8>,
    src_files_added: Vec<Utf8PathBuf>,
    generated_files_added: Vec<(Utf8PathBuf, String)>,
    dependencies: HashMap<EcoString, Dependency>,
}

pub fn build_hex_tarball(paths: &ProjectPaths, config: &mut PackageConfig) -> Result<Vec<u8>> {
    let Tarball { data, .. } = do_build_hex_tarball(paths, config)?;
    Ok(data)
}

fn do_build_hex_tarball(paths: &ProjectPaths, config: &mut PackageConfig) -> Result<Tarball> {
    let target = config.target;
    check_config_for_publishing(config)?;

    // Reset the build directory so we know the state of the project
    fs::delete_directory(&paths.build_directory_for_target(Mode::Prod, target))?;

    let manifest = build::download_dependencies(paths, cli::Reporter::new())?;
    let dependencies = manifest
        .packages
        .iter()
        .map(|package| {
            (
                package.name.clone(),
                Dependency {
                    version: package.version.clone(),
                    kind: match &package.source {
                        ManifestPackageSource::Hex { .. } => DependencyKind::Hex,
                        ManifestPackageSource::Git { .. } => DependencyKind::Git,
                        ManifestPackageSource::Local { .. } => DependencyKind::Path,
                    },
                },
            )
        })
        .collect();

    // Build the project to check that it is valid
    let built = build::main(
        paths,
        Options {
            root_target_support: TargetSupport::Enforced,
            warnings_as_errors: false,
            mode: Mode::Prod,
            target: Some(target),
            codegen: Codegen::All,
            compile: Compile::All,
            no_print_progress: false,
        },
        manifest,
    )?;

    let minimum_required_version = built.minimum_required_version();
    match &config.gleam_version {
        // If the package has no explicit `gleam` version in its `gleam.toml`
        // then we want to add the automatically inferred one so we know it's
        // correct and folks getting the package from Hex won't have unpleasant
        // surprises if the author forgot to manualy write it down.
        None => {
            // If we're automatically adding the minimum required version
            // constraint we want it to at least be `>= 1.0.0`, even if the
            // inferred lower bound could be lower.
            let minimum_required_version =
                std::cmp::max(minimum_required_version, Version::new(1, 0, 0));
            let inferred_version_range = pubgrub::Range::higher_than(minimum_required_version);
            config.gleam_version = Some(GleamVersion::from_pubgrub(inferred_version_range));
        }
        // Otherwise we need to check that the annotated version range is
        // correct and includes the minimum required version.
        Some(gleam_version) => {
            if let Some(lowest_allowed_version) = gleam_version.lowest_version()
                && lowest_allowed_version < minimum_required_version
            {
                return Err(Error::CannotPublishWrongVersion {
                    minimum_required_version: SmallVersion::from_hexpm(minimum_required_version),
                    wrongfully_allowed_version: SmallVersion::from_hexpm(lowest_allowed_version),
                });
            }
        }
    }

    // If any of the modules in the package contain a todo or an echo then
    // refuse to publish as the package is not yet finished.
    let mut modules_containing_todo = vec![];
    let mut modules_containing_echo = vec![];

    for module in built.root_package.modules.iter() {
        if module.ast.type_info.contains_todo() {
            modules_containing_todo.push(module.name.clone());
        } else if module.ast.type_info.contains_echo {
            modules_containing_echo.push(module.name.clone());
        }
    }

    if !modules_containing_todo.is_empty() {
        return Err(Error::CannotPublishTodo {
            unfinished: modules_containing_todo,
        });
    }

    if !modules_containing_echo.is_empty() {
        return Err(Error::CannotPublishEcho {
            unfinished: modules_containing_echo,
        });
    }

    // empty_modules is a list of modules that do not export any values or types.
    // We do not allow publishing packages that contain empty modules.
    let empty_modules: Vec<_> = built
        .root_package
        .modules
        .iter()
        .filter(|module| {
            built
                .module_interfaces
                .get(&module.name)
                .map(|interface| {
                    // Check if the module exports any values or types
                    interface.values.is_empty() && interface.types.is_empty()
                })
                .unwrap_or(false)
        })
        .map(|module| module.name.clone())
        .collect();

    if !empty_modules.is_empty() {
        return Err(Error::CannotPublishEmptyModules {
            unfinished: empty_modules,
        });
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
    let src_files = project_files(Utf8Path::new(""))?;
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
        cached_modules: built.module_interfaces,
        data: tarball,
        src_files_added: src_files,
        generated_files_added: generated_files,
        dependencies,
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
    let repo_url = http::Uri::try_from(
        config
            .repository
            .as_ref()
            .map(|r| r.url())
            .unwrap_or_default(),
    )
    .ok();
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

fn project_files(base_path: &Utf8Path) -> Result<Vec<Utf8PathBuf>> {
    let src = base_path.join(Utf8Path::new("src"));
    let mut files: Vec<Utf8PathBuf> = fs::gleam_files(&src)
        .chain(fs::native_files(&src))
        .collect();
    let private = base_path.join(Utf8Path::new("priv"));
    let mut private_files: Vec<Utf8PathBuf> = fs::private_files(&private).collect();
    files.append(&mut private_files);
    let mut add = |path| {
        let path = base_path.join(path);
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
        // Do not publish test/ and dev/ code
        if !module.origin.is_src() {
            continue;
        }

        let name = module.compiled_erlang_path();
        files.push((tar_src.join(&name), fs::read(build.join(name))?));
    }

    // Erlang headers
    if include.is_dir() {
        for file in fs::erlang_files(&include) {
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

impl ReleaseMetadata<'_> {
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
impl ReleaseRequirement<'_> {
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
    let req1 = Range::new("~> 1.2.3 or >= 5.0.0".into()).unwrap();
    let req2 = Range::new("~> 1.2".into()).unwrap();
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
    let config = PackageConfig {
        dependencies: [("provided".into(), Requirement::path("./path/to/package"))].into(),
        ..Default::default()
    };
    assert_eq!(
        metadata_config(&config, &[], &[]),
        Err(Error::PublishNonHexDependencies {
            package: "provided".into()
        })
    );
}

#[test]
fn prevent_publish_git_dependency() {
    let config = PackageConfig {
        dependencies: [(
            "provided".into(),
            Requirement::git("https://github.com/gleam-lang/gleam.git", "da6e917"),
        )]
        .into(),
        ..Default::default()
    };
    assert_eq!(
        metadata_config(&config, &[], &[]),
        Err(Error::PublishNonHexDependencies {
            package: "provided".into()
        })
    );
}

fn quotes(x: &str) -> String {
    format!(r#"<<"{x}">>"#)
}

#[test]
fn exported_project_files_test() {
    let tmp = tempfile::tempdir().unwrap();
    let path = Utf8PathBuf::from_path_buf(tmp.path().join("my_project")).expect("Non Utf8 Path");

    let exported_project_files = &[
        "LICENCE",
        "LICENCE.md",
        "LICENCE.txt",
        "LICENSE",
        "LICENSE.md",
        "LICENSE.txt",
        "NOTICE",
        "NOTICE.md",
        "NOTICE.txt",
        "README",
        "README.md",
        "README.txt",
        "gleam.toml",
        "priv/ignored",
        "priv/wibble",
        "priv/wobble.js",
        "src/.hidden/hidden_ffi.erl",
        "src/.hidden/hidden_ffi.mjs",
        "src/.hidden_ffi.erl",
        "src/.hidden_ffi.mjs",
        "src/exported.gleam",
        "src/exported_ffi.erl",
        "src/exported_ffi.ex",
        "src/exported_ffi.hrl",
        "src/exported_ffi.js",
        "src/exported_ffi.mjs",
        "src/exported_ffi.ts",
        "src/ignored.gleam",
        "src/ignored_ffi.erl",
        "src/ignored_ffi.mjs",
        "src/nested/exported.gleam",
        "src/nested/exported_ffi.erl",
        "src/nested/exported_ffi.ex",
        "src/nested/exported_ffi.hrl",
        "src/nested/exported_ffi.js",
        "src/nested/exported_ffi.mjs",
        "src/nested/exported_ffi.ts",
        "src/nested/ignored.gleam",
        "src/nested/ignored_ffi.erl",
        "src/nested/ignored_ffi.mjs",
    ];

    let unexported_project_files = &[
        ".git/",
        ".github/workflows/test.yml",
        ".gitignore",
        "build/",
        "ignored.txt",
        "src/.hidden/hidden.gleam", // Not a valid Gleam module path
        "src/.hidden.gleam",        // Not a valid Gleam module name
        "src/also-ignored.gleam",   // Not a valid Gleam module name
        "test/exported_test.gleam",
        "test/exported_test_ffi.erl",
        "test/exported_test_ffi.ex",
        "test/exported_test_ffi.hrl",
        "test/exported_test_ffi.js",
        "test/exported_test_ffi.mjs",
        "test/exported_test_ffi.ts",
        "test/ignored_test.gleam",
        "test/ignored_test_ffi.erl",
        "test/ignored_test_ffi.mjs",
        "test/nested/exported_test.gleam",
        "test/nested/exported_test_ffi.erl",
        "test/nested/exported_test_ffi.ex",
        "test/nested/exported_test_ffi.hrl",
        "test/nested/exported_test_ffi.js",
        "test/nested/exported_test_ffi.mjs",
        "test/nested/exported_test_ffi.ts",
        "test/nested/ignored.gleam",
        "test/nested/ignored_test_ffi.erl",
        "test/nested/ignored_test_ffi.mjs",
        "dev/exported_test_ffi.erl",
        "dev/exported_test_ffi.ex",
        "dev/exported_test_ffi.hrl",
        "dev/exported_test_ffi.js",
        "dev/exported_test_ffi.mjs",
        "dev/exported_test_ffi.ts",
        "dev/ignored_test.gleam",
        "dev/ignored_test_ffi.erl",
        "dev/ignored_test_ffi.mjs",
        "dev/nested/exported_test.gleam",
        "dev/nested/exported_test_ffi.erl",
        "dev/nested/exported_test_ffi.ex",
        "dev/nested/exported_test_ffi.hrl",
        "dev/nested/exported_test_ffi.js",
        "dev/nested/exported_test_ffi.mjs",
        "dev/nested/exported_test_ffi.ts",
        "dev/nested/ignored.gleam",
        "dev/nested/ignored_test_ffi.erl",
        "dev/nested/ignored_test_ffi.mjs",
        "unrelated-file.txt",
    ];

    let gitignore = "ignored*
src/also-ignored.gleam";

    for &file in exported_project_files
        .iter()
        .chain(unexported_project_files)
    {
        if file.ends_with("/") {
            fs::mkdir(path.join(file)).unwrap();
            continue;
        }

        let contents = match file {
            ".gitignore" => gitignore,
            _ => "",
        };

        fs::write(&path.join(file), contents).unwrap();
    }

    let mut chosen_exported_files = project_files(&path).unwrap();
    chosen_exported_files.sort_unstable();

    let expected_exported_files = exported_project_files
        .iter()
        .map(|s| path.join(s))
        .collect_vec();

    assert_eq!(expected_exported_files, chosen_exported_files);
}
