use std::{
    collections::{HashMap, HashSet},
    time::Instant,
};

use flate2::read::GzDecoder;
use futures::future;
use gleam_core::{
    build::Mode,
    config::PackageConfig,
    error::{FileIoAction, FileKind},
    hex::{self, HEXPM_PUBLIC_KEY},
    io::{HttpClient as _, TarUnpacker, Utf8Writer, WrappedReader},
    paths, Error, Result,
};
use hexpm::version::{Range, Version};
use itertools::Itertools;

use crate::{
    cli,
    fs::{self, FileSystemAccessor},
    http::HttpClient,
};

pub fn download() -> Result<Manifest> {
    let span = tracing::info_span!("dependencies");
    let _enter = span.enter();
    let mode = Mode::Dev;

    let http = HttpClient::boxed();
    let fs = FileSystemAccessor::boxed();
    let downloader = hex::Downloader::new(fs, http, Untar::boxed());

    // Read the project config
    let config = crate::config::root_config()?;
    let project_name = config.name.clone();

    // Start event loop so we can run async functions to call the Hex API
    let runtime = tokio::runtime::Runtime::new().expect("Unable to start Tokio async runtime");

    // Determine what versions we need
    let (manifest_updated, manifest) = get_manifest(runtime.handle().clone(), mode, &config)?;
    let local = LocalPackages::read_from_disc()?;

    // Remove any packages that are no longer required due to gleam.toml changes
    remove_extra_packages(&local, &manifest)?;

    // Download them from Hex to the local cache
    runtime.block_on(download_missing_packages(
        downloader,
        &manifest,
        &local,
        project_name,
    ))?;

    // Record new state of the packages directory
    if manifest_updated {
        tracing::info!("writing_manifest_toml");
        manifest.write_to_disc()?;
    }
    LocalPackages::from_manifest(&manifest).write_to_disc()?;

    Ok(manifest)
}

async fn download_missing_packages(
    downloader: hex::Downloader,
    manifest: &Manifest,
    local: &LocalPackages,
    project_name: String,
) -> Result<(), Error> {
    let missing = local.missing_local_packages(manifest, &project_name);
    if !missing.is_empty() {
        let start = Instant::now();
        cli::print_downloading("packages");
        downloader
            .download_hex_packages(&missing, &project_name)
            .await?;
        cli::print_packages_downloaded(start, missing.len());
    }
    Ok(())
}

fn remove_extra_packages(local: &LocalPackages, manifest: &Manifest) -> Result<()> {
    for (package, version) in local.extra_local_packages(manifest) {
        let path = paths::build_deps_package(&package);
        if path.exists() {
            tracing::info!(package=%package, version=%version, "removing_unneeded_package");
            fs::delete_dir(&path)?;
        }
    }
    Ok(())
}

#[derive(Debug, Clone, serde::Serialize, serde::Deserialize, PartialEq, Eq)]
pub struct Manifest {
    #[serde(serialize_with = "ordered_map")]
    pub requirements: HashMap<String, Range>,
    #[serde(serialize_with = "sorted_vec")]
    pub packages: Vec<ManifestPackage>,
}

impl Manifest {
    pub fn read_from_disc() -> Result<Self> {
        tracing::info!("Reading manifest.toml");
        let manifest_path = paths::manifest();
        let toml = crate::fs::read(&manifest_path)?;
        let manifest = toml::from_str(&toml).map_err(|e| Error::FileIo {
            action: FileIoAction::Parse,
            kind: FileKind::File,
            path: manifest_path.clone(),
            err: Some(e.to_string()),
        })?;
        Ok(manifest)
    }

    pub fn write_to_disc(&self) -> Result<()> {
        let path = paths::manifest();
        let mut file = fs::writer(&path)?;
        let result = self.write_to(&mut file);
        file.wrap_result(result)?;
        Ok(())
    }

    // Rather than using the toml library to do serialization we implement it
    // manually so that we can control the formatting.
    // We want to keep entries on a single line each so that they are more
    // resistant to merge conflicts and are easier to fix when it does happen.
    pub fn write_to<W: std::fmt::Write>(&self, mut buffer: W) -> std::fmt::Result {
        let Self {
            requirements,
            packages,
        } = self;
        write!(
            buffer,
            "# This file was generated by Gleam
# You typically do not need to edit this file

",
        )?;

        // Packages
        writeln!(buffer, "packages = [")?;
        for ManifestPackage {
            name,
            version,
            otp_app,
            build_tools,
            outer_checksum,
        } in packages.iter().sorted_by(|a, b| a.name.cmp(&b.name))
        {
            write!(
                buffer,
                "  {{ name = \"{name}\", version = \"{version}\", outer_checksum = \"{checksum}\", build_tools = [",
                name = name,
                version = version,
                checksum = outer_checksum.to_string(),
            )?;
            for (i, tool) in build_tools.iter().enumerate() {
                if i != 0 {
                    write!(buffer, ", ")?;
                }
                write!(buffer, "\"{}\"", tool)?;
            }
            write!(buffer, "]")?;
            if let Some(app) = otp_app {
                write!(buffer, ", otp_app = \"{}\"", app)?;
            }
            writeln!(buffer, " }},")?;
        }
        write!(buffer, "]\n\n")?;

        // Requirements
        writeln!(buffer, "[requirements]")?;
        for (name, range) in requirements.iter().sorted_by(|a, b| a.0.cmp(b.0)) {
            writeln!(buffer, "{} = \"{}\"", name, range)?;
        }

        Ok(())
    }
}

#[test]
fn manifest_toml_format() {
    let mut buffer = String::new();
    let mut manifest = Manifest {
        requirements: [
            ("zzz".to_string(), Range::new("> 0.0.0".to_string())),
            ("aaa".to_string(), Range::new("> 0.0.0".to_string())),
            (
                "gleam_stdlib".to_string(),
                Range::new("~> 0.17".to_string()),
            ),
            ("gleeunit".to_string(), Range::new("~> 0.1".to_string())),
        ]
        .into(),
        packages: vec![
            ManifestPackage {
                name: "gleam_stdlib".to_string(),
                version: Version::new(0, 17, 1),
                build_tools: ["gleam".into()].into(),
                outer_checksum: Base16Checksum(vec![1, 22]),
                otp_app: None,
            },
            ManifestPackage {
                name: "aaa".to_string(),
                version: Version::new(0, 4, 0),
                build_tools: ["rebar3".into(), "make".into()].into(),
                outer_checksum: Base16Checksum(vec![3, 22]),
                otp_app: Some("aaa_app".into()),
            },
            ManifestPackage {
                name: "zzz".to_string(),
                version: Version::new(0, 4, 0),
                build_tools: ["mix".into()].into(),
                outer_checksum: Base16Checksum(vec![3, 22]),
                otp_app: None,
            },
            ManifestPackage {
                name: "gleeunit".to_string(),
                version: Version::new(0, 4, 0),
                build_tools: ["gleam".into()].into(),
                outer_checksum: Base16Checksum(vec![3, 46]),
                otp_app: None,
            },
        ],
    };
    manifest.write_to(&mut buffer).unwrap();
    assert_eq!(
        buffer,
        r#"# This file was generated by Gleam
# You typically do not need to edit this file

packages = [
  { name = "aaa", version = "0.4.0", outer_checksum = "0316", build_tools = ["rebar3", "make"], otp_app = "aaa_app" },
  { name = "gleam_stdlib", version = "0.17.1", outer_checksum = "0116", build_tools = ["gleam"] },
  { name = "gleeunit", version = "0.4.0", outer_checksum = "032E", build_tools = ["gleam"] },
  { name = "zzz", version = "0.4.0", outer_checksum = "0316", build_tools = ["mix"] },
]

[requirements]
aaa = "> 0.0.0"
gleam_stdlib = "~> 0.17"
gleeunit = "~> 0.1"
zzz = "> 0.0.0"
"#
    );
    let deserialised: Manifest = toml::from_str(&buffer).unwrap();
    manifest.packages.sort_by(|a, b| a.name.cmp(&b.name));
    assert_eq!(deserialised, manifest);
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct Base16Checksum(Vec<u8>);

impl ToString for Base16Checksum {
    fn to_string(&self) -> String {
        base16::encode_upper(&self.0)
    }
}

impl serde::Serialize for Base16Checksum {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        serializer.serialize_str(&base16::encode_upper(&self.0))
    }
}

impl<'de> serde::Deserialize<'de> for Base16Checksum {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        let s: &str = serde::de::Deserialize::deserialize(deserializer)?;
        base16::decode(s)
            .map(Base16Checksum)
            .map_err(serde::de::Error::custom)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, serde::Serialize, serde::Deserialize)]
pub struct ManifestPackage {
    pub name: String,
    pub version: Version,
    pub build_tools: Vec<String>,
    pub outer_checksum: Base16Checksum,
    #[serde(default)]
    pub otp_app: Option<String>,
}

fn ordered_map<S, K, V>(value: &HashMap<K, V>, serializer: S) -> Result<S::Ok, S::Error>
where
    S: serde::Serializer,
    K: serde::Serialize + Ord,
    V: serde::Serialize,
{
    use serde::Serialize;
    let ordered: std::collections::BTreeMap<_, _> = value.iter().collect();
    ordered.serialize(serializer)
}

fn sorted_vec<S, T>(value: &[T], serializer: S) -> Result<S::Ok, S::Error>
where
    S: serde::Serializer,
    T: serde::Serialize + Ord,
{
    use serde::Serialize;
    let mut value: Vec<&T> = value.iter().collect();
    value.sort();
    value.serialize(serializer)
}

#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
struct LocalPackages {
    packages: HashMap<String, Version>,
}

impl LocalPackages {
    pub fn extra_local_packages(&self, manifest: &Manifest) -> Vec<(String, Version)> {
        let manifest_packages: HashSet<_> = manifest
            .packages
            .iter()
            .map(|p| (&p.name, &p.version))
            .collect();
        self.packages
            .iter()
            .filter(|(n, v)| !manifest_packages.contains(&(n, v)))
            .map(|(n, v)| (n.clone(), v.clone()))
            .collect()
    }

    pub fn missing_local_packages(
        &self,
        manifest: &Manifest,
        root: &str,
    ) -> Vec<(String, Version)> {
        manifest
            .packages
            .iter()
            .filter(|p| p.name != root && self.packages.get(&p.name) != Some(&p.version))
            .map(|p| (p.name.clone(), p.version.clone()))
            .collect()
    }

    pub fn read_from_disc() -> Result<Self> {
        let path = paths::packages_toml();
        if !path.exists() {
            return Ok(Self {
                packages: HashMap::new(),
            });
        }
        let toml = crate::fs::read(&path)?;
        toml::from_str(&toml).map_err(|e| Error::FileIo {
            action: FileIoAction::Parse,
            kind: FileKind::File,
            path: path.clone(),
            err: Some(e.to_string()),
        })
    }

    pub fn write_to_disc(&self) -> Result<()> {
        let path = paths::packages_toml();
        let toml = toml::to_string(&self).expect("packages.toml serialization");
        fs::write(&path, &toml)
    }

    pub fn from_manifest(manifest: &Manifest) -> Self {
        Self {
            packages: manifest
                .packages
                .iter()
                .map(|p| (p.name.clone(), p.version.clone()))
                .collect(),
        }
    }
}

#[test]
fn missing_local_packages() {
    let mut extra = LocalPackages {
        packages: [
            ("local2".to_string(), Version::parse("2.0.0").unwrap()),
            ("local3".to_string(), Version::parse("3.0.0").unwrap()),
        ]
        .into(),
    }
    .missing_local_packages(
        &Manifest {
            requirements: HashMap::new(),
            packages: vec![
                ManifestPackage {
                    name: "root".to_string(),
                    version: Version::parse("1.0.0").unwrap(),
                    build_tools: ["gleam".into()].into(),
                    outer_checksum: Base16Checksum(vec![1, 2, 3, 4]),
                    otp_app: None,
                },
                ManifestPackage {
                    name: "local1".to_string(),
                    version: Version::parse("1.0.0").unwrap(),
                    build_tools: ["gleam".into()].into(),
                    outer_checksum: Base16Checksum(vec![1, 2, 3, 4, 5]),
                    otp_app: None,
                },
                ManifestPackage {
                    name: "local2".to_string(),
                    version: Version::parse("3.0.0").unwrap(),
                    build_tools: ["gleam".into()].into(),
                    outer_checksum: Base16Checksum(vec![1, 2, 3, 4, 5]),
                    otp_app: None,
                },
            ],
        },
        "root",
    );
    extra.sort();
    assert_eq!(
        extra,
        [
            ("local1".to_string(), Version::new(1, 0, 0)),
            ("local2".to_string(), Version::new(3, 0, 0)),
        ]
    )
}

#[test]
fn extra_local_packages() {
    let mut extra = LocalPackages {
        packages: [
            ("local1".to_string(), Version::parse("1.0.0").unwrap()),
            ("local2".to_string(), Version::parse("2.0.0").unwrap()),
            ("local3".to_string(), Version::parse("3.0.0").unwrap()),
        ]
        .into(),
    }
    .extra_local_packages(&Manifest {
        requirements: HashMap::new(),
        packages: vec![
            ManifestPackage {
                name: "local1".to_string(),
                version: Version::parse("1.0.0").unwrap(),
                build_tools: ["gleam".into()].into(),
                outer_checksum: Base16Checksum(vec![1, 2, 3, 4, 5]),
                otp_app: None,
            },
            ManifestPackage {
                name: "local2".to_string(),
                version: Version::parse("3.0.0").unwrap(),
                build_tools: ["gleam".into()].into(),
                outer_checksum: Base16Checksum(vec![4, 5]),
                otp_app: None,
            },
        ],
    });
    extra.sort();
    assert_eq!(
        extra,
        [
            ("local2".to_string(), Version::new(2, 0, 0)),
            ("local3".to_string(), Version::new(3, 0, 0)),
        ]
    )
}

fn get_manifest(
    runtime: tokio::runtime::Handle,
    mode: Mode,
    config: &PackageConfig,
) -> Result<(bool, Manifest)> {
    // If there's no manifest then resolve the versions anew
    if !paths::manifest().exists() {
        tracing::info!("manifest_not_present");
        let manifest = resolve_versions(runtime, mode, config, &[])?;
        return Ok((true, manifest));
    }

    let manifest = Manifest::read_from_disc()?;

    // If the config has unchanged since the manifest was written then it is up
    // to date so we can return it unmodified.
    if manifest.requirements == config.all_dependencies()? {
        tracing::info!("manifest_up_to_date");
        Ok((false, manifest))
    } else {
        tracing::info!("manifest_outdated");
        let manifest = resolve_versions(runtime, mode, config, &manifest.packages)?;
        Ok((true, manifest))
    }
}

fn resolve_versions(
    runtime: tokio::runtime::Handle,
    mode: Mode,
    config: &PackageConfig,
    locked: &[ManifestPackage],
) -> Result<Manifest, Error> {
    cli::print_resolving_versions();
    let locked = locked
        .iter()
        // TODO: remove clones. Will require library modification.
        .map(|p| (p.name.clone(), p.version.clone()))
        .collect();
    let resolved = hex::resolve_versions(
        PackageFetcher::boxed(runtime.clone()),
        mode,
        config,
        &locked,
    )?;
    let packages = runtime.block_on(future::try_join_all(
        resolved
            .into_iter()
            .map(|(name, version)| lookup_package(name, version)),
    ))?;
    let manifest = Manifest {
        packages,
        requirements: config.all_dependencies()?,
    };
    Ok(manifest)
}

async fn lookup_package(name: String, version: Version) -> Result<ManifestPackage> {
    let config = hexpm::Config::new();
    let release = hex::get_package_release(&name, &version, &config, &HttpClient::new()).await?;
    let manifest = ManifestPackage {
        name,
        version,
        otp_app: Some(release.meta.app),
        build_tools: release.meta.build_tools,
        outer_checksum: Base16Checksum(release.outer_checksum),
    };
    Ok(manifest)
}

struct PackageFetcher {
    runtime: tokio::runtime::Handle,
    http: HttpClient,
}

impl PackageFetcher {
    pub fn boxed(runtime: tokio::runtime::Handle) -> Box<Self> {
        Box::new(Self {
            runtime,
            http: HttpClient::new(),
        })
    }
}

#[derive(Debug)]
pub struct Untar;

impl Untar {
    pub fn boxed() -> Box<Self> {
        Box::new(Self)
    }
}

impl TarUnpacker for Untar {
    fn io_result_entries<'a>(
        &self,
        archive: &'a mut tar::Archive<WrappedReader>,
    ) -> std::io::Result<tar::Entries<'a, WrappedReader>> {
        archive.entries()
    }

    fn io_result_unpack(
        &self,
        path: &std::path::Path,
        mut archive: tar::Archive<GzDecoder<tar::Entry<'_, WrappedReader>>>,
    ) -> std::io::Result<()> {
        archive.unpack(path)
    }
}

impl hexpm::version::PackageFetcher for PackageFetcher {
    fn get_dependencies(
        &self,
        package: &str,
    ) -> Result<hexpm::Package, Box<dyn std::error::Error>> {
        tracing::info!(package = package, "Looking up package in Hex API");
        let config = hexpm::Config::new();
        let request = hexpm::get_package_request(package, None, &config);
        let response = self
            .runtime
            .block_on(self.http.send(request))
            .map_err(Box::new)?;
        hexpm::get_package_response(response, HEXPM_PUBLIC_KEY).map_err(|e| e.into())
    }
}
