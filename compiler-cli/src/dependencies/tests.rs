use std::collections::HashMap;

use camino::{Utf8Path, Utf8PathBuf};
use ecow::EcoString;
use hexpm::version::Version;
use pretty_assertions::assert_eq;

use gleam_core::{
    Error,
    build::Runtime,
    config::{DenoConfig, DenoFlag, Docs, ErlangConfig, JavaScriptConfig},
    manifest::{Base16Checksum, Manifest, ManifestPackage, ManifestPackageSource},
    paths::ProjectPaths,
    requirement::Requirement,
};

use crate::dependencies::*;

#[test]
fn list_manifest_format() {
    let mut buffer = vec![];
    let manifest = Manifest {
        requirements: HashMap::new(),
        packages: vec![
            ManifestPackage {
                name: "root".into(),
                version: Version::parse("1.0.0").unwrap(),
                build_tools: ["gleam".into()].into(),
                otp_app: None,
                requirements: vec![],
                source: ManifestPackageSource::Hex {
                    outer_checksum: Base16Checksum(vec![1, 2, 3, 4]),
                },
            },
            ManifestPackage {
                name: "aaa".into(),
                version: Version::new(0, 4, 2),
                build_tools: ["rebar3".into(), "make".into()].into(),
                otp_app: Some("aaa_app".into()),
                requirements: vec!["zzz".into(), "gleam_stdlib".into()],
                source: ManifestPackageSource::Hex {
                    outer_checksum: Base16Checksum(vec![3, 22]),
                },
            },
            ManifestPackage {
                name: "zzz".into(),
                version: Version::new(0, 4, 0),
                build_tools: ["mix".into()].into(),
                otp_app: None,
                requirements: vec![],
                source: ManifestPackageSource::Hex {
                    outer_checksum: Base16Checksum(vec![3, 22]),
                },
            },
        ],
    };
    list_manifest_packages(&mut buffer, manifest).unwrap();
    assert_eq!(
        std::str::from_utf8(&buffer).unwrap(),
        "Package  Version
-------  -------
root     1.0.0
aaa      0.4.2
zzz      0.4.0
"
    )
}

#[test]
fn tree_format() {
    let mut buffer = vec![];
    let manifest = Manifest {
        requirements: HashMap::new(),
        packages: vec![
            ManifestPackage {
                name: "deps_proj".into(),
                version: Version::parse("1.0.0").unwrap(),
                build_tools: [].into(),
                otp_app: None,
                requirements: vec!["gleam_regexp".into(), "gleam_stdlib".into()],
                source: ManifestPackageSource::Hex {
                    outer_checksum: Base16Checksum(vec![1, 2, 3, 4]),
                },
            },
            ManifestPackage {
                name: "gleam_stdlib".into(),
                version: Version::new(0, 52, 0),
                build_tools: ["rebar3".into(), "make".into()].into(),
                otp_app: Some("aaa_app".into()),
                requirements: vec![],
                source: ManifestPackageSource::Hex {
                    outer_checksum: Base16Checksum(vec![3, 22]),
                },
            },
            ManifestPackage {
                name: "gleam_regexp".into(),
                version: Version::new(1, 0, 0),
                build_tools: ["mix".into()].into(),
                otp_app: None,
                requirements: vec!["gleam_stdlib".into()],
                source: ManifestPackageSource::Hex {
                    outer_checksum: Base16Checksum(vec![3, 22]),
                },
            },
        ],
    };

    let options = TreeOptions {
        package: None,
        invert: None,
    };

    let root_package_name = EcoString::from("deps_proj");

    list_package_and_dependencies_tree(
        &mut buffer,
        options,
        manifest.packages.clone(),
        root_package_name,
    )
    .unwrap();
    assert_eq!(
        std::str::from_utf8(&buffer).unwrap(),
        r#"deps_proj v1.0.0
├── gleam_regexp v1.0.0
│   └── gleam_stdlib v0.52.0
└── gleam_stdlib v0.52.0
"#
    )
}

#[test]
fn tree_package_format() {
    let mut buffer = vec![];
    let manifest = Manifest {
        requirements: HashMap::new(),
        packages: vec![
            ManifestPackage {
                name: "gleam_stdlib".into(),
                version: Version::new(0, 52, 0),
                build_tools: ["rebar3".into(), "make".into()].into(),
                otp_app: Some("aaa_app".into()),
                requirements: vec![],
                source: ManifestPackageSource::Hex {
                    outer_checksum: Base16Checksum(vec![3, 22]),
                },
            },
            ManifestPackage {
                name: "deps_proj".into(),
                version: Version::parse("1.0.0").unwrap(),
                build_tools: [].into(),
                otp_app: None,
                requirements: vec!["gleam_stdlib".into(), "gleam_regexp".into()],
                source: ManifestPackageSource::Hex {
                    outer_checksum: Base16Checksum(vec![1, 2, 3, 4]),
                },
            },
            ManifestPackage {
                name: "gleam_regexp".into(),
                version: Version::new(1, 0, 0),
                build_tools: ["mix".into()].into(),
                otp_app: None,
                requirements: vec!["gleam_stdlib".into()],
                source: ManifestPackageSource::Hex {
                    outer_checksum: Base16Checksum(vec![3, 22]),
                },
            },
        ],
    };
    let options = TreeOptions {
        package: Some("gleam_regexp".to_string()),
        invert: None,
    };

    let root_package_name = EcoString::from("deps_proj");

    list_package_and_dependencies_tree(
        &mut buffer,
        options,
        manifest.packages.clone(),
        root_package_name,
    )
    .unwrap();
    assert_eq!(
        std::str::from_utf8(&buffer).unwrap(),
        r#"gleam_regexp v1.0.0
└── gleam_stdlib v0.52.0
"#
    )
}

#[test]
fn tree_invert_format() {
    let mut buffer = vec![];
    let manifest = Manifest {
        requirements: HashMap::new(),
        packages: vec![
            ManifestPackage {
                name: "gleam_stdlib".into(),
                version: Version::new(0, 52, 0),
                build_tools: ["rebar3".into(), "make".into()].into(),
                otp_app: Some("aaa_app".into()),
                requirements: vec![],
                source: ManifestPackageSource::Hex {
                    outer_checksum: Base16Checksum(vec![3, 22]),
                },
            },
            ManifestPackage {
                name: "deps_proj".into(),
                version: Version::parse("1.0.0").unwrap(),
                build_tools: [].into(),
                otp_app: None,
                requirements: vec!["gleam_stdlib".into(), "gleam_regexp".into()],
                source: ManifestPackageSource::Hex {
                    outer_checksum: Base16Checksum(vec![1, 2, 3, 4]),
                },
            },
            ManifestPackage {
                name: "gleam_regexp".into(),
                version: Version::new(1, 0, 0),
                build_tools: ["mix".into()].into(),
                otp_app: None,
                requirements: vec!["gleam_stdlib".into()],
                source: ManifestPackageSource::Hex {
                    outer_checksum: Base16Checksum(vec![3, 22]),
                },
            },
        ],
    };
    let options = TreeOptions {
        package: None,
        invert: Some("gleam_stdlib".to_string()),
    };

    let root_package_name = EcoString::from("deps_proj");

    list_package_and_dependencies_tree(
        &mut buffer,
        options,
        manifest.packages.clone(),
        root_package_name,
    )
    .unwrap();
    assert_eq!(
        std::str::from_utf8(&buffer).unwrap(),
        r#"gleam_stdlib v0.52.0
├── deps_proj v1.0.0
└── gleam_regexp v1.0.0
    └── deps_proj v1.0.0
"#
    )
}

#[test]
fn list_tree_invalid_package_format() {
    let mut buffer = vec![];
    let manifest = Manifest {
        requirements: HashMap::new(),
        packages: vec![
            ManifestPackage {
                name: "gleam_stdlib".into(),
                version: Version::new(0, 52, 0),
                build_tools: ["rebar3".into(), "make".into()].into(),
                otp_app: Some("aaa_app".into()),
                requirements: vec![],
                source: ManifestPackageSource::Hex {
                    outer_checksum: Base16Checksum(vec![3, 22]),
                },
            },
            ManifestPackage {
                name: "gleam_regexp".into(),
                version: Version::new(1, 0, 0),
                build_tools: ["mix".into()].into(),
                otp_app: None,
                requirements: vec!["gleam_stdlib".into()],
                source: ManifestPackageSource::Hex {
                    outer_checksum: Base16Checksum(vec![3, 22]),
                },
            },
            ManifestPackage {
                name: "root".into(),
                version: Version::parse("1.0.0").unwrap(),
                build_tools: [].into(),
                otp_app: None,
                requirements: vec!["gleam_regexp".into(), "gleam_stdlib".into()],
                source: ManifestPackageSource::Hex {
                    outer_checksum: Base16Checksum(vec![1, 2, 3, 4]),
                },
            },
        ],
    };
    let options = TreeOptions {
        package: Some("zzzzzz".to_string()),
        invert: None,
    };

    let root_package_name = EcoString::from("deps_proj");

    list_package_and_dependencies_tree(
        &mut buffer,
        options,
        manifest.packages.clone(),
        root_package_name,
    )
    .unwrap();
    assert_eq!(
        std::str::from_utf8(&buffer).unwrap(),
        r#"Package not found. Please check the package name.
"#
    )
}

#[test]
fn parse_gleam_add_specifier_invalid_semver() {
    assert!(parse_gleam_add_specifier("some_package@1.2.3.4").is_err());
}

#[test]
fn parse_gleam_add_specifier_non_numeric_version() {
    assert!(parse_gleam_add_specifier("some_package@not_a_version").is_err());
}

#[test]
fn parse_gleam_add_specifier_default() {
    let provided = "some_package";
    let expected = Requirement::hex(">= 0.0.0").unwrap();
    let (package, version) = parse_gleam_add_specifier(provided).unwrap();
    assert_eq!(version, expected);
    assert_eq!("some_package", package);
}

#[test]
fn parse_gleam_add_specifier_major_only() {
    let provided = "wobble@1";
    let expected = Requirement::hex(">= 1.0.0 and < 2.0.0").unwrap();
    let (package, version) = parse_gleam_add_specifier(provided).unwrap();
    assert_eq!(version, expected);
    assert_eq!("wobble", package);
}

#[test]
fn parse_gleam_add_specifier_major_and_minor() {
    let provided = "wibble@1.2";
    let expected = Requirement::hex(">= 1.2.0 and < 2.0.0").unwrap();
    let (package, version) = parse_gleam_add_specifier(provided).unwrap();
    assert_eq!(version, expected);
    assert_eq!("wibble", package);
}

#[test]
fn parse_gleam_add_specifier_major_minor_and_patch() {
    let provided = "bobble@1.2.3";
    let expected = Requirement::hex("1.2.3").unwrap();
    let (package, version) = parse_gleam_add_specifier(provided).unwrap();
    assert_eq!(version, expected);
    assert_eq!("bobble", package);
}

#[test]
fn missing_local_packages() {
    let manifest = Manifest {
        requirements: HashMap::new(),
        packages: vec![
            ManifestPackage {
                name: "root".into(),
                version: Version::parse("1.0.0").unwrap(),
                build_tools: ["gleam".into()].into(),
                otp_app: None,
                requirements: vec![],
                source: ManifestPackageSource::Hex {
                    outer_checksum: Base16Checksum(vec![1, 2, 3, 4]),
                },
            },
            ManifestPackage {
                name: "local1".into(),
                version: Version::parse("1.0.0").unwrap(),
                build_tools: ["gleam".into()].into(),
                otp_app: None,
                requirements: vec![],
                source: ManifestPackageSource::Hex {
                    outer_checksum: Base16Checksum(vec![1, 2, 3, 4, 5]),
                },
            },
            ManifestPackage {
                name: "local2".into(),
                version: Version::parse("3.0.0").unwrap(),
                build_tools: ["gleam".into()].into(),
                otp_app: None,
                requirements: vec![],
                source: ManifestPackageSource::Hex {
                    outer_checksum: Base16Checksum(vec![1, 2, 3, 4, 5]),
                },
            },
        ],
    };
    let mut extra = LocalPackages {
        packages: [
            ("local2".into(), Version::parse("2.0.0").unwrap()),
            ("local3".into(), Version::parse("3.0.0").unwrap()),
        ]
        .into(),
    }
    .missing_local_packages(&manifest, "root");
    extra.sort();
    assert_eq!(
        extra,
        [
            &ManifestPackage {
                name: "local1".into(),
                version: Version::parse("1.0.0").unwrap(),
                build_tools: ["gleam".into()].into(),
                otp_app: None,
                requirements: vec![],
                source: ManifestPackageSource::Hex {
                    outer_checksum: Base16Checksum(vec![1, 2, 3, 4, 5]),
                },
            },
            &ManifestPackage {
                name: "local2".into(),
                version: Version::parse("3.0.0").unwrap(),
                build_tools: ["gleam".into()].into(),
                otp_app: None,
                requirements: vec![],
                source: ManifestPackageSource::Hex {
                    outer_checksum: Base16Checksum(vec![1, 2, 3, 4, 5]),
                },
            },
        ]
    )
}

#[test]
fn extra_local_packages() {
    let mut extra = LocalPackages {
        packages: [
            ("local1".into(), Version::parse("1.0.0").unwrap()),
            ("local2".into(), Version::parse("2.0.0").unwrap()),
            ("local3".into(), Version::parse("3.0.0").unwrap()),
        ]
        .into(),
    }
    .extra_local_packages(&Manifest {
        requirements: HashMap::new(),
        packages: vec![
            ManifestPackage {
                name: "local1".into(),
                version: Version::parse("1.0.0").unwrap(),
                build_tools: ["gleam".into()].into(),
                otp_app: None,
                requirements: vec![],
                source: ManifestPackageSource::Hex {
                    outer_checksum: Base16Checksum(vec![1, 2, 3, 4, 5]),
                },
            },
            ManifestPackage {
                name: "local2".into(),
                version: Version::parse("3.0.0").unwrap(),
                build_tools: ["gleam".into()].into(),
                otp_app: None,
                requirements: vec![],
                source: ManifestPackageSource::Hex {
                    outer_checksum: Base16Checksum(vec![4, 5]),
                },
            },
        ],
    });
    extra.sort();
    assert_eq!(
        extra,
        [
            ("local2".into(), Version::new(2, 0, 0)),
            ("local3".into(), Version::new(3, 0, 0)),
        ]
    )
}

#[test]
fn provide_wrong_package() {
    let mut provided = HashMap::new();
    let project_paths = crate::project_paths_at_current_directory_without_toml();
    let result = provide_local_package(
        "wrong_name".into(),
        Utf8Path::new("./test/hello_world"),
        Utf8Path::new("./"),
        &project_paths,
        &mut provided,
        &mut vec!["root".into(), "subpackage".into()],
    );
    match result {
        Err(Error::WrongDependencyProvided {
            expected, found, ..
        }) => {
            assert_eq!(expected, "wrong_name");
            assert_eq!(found, "hello_world");
        }
        _ => {
            panic!("Expected WrongDependencyProvided error")
        }
    }
}

#[test]
fn provide_existing_package() {
    let mut provided = HashMap::new();
    let project_paths = crate::project_paths_at_current_directory_without_toml();

    let result = provide_local_package(
        "hello_world".into(),
        Utf8Path::new("./test/hello_world"),
        Utf8Path::new("./"),
        &project_paths,
        &mut provided,
        &mut vec!["root".into(), "subpackage".into()],
    );
    assert_eq!(
        result,
        Ok(hexpm::version::Range::new("== 0.1.0".into()).unwrap())
    );

    let result = provide_local_package(
        "hello_world".into(),
        Utf8Path::new("./test/hello_world"),
        Utf8Path::new("./"),
        &project_paths,
        &mut provided,
        &mut vec!["root".into(), "subpackage".into()],
    );
    assert_eq!(
        result,
        Ok(hexpm::version::Range::new("== 0.1.0".into()).unwrap())
    );
}

#[test]
fn provide_conflicting_package() {
    let mut provided = HashMap::new();
    let project_paths = crate::project_paths_at_current_directory_without_toml();
    let result = provide_local_package(
        "hello_world".into(),
        Utf8Path::new("./test/hello_world"),
        Utf8Path::new("./"),
        &project_paths,
        &mut provided,
        &mut vec!["root".into(), "subpackage".into()],
    );
    assert_eq!(
        result,
        Ok(hexpm::version::Range::new("== 0.1.0".into()).unwrap())
    );

    let result = provide_package(
        "hello_world".into(),
        Utf8PathBuf::from("./test/other"),
        ProvidedPackageSource::Local {
            path: Utf8Path::new("./test/other").to_path_buf(),
        },
        &project_paths,
        &mut provided,
        &mut vec!["root".into(), "subpackage".into()],
    );
    match result {
        Err(Error::ProvidedDependencyConflict { package, .. }) => {
            assert_eq!(package, "hello_world");
        }
        _ => {
            panic!("Expected ProvidedDependencyConflict error")
        }
    }
}

#[test]
fn provided_is_absolute() {
    let mut provided = HashMap::new();
    let project_paths = crate::project_paths_at_current_directory_without_toml();
    let result = provide_local_package(
        "hello_world".into(),
        Utf8Path::new("./test/hello_world"),
        Utf8Path::new("./"),
        &project_paths,
        &mut provided,
        &mut vec!["root".into(), "subpackage".into()],
    );
    assert_eq!(
        result,
        Ok(hexpm::version::Range::new("== 0.1.0".into()).unwrap())
    );
    let package = provided.get("hello_world").unwrap().clone();
    match package.source {
        ProvidedPackageSource::Local { path } => {
            assert!(path.is_absolute())
        }
        _ => {
            panic!("Provide_local_package provided a package that is not local!")
        }
    }
}

#[test]
fn provided_recursive() {
    let mut provided = HashMap::new();
    let project_paths = crate::project_paths_at_current_directory_without_toml();
    let result = provide_local_package(
        "hello_world".into(),
        Utf8Path::new("./test/hello_world"),
        Utf8Path::new("./"),
        &project_paths,
        &mut provided,
        &mut vec!["root".into(), "hello_world".into(), "subpackage".into()],
    );
    assert_eq!(
        result,
        Err(Error::PackageCycle {
            packages: vec!["subpackage".into(), "hello_world".into()],
        })
    )
}

#[test]
fn provided_local_to_hex() {
    let provided_package = ProvidedPackage {
        version: Version::new(1, 0, 0),
        source: ProvidedPackageSource::Local {
            path: "canonical/path/to/package".into(),
        },
        requirements: [
            (
                "req_1".into(),
                hexpm::version::Range::new("~> 1.0.0".into()).unwrap(),
            ),
            (
                "req_2".into(),
                hexpm::version::Range::new("== 1.0.0".into()).unwrap(),
            ),
        ]
        .into(),
    };

    let hex_package = hexpm::Package {
        name: "package".into(),
        repository: "local".into(),
        releases: vec![hexpm::Release {
            version: Version::new(1, 0, 0),
            retirement_status: None,
            outer_checksum: vec![],
            meta: (),
            requirements: [
                (
                    "req_1".into(),
                    hexpm::Dependency {
                        requirement: hexpm::version::Range::new("~> 1.0.0".into()).unwrap(),
                        optional: false,
                        app: None,
                        repository: None,
                    },
                ),
                (
                    "req_2".into(),
                    hexpm::Dependency {
                        requirement: hexpm::version::Range::new("== 1.0.0".into()).unwrap(),
                        optional: false,
                        app: None,
                        repository: None,
                    },
                ),
            ]
            .into(),
        }],
    };

    assert_eq!(
        provided_package.to_hex_package(&"package".into()),
        hex_package
    );
}

#[test]
fn provided_git_to_hex() {
    let provided_package = ProvidedPackage {
        version: Version::new(1, 0, 0),
        source: ProvidedPackageSource::Git {
            repo: "https://github.com/gleam-lang/gleam.git".into(),
            commit: "bd9fe02f72250e6a136967917bcb1bdccaffa3c8".into(),
        },
        requirements: [
            (
                "req_1".into(),
                hexpm::version::Range::new("~> 1.0.0".into()).unwrap(),
            ),
            (
                "req_2".into(),
                hexpm::version::Range::new("== 1.0.0".into()).unwrap(),
            ),
        ]
        .into(),
    };

    let hex_package = hexpm::Package {
        name: "package".into(),
        repository: "local".into(),
        releases: vec![hexpm::Release {
            version: Version::new(1, 0, 0),
            retirement_status: None,
            outer_checksum: vec![],
            meta: (),
            requirements: [
                (
                    "req_1".into(),
                    hexpm::Dependency {
                        requirement: hexpm::version::Range::new("~> 1.0.0".into()).unwrap(),
                        optional: false,
                        app: None,
                        repository: None,
                    },
                ),
                (
                    "req_2".into(),
                    hexpm::Dependency {
                        requirement: hexpm::version::Range::new("== 1.0.0".into()).unwrap(),
                        optional: false,
                        app: None,
                        repository: None,
                    },
                ),
            ]
            .into(),
        }],
    };

    assert_eq!(
        provided_package.to_hex_package(&"package".into()),
        hex_package
    );
}

#[test]
fn provided_local_to_manifest() {
    let provided_package = ProvidedPackage {
        version: Version::new(1, 0, 0),
        source: ProvidedPackageSource::Local {
            path: "canonical/path/to/package".into(),
        },
        requirements: [
            (
                "req_1".into(),
                hexpm::version::Range::new("~> 1.0.0".into()).unwrap(),
            ),
            (
                "req_2".into(),
                hexpm::version::Range::new("== 1.0.0".into()).unwrap(),
            ),
        ]
        .into(),
    };

    let manifest_package = ManifestPackage {
        name: "package".into(),
        version: Version::new(1, 0, 0),
        otp_app: None,
        build_tools: vec!["gleam".into()],
        requirements: vec!["req_1".into(), "req_2".into()],
        source: ManifestPackageSource::Local {
            path: "canonical/path/to/package".into(),
        },
    };

    assert_eq!(
        provided_package.to_manifest_package("package"),
        manifest_package
    );
}

#[test]
fn provided_git_to_manifest() {
    let provided_package = ProvidedPackage {
        version: Version::new(1, 0, 0),
        source: ProvidedPackageSource::Git {
            repo: "https://github.com/gleam-lang/gleam.git".into(),
            commit: "bd9fe02f72250e6a136967917bcb1bdccaffa3c8".into(),
        },
        requirements: [
            (
                "req_1".into(),
                hexpm::version::Range::new("~> 1.0.0".into()).unwrap(),
            ),
            (
                "req_2".into(),
                hexpm::version::Range::new("== 1.0.0".into()).unwrap(),
            ),
        ]
        .into(),
    };

    let manifest_package = ManifestPackage {
        name: "package".into(),
        version: Version::new(1, 0, 0),
        otp_app: None,
        build_tools: vec!["gleam".into()],
        requirements: vec!["req_1".into(), "req_2".into()],
        source: ManifestPackageSource::Git {
            repo: "https://github.com/gleam-lang/gleam.git".into(),
            commit: "bd9fe02f72250e6a136967917bcb1bdccaffa3c8".into(),
        },
    };

    assert_eq!(
        provided_package.to_manifest_package("package"),
        manifest_package
    );
}

#[test]
fn verified_requirements_equality_with_canonicalized_paths() {
    let temp_dir = tempfile::tempdir().expect("Failed to create a temp directory");
    let temp_path = Utf8PathBuf::from_path_buf(temp_dir.path().to_path_buf())
        .expect("Path should be valid UTF-8");

    let sub_dir = temp_path.join("subdir");
    std::fs::create_dir(&sub_dir).expect("Failed to create a subdir");
    let file_path = sub_dir.join("file.txt");
    fs::write(&file_path, "content").expect("Failed to write to file");

    let canonical_path = std::fs::canonicalize(&file_path).expect("Failed to canonicalize path");
    let relative_path = temp_path.join("./subdir/../subdir/./file.txt");

    let requirements1 = HashMap::from([(
        EcoString::from("dep1"),
        Requirement::Path {
            path: Utf8PathBuf::from(canonical_path.to_str().expect("Path should be valid UTF-8")),
        },
    )]);

    let requirements2 = HashMap::from([(
        EcoString::from("dep1"),
        Requirement::Path {
            path: Utf8PathBuf::from(relative_path.to_string()),
        },
    )]);

    assert!(
        is_same_requirements(&requirements1, &requirements2, &temp_path)
            .expect("Requirements should be the same")
    );
}

#[test]
fn test_path_dependency_manifest_hash_change() {
    let temp_dir = tempfile::tempdir().expect("Failed to create a temp directory");
    let root_path = Utf8PathBuf::from_path_buf(temp_dir.path().to_path_buf())
        .expect("Path should be valid UTF-8");

    let dep_path = root_path.join("dep");
    std::fs::create_dir_all(&dep_path).expect("Failed to create dependency directory");

    let build_packages_dir = root_path.join("build").join("packages");
    std::fs::create_dir_all(&build_packages_dir)
        .expect("Failed to create build/packages directory");

    let dep_manifest_path = dep_path.join("manifest.toml");
    fs::write(&dep_manifest_path, "# Initial manifest content")
        .expect("Failed to write to manifest file");

    let requirements = HashMap::from([(
        EcoString::from("dep"),
        Requirement::Path {
            path: Utf8PathBuf::from("dep"),
        },
    )]);

    let project_paths = ProjectPaths::new(root_path.clone());
    let first_check = check_path_dependency_manifests(&requirements, &project_paths)
        .expect("First check should succeed");
    assert!(
        !first_check,
        "First check should be false as no hash exists yet"
    );

    let hash_path = build_packages_dir.join("dep.manifest_hash");
    assert!(hash_path.exists(), "Hash file should have been created");

    let second_check = check_path_dependency_manifests(&requirements, &project_paths)
        .expect("Second check should succeed");
    assert!(
        second_check,
        "Second check should be true as manifest hasn't changed"
    );

    std::thread::sleep(std::time::Duration::from_millis(100));

    fs::write(&dep_manifest_path, "# Modified manifest content\nname = \"dep\"\nversion = \"0.1.0\"\n\n[dependencies]\nsome_package = \"1.0.0\"")
        .expect("Failed to update manifest file");

    let future_time = std::time::SystemTime::now() + std::time::Duration::from_secs(10);
    filetime::set_file_mtime(
        &dep_manifest_path,
        filetime::FileTime::from_system_time(future_time),
    )
    .expect("Failed to set manifest file mtime");

    let third_check = check_path_dependency_manifests(&requirements, &project_paths)
        .expect("Third check should succeed");
    assert!(
        !third_check,
        "Third check should be false as manifest has changed"
    );

    let fourth_check = check_path_dependency_manifests(&requirements, &project_paths)
        .expect("Fourth check should succeed");
    assert!(
        fourth_check,
        "Fourth check should be true as hash has been updated"
    );
}

#[test]
fn test_path_dependency_with_missing_manifest() {
    let temp_dir = tempfile::tempdir().expect("Failed to create a temp directory");
    let root_path = Utf8PathBuf::from_path_buf(temp_dir.path().to_path_buf())
        .expect("Path should be valid UTF-8");

    let dep_path = root_path.join("dep");
    std::fs::create_dir_all(&dep_path).expect("Failed to create dependency directory");

    let build_packages_dir = root_path.join("build").join("packages");
    std::fs::create_dir_all(&build_packages_dir)
        .expect("Failed to create build/packages directory");

    let requirements = HashMap::from([(
        EcoString::from("dep"),
        Requirement::Path {
            path: Utf8PathBuf::from("dep"),
        },
    )]);

    let project_paths = ProjectPaths::new(root_path.clone());
    let check = check_path_dependency_manifests(&requirements, &project_paths)
        .expect("Check should succeed");
    assert!(check, "Check should be true when manifest is missing");

    let hash_path = build_packages_dir.join("dep.manifest_hash");
    assert!(
        !hash_path.exists(),
        "Hash file should not be created when manifest is missing"
    );
}

#[test]
fn test_path_dependency_manifest_mtime_optimization() {
    let temp_dir = tempfile::tempdir().expect("Failed to create a temp directory");
    let root_path = Utf8PathBuf::from_path_buf(temp_dir.path().to_path_buf())
        .expect("Path should be valid UTF-8");

    let dep_path = root_path.join("dep");
    std::fs::create_dir_all(&dep_path).expect("Failed to create dependency directory");

    let build_packages_dir = root_path.join("build").join("packages");
    std::fs::create_dir_all(&build_packages_dir)
        .expect("Failed to create build/packages directory");

    let dep_manifest_path = dep_path.join("manifest.toml");
    fs::write(&dep_manifest_path, "# Initial manifest content")
        .expect("Failed to write to manifest file");
    let requirements = HashMap::from([(
        EcoString::from("dep"),
        Requirement::Path {
            path: Utf8PathBuf::from("dep"),
        },
    )]);

    let project_paths = ProjectPaths::new(root_path.clone());
    let first_check = check_path_dependency_manifests(&requirements, &project_paths)
        .expect("First check should succeed");
    assert!(
        !first_check,
        "First check should be false as no hash exists yet"
    );

    let hash_path = build_packages_dir.join("dep.manifest_hash");
    assert!(hash_path.exists(), "Hash file should have been created");

    let manifest_metadata =
        std::fs::metadata(&dep_manifest_path).expect("Should get manifest metadata");
    let manifest_mtime = manifest_metadata
        .modified()
        .expect("Should get manifest mtime");

    std::thread::sleep(std::time::Duration::from_millis(10));

    let original_hash =
        std::fs::read_to_string(&hash_path).expect("Should be able to read hash file");
    fs::write(&hash_path, "deliberately wrong hash").expect("Failed to update hash file");
    let future_time = manifest_mtime + std::time::Duration::from_secs(10);
    filetime::set_file_mtime(
        &hash_path,
        filetime::FileTime::from_system_time(future_time),
    )
    .expect("Failed to set hash file mtime");

    let check = check_path_dependency_manifests(&requirements, &project_paths)
        .expect("Check should succeed");
    assert!(
        check,
        "Check should be true when hash file mtime is newer than manifest mtime"
    );

    let current_hash =
        std::fs::read_to_string(&hash_path).expect("Should be able to read hash file");
    assert_ne!(
        original_hash, current_hash,
        "Hash content should not have been updated"
    );
    assert_eq!(
        current_hash, "deliberately wrong hash",
        "Hash should not have been recalculated"
    );
}

#[test]
fn test_adding_dependency_to_path_dependency_manifest() {
    let temp_dir = tempfile::tempdir().expect("Failed to create a temp directory");
    let root_path = Utf8PathBuf::from_path_buf(temp_dir.path().to_path_buf())
        .expect("Path should be valid UTF-8");

    let bar_path = root_path.join("bar");
    std::fs::create_dir_all(&bar_path).expect("Failed to create bar directory");

    let bar_manifest_path = bar_path.join("manifest.toml");
    let initial_bar_manifest = r#"name = "bar"
version = "0.1.0"

[dependencies]
gleam_stdlib = "~> 0.29"
"#;

    fs::write(&bar_manifest_path, initial_bar_manifest)
        .expect("Failed to write initial bar manifest file");

    let build_packages_dir = root_path.join("build").join("packages");
    std::fs::create_dir_all(&build_packages_dir)
        .expect("Failed to create build/packages directory");
    let requirements = HashMap::from([(
        EcoString::from("bar"),
        Requirement::Path {
            path: Utf8PathBuf::from("bar"),
        },
    )]);

    let project_paths = ProjectPaths::new(root_path.clone());
    let first_check = check_path_dependency_manifests(&requirements, &project_paths)
        .expect("First check should succeed");
    assert!(
        !first_check,
        "First check should be false as no hash exists yet"
    );

    let hash_path = build_packages_dir.join("bar.manifest_hash");
    assert!(hash_path.exists(), "Hash file should have been created");

    let second_check = check_path_dependency_manifests(&requirements, &project_paths)
        .expect("Second check should succeed");
    assert!(
        second_check,
        "Second check should be true as manifest hasn't changed"
    );

    std::thread::sleep(std::time::Duration::from_millis(100));
    let updated_bar_manifest = r#"name = "bar"
version = "0.1.0"

[dependencies]
gleam_stdlib = "~> 0.29"
simplifile = "~> 0.1"
some_other_package = "1.2.3"
"#;

    fs::write(&bar_manifest_path, updated_bar_manifest)
        .expect("Failed to update bar manifest file");

    let future_time = std::time::SystemTime::now() + std::time::Duration::from_secs(10);
    filetime::set_file_mtime(
        &bar_manifest_path,
        filetime::FileTime::from_system_time(future_time),
    )
    .expect("Failed to set manifest file mtime");

    let third_check = check_path_dependency_manifests(&requirements, &project_paths)
        .expect("Third check should succeed");
    assert!(
        !third_check,
        "Third check should be false as manifest has changed with new dependency"
    );

    let fourth_check = check_path_dependency_manifests(&requirements, &project_paths)
        .expect("Fourth check should succeed");
    assert!(
        fourth_check,
        "Fourth check should be true as hash has been updated"
    );
}

fn create_testable_unlock_manifest(
    packages: Vec<(EcoString, Version, Vec<EcoString>)>,
    requirements: Vec<(EcoString, EcoString)>,
) -> Manifest {
    let manifest_packages = packages
        .into_iter()
        .map(|(name, version, requirements)| ManifestPackage {
            name,
            version,
            build_tools: vec!["gleam".into()],
            otp_app: None,
            requirements,
            source: ManifestPackageSource::Hex {
                outer_checksum: Base16Checksum(vec![]),
            },
        })
        .collect();

    let root_requirements = requirements
        .into_iter()
        .map(|(name, range)| {
            (
                name,
                Requirement::Hex {
                    version: hexpm::version::Range::new(range.into()).unwrap(),
                },
            )
        })
        .collect();

    Manifest {
        packages: manifest_packages,
        requirements: root_requirements,
    }
}

#[test]
fn test_unlock_package() {
    let mut locked = HashMap::from([
        ("package_a".into(), Version::new(1, 0, 0)),
        ("package_b".into(), Version::new(2, 0, 0)),
        ("package_c".into(), Version::new(3, 0, 0)),
        ("package_d".into(), Version::new(4, 0, 0)),
    ]);

    let packages = vec![
        (
            "package_a".into(),
            Version::new(1, 0, 0),
            vec!["package_b".into()],
        ),
        (
            "package_b".into(),
            Version::new(2, 0, 0),
            vec!["package_c".into()],
        ),
        ("package_c".into(), Version::new(3, 0, 0), vec![]),
        ("package_d".into(), Version::new(4, 0, 0), vec![]),
    ];

    let manifest = create_testable_unlock_manifest(packages, Vec::new());

    let packages_to_unlock = vec!["package_a".into()];
    unlock_packages(&mut locked, &packages_to_unlock, Some(&manifest)).unwrap();

    assert!(!locked.contains_key("package_a"));
    assert!(!locked.contains_key("package_b"));
    assert!(!locked.contains_key("package_c"));
    assert!(locked.contains_key("package_d"));
}

#[test]
fn test_unlock_package_without_manifest() {
    let mut locked = HashMap::from([
        ("package_a".into(), Version::new(1, 0, 0)),
        ("package_b".into(), Version::new(2, 0, 0)),
        ("package_c".into(), Version::new(3, 0, 0)),
    ]);

    let packages_to_unlock = vec!["package_a".into()];
    unlock_packages(&mut locked, &packages_to_unlock, None).unwrap();

    assert!(!locked.contains_key("package_a"));
    assert!(locked.contains_key("package_b"));
    assert!(locked.contains_key("package_c"));
}

#[test]
fn test_unlock_nonexistent_package() {
    let initial_locked = HashMap::from([
        ("package_a".into(), Version::new(1, 0, 0)),
        ("package_b".into(), Version::new(2, 0, 0)),
    ]);

    let packages = vec![
        (
            "package_a".into(),
            Version::new(1, 0, 0),
            vec!["package_b".into()],
        ),
        ("package_b".into(), Version::new(2, 0, 0), vec![]),
    ];

    let manifest = create_testable_unlock_manifest(packages, Vec::new());

    let packages_to_unlock = vec!["nonexistent_package".into()];
    let mut locked = initial_locked.clone();
    unlock_packages(&mut locked, &packages_to_unlock, Some(&manifest)).unwrap();

    assert_eq!(
        initial_locked, locked,
        "Locked packages should remain unchanged"
    );
}

#[test]
fn test_unlock_multiple_packages() {
    let mut locked = HashMap::from([
        ("package_a".into(), Version::new(1, 0, 0)),
        ("package_b".into(), Version::new(2, 0, 0)),
        ("package_c".into(), Version::new(3, 0, 0)),
        ("package_d".into(), Version::new(4, 0, 0)),
        ("package_e".into(), Version::new(5, 0, 0)),
    ]);

    let packages = vec![
        (
            "package_a".into(),
            Version::new(1, 0, 0),
            vec!["package_b".into()],
        ),
        (
            "package_b".into(),
            Version::new(2, 0, 0),
            vec!["package_c".into()],
        ),
        ("package_c".into(), Version::new(3, 0, 0), vec![]),
        (
            "package_d".into(),
            Version::new(4, 0, 0),
            vec!["package_e".into()],
        ),
        ("package_e".into(), Version::new(5, 0, 0), vec![]),
    ];

    let manifest = create_testable_unlock_manifest(packages, Vec::new());

    let packages_to_unlock = vec!["package_a".into(), "package_d".into()];
    unlock_packages(&mut locked, &packages_to_unlock, Some(&manifest)).unwrap();

    assert!(!locked.contains_key("package_a"));
    assert!(!locked.contains_key("package_b"));
    assert!(!locked.contains_key("package_c"));
    assert!(!locked.contains_key("package_d"));
    assert!(!locked.contains_key("package_e"));
}

#[test]
fn test_unlock_packages_empty_input() {
    let initial_locked = HashMap::from([
        ("package_a".into(), Version::new(1, 0, 0)),
        ("package_b".into(), Version::new(2, 0, 0)),
    ]);

    let packages = vec![
        (
            "package_a".into(),
            Version::new(1, 0, 0),
            vec!["package_b".into()],
        ),
        ("package_b".into(), Version::new(2, 0, 0), vec![]),
    ];

    let manifest = create_testable_unlock_manifest(packages, Vec::new());

    let packages_to_unlock: Vec<EcoString> = vec![];
    let mut locked = initial_locked.clone();
    unlock_packages(&mut locked, &packages_to_unlock, Some(&manifest)).unwrap();

    assert_eq!(
        initial_locked, locked,
        "Locked packages should remain unchanged when no packages are specified to unlock"
    );
}

#[test]
fn test_unlock_package_preserve_shared_deps() {
    let mut locked = HashMap::from([
        ("package_a".into(), Version::new(1, 0, 0)),
        ("package_b".into(), Version::new(2, 0, 0)),
        ("package_c".into(), Version::new(3, 0, 0)),
    ]);

    let packages = vec![
        (
            "package_a".into(),
            Version::new(1, 0, 0),
            vec!["package_c".into()],
        ),
        (
            "package_b".into(),
            Version::new(2, 0, 0),
            vec!["package_c".into()],
        ),
        ("package_c".into(), Version::new(3, 0, 0), vec![]),
    ];

    let manifest = create_testable_unlock_manifest(packages, Vec::new());

    let packages_to_unlock: Vec<EcoString> = vec!["package_a".into()];
    unlock_packages(&mut locked, &packages_to_unlock, Some(&manifest)).unwrap();

    assert!(!locked.contains_key("package_a"));
    assert!(locked.contains_key("package_b"));
    assert!(locked.contains_key("package_c"));
}

#[test]
fn test_unlock_package_with_root_dep() {
    let mut locked = HashMap::from([
        ("package_a".into(), Version::new(1, 0, 0)),
        ("package_b".into(), Version::new(2, 0, 0)),
        ("package_c".into(), Version::new(3, 0, 0)),
    ]);

    let packages = vec![
        (
            "package_a".into(),
            Version::new(1, 0, 0),
            vec!["package_b".into()],
        ),
        (
            "package_b".into(),
            Version::new(2, 0, 0),
            vec!["package_c".into()],
        ),
        ("package_c".into(), Version::new(3, 0, 0), vec![]),
    ];

    let requirements = vec![("package_b".into(), ">= 2.0.0".into())];

    let manifest = create_testable_unlock_manifest(packages, requirements);

    let packages_to_unlock: Vec<EcoString> = vec!["package_a".into()];
    unlock_packages(&mut locked, &packages_to_unlock, Some(&manifest)).unwrap();

    assert!(!locked.contains_key("package_a"));
    assert!(locked.contains_key("package_b"));
    assert!(locked.contains_key("package_c"));
}

#[test]
fn test_unlock_root_dep_package() {
    let mut locked = HashMap::from([
        ("package_a".into(), Version::new(1, 0, 0)),
        ("package_b".into(), Version::new(2, 0, 0)),
        ("package_c".into(), Version::new(3, 0, 0)),
    ]);

    let packages = vec![
        (
            "package_a".into(),
            Version::new(1, 0, 0),
            vec!["package_b".into()],
        ),
        ("package_b".into(), Version::new(2, 0, 0), vec![]),
        ("package_c".into(), Version::new(3, 0, 0), vec![]),
    ];

    let requirements = vec![("package_a".into(), ">= 1.0.0".into())];

    let manifest = create_testable_unlock_manifest(packages, requirements);

    let packages_to_unlock: Vec<EcoString> = vec!["package_a".into()];
    unlock_packages(&mut locked, &packages_to_unlock, Some(&manifest)).unwrap();

    assert!(!locked.contains_key("package_a"));
    assert!(!locked.contains_key("package_b"));
    assert!(locked.contains_key("package_c"));
}

#[test]
fn test_unlock_package_with_and_without_root_dep() {
    let mut locked = HashMap::from([
        ("package_a".into(), Version::new(1, 0, 0)),
        ("package_b".into(), Version::new(2, 0, 0)),
        ("package_c".into(), Version::new(3, 0, 0)),
    ]);

    let packages = vec![
        (
            "package_a".into(),
            Version::new(1, 0, 0),
            vec!["package_b".into(), "package_c".into()],
        ),
        ("package_b".into(), Version::new(2, 0, 0), vec![]),
        ("package_c".into(), Version::new(3, 0, 0), vec![]),
    ];

    let requirements = vec![("package_b".into(), ">= 2.0.0".into())];

    let manifest = create_testable_unlock_manifest(packages, requirements);

    let packages_to_unlock: Vec<EcoString> = vec!["package_a".into()];
    unlock_packages(&mut locked, &packages_to_unlock, Some(&manifest)).unwrap();

    assert!(!locked.contains_key("package_a"));
    assert!(locked.contains_key("package_b"));
    assert!(!locked.contains_key("package_c"));
}

fn manifest_package(name: &str, version: &str, requirements: Vec<EcoString>) -> ManifestPackage {
    ManifestPackage {
        name: name.into(),
        version: Version::parse(version).unwrap(),
        build_tools: ["gleam".into()].into(),
        otp_app: None,
        requirements,
        source: ManifestPackageSource::Hex {
            outer_checksum: Base16Checksum(vec![1, 2, 3, 4]),
        },
    }
}

fn package_config(
    dependencies: HashMap<EcoString, Requirement>,
    dev_dependencies: HashMap<EcoString, Requirement>,
) -> PackageConfig {
    PackageConfig {
        name: "the_package".into(),
        version: Version::parse("1.0.0").unwrap(),
        gleam_version: None,
        licences: vec![],
        description: "".into(),
        documentation: Docs { pages: vec![] },
        dependencies,
        dev_dependencies,
        repository: None,
        links: vec![],
        erlang: ErlangConfig {
            application_start_module: None,
            application_start_argument: None,
            extra_applications: vec![],
        },
        javascript: JavaScriptConfig {
            typescript_declarations: false,
            runtime: Runtime::NodeJs,
            deno: DenoConfig {
                allow_env: DenoFlag::AllowAll,
                allow_sys: true,
                allow_hrtime: true,
                allow_net: DenoFlag::AllowAll,
                allow_ffi: true,
                allow_read: DenoFlag::AllowAll,
                allow_run: DenoFlag::AllowAll,
                allow_write: DenoFlag::AllowAll,
                allow_all: true,
                unstable: true,
                location: None,
            },
        },
        target: Target::Erlang,
        internal_modules: None,
    }
}

#[test]
fn test_remove_do_nothing() {
    let config = package_config(
        HashMap::from([("a".into(), Requirement::hex("~>1.0").unwrap())]),
        HashMap::from([("b".into(), Requirement::hex("~>2.0").unwrap())]),
    );

    let mut manifest = Manifest {
        requirements: HashMap::from([
            ("a".into(), Requirement::hex("~>1.0").unwrap()),
            ("b".into(), Requirement::hex("~>2.0").unwrap()),
        ]),
        packages: vec![
            manifest_package("a", "1.0.0", vec![]),
            manifest_package("b", "2.0.8", vec![]),
        ],
    };

    let manifest_copy = manifest.clone();

    remove_extra_requirements(&config, &mut manifest).unwrap();

    assert_eq!(manifest.requirements, manifest_copy.requirements);
    assert_eq!(manifest.packages, manifest_copy.packages);
}

#[test]
fn test_remove_simple() {
    let config = package_config(HashMap::new(), HashMap::new());

    let mut manifest = Manifest {
        requirements: HashMap::from([("a".into(), Requirement::hex("~>1.0").unwrap())]),
        packages: vec![manifest_package("a", "1.0.0", vec![])],
    };

    remove_extra_requirements(&config, &mut manifest).unwrap();

    assert_eq!(manifest.requirements, config.dependencies);
    assert_eq!(manifest.packages, vec![]);
}

#[test]
fn test_remove_package_with_transitive_dependencies() {
    let config = package_config(HashMap::new(), HashMap::new());

    let mut manifest = Manifest {
        requirements: HashMap::from([("a".into(), Requirement::hex("~>1.0").unwrap())]),
        packages: vec![
            manifest_package("a", "1.0.0", vec!["b".into()]),
            manifest_package("b", "1.2.3", vec!["c".into()]),
            manifest_package("c", "2.0.0", vec![]),
        ],
    };

    remove_extra_requirements(&config, &mut manifest).unwrap();

    assert_eq!(manifest.requirements, config.dependencies);
    assert_eq!(manifest.packages, vec![]);
}

#[test]
fn test_remove_package_with_shared_transitive_dependencies() {
    let config = package_config(
        HashMap::from([("a".into(), Requirement::hex("~>1.0").unwrap())]),
        HashMap::new(),
    );

    let mut manifest = Manifest {
        requirements: HashMap::from([
            ("a".into(), Requirement::hex("~>1.0").unwrap()),
            ("b".into(), Requirement::hex("~>1.0").unwrap()),
        ]),
        packages: vec![
            manifest_package("a", "1.0.0", vec!["c".into()]),
            manifest_package("b", "1.2.3", vec!["c".into(), "d".into()]),
            manifest_package("c", "2.0.0", vec![]),
            manifest_package("d", "0.1.0", vec![]),
        ],
    };

    remove_extra_requirements(&config, &mut manifest).unwrap();

    assert_eq!(manifest.requirements, config.dependencies);
    assert_eq!(
        manifest.packages,
        vec![
            manifest_package("a", "1.0.0", vec!["c".into()]),
            manifest_package("c", "2.0.0", vec![]),
        ]
    );
}

#[test]
fn test_remove_package_that_is_also_a_transitive_dependency() {
    let config = package_config(
        HashMap::from([("a".into(), Requirement::hex("~>1.0").unwrap())]),
        HashMap::new(),
    );

    let mut manifest = Manifest {
        requirements: HashMap::from([
            ("a".into(), Requirement::hex("~>1.0").unwrap()),
            ("b".into(), Requirement::hex("~>1.0").unwrap()),
        ]),
        packages: vec![
            manifest_package("a", "1.0.0", vec!["b".into(), "c".into()]),
            manifest_package("b", "1.2.3", vec!["c".into(), "d".into()]),
            manifest_package("c", "2.0.0", vec![]),
            manifest_package("d", "0.1.0", vec![]),
        ],
    };

    let manifest_copy = manifest.clone();

    remove_extra_requirements(&config, &mut manifest).unwrap();

    assert_eq!(manifest.requirements, config.dependencies);
    assert_eq!(manifest.packages, manifest_copy.packages);
}

#[test]
fn test_pretty_print_major_versions_available() {
    let versions = vec![
        (
            "very_long_package_name".to_string(),
            (Version::new(18, 382, 43), Version::new(19, 0, 38)),
        ),
        (
            "gleam_stdlib".to_string(),
            (Version::new(0, 45, 0), Version::new(1, 0, 0)),
        ),
        (
            "short_name".to_string(),
            (Version::new(1, 0, 0), Version::new(2, 0, 0)),
        ),
    ]
    .into_iter()
    .collect();

    let output = pretty_print_major_versions_available(versions);

    insta::assert_snapshot!(output);
}

#[test]
fn test_pretty_print_version_updates() {
    let versions = vec![
        (
            "gleam_stdlib".to_string(),
            (Version::new(0, 45, 0), Version::new(0, 46, 0)),
        ),
        (
            "wisp".to_string(),
            (Version::new(2, 1, 0), Version::new(2, 1, 1)),
        ),
        (
            "very_long_package_name".to_string(),
            (Version::new(12, 12, 12), Version::new(120, 12, 12)),
        ),
    ]
    .into_iter()
    .collect();

    let output = pretty_print_version_updates(versions);

    insta::assert_snapshot!(output);
}

#[test]
fn test_ensure_packages_exist_locally_all_present() {
    let manifest = Manifest {
        requirements: HashMap::new(),
        packages: vec![
            manifest_package("package_a", "1.0.0", vec![]),
            manifest_package("package_b", "2.0.0", vec![]),
            manifest_package("package_c", "3.0.0", vec![]),
        ],
    };

    let packages_to_check = vec!["package_a".into(), "package_b".into()];
    let result = dependency_manager::ensure_packages_exist_locally(&manifest, &packages_to_check);

    assert!(result.is_ok(), "All packages exist, should return Ok");
}

#[test]
fn test_ensure_packages_exist_locally_some_missing() {
    let manifest = Manifest {
        requirements: HashMap::new(),
        packages: vec![
            manifest_package("package_a", "1.0.0", vec![]),
            manifest_package("package_b", "2.0.0", vec![]),
        ],
    };

    let packages_to_check = vec![
        "package_a".into(),
        "package_b".into(),
        "missing_package".into(),
        "another_missing".into(),
    ];
    let result = dependency_manager::ensure_packages_exist_locally(&manifest, &packages_to_check);

    match result {
        Err(Error::PackagesToUpdateNotExist { packages }) => {
            assert_eq!(packages.len(), 2);
            assert!(packages.contains(&"missing_package".into()));
            assert!(packages.contains(&"another_missing".into()));
        }
        _ => panic!("Expected PackagesToUpdateNotExist error"),
    }
}
