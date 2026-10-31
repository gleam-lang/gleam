// SPDX-License-Identifier: Apache-2.0
// SPDX-FileCopyrightText: 2026 The Gleam contributors

use hexpm::RetirementStatus;
use itertools::Itertools;

use crate::{
    manifest::{Base16Checksum, ManifestPackage, ManifestPackageSource},
    requirement,
};

use super::*;

struct Remote {
    deps: HashMap<String, Rc<hexpm::Package>>,
}

impl PackageFetcher for Remote {
    fn get_package(&self, package: &str) -> Result<Rc<hexpm::Package>, Error> {
        self.deps.get(package).map(Rc::clone).ok_or_else(|| {
            Error::Http(format!(
                "the package {package} was not found in the package repository"
            ))
        })
    }
}

fn make_remote() -> Remote {
    remote(vec![
        (
            "gleam_stdlib",
            vec![
                release("0.1.0", vec![]),
                release("0.2.0", vec![]),
                release("0.2.2", vec![]),
                release("0.3.0", vec![]),
            ],
        ),
        (
            "gleam_otp",
            vec![
                release("0.1.0", vec![("gleam_stdlib", ">= 0.1.0")]),
                release("0.2.0", vec![("gleam_stdlib", ">= 0.1.0")]),
                release("0.3.0-rc1", vec![("gleam_stdlib", ">= 0.1.0")]),
                release("0.3.0-rc2", vec![("gleam_stdlib", ">= 0.1.0")]),
            ],
        ),
        (
            "package_with_retired",
            vec![
                release("0.1.0", vec![]),
                retired_release(
                    "0.2.0",
                    vec![],
                    hexpm::RetirementReason::Security,
                    "it's bad",
                ),
            ],
        ),
        (
            "package_with_optional",
            vec![release_with_optional(
                "0.1.0",
                vec![],
                vec![("gleam_stdlib", ">= 0.1.0 and < 0.3.0")],
            )],
        ),
        (
            "package_with_optional_chain",
            vec![release_with_optional(
                "0.1.0",
                vec![],
                vec![
                    ("pkg_that_gains_a_dep", ">= 0.1.0 and < 0.2.0"),
                    ("indirect_optional_target", ">= 0.1.0 and < 0.2.0"),
                ],
            )],
        ),
        (
            "pkg_that_gains_a_dep",
            vec![
                release("0.1.0", vec![("indirect_optional_target", ">= 0.1.0")]),
                release("0.2.0", vec![]),
            ],
        ),
        (
            "indirect_optional_target",
            vec![release("0.1.0", vec![]), release("0.2.0", vec![])],
        ),
        (
            "direct_pkg_with_major_version",
            vec![
                release("0.1.0", vec![("gleam_stdlib", ">= 0.1.0 and < 0.3.0")]),
                release("1.0.0", vec![("gleam_stdlib", ">= 0.1.0 and < 0.3.0")]),
                release("1.1.0", vec![("gleam_stdlib", ">= 0.1.0 and < 0.3.0")]),
            ],
        ),
        (
            "depends_on_old_version_of_direct_pkg",
            vec![release(
                "0.1.0",
                vec![("direct_pkg_with_major_version", ">= 0.1.0 and < 0.3.0")],
            )],
        ),
        (
            "this_pkg_depends_on_indirect_pkg",
            vec![release(
                "0.1.0",
                vec![("indirect_pkg_with_major_version", ">= 0.1.0 and < 1.0.0")],
            )],
        ),
        (
            "indirect_pkg_with_major_version",
            vec![
                release("0.1.0", vec![("gleam_stdlib", ">= 0.1.0 and < 0.3.0")]),
                release("1.0.0", vec![("gleam_stdlib", ">= 0.1.0 and < 0.3.0")]),
                release("1.1.0", vec![("gleam_stdlib", ">= 0.1.0 and < 0.3.0")]),
            ],
        ),
    ])
}

#[test]
fn resolution_with_locked() {
    let locked_stdlib = ("gleam_stdlib".into(), Version::parse("0.1.0").unwrap());
    let result = resolve_versions(
        &make_remote(),
        HashMap::new(),
        "app".into(),
        vec![("gleam_stdlib".into(), Range::new("~> 0.1".into()).unwrap())].into_iter(),
        &vec![locked_stdlib].into_iter().collect(),
    )
    .unwrap();
    assert_eq!(
        result,
        vec![("gleam_stdlib".into(), Version::parse("0.1.0").unwrap())]
            .into_iter()
            .collect()
    );
}

#[test]
fn resolution_without_deps() {
    let result = resolve_versions(
        &make_remote(),
        HashMap::new(),
        "app".into(),
        vec![].into_iter(),
        &vec![].into_iter().collect(),
    )
    .unwrap();
    assert_eq!(result, vec![].into_iter().collect())
}

#[test]
fn resolution_1_dep() {
    let result = resolve_versions(
        &make_remote(),
        HashMap::new(),
        "app".into(),
        vec![("gleam_stdlib".into(), Range::new("~> 0.1".into()).unwrap())].into_iter(),
        &vec![].into_iter().collect(),
    )
    .unwrap();
    assert_eq!(
        result,
        vec![("gleam_stdlib".into(), Version::try_from("0.3.0").unwrap())]
            .into_iter()
            .collect()
    );
}

#[test]
fn resolution_with_nested_deps() {
    let result = resolve_versions(
        &make_remote(),
        HashMap::new(),
        "app".into(),
        vec![("gleam_otp".into(), Range::new("~> 0.1".into()).unwrap())].into_iter(),
        &vec![].into_iter().collect(),
    )
    .unwrap();
    assert_eq!(
        result,
        vec![
            ("gleam_otp".into(), Version::try_from("0.2.0").unwrap()),
            ("gleam_stdlib".into(), Version::try_from("0.3.0").unwrap())
        ]
        .into_iter()
        .collect()
    );
}

#[test]
fn resolution_with_optional_deps() {
    let result = resolve_versions(
        &make_remote(),
        HashMap::new(),
        "app".into(),
        vec![(
            "package_with_optional".into(),
            Range::new("~> 0.1".into()).unwrap(),
        )]
        .into_iter(),
        &vec![].into_iter().collect(),
    )
    .unwrap();
    assert_eq!(
        result,
        vec![(
            "package_with_optional".into(),
            Version::try_from("0.1.0").unwrap()
        )]
        .into_iter()
        .collect()
    );
}

#[test]
fn resolution_with_optional_deps_explicitly_provided() {
    let result = resolve_versions(
        &make_remote(),
        HashMap::new(),
        "app".into(),
        vec![
            ("gleam_stdlib".into(), Range::new("~> 0.1".into()).unwrap()),
            (
                "package_with_optional".into(),
                Range::new("~> 0.1".into()).unwrap(),
            ),
        ]
        .into_iter(),
        &vec![].into_iter().collect(),
    )
    .unwrap()
    .into_iter()
    .sorted_by(|left, right| left.0.cmp(&right.0))
    .collect_vec();
    assert_eq!(
        result,
        vec![
            ("gleam_stdlib".into(), Version::try_from("0.2.2").unwrap()),
            (
                "package_with_optional".into(),
                Version::try_from("0.1.0").unwrap()
            ),
        ]
    );
}

#[test]
fn resolution_with_optional_deps_not_pulled_in_by_provided_package() {
    let gleam_stdlib = (*make_remote().get_package("gleam_stdlib").unwrap()).clone();
    let provided = vec![(EcoString::from("gleam_stdlib"), gleam_stdlib)]
        .into_iter()
        .collect();

    let result = resolve_versions(
        &make_remote(),
        provided,
        "app".into(),
        vec![(
            "package_with_optional".into(),
            Range::new("~> 0.1".into()).unwrap(),
        )]
        .into_iter(),
        &vec![].into_iter().collect(),
    )
    .unwrap();

    assert_eq!(
        result,
        vec![(
            "package_with_optional".into(),
            Version::try_from("0.1.0").unwrap()
        )]
        .into_iter()
        .collect()
    );
}

#[test]
fn resolution_with_optional_dep_on_package_pulled_in_by_another_optional_dep() {
    let result = resolve_versions(
        &make_remote(),
        HashMap::new(),
        "app".into(),
        vec![
            (
                "package_with_optional_chain".into(),
                Range::new("~> 0.1".into()).unwrap(),
            ),
            (
                "pkg_that_gains_a_dep".into(),
                Range::new("~> 0.1".into()).unwrap(),
            ),
        ]
        .into_iter(),
        &vec![].into_iter().collect(),
    )
    .unwrap()
    .into_iter()
    .sorted_by(|left, right| left.0.cmp(&right.0))
    .collect_vec();

    assert_eq!(
        result,
        vec![
            (
                "indirect_optional_target".into(),
                Version::try_from("0.1.0").unwrap()
            ),
            (
                "package_with_optional_chain".into(),
                Version::try_from("0.1.0").unwrap()
            ),
            (
                "pkg_that_gains_a_dep".into(),
                Version::try_from("0.1.0").unwrap()
            ),
        ]
    );
}

#[test]
fn resolution_with_optional_deps_incompatible() {
    let result = resolve_versions(
        &make_remote(),
        HashMap::new(),
        "app".into(),
        vec![
            (
                "package_with_optional".into(),
                Range::new("~> 0.1".into()).unwrap(),
            ),
            ("gleam_stdlib".into(), Range::new("~> 0.3".into()).unwrap()),
        ]
        .into_iter(),
        &vec![].into_iter().collect(),
    );
    assert!(result.is_err());
}

#[test]
fn resolution_with_optional_deps_required_by_nested_deps() {
    let result = resolve_versions(
        &make_remote(),
        HashMap::new(),
        "app".into(),
        vec![
            (
                "package_with_optional".into(),
                Range::new("~> 0.1".into()).unwrap(),
            ),
            ("gleam_otp".into(), Range::new("~> 0.1".into()).unwrap()),
        ]
        .into_iter(),
        &vec![].into_iter().collect(),
    )
    .unwrap()
    .into_iter()
    .sorted_by(|left, right| left.0.cmp(&right.0))
    .collect_vec();
    assert_eq!(
        result,
        vec![
            ("gleam_otp".into(), Version::try_from("0.2.0").unwrap()),
            ("gleam_stdlib".into(), Version::try_from("0.2.2").unwrap()),
            (
                "package_with_optional".into(),
                Version::try_from("0.1.0").unwrap()
            ),
        ]
    );
}

#[test]
fn resolution_locked_to_older_version() {
    let result = resolve_versions(
        &make_remote(),
        HashMap::new(),
        "app".into(),
        vec![("gleam_otp".into(), Range::new("~> 0.1.0".into()).unwrap())].into_iter(),
        &vec![].into_iter().collect(),
    )
    .unwrap();
    assert_eq!(
        result,
        vec![
            ("gleam_otp".into(), Version::try_from("0.1.0").unwrap()),
            ("gleam_stdlib".into(), Version::try_from("0.3.0").unwrap())
        ]
        .into_iter()
        .collect()
    );
}

#[test]
fn resolution_retired_versions_not_used_by_default() {
    let result = resolve_versions(
        &make_remote(),
        HashMap::new(),
        "app".into(),
        vec![(
            "package_with_retired".into(),
            Range::new("> 0.0.0".into()).unwrap(),
        )]
        .into_iter(),
        &vec![].into_iter().collect(),
    )
    .unwrap();
    assert_eq!(
        result,
        vec![(
            "package_with_retired".into(),
            // Uses the older version that hasn't been retired
            Version::try_from("0.1.0").unwrap()
        ),]
        .into_iter()
        .collect()
    );
}

#[test]
fn resolution_retired_versions_can_be_used_if_locked() {
    let result = resolve_versions(
        &make_remote(),
        HashMap::new(),
        "app".into(),
        vec![(
            "package_with_retired".into(),
            Range::new("> 0.0.0".into()).unwrap(),
        )]
        .into_iter(),
        &vec![("package_with_retired".into(), Version::new(0, 2, 0))]
            .into_iter()
            .collect(),
    )
    .unwrap();
    assert_eq!(
        result,
        vec![(
            "package_with_retired".into(),
            // Uses the locked version even though it's retired
            Version::new(0, 2, 0)
        ),]
        .into_iter()
        .collect()
    );
}

#[test]
fn resolution_prerelease_can_be_selected() {
    let result = resolve_versions(
        &make_remote(),
        HashMap::new(),
        "app".into(),
        vec![(
            "gleam_otp".into(),
            Range::new("~> 0.3.0-rc1".into()).unwrap(),
        )]
        .into_iter(),
        &vec![].into_iter().collect(),
    )
    .unwrap();
    assert_eq!(
        result,
        vec![
            ("gleam_stdlib".into(), Version::try_from("0.3.0").unwrap()),
            ("gleam_otp".into(), Version::try_from("0.3.0-rc2").unwrap()),
        ]
        .into_iter()
        .collect(),
    );
}

#[test]
fn resolution_exact_prerelease_can_be_selected() {
    let result = resolve_versions(
        &make_remote(),
        HashMap::new(),
        "app".into(),
        vec![("gleam_otp".into(), Range::new("0.3.0-rc1".into()).unwrap())].into_iter(),
        &vec![].into_iter().collect(),
    )
    .unwrap();
    assert_eq!(
        result,
        vec![
            ("gleam_stdlib".into(), Version::try_from("0.3.0").unwrap()),
            ("gleam_otp".into(), Version::try_from("0.3.0-rc1").unwrap()),
        ]
        .into_iter()
        .collect(),
    );
}

#[test]
fn resolution_not_found_dep() {
    let err = resolve_versions(
        &make_remote(),
        HashMap::new(),
        "app".into(),
        vec![("unknown".into(), Range::new("~> 0.1".into()).unwrap())].into_iter(),
        &vec![].into_iter().collect(),
    )
    .unwrap_err();
    match err {
        Error::DependencyResolutionError(error) => assert_eq!(
            error,
            "An error occurred while choosing the version of unknown: \
the package unknown was not found in the package repository"
        ),
        _ => panic!("wrong error: {err}"),
    }
}

#[test]
fn resolution_no_matching_version() {
    let _ = resolve_versions(
        &make_remote(),
        HashMap::new(),
        "app".into(),
        vec![("gleam_stdlib".into(), Range::new("~> 99.0".into()).unwrap())].into_iter(),
        &vec![].into_iter().collect(),
    )
    .unwrap_err();
}

#[test]
fn resolution_locked_version_doesnt_satisfy_requirements() {
    let err = resolve_versions(
        &make_remote(),
        HashMap::new(),
        "app".into(),
        vec![(
            "gleam_stdlib".into(),
            Range::new("~> 0.1.0".into()).unwrap(),
        )]
        .into_iter(),
        &vec![("gleam_stdlib".into(), Version::new(0, 2, 0))]
            .into_iter()
            .collect(),
    )
    .unwrap_err();

    match err {
        Error::IncompatibleLockedVersion { error } => assert_eq!(
            error,
            "gleam_stdlib is specified with the requirement `~> 0.1.0`, \
but it is locked to 0.2.0, which is incompatible."
        ),
        _ => panic!("wrong error: {err}"),
    }
}

#[test]
fn resolution_with_exact_dep() {
    let result = resolve_versions(
        &make_remote(),
        HashMap::new(),
        "app".into(),
        vec![("gleam_stdlib".into(), Range::new("0.1.0".into()).unwrap())].into_iter(),
        &vec![].into_iter().collect(),
    )
    .unwrap();
    assert_eq!(
        result,
        vec![("gleam_stdlib".into(), Version::try_from("0.1.0").unwrap())]
            .into_iter()
            .collect()
    );
}

#[test]
fn parse_exact_version_test() {
    assert_eq!(
        parse_exact_version("1.0.0"),
        Some(Version::parse("1.0.0").unwrap())
    );
    assert_eq!(
        parse_exact_version("==1.0.0"),
        Some(Version::parse("1.0.0").unwrap())
    );
    assert_eq!(
        parse_exact_version("== 1.0.0"),
        Some(Version::parse("1.0.0").unwrap())
    );
    assert_eq!(parse_exact_version("~> 1.0.0"), None);
    assert_eq!(parse_exact_version(">= 1.0.0"), None);
}

#[test]
fn resolve_major_version_upgrades() {
    let manifest = manifest::Manifest {
        requirements: vec![
            (
                EcoString::from("package_depends_on_indirect_pkg"),
                requirement::Requirement::Hex {
                    version: Range::new("> 0.1.0 and <= 1.0.0".into()).unwrap(),
                },
            ),
            (
                EcoString::from("direct_pkg_with_major_version"),
                requirement::Requirement::Hex {
                    version: Range::new("> 0.1.0 and <= 2.0.0".into()).unwrap(),
                },
            ),
            (
                EcoString::from("depends_on_old_version_of_direct_pkg"),
                requirement::Requirement::Hex {
                    version: Range::new("> 0.1.0 and <= 1.0.0".into()).unwrap(),
                },
            ),
        ]
        .into_iter()
        .collect(),
        packages: vec![
            ManifestPackage {
                name: "direct_pkg_with_major_version".into(),
                version: Version::parse("0.1.0").unwrap(),
                build_tools: ["gleam".into()].into(),
                otp_app: None,
                requirements: vec![],
                source: ManifestPackageSource::Hex {
                    outer_checksum: Base16Checksum(vec![1, 2, 3]),
                },
            },
            ManifestPackage {
                name: "depends_on_old_version_of_direct_pkg".into(),
                version: Version::parse("0.1.0").unwrap(),
                build_tools: ["gleam".into()].into(),
                otp_app: None,
                requirements: vec!["direct_pkg_with_major_version".into()],
                source: ManifestPackageSource::Hex {
                    outer_checksum: Base16Checksum(vec![1, 2, 3]),
                },
            },
            ManifestPackage {
                name: "pkg_depends_on_indirect_pkg".into(),
                version: Version::parse("0.1.0").unwrap(),
                build_tools: ["gleam".into()].into(),
                otp_app: None,
                requirements: vec!["indirect_pkg_with_major_version".into()],
                source: ManifestPackageSource::Hex {
                    outer_checksum: Base16Checksum(vec![1, 2, 3]),
                },
            },
            ManifestPackage {
                name: "indirect_pkg_with_major_version".into(),
                version: Version::parse("0.1.0").unwrap(),
                build_tools: ["gleam".into()].into(),
                otp_app: None,
                requirements: vec![],
                source: ManifestPackageSource::Hex {
                    outer_checksum: Base16Checksum(vec![1, 2, 3]),
                },
            },
        ],
    };
    let result = check_for_major_version_updates(&manifest, &make_remote());

    // indirect package with major version will not be in the result even though a major
    // version of it is available
    assert_eq!(
        result,
        vec![(
            "direct_pkg_with_major_version".into(),
            (
                Version::try_from("0.1.0").unwrap(),
                Version::try_from("1.1.0").unwrap()
            )
        ),]
        .into_iter()
        .collect()
    );
}

#[test]
fn git_deps_are_not_checked_on_hex_for_major_version_updates() {
    let manifest = manifest::Manifest {
        requirements: vec![(
            EcoString::from("wibble"),
            requirement::Requirement::Git {
                git: "git".into(),
                ref_: "ref".into(),
                path: None,
            },
        )]
        .into_iter()
        .collect(),
        packages: vec![ManifestPackage {
            name: "wibble".into(),
            version: Version::parse("0.1.0").unwrap(),
            build_tools: ["gleam".into()].into(),
            otp_app: None,
            requirements: vec![],
            source: ManifestPackageSource::Git {
                repo: "repo".into(),
                commit: "commit".into(),
                path: None,
            },
        }],
    };

    // There's a package on hex with the same name of the git dependency
    // and a new major version. Those are not the same package though!!
    let remote = remote(vec![("wibble", vec![release("2.0.0", vec![])])]);
    let result = check_for_major_version_updates(&manifest, &remote);
    assert_eq!(result, HashMap::new());
}

#[test]
fn local_deps_are_not_checked_on_hex_for_major_version_updates() {
    let manifest = manifest::Manifest {
        requirements: vec![(
            EcoString::from("wibble"),
            requirement::Requirement::Path {
                path: "path".into(),
            },
        )]
        .into_iter()
        .collect(),
        packages: vec![ManifestPackage {
            name: "wibble".into(),
            version: Version::parse("0.1.0").unwrap(),
            build_tools: ["gleam".into()].into(),
            otp_app: None,
            requirements: vec![],
            source: ManifestPackageSource::Local {
                path: "path".into(),
            },
        }],
    };

    // There's a package on hex with the same name of the local dependency
    // and a new major version. Those are not the same package though!!
    let remote = remote(vec![("wibble", vec![release("2.0.0", vec![])])]);
    let result = check_for_major_version_updates(&manifest, &remote);
    assert_eq!(result, HashMap::new());
}

fn retired_release(
    version: &str,
    requirements: Vec<(&str, &str)>,
    reason: hexpm::RetirementReason,
    message: &str,
) -> Release<()> {
    Release {
        retirement_status: Some(RetirementStatus {
            reason,
            message: message.into(),
        }),
        ..release(version, requirements)
    }
}
fn release(version: &str, requirements: Vec<(&str, &str)>) -> Release<()> {
    release_with_optional(version, requirements, vec![])
}

fn release_with_optional(
    version: &str,
    requirements: Vec<(&str, &str)>,
    optional_requirements: Vec<(&str, &str)>,
) -> Release<()> {
    let mut all_requirements = HashMap::new();

    for (name, range) in requirements {
        let requirement = Range::new(range.to_string()).unwrap();
        let dependency = Dependency {
            requirement,
            optional: false,
            app: None,
            repository: None,
        };
        let _ = all_requirements.insert(name.to_string(), dependency);
    }

    for (name, range) in optional_requirements {
        let requirement = Range::new(range.to_string()).unwrap();
        let dependency = Dependency {
            requirement,
            optional: true,
            app: None,
            repository: None,
        };
        let _ = all_requirements.insert(name.to_string(), dependency);
    }

    Release {
        version: Version::try_from(version).unwrap(),
        requirements: all_requirements,
        retirement_status: None,
        outer_checksum: vec![1, 2, 3],
        security_advisories: vec![],
        meta: (),
    }
}

fn remote(dependencies: Vec<(&str, Vec<Release<()>>)>) -> Remote {
    let mut deps = HashMap::new();
    for (package, releases) in dependencies {
        let _ = deps.insert(
            package.into(),
            Rc::new(hexpm::Package {
                name: package.into(),
                repository: "hexpm".into(),
                advisories: vec![],
                releases,
            }),
        );
    }
    Remote { deps }
}

#[test]
fn resolution_error_message() {
    let remote = remote(vec![
        (
            "wibble",
            vec![
                release("1.2.0", vec![("wobble", ">= 1.0.0 and < 2.0.0")]),
                release("1.3.0", vec![("wobble", ">= 2.0.0 and < 3.0.0")]),
            ],
        ),
        (
            "wobble",
            vec![
                release("1.1.0", vec![("woo", ">= 1.0.0 and < 2.0.0")]),
                release("2.0.0", vec![("waa", ">= 1.0.0 and < 2.0.0")]),
            ],
        ),
        (
            "woo",
            vec![release("1.0.0", vec![]), release("2.0.0", vec![])],
        ),
        (
            "waa",
            vec![release("1.0.0", vec![]), release("2.0.0", vec![])],
        ),
    ]);

    let result = resolve_versions(
        &remote,
        HashMap::new(),
        "app".into(),
        vec![
            (
                "wibble".into(),
                Range::new(">= 1.0.0 and < 2.0.0".into()).unwrap(),
            ),
            (
                "woo".into(),
                Range::new(">= 2.0.0 and < 3.0.0".into()).unwrap(),
            ),
            (
                "waa".into(),
                Range::new(">= 2.0.0 and < 3.0.0".into()).unwrap(),
            ),
        ]
        .into_iter(),
        &vec![].into_iter().collect(),
    );

    if let Err(error @ Error::DependencyResolutionNoSolution { .. }) = result {
        let message = error.pretty_string();
        insta::assert_snapshot!(message)
    } else {
        panic!("expected a resolution error message")
    }
}

#[test]
fn resolution_error_message_for_requirement_with_no_lower_bound() {
    let remote = remote(vec![(
        "wibble",
        vec![release("1.2.0", vec![]), release("1.3.0", vec![])],
    )]);

    let result = resolve_versions(
        &remote,
        HashMap::new(),
        "app".into(),
        vec![("wibble".into(), Range::new("< 1.0.0".into()).unwrap())].into_iter(),
        &vec![].into_iter().collect(),
    );

    if let Err(error @ Error::DependencyResolutionNoSolution { .. }) = result {
        let message = error.pretty_string();
        insta::assert_snapshot!(message)
    } else {
        panic!("expected a resolution error message")
    }
}

#[test]
fn resolution_error_message_for_optional_requirement() {
    let result = resolve_versions(
        &make_remote(),
        HashMap::new(),
        "app".into(),
        vec![
            (
                "package_with_optional".into(),
                Range::new("~> 0.1".into()).unwrap(),
            ),
            ("gleam_stdlib".into(), Range::new("~> 0.3".into()).unwrap()),
        ]
        .into_iter(),
        &vec![].into_iter().collect(),
    );

    if let Err(error @ Error::DependencyResolutionNoSolution { .. }) = result {
        let message = error.pretty_string();
        insta::assert_snapshot!(message)
    } else {
        panic!("expected a resolution error message")
    }
}

#[test]
fn resolution_error_message_for_optional_requirement_with_no_lower_bound() {
    let remote = remote(vec![
        (
            "wibble",
            vec![release_with_optional(
                "1.0.0",
                vec![],
                vec![("wobble", "< 3.0.0")],
            )],
        ),
        (
            "wobble",
            vec![release("2.0.0", vec![]), release("3.0.0", vec![])],
        ),
    ]);

    let result = resolve_versions(
        &remote,
        HashMap::new(),
        "app".into(),
        vec![
            ("wibble".into(), Range::new("~> 1.0".into()).unwrap()),
            ("wobble".into(), Range::new(">= 3.0.0".into()).unwrap()),
        ]
        .into_iter(),
        &vec![].into_iter().collect(),
    );

    if let Err(error @ Error::DependencyResolutionNoSolution { .. }) = result {
        let message = error.pretty_string();
        insta::assert_snapshot!(message)
    } else {
        panic!("expected a resolution error message")
    }
}

#[test]
fn adding_package_version_that_does_not_exist() {
    let remote = remote(vec![(
        "wibble",
        vec![release("1.2.0", vec![]), release("1.3.0", vec![])],
    )]);

    let result = resolve_versions(
        &remote,
        HashMap::new(),
        "app".into(),
        vec![("wibble".into(), Range::new("1.0.0".into()).unwrap())].into_iter(),
        &vec![].into_iter().collect(),
    );

    if let Err(error @ Error::DependencyResolutionNoSolution { .. }) = result {
        let message = error.pretty_string();
        insta::assert_snapshot!(message)
    } else {
        panic!("expected a resolution error message")
    }
}

#[test]
fn adding_package_version_that_does_not_exist_2() {
    let remote = remote(vec![(
        "wibble",
        vec![release("1.2.0", vec![]), release("1.3.0", vec![])],
    )]);

    let result = resolve_versions(
        &remote,
        HashMap::new(),
        "app".into(),
        vec![("wibble".into(), Range::new("> 2.0.0".into()).unwrap())].into_iter(),
        &vec![].into_iter().collect(),
    );

    if let Err(error @ Error::DependencyResolutionNoSolution { .. }) = result {
        let message = error.pretty_string();
        insta::assert_snapshot!(message)
    } else {
        panic!("expected a resolution error message")
    }
}
