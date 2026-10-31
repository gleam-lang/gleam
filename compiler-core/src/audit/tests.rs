// SPDX-License-Identifier: Apache-2.0
// SPDX-FileCopyrightText: 2026 The Gleam contributors

use super::{Audit, AuditOutputFormat, AuditPackage};
use crate::{
    Error,
    audit::SecurityAdvisory,
    dependency::PackageFetcher,
    manifest::{Base16Checksum, ManifestPackage},
};
use hexpm::{AdvisorySeverity, RetirementReason, RetirementStatus, version::Version};
use std::{collections::HashMap, rc::Rc};

fn example_audit() -> Audit {
    Audit {
        packages: vec![
            AuditPackage {
                name: "retired_package".into(),
                version: Version::new(1, 2, 3),
                retirement: Some(RetirementStatus {
                    reason: RetirementReason::Security,
                    message: "whoops! oh dear!".into(),
                }),
                advisories: vec![],
            },
            AuditPackage {
                name: "advisory_package".into(),
                version: Version::new(2, 1, 2),
                retirement: None,
                advisories: vec![SecurityAdvisory {
                    id: "CVE-1234-5678".into(),
                    summary: "summary 1".into(),
                    html_url: "https://example.com/1".into(),
                    cvss_rating: None,
                    aliases: vec!["ALIAS-1234-5678".into()],
                }],
            },
            AuditPackage {
                name: "both_package".into(),
                version: Version::new(4, 3, 4),
                retirement: Some(RetirementStatus {
                    reason: RetirementReason::Invalid,
                    message: "retired!".into(),
                }),
                advisories: vec![
                    SecurityAdvisory {
                        id: "CVE-5364-6353".into(),
                        summary: "summary 2".into(),
                        html_url: "https://example.com/2".into(),
                        cvss_rating: Some(AdvisorySeverity::High),
                        aliases: vec![],
                    },
                    SecurityAdvisory {
                        id: "CVE-9876-9999".into(),
                        summary: "summary 3".into(),
                        html_url: "https://example.com/2".into(),
                        cvss_rating: Some(AdvisorySeverity::Critical),
                        aliases: vec!["ALIAS-6554-3677".into(), "OTHER-3558-9935".into()],
                    },
                ],
            },
        ],
    }
}

#[test]
fn audit_text_format() {
    insta::assert_snapshot!(example_audit().as_output(AuditOutputFormat::Text));
}

#[test]
fn audit_json_format() {
    insta::assert_snapshot!(example_audit().as_output(AuditOutputFormat::Json));
}

#[derive(Default)]
struct PackageRemote {
    deps: HashMap<String, Rc<hexpm::Package>>,
}

impl PackageRemote {
    fn insert(
        &mut self,
        name: &str,
        releases: Vec<(
            Version,
            Option<RetirementStatus>,
            Vec<hexpm::SecurityAdvisory>,
        )>,
    ) {
        let advisories = releases
            .iter()
            .flat_map(|(_, _, advisories)| advisories)
            .cloned()
            .collect();
        let releases = releases
            .into_iter()
            .map(
                |(version, retirement_status, security_advisories)| hexpm::Release {
                    version,
                    requirements: HashMap::new(),
                    retirement_status,
                    outer_checksum: vec![],
                    security_advisories,
                    meta: (),
                },
            )
            .collect();
        let package = hexpm::Package {
            name: name.into(),
            repository: "hexpm".into(),
            releases,
            advisories,
        };
        _ = self.deps.insert(name.into(), Rc::new(package));
    }
}

impl PackageFetcher for PackageRemote {
    fn get_package(&self, package: &str) -> Result<Rc<hexpm::Package>, Error> {
        self.deps.get(package).map(Rc::clone).ok_or_else(|| {
            Error::Http(format!(
                "the package {package} was not found in the package repository"
            ))
        })
    }
}

fn manifest_packages(packages: &[(&str, u32, u32, u32)]) -> Vec<ManifestPackage> {
    packages
        .iter()
        .map(|(name, major, minor, patch)| ManifestPackage {
            name: (*name).into(),
            version: Version::new(*major, *minor, *patch),
            build_tools: vec!["gleam".into()],
            otp_app: None,
            requirements: vec![],
            source: crate::manifest::ManifestPackageSource::Hex {
                outer_checksum: Base16Checksum(vec![]),
            },
        })
        .collect()
}

#[test]
fn audit_manifest_not_dependency() {
    let mut remote = PackageRemote::default();
    let packages = vec![];

    // This package exists in the remote, is retired, and has
    // advisories, but is not a dependency.
    remote.insert(
        "not_a_dependency",
        vec![(
            Version::new(1, 2, 3),
            Some(RetirementStatus {
                reason: RetirementReason::Security,
                message: "something".into(),
            }),
            vec![hexpm::SecurityAdvisory {
                id: "123".into(),
                summary: "summary".into(),
                cvss_rating: None,
                aliases: vec![],
                cvss_score: None,
                html_url: "https://example.com".into(),
                api_url: "https://example.com/api".into(),
            }],
        )],
    );

    let audit = Audit::audit_manifest(&packages, &remote).unwrap();
    assert!(audit.is_empty());
}

#[test]
fn audit_manifest_other_version_has_problems() {
    let mut remote = PackageRemote::default();

    // This package exists in the remote, is retired, and has
    // advisories, but is not a dependency.
    remote.insert(
        "wibble",
        vec![
            (Version::new(1, 0, 0), None, vec![]),
            (
                Version::new(2, 0, 0),
                Some(RetirementStatus {
                    reason: RetirementReason::Security,
                    message: "something".into(),
                }),
                vec![],
            ),
        ],
    );
    let packages = manifest_packages(&[("wibble", 1, 0, 0)]);

    let audit = Audit::audit_manifest(&packages, &remote).unwrap();
    assert!(audit.is_empty());
}

#[test]
fn audit_manifest_retired() {
    let mut remote = PackageRemote::default();

    // This package exists in the remote, is retired, and has
    // advisories, but is not a dependency.
    remote.insert(
        "wibble",
        vec![
            (Version::new(1, 0, 0), None, vec![]),
            (
                Version::new(2, 0, 0),
                Some(RetirementStatus {
                    reason: RetirementReason::Security,
                    message: "something".into(),
                }),
                vec![],
            ),
        ],
    );
    let packages = manifest_packages(&[("wibble", 2, 0, 0)]);

    let audit = Audit::audit_manifest(&packages, &remote).unwrap();
    assert_eq!(
        audit,
        Audit {
            packages: vec![AuditPackage {
                name: "wibble".into(),
                version: Version::new(2, 0, 0),
                retirement: Some(RetirementStatus {
                    reason: RetirementReason::Security,
                    message: "something".into(),
                }),
                advisories: vec![],
            }]
        }
    );
}

#[test]
fn audit_manifest_cve() {
    let mut remote = PackageRemote::default();

    // This package exists in the remote, is retired, and has
    // advisories, but is not a dependency.
    remote.insert(
        "wibble",
        vec![(
            Version::new(2, 0, 0),
            None,
            vec![hexpm::SecurityAdvisory {
                id: "123".into(),
                summary: "summary".into(),
                cvss_rating: None,
                aliases: vec![],
                cvss_score: None,
                html_url: "https://example.com".into(),
                api_url: "https://example.com/api".into(),
            }],
        )],
    );
    let packages = manifest_packages(&[("wibble", 2, 0, 0)]);

    let audit = Audit::audit_manifest(&packages, &remote).unwrap();
    assert_eq!(
        audit,
        Audit {
            packages: vec![AuditPackage {
                name: "wibble".into(),
                version: Version::new(2, 0, 0),
                retirement: None,
                advisories: vec![SecurityAdvisory {
                    id: "123".into(),
                    summary: "summary".into(),
                    cvss_rating: None,
                    aliases: vec![],
                    html_url: "https://example.com".into(),
                }],
            }]
        }
    );
}
