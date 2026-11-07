use crate::audit::SecurityAdvisory;

use super::{Audit, AuditOutputFormat, AuditPackage};
use hexpm::{AdvisorySeverity, RetirementStatus, version::Version};

fn example_audit() -> Audit {
    Audit {
        packages: vec![
            AuditPackage {
                name: "retired_package".into(),
                version: Version::new(1, 2, 3),
                retirement: Some(RetirementStatus {
                    reason: hexpm::RetirementReason::Security,
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
                    reason: hexpm::RetirementReason::Invalid,
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
