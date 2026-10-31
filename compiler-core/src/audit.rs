#[cfg(test)]
mod tests;

use crate::{Error, dependency::PackageFetcher, manifest};
use clap::ValueEnum;
use ecow::EcoString;
use hexpm::{Release, version::Version};
use strum::{EnumString, VariantNames};

#[derive(Debug, EnumString, VariantNames, ValueEnum, Clone, Copy, PartialEq, Eq)]
pub enum AuditOutputFormat {
    Text,
    Json,
}

#[derive(Debug, Default, PartialEq, serde::Serialize, serde::Deserialize)]
pub struct Audit {
    pub packages: Vec<AuditPackage>,
}

impl Audit {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn audit_manifest(
        manifest: &manifest::Manifest,
        package_fetcher: &impl PackageFetcher,
    ) -> Result<Audit, Error> {
        let mut advisories = Audit::default();

        for package in &manifest.packages {
            let name = &package.name;
            let version = &package.version;

            // We can only look-up advisories information for dependencies from Hex.
            if !package.is_hex() {
                continue;
            }

            let hex_package = package_fetcher.get_package(&package.name)?;

            let Some(release) = hex_package
                .releases
                .iter()
                .find(|release| release.version == package.version)
            else {
                return Err(Error::CouldNotFindPackageVersionInHexRepo {
                    package: name.clone(),
                    version: version.clone(),
                });
            };

            advisories.record_problems(name, version, release);
        }

        Ok(advisories)
    }

    pub fn is_empty(&self) -> bool {
        self.packages.is_empty()
    }

    pub fn as_output(&self, format: AuditOutputFormat) -> String {
        match format {
            AuditOutputFormat::Text => self.as_text(),
            AuditOutputFormat::Json => self.as_json(),
        }
    }

    fn as_json(&self) -> String {
        serde_json::to_string_pretty(self).expect("JSON audit serialisation")
    }

    fn as_text(&self) -> String {
        let mut out = String::new();

        for package in &self.packages {
            out.push_str(&package.name);
            out.push(' ');
            out.push_str(package.version.to_string().as_str());

            if let Some(retirement) = package.retirement.as_ref() {
                out.push_str("\n  Retired (");
                out.push_str(retirement.reason.to_str());
                out.push_str(")\n  ");
                out.push_str(retirement.message.as_str());
                out.push('\n');
            }

            for advisory in &package.advisories {
                out.push_str("\n  ");
                out.push_str(&advisory.id);
                if let Some(rating) = advisory.cvss_rating.as_ref() {
                    out.push_str(" (");
                    out.push_str(rating.to_str());
                    out.push(')');
                }
                if !advisory.aliases.is_empty() {
                    let mut first = true;
                    for alias in &advisory.aliases {
                        if first {
                            first = false;
                            out.push_str("\n  aliases: ");
                        } else {
                            out.push_str(", ");
                        }
                        out.push_str(alias);
                    }
                }
                out.push_str("\n  ");
                out.push_str(&advisory.summary);
                out.push_str("\n  ");
                out.push_str(&advisory.html_url);
                out.push('\n');
            }

            out.push('\n');
        }

        out
    }

    pub fn record_problems(
        &mut self,
        package: &EcoString,
        version: &Version,
        release: &Release<()>,
    ) {
        if !release.retirement_status.is_some() && release.security_advisories.is_empty() {
            return;
        }

        let advisories = release
            .security_advisories
            .iter()
            .map(|advisory| SecurityAdvisory {
                id: advisory.id.clone(),
                summary: advisory.summary.clone(),
                html_url: advisory.html_url.clone(),
                cvss_rating: advisory.cvss_rating.clone(),
                aliases: advisory.aliases.clone(),
            })
            .collect();

        self.packages.push(AuditPackage {
            name: package.clone(),
            version: version.clone(),
            retirement: release.retirement_status.clone(),
            advisories,
        });
    }
}

#[derive(Debug, PartialEq, serde::Serialize, serde::Deserialize)]
pub struct AuditPackage {
    pub name: EcoString,
    pub version: Version,
    pub retirement: Option<hexpm::RetirementStatus>,
    pub advisories: Vec<SecurityAdvisory>,
}

#[derive(Debug, PartialEq, serde::Serialize, serde::Deserialize)]
pub struct SecurityAdvisory {
    pub id: String,
    pub summary: String,
    pub html_url: String,
    pub cvss_rating: Option<hexpm::AdvisorySeverity>,
    pub aliases: Vec<String>,
}
