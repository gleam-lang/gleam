use crate::manifest::Manifest;
use crate::requirement::Requirement;
use ecow::EcoString;
use hexpm::version::Version;
use std::collections::{HashMap, HashSet};

#[derive(Debug)]
pub struct StalePackageRemover<'a> {
    // These are the packages for which the requirement or their parents
    // requirement has not changed.
    fresh: HashSet<&'a str>,
    locked: HashMap<EcoString, &'a Vec<EcoString>>,
}

impl<'a> StalePackageRemover<'a> {
    pub fn fresh_and_locked(
        requirements: &'a HashMap<EcoString, Requirement>,
        manifest: &'a Manifest,
    ) -> HashMap<EcoString, Version> {
        let locked = manifest
            .packages
            .iter()
            .map(|p| (p.name.clone(), &p.requirements))
            .collect();
        Self {
            fresh: HashSet::new(),
            locked,
        }
        .run(requirements, manifest)
    }

    fn run(
        &mut self,
        requirements: &'a HashMap<EcoString, Requirement>,
        manifest: &'a Manifest,
    ) -> HashMap<EcoString, Version> {
        // Record all the requirements that have not changed
        for (name, requirement) in requirements {
            if manifest.requirements.get(name) != Some(requirement) {
                continue; // This package has changed, don't record it
            }

            // Recursively record the package and its deps as being fresh
            self.record_tree_fresh(name);
        }

        // Return all the previously resolved packages that have not been
        // recorded as fresh
        manifest
            .packages
            .iter()
            .filter(|package| {
                let new = requirements.contains_key(package.name.as_str())
                    && !manifest.requirements.contains_key(package.name.as_str());
                let fresh = self.fresh.contains(package.name.as_str());
                let locked = !new && fresh;
                if !locked {
                    tracing::info!(name = package.name.as_str(), "unlocking_stale_package");
                }
                locked
            })
            .map(|package| (package.name.clone(), package.version.clone()))
            .collect()
    }

    fn record_tree_fresh(&mut self, name: &'a str) {
        // Record the top level package
        let _ = self.fresh.insert(name);

        let Some(deps) = self.locked.get(name) else {
            // If the package is not in the manifest then it means that the package is an optional
            // dependency that has not been included. That or someone has been editing the manifest
            // and broken it, but let's hope that's not the case.
            return;
        };

        // Record each of its deps recursively
        for package in *deps {
            self.record_tree_fresh(package);
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::manifest::{Base16Checksum, Manifest, ManifestPackage, ManifestPackageSource};
    use crate::requirement::Requirement;
    use hexpm::version::{Range, Version};
    use std::collections::HashMap;

    // https://github.com/gleam-lang/gleam/issues/4152
    #[test]
    fn optional_package_not_in_manifest() {
        let requirements = HashMap::from_iter([(
            "required_package".into(),
            Requirement::Hex {
                version: Range::new("1.0.0".into()).unwrap(),
            },
        )]);
        let manifest = Manifest {
            requirements: requirements.clone(),
            packages: vec![ManifestPackage {
                name: "required_package".into(),
                version: Version::new(1, 0, 0),
                build_tools: vec!["gleam".into()],
                otp_app: None,
                requirements: vec![
                    // NOTE: this package isn't in the manifest. This will have been because it is
                    // an optional dep of `required_package`.
                    "optional_package".into(),
                ],
                source: ManifestPackageSource::Hex {
                    outer_checksum: Base16Checksum(vec![]),
                },
            }],
        };

        assert_eq!(
            StalePackageRemover::fresh_and_locked(&requirements, &manifest),
            HashMap::from_iter([("required_package".into(), Version::new(1, 0, 0))])
        );
    }
}
