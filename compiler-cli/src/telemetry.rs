use std::time::Instant;

use gleam_core::build::Telemetry;

#[derive(Debug, Clone, Copy)]
pub(crate) struct NullTelemetry;

impl Telemetry for NullTelemetry {
    fn waiting_for_build_directory_lock(&self) {}
    fn resolving_package_versions(&self) {}
    fn downloading_package(&self, _name: &str) {}
    fn compiling_package(&self, _name: &str) {}
    fn checking_package(&self, _name: &str) {}
    fn warning(&self, _warning: &gleam_core::Warning) {}
    fn packages_downloaded(&self, _start: Instant, _count: usize) {}
}
