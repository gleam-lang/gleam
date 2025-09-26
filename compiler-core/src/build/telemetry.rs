use std::{
    fmt::Debug,
    time::{Duration, Instant},
};

use crate::{Warning, manifest::Resolved};

pub trait Telemetry: Debug {
    fn waiting_for_build_directory_lock(&self);
    fn running(&self, name: &str);
    fn resolving_package_versions(&self);
    fn resolved_package_versions(&self, resolved: &Resolved);
    fn downloading_package(&self, name: &str);
    fn packages_downloaded(&self, start: Instant, count: usize);
    fn compiled_package(&self, duration: Duration);
    fn compiling_package(&self, name: &str);
    fn checked_package(&self, duration: Duration);
    fn checking_package(&self, name: &str);
}

#[derive(Debug, Clone, Copy)]
pub struct NullTelemetry;

impl Telemetry for NullTelemetry {
    fn waiting_for_build_directory_lock(&self) {}
    fn running(&self, name: &str) {}
    fn resolving_package_versions(&self) {}
    fn downloading_package(&self, _name: &str) {}
    fn compiled_package(&self, _duration: Duration) {}
    fn compiling_package(&self, _name: &str) {}
    fn checked_package(&self, _duration: Duration) {}
    fn checking_package(&self, _name: &str) {}
    fn packages_downloaded(&self, _start: Instant, _count: usize) {}
    fn resolved_package_versions(&self, _resolved: &Resolved) {}
}
