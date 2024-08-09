use std::{
    fmt::Debug,
    time::{Duration, Instant},
};

use crate::Warning;

pub trait Telemetry: Debug {
    fn checked_packages(&self, start: Instant);
    fn checking_package(&self, name: &str);
    fn compiled_packages(&self, start: Instant);
    fn compiling_package(&self, name: &str);
    fn downloading_package(&self, name: &str);
    fn packages_downloaded(&self, start: Instant, count: usize);
    fn resolving_package_versions(&self);
    fn running_module(&self, name: &str);
    fn waiting_for_build_directory_lock(&self);
}

#[derive(Debug, Clone, Copy)]
pub struct NullTelemetry;

impl Telemetry for NullTelemetry {
    fn checked_packages(&self, _start: Instant) {}
    fn compiled_packages(&self, _start: Instant) {}
    fn waiting_for_build_directory_lock(&self) {}
    fn resolving_package_versions(&self) {}
    fn downloading_package(&self, _name: &str) {}
    fn compiling_package(&self, _name: &str) {}
    fn checking_package(&self, _name: &str) {}
    fn packages_downloaded(&self, _start: Instant, _count: usize) {}
    fn running_module(&self, _name: &str) {}
}
