use std::{
    fmt::Debug,
    time::{Duration, Instant},
};

use crate::Warning;

pub trait Telemetry: Debug {
    fn waiting_for_build_directory_lock(&self);
    fn resolving_package_versions(&self);
    fn downloading_package(&self, name: &str);
    fn packages_downloaded(&self, start: Instant, count: usize);
    fn compiling_package(&self, name: &str);
    fn checking_package(&self, name: &str);
}

#[derive(Debug, Clone, Copy)]
pub struct NullTelemetry;

impl Telemetry for NullTelemetry {
    fn waiting_for_build_directory_lock(&self) {}
    fn resolving_package_versions(&self) {}
    fn downloading_package(&self, _name: &str) {}
    fn compiling_package(&self, _name: &str) {}
    fn checking_package(&self, _name: &str) {}
    fn packages_downloaded(&self, _start: Instant, _count: usize) {}
}
