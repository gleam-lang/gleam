use std::{
    fmt::Debug,
    time::{Duration, Instant},
};

use crate::Warning;

pub trait Telemetry: Debug {
    fn waiting_for_build_directory_lock(&self);
    fn resolving_package_versions(&self);
    fn downloading_package(&self, name: &str, print_progress: bool);
    fn packages_downloaded(&self, start: Instant, count: usize, print_progress: bool);
    fn compiling_package(&self, name: &str, print_progress: bool);
    fn checking_package(&self, name: &str);
}

#[derive(Debug, Clone, Copy)]
pub struct NullTelemetry;

impl Telemetry for NullTelemetry {
    fn waiting_for_build_directory_lock(&self) {}
    fn resolving_package_versions(&self) {}
    fn downloading_package(&self, _name: &str, _print_progress: bool) {}
    fn compiling_package(&self, _name: &str, _print_progress: bool) {}
    fn checking_package(&self, _name: &str) {}
    fn packages_downloaded(&self, _start: Instant, _count: usize, _print_progress: bool) {}
}
