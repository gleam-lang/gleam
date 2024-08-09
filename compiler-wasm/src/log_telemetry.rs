use gleam_core::build::Telemetry;
#[derive(Debug)]
pub struct LogTelemetry;

impl Telemetry for LogTelemetry {
    fn checked_packages(&self, _start: std::time::Instant) {
        tracing::info!("Checked packages");
    }

    fn compiled_packages(&self, _start: std::time::Instant) {
        tracing::info!("Compiled packages");
    }

    fn compiling_package(&self, name: &str) {
        tracing::info!("Compiling package: {}", name);
    }

    fn checking_package(&self, name: &str) {
        tracing::info!("Checking package: {}", name);
    }

    fn downloading_package(&self, name: &str) {
        tracing::info!("Downloading package: {}", name);
    }

    fn packages_downloaded(&self, _start: std::time::Instant, count: usize) {
        tracing::info!("Downloaded {} packages", count);
    }

    fn resolving_package_versions(&self) {
        tracing::info!("Resolving package versions");
    }

    fn running_module(&self, name: &str) {
        tracing::info!("Running {}.main", name);
    }

    fn waiting_for_build_directory_lock(&self) {
        tracing::info!("Waiting for build directory lock");
    }
}
