use gleam_core::build::Telemetry;

#[allow(dead_code)]
#[derive(Debug)]
pub struct LogTelemetry;

impl Telemetry for LogTelemetry {
    fn compiled_package(&self, duration: std::time::Duration) {
        tracing::info!("Compiled in {} ", seconds(duration));
    }

    fn compiling_package(&self, name: &str) {
        tracing::info!("Compiling package: {}", name);
    }

    fn checked_package(&self, duration: std::time::Duration) {
        tracing::info!("Checked in {}", seconds(duration));
    }

    fn checking_package(&self, name: &str) {
        tracing::info!("Checking package: {}", name);
    }

    fn downloading_package(&self, name: &str) {
        tracing::info!("Downloading package: {}", name);
    }

    fn running(&self, name: &str) {
        tracing::info!("Running {}", name);
    }

    fn resolving_package_versions(&self) {
        tracing::info!("Resolving package versions");
    }

    fn packages_downloaded(&self, _start: std::time::Instant, count: usize) {
        tracing::info!("Downloaded {} packages", count);
    }

    fn waiting_for_build_directory_lock(&self) {
        tracing::info!("Waiting for build directory lock");
    }
}

#[allow(dead_code)]
pub fn seconds(duration: std::time::Duration) -> String {
    format!("{:.2}s", duration.as_millis() as f32 / 1000.)
}
