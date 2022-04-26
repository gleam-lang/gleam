use gleam_core::build::Telemetry;
use gleam_core::Warning;
use termcolor::Buffer;

#[derive(Debug)]
pub struct LogTelemetry;

impl Telemetry for LogTelemetry {
    fn compiling_package(&self, name: &str) {
        tracing::info!("Compiling package: {}", name);
    }

    fn checking_package(&self, name: &str) {
        tracing::info!("Checking package: {}", name);
    }

    fn warning(&self, warning: &Warning) {
        let mut buffer = Buffer::no_color();
        warning.pretty(&mut buffer);

        let string =
            std::str::from_utf8(buffer.as_slice()).expect("buffer should be utf-8 encoded");

        tracing::warn!("Warning: {:?}", string);
    }

    fn downloading_package(&self, name: &str) {
        tracing::info!("Downloading package: {}", name);
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
