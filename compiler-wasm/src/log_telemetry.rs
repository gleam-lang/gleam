use gleam_core::build::Telemetry;

#[derive(Debug)]
pub struct LogTelemetry;

impl Telemetry for LogTelemetry {
    fn compiling_package(&self, name: &str) {
      log::info!("Compiling package: {}", name);
    }

    fn checking_package(&self, name: &str) {
      log::info!("Checking package: {}", name);
    }
}