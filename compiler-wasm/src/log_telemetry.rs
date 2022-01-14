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
}
