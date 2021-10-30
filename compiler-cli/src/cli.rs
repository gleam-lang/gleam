use gleam_core::{
    build::Telemetry,
    error::{Error, StandardIoAction},
};
use hexpm::version::Version;
use std::{
    io::Write,
    time::{Duration, Instant},
};
use termcolor::{BufferWriter, Color, ColorChoice, ColorSpec, WriteColor};

#[derive(Debug, Default, Clone)]
pub struct Reporter;

impl Reporter {
    pub fn new() -> Self {
        Self::default()
    }
}

impl Telemetry for Reporter {
    fn compiling_package(&self, name: &str) {
        print_compiling(name);
    }
}

pub fn ask(question: &str) -> Result<String, Error> {
    print!("{}: ", question);
    std::io::stdout().flush().expect("ask stdout flush");
    let mut answer = String::new();
    let _ = std::io::stdin()
        .read_line(&mut answer)
        .map_err(|e| Error::StandardIo {
            action: StandardIoAction::Read,
            err: Some(e.kind()),
        })?;
    Ok(answer.trim().to_string())
}

pub fn ask_password(question: &str) -> Result<String, Error> {
    let prompt = format!("{} (will not be printed as you type): ", question);
    rpassword::read_password_from_tty(Some(prompt.as_str()))
        .map_err(|e| Error::StandardIo {
            action: StandardIoAction::Read,
            err: Some(e.kind()),
        })
        .map(|s| s.trim().to_string())
}

pub fn print_publishing(name: &str, version: &Version) {
    print_colourful_prefix(" Publishing", &format!("{} v{}", name, version.to_string()))
}

pub fn print_published(duration: Duration) {
    print_colourful_prefix("  Published", &format!("in {}", seconds(duration)))
}

pub fn print_downloading(text: &str) {
    print_colourful_prefix("Downloading", text)
}

pub fn print_resolving_versions() {
    print_colourful_prefix("  Resolving", "versions")
}

pub fn print_compiling(text: &str) {
    print_colourful_prefix("  Compiling", text)
}

pub fn print_compiled(duration: Duration) {
    print_colourful_prefix("   Compiled", &format!("in {}", seconds(duration)))
}

pub fn print_running(text: &str) {
    print_colourful_prefix("    Running", text)
}

pub fn print_packages_downloaded(start: Instant, count: usize) {
    let elapsed = seconds(start.elapsed());
    let msg = match count {
        1 => format!("1 package in {}", elapsed),
        _ => format!("{} packages in {}", count, elapsed),
    };
    print_colourful_prefix(" Downloaded", &msg)
}

pub fn seconds(duration: Duration) -> String {
    format!("{:.2}s", duration.as_millis() as f32 / 1000.)
}

pub fn print_colourful_prefix(prefix: &str, text: &str) {
    let buffer_writer = stdout_buffer_writer();
    let mut buffer = buffer_writer.buffer();
    buffer
        .set_color(
            ColorSpec::new()
                .set_intense(true)
                .set_fg(Some(Color::Magenta)),
        )
        .expect("print_green_prefix");
    write!(buffer, "{}", prefix).expect("print_green_prefix");
    buffer
        .set_color(&ColorSpec::new())
        .expect("print_green_prefix");
    writeln!(buffer, " {}", text).expect("print_green_prefix");
    buffer_writer.print(&buffer).expect("print_green_prefix");
}

pub fn stderr_buffer_writer() -> BufferWriter {
    // Don't add color codes to the output if standard error isn't connected to a terminal
    termcolor::BufferWriter::stderr(color_choice())
}

pub fn stdout_buffer_writer() -> BufferWriter {
    // Don't add color codes to the output if standard error isn't connected to a terminal
    termcolor::BufferWriter::stdout(color_choice())
}

fn color_choice() -> ColorChoice {
    if atty::is(atty::Stream::Stderr) {
        termcolor::ColorChoice::Auto
    } else {
        termcolor::ColorChoice::Never
    }
}
