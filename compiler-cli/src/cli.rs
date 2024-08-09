use gleam_core::{
    build::Telemetry,
    error::{Error, StandardIoAction},
};
use hexpm::version::Version;
use std::{
    io::{IsTerminal, Write},
    time::{Duration, Instant},
};
use termcolor::{BufferWriter, Color, ColorChoice, ColorSpec, WriteColor};

#[derive(Debug, Default, Clone)]
pub struct Reporter;

impl Reporter {
    pub fn new() -> Self {
        Self
    }
}

impl Telemetry for Reporter {
    fn checked_packages(&self, start: Instant) {
        print_checked(start.elapsed())
    }

    fn checking_package(&self, name: &str) {
        print_checking(name);
    }

    fn compiled_packages(&self, start: Instant) {
        print_compiled(start.elapsed())
    }

    fn compiling_package(&self, name: &str) {
        print_compiling(name);
    }

    fn downloading_package(&self, name: &str) {
        print_downloading(name)
    }

    fn packages_downloaded(&self, start: Instant, count: usize) {
        print_packages_downloaded(start, count)
    }

    fn resolving_package_versions(&self) {
        print_resolving_versions()
    }

    fn waiting_for_build_directory_lock(&self) {
        print_waiting_for_build_directory_lock()
    }

    fn running_module(&self, name: &str) {
        print_running(&format!("{name}.main"))
    }
}

pub fn ask(question: &str) -> Result<String, Error> {
    print!("{question}: ");
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

pub fn confirm(question: &str) -> Result<bool, Error> {
    let answer = ask(&format!("{question} [y/n]", question = question))?;
    match answer.as_str() {
        "y" | "yes" | "Y" | "YES" => Ok(true),
        _ => Ok(false),
    }
}

pub fn ask_password(question: &str) -> Result<String, Error> {
    let prompt = format!("{question} (will not be printed as you type): ");
    rpassword::prompt_password(prompt)
        .map_err(|e| Error::StandardIo {
            action: StandardIoAction::Read,
            err: Some(e.kind()),
        })
        .map(|s| s.trim().to_string())
}

pub fn print_publishing(name: &str, version: &Version) {
    print_colourful_prefix("Publishing", &format!("{name} v{version}"))
}

pub fn print_published(duration: Duration) {
    print_colourful_prefix("Published", &format!("in {}", seconds(duration)))
}

pub fn print_retired(package: &str, version: &str) {
    print_colourful_prefix("Retired", &format!("{package} {version}"))
}

pub fn print_unretired(package: &str, version: &str) {
    print_colourful_prefix("Unretired", &format!("{package} {version}"))
}

pub fn print_publishing_documentation() {
    print_colourful_prefix("Publishing", "documentation");
}

fn print_downloading(text: &str) {
    print_colourful_prefix("Downloading", text)
}

fn print_waiting_for_build_directory_lock() {
    print_colourful_prefix("Waiting", "for build directory lock")
}

fn print_resolving_versions() {
    print_colourful_prefix("Resolving", "versions")
}

fn print_compiling(text: &str) {
    print_colourful_prefix("Compiling", text)
}

pub(crate) fn print_exported(text: &str) {
    print_colourful_prefix("Exported", text)
}

pub(crate) fn print_checking(text: &str) {
    print_colourful_prefix("Checking", text)
}

pub(crate) fn print_compiled(duration: Duration) {
    print_colourful_prefix("Compiled", &format!("in {}", seconds(duration)))
}

pub(crate) fn print_checked(duration: Duration) {
    print_colourful_prefix("Checked", &format!("in {}", seconds(duration)))
}

pub(crate) fn print_running(text: &str) {
    print_colourful_prefix("Running", text)
}

pub(crate) fn print_added(text: &str) {
    print_colourful_prefix("Added", text)
}

pub(crate) fn print_removed(text: &str) {
    print_colourful_prefix("Removed", text)
}

pub(crate) fn print_generating_documentation() {
    print_colourful_prefix("Generating", "documentation")
}

fn print_packages_downloaded(start: Instant, count: usize) {
    let elapsed = seconds(start.elapsed());
    let msg = match count {
        1 => format!("1 package in {elapsed}"),
        _ => format!("{count} packages in {elapsed}"),
    };
    print_colourful_prefix("Downloaded", &msg)
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
    write!(buffer, "{prefix: >11}").expect("print_green_prefix");
    buffer
        .set_color(&ColorSpec::new())
        .expect("print_green_prefix");
    writeln!(buffer, " {text}").expect("print_green_prefix");
    buffer_writer.print(&buffer).expect("print_green_prefix");
}

pub fn stderr_buffer_writer() -> BufferWriter {
    // Don't add color codes to the output if standard error isn't connected to a terminal
    BufferWriter::stderr(color_choice())
}

pub fn stdout_buffer_writer() -> BufferWriter {
    // Don't add color codes to the output if standard error isn't connected to a terminal
    BufferWriter::stdout(color_choice())
}

fn colour_forced() -> bool {
    if let Ok(force) = std::env::var("FORCE_COLOR") {
        !force.is_empty()
    } else {
        false
    }
}

fn color_choice() -> ColorChoice {
    if colour_forced() {
        ColorChoice::Always
    } else if std::io::stderr().is_terminal() {
        ColorChoice::Auto
    } else {
        ColorChoice::Never
    }
}
