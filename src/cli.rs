use crate::error::{Error, StandardIoAction};
use std::io::Write;
use termcolor::{BufferWriter, Color, ColorChoice, ColorSpec, WriteColor};

pub fn ask(question: &str) -> Result<String, Error> {
    print!("{}: ", question);
    std::io::stdout().flush().unwrap();
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

pub fn print_compiling(text: &str) {
    print_green_prefix("Compiling", text)
}

pub fn print_running(text: &str) {
    print_green_prefix("  Running", text)
}

pub fn print_green_prefix(prefix: &str, text: &str) {
    let buffer_writer = stdout_buffer_writer();
    let mut buffer = buffer_writer.buffer();
    buffer
        .set_color(ColorSpec::new().set_bold(true).set_fg(Some(Color::Green)))
        .unwrap();
    write!(buffer, "{}", prefix).unwrap();
    buffer.set_color(&ColorSpec::new()).unwrap();
    writeln!(buffer, " {}", text).unwrap();
    buffer_writer.print(&buffer).unwrap();
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
