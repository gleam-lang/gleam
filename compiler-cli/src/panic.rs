#![allow(clippy::unwrap_used)]
use std::panic::PanicInfo;

pub fn add_handler() {
    std::panic::set_hook(Box::new(move |info: &PanicInfo<'_>| {
        print_compiler_bug_message(info)
    }));
}

fn print_compiler_bug_message(info: &PanicInfo<'_>) {
    let message = match (
        info.payload().downcast_ref::<&str>(),
        info.payload().downcast_ref::<String>(),
    ) {
        (Some(s), _) => (*s).to_string(),
        (_, Some(s)) => s.to_string(),
        (None, None) => "unknown error".into(),
    };
    let location = match info.location() {
        None => "".into(),
        Some(location) => format!("{}:{}\n\t", location.file(), location.line()),
    };

    let buffer_writer = crate::cli::stderr_buffer_writer();
    let mut buffer = buffer_writer.buffer();
    use std::io::Write;
    use termcolor::{Color, ColorSpec, WriteColor};
    buffer
        .set_color(ColorSpec::new().set_bold(true).set_fg(Some(Color::Red)))
        .unwrap();
    write!(buffer, "error").unwrap();
    buffer.set_color(ColorSpec::new().set_bold(true)).unwrap();
    write!(buffer, ": Fatal compiler bug!\n\n").unwrap();
    buffer.set_color(&ColorSpec::new()).unwrap();
    writeln!(
        buffer,
        "This is a bug in the Gleam compiler, sorry!

Please report this crash to https://github.com/gleam-lang/gleam/issues/new
and include this error message with your report.

Panic: {location}{message}
Gleam version: {version}
Operating system: {os}

If you can also share your code and say what file you were editing or any
steps to reproduce the crash that would be a great help.

You may also want to try again with the `GLEAM_LOG=trace` environment
variable set.
",
        location = location,
        message = message,
        version = env!("CARGO_PKG_VERSION"),
        os = std::env::consts::OS,
    )
    .unwrap();
    buffer_writer.print(&buffer).unwrap();
}
