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
        (None, None) => "unknown error".to_string(),
    };
    let location = match info.location() {
        None => "".to_string(),
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
with this information and the code that produces the crash.

\t{}{}",
        location, message
    )
    .unwrap();
    buffer_writer.print(&buffer).unwrap();
}
