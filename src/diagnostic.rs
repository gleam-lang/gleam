pub use codespan_reporting::diagnostic::Severity;
use termcolor::Buffer;

pub struct Diagnostic {
    pub file: String,
    pub location: crate::ast::SrcSpan,
    pub src: String,
    pub title: String,
    pub label: String,
}

pub fn write(mut buffer: &mut Buffer, d: Diagnostic, severity: Severity) {
    use codespan::Files;
    use codespan_reporting::diagnostic::Label;
    use codespan_reporting::term::emit;

    let mut files = Files::new();
    let file_id = files.add(d.file, d.src);

    let diagnostic = codespan_reporting::diagnostic::Diagnostic::new(
        severity,
        d.title,
        Label::new(
            file_id,
            (d.location.start as u32)..(d.location.end as u32),
            d.label,
        ),
    );

    let config = codespan_reporting::term::Config::default();
    emit(&mut buffer, &config, &files, &diagnostic).unwrap();
}

pub fn buffer_writer() -> termcolor::BufferWriter {
    // Don't add color codes to the output if standard error isn't connected to a terminal
    let color_choice = if atty::is(atty::Stream::Stderr) {
        termcolor::ColorChoice::Auto
    } else {
        termcolor::ColorChoice::Never
    };
    termcolor::BufferWriter::stderr(color_choice)
}

/// Describes an error encountered while compiling the project (eg. a name collision
/// between files).
///
pub struct ProjectErrorDiagnostic {
    pub title: String,
    pub label: String,
}

pub fn write_title(buffer: &mut Buffer, title: &str) {
    use std::io::Write;
    use termcolor::{Color, ColorSpec, WriteColor};
    buffer
        .set_color(ColorSpec::new().set_bold(true).set_fg(Some(Color::Red)))
        .unwrap();
    write!(buffer, "error").unwrap();
    buffer.set_color(ColorSpec::new().set_bold(true)).unwrap();
    write!(buffer, ": {}\n\n", title).unwrap();
    buffer.set_color(&ColorSpec::new()).unwrap();
}

pub fn write_project(buffer: &mut Buffer, d: ProjectErrorDiagnostic) {
    use std::io::Write;
    use termcolor::{ColorSpec, WriteColor};
    write_title(buffer, d.title.as_ref());
    buffer.set_color(&ColorSpec::new()).unwrap();
    write!(buffer, "{}", d.label).unwrap();
}
