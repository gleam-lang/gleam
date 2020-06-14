use codespan::{FileId, Files};
pub use codespan_reporting::diagnostic::Severity;
use codespan_reporting::{
    diagnostic::{Label, LabelStyle},
    term::emit,
};
use termcolor::Buffer;

pub struct Diagnostic {
    pub file: String,
    pub location: crate::ast::SrcSpan,
    pub src: String,
    pub title: String,
    pub label: String,
}

pub fn write(mut buffer: &mut Buffer, d: Diagnostic, severity: Severity) {
    let mut files = Files::new();
    let file_id: FileId = files.add(d.file, d.src);

    let label: Label<FileId> = Label::new(
        LabelStyle::Primary,
        file_id,
        (d.location.start as usize)..(d.location.end as usize),
    )
    .with_message(d.label);

    let diagnostic = codespan_reporting::diagnostic::Diagnostic::new(severity)
        .with_message(d.title)
        .with_labels(vec![label]);

    let config = codespan_reporting::term::Config::default();
    emit(&mut buffer, &config, &files, &diagnostic).unwrap();
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
    writeln!(buffer, "{}", d.label).unwrap();
}
