use codespan::{FileId, Files};
pub use codespan_reporting::diagnostic::{LabelStyle, Severity};
use codespan_reporting::{diagnostic::Label, term::emit};
use termcolor::Buffer;

pub struct DiagnosticLabel {
    pub style: LabelStyle,
    pub location: crate::ast::SrcSpan,
    pub label: String,
}
pub struct Diagnostic {
    pub file: String,
    pub location: crate::ast::SrcSpan,
    pub src: String,
    pub title: String,
    pub label: String,
}

pub struct MultiLineDiagnostic {
    pub file: String,
    pub src: String,
    pub title: String,
    pub labels: Vec<DiagnosticLabel>,
}

pub fn write(buffer: &mut Buffer, d: Diagnostic, severity: Severity) {
    let diagnostic = MultiLineDiagnostic {
        file: d.file,
        src: d.src,
        title: d.title,
        labels: vec![DiagnosticLabel {
            style: LabelStyle::Primary,
            location: d.location,
            label: d.label,
        }],
    };

    write_diagnostic(buffer, diagnostic, severity)
}

pub fn write_diagnostic(mut buffer: &mut Buffer, d: MultiLineDiagnostic, severity: Severity) {
    let mut files = Files::new();
    let file_id: FileId = files.add(d.file, d.src);

    let labels: Vec<Label<FileId>> = d
        .labels
        .iter()
        .map(|l| {
            Label::new(
                l.style,
                file_id,
                (l.location.start as usize)..(l.location.end as usize),
            )
            .with_message(l.label.clone())
        })
        .collect();

    let diagnostic = codespan_reporting::diagnostic::Diagnostic::new(severity)
        .with_message(d.title)
        .with_labels(labels);

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
