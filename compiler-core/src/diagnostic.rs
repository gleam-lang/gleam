pub use codespan_reporting::diagnostic::{LabelStyle, Severity};
use codespan_reporting::{diagnostic::Label as CodespanLabel, files::SimpleFile, term::emit};
use termcolor::Buffer;

use crate::ast::SrcSpan;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Level {
    Error,
    Warning,
}

#[derive(Debug, Clone)]
pub struct Label {
    pub text: Option<String>,
    pub span: SrcSpan,
}

#[derive(Debug, Clone)]
pub struct Location {
    pub src: String,
    pub path: PathBuf,
    pub label: Label,
    pub extra_labels: Vec<Label>,
}

#[derive(Debug, Clone)]
pub struct Diagnostic {
    pub title: String,
    pub text: String,
    pub level: Level,
    pub location: Option<Location>,
}

#[derive(Debug)]
pub struct DiagnosticLabel {
    pub style: LabelStyle,
    pub location: SrcSpan,
    pub label: String,
}

#[derive(Debug, PartialEq, Clone)]
pub struct OldDiagnostic {
    pub file: String,
    pub location: SrcSpan,
    pub src: String,
    pub title: String,
    pub label: String,
}

#[derive(Debug)]
pub struct OldMultiLineDiagnostic {
    pub file: String,
    pub src: String,
    pub title: String,
    pub labels: Vec<DiagnosticLabel>,
}

pub fn write_old(buffer: &mut Buffer, d: OldDiagnostic, severity: Severity) {
    let diagnostic = OldMultiLineDiagnostic {
        file: d.file,
        src: d.src,
        title: d.title,
        labels: vec![DiagnosticLabel {
            style: LabelStyle::Primary,
            location: d.location,
            label: d.label,
        }],
    };

    write_diagnostic_old(buffer, diagnostic, severity)
}

pub fn write_diagnostic_old(
    mut buffer: &mut Buffer,
    d: OldMultiLineDiagnostic,
    severity: Severity,
) {
    let file = SimpleFile::new(d.file, d.src);

    let labels = d
        .labels
        .iter()
        .map(|l| {
            CodespanLabel::new(l.style, (), (l.location.start)..(l.location.end))
                .with_message(l.label.clone())
        })
        .collect();

    let diagnostic = codespan_reporting::diagnostic::Diagnostic::new(severity)
        .with_message(d.title)
        .with_labels(labels);

    let config = codespan_reporting::term::Config::default();
    emit(&mut buffer, &config, &file, &diagnostic).expect("write_diagnostic");
}

/// Describes an error encountered while compiling the project (eg. a name collision
/// between files).
///
#[derive(Debug)]
pub struct ProjectErrorDiagnostic {
    pub title: String,
    pub label: String,
}

pub fn write_title(buffer: &mut Buffer, title: &str, severity: Severity) {
    use std::io::Write;
    use termcolor::{Color, ColorSpec, WriteColor};
    let (kind, colour) = match severity {
        Severity::Bug => ("bug", Color::Red),
        Severity::Error => ("error", Color::Red),
        Severity::Warning => ("warning", Color::Yellow),
        Severity::Note => ("note", Color::Blue),
        Severity::Help => ("help", Color::Blue),
    };
    buffer
        .set_color(ColorSpec::new().set_bold(true).set_fg(Some(colour)))
        .expect("write_title_color1");
    write!(buffer, "{}", kind).expect("write_title_kind");
    buffer
        .set_color(ColorSpec::new().set_bold(true))
        .expect("write_title_color2");
    write!(buffer, ": {}\n\n", title).expect("write_title_title");
    buffer
        .set_color(&ColorSpec::new())
        .expect("write_title_reset");
}

pub fn write_project(buffer: &mut Buffer, d: ProjectErrorDiagnostic) {
    use std::io::Write;
    write_title(buffer, &d.title, Severity::Error);
    writeln!(buffer, "{}", d.label).expect("write_project");
}
