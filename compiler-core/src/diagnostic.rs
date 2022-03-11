use std::path::PathBuf;

pub use codespan_reporting::diagnostic::{LabelStyle, Severity};
use codespan_reporting::{diagnostic::Label as CodespanLabel, files::SimpleFile};
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

impl Location {
    fn labels(&self) -> impl Iterator<Item = &Label> {
        std::iter::once(&self.label).chain(self.extra_labels.iter())
    }
}

#[derive(Debug, Clone)]
pub struct Diagnostic {
    pub title: String,
    pub text: String,
    pub level: Level,
    pub location: Option<Location>,
    pub hint: Option<String>,
}

impl Diagnostic {
    pub fn write(&self, buffer: &mut Buffer) {
        use std::io::Write;
        match &self.location {
            Some(location) => self.write_span(location, buffer),
            None => self.write_title(buffer),
        };

        if !self.text.is_empty() {
            writeln!(buffer, "{}", self.text).expect("write text");
        }

        if let Some(hint) = &self.hint {
            writeln!(buffer, "Hint: {}", hint).expect("write hint");
        }
    }

    fn write_span(&self, location: &Location, buffer: &mut Buffer) {
        let file = SimpleFile::new(location.path.to_string_lossy().to_string(), &location.src);
        let labels = location
            .labels()
            .map(|l| {
                let label =
                    CodespanLabel::new(LabelStyle::Primary, (), (l.span.start)..(l.span.end));
                match &l.text {
                    None => label,
                    Some(text) => label.with_message(text.clone()),
                }
            })
            .collect();

        let severity = match self.level {
            Level::Error => Severity::Error,
            Level::Warning => Severity::Warning,
        };

        let diagnostic = codespan_reporting::diagnostic::Diagnostic::new(severity)
            .with_message(&self.title)
            .with_labels(labels);
        let config = codespan_reporting::term::Config::default();
        codespan_reporting::term::emit(buffer, &config, &file, &diagnostic)
            .expect("write_diagnostic");
    }

    fn write_title(&self, buffer: &mut Buffer) {
        use std::io::Write;
        use termcolor::{Color, ColorSpec, WriteColor};
        let (kind, colour) = match self.level {
            Level::Error => ("error", Color::Red),
            Level::Warning => ("warning", Color::Yellow),
        };
        buffer
            .set_color(ColorSpec::new().set_bold(true).set_fg(Some(colour)))
            .expect("write_title_color1");
        write!(buffer, "{}", kind).expect("write_title_kind");
        buffer
            .set_color(ColorSpec::new().set_bold(true))
            .expect("write_title_color2");
        write!(buffer, ": {}\n\n", self.title).expect("write_title_title");
        buffer
            .set_color(&ColorSpec::new())
            .expect("write_title_reset");
    }
}
