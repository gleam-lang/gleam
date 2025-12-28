use std::collections::HashMap;

use camino::Utf8PathBuf;

pub use codespan_reporting::diagnostic::{LabelStyle, Severity};
use codespan_reporting::{diagnostic::Label as CodespanLabel, files::SimpleFiles};
use ecow::EcoString;
use termcolor::Buffer;

use crate::ast::SrcSpan;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Level {
    Error,
    Warning,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Label {
    pub text: Option<String>,
    pub span: SrcSpan,
}

impl Label {
    fn to_codespan_label(&self, fileid: usize, style: LabelStyle) -> CodespanLabel<usize> {
        let label = CodespanLabel::new(
            style,
            fileid,
            (self.span.start as usize)..(self.span.end as usize),
        );
        match &self.text {
            None => label,
            Some(text) => label.with_message(text.clone()),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ExtraLabel {
    pub src_info: Option<(EcoString, Utf8PathBuf)>,
    pub label: Label,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Location {
    pub src: EcoString,
    pub path: Utf8PathBuf,
    pub label: Label,
    pub extra_labels: Vec<ExtraLabel>,
}

// TODO: split this into locationed diagnostics and locationless diagnostics
#[derive(Debug, Clone, PartialEq, Eq)]
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
            writeln!(buffer, "Hint: {hint}").expect("write hint");
        }
    }

    fn write_span(&self, location: &Location, buffer: &mut Buffer) {
        let mut file_map = HashMap::new();
        let mut files = SimpleFiles::new();

        let main_location_path = location.path.as_str();
        let main_location_src = location.src.as_str();
        let main_file_id = files.add(main_location_path, main_location_src);
        let _ = file_map.insert(main_location_path, main_file_id);

        let mut labels = vec![
            location
                .label
                .to_codespan_label(main_file_id, LabelStyle::Primary),
        ];

        location
            .extra_labels
            .iter()
            .map(|label| {
                let (location_src, location_path) = match &label.src_info {
                    Some(info) => (info.0.as_str(), info.1.as_str()),
                    _ => (main_location_src, main_location_path),
                };
                match file_map.get(location_path) {
                    None => {
                        let file_id = files.add(location_path, location_src);
                        let _ = file_map.insert(location_path, file_id);
                        label
                            .label
                            .to_codespan_label(file_id, LabelStyle::Secondary)
                    }
                    Some(i) => label.label.to_codespan_label(*i, LabelStyle::Secondary),
                }
            })
            .for_each(|label| labels.push(label));

        let severity = match self.level {
            Level::Error => Severity::Error,
            Level::Warning => Severity::Warning,
        };

        let diagnostic = codespan_reporting::diagnostic::Diagnostic::new(severity)
            .with_message(&self.title)
            .with_labels(labels);
        let config = codespan_reporting::term::Config::default();
        codespan_reporting::term::emit(buffer, &config, &files, &diagnostic)
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
        write!(buffer, "{kind}").expect("write_title_kind");
        buffer
            .set_color(ColorSpec::new().set_bold(true))
            .expect("write_title_color2");
        write!(buffer, ": {}\n\n", self.title).expect("write_title_title");
        buffer
            .set_color(&ColorSpec::new())
            .expect("write_title_reset");
    }
}
