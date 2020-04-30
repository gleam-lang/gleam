use crate::diagnostic::{buffer_writer, write, Diagnostic, Severity};
use std::path::PathBuf;
use termcolor::Buffer;

pub type Src = String;

#[derive(Debug, PartialEq)]
pub enum Warning {
    Type {
        path: PathBuf,
        src: Src,
        warning: crate::typ::Warning,
    },
}

impl Warning {
    pub fn pretty(&self, buffer: &mut Buffer) {
        use crate::typ::Warning::*;
        use std::io::Write;

        buffer
            .write_all(b"\n")
            .expect("error pretty buffer write space before");

        match self {
            Warning::Type { path, src, warning } => match warning {
                DeprecatedListPrependSyntax { location } => {
                    let diagnostic = Diagnostic {
                        title: "Deprecated syntax".to_string(),
                        label: "The | operator in lists is deprecated".to_string(),
                        file: path.to_str().unwrap().to_string(),
                        src: src.to_string(),
                        location: location.clone(),
                    };
                    write(buffer, diagnostic, Severity::Warning);
                    writeln!(
                            buffer,
                            "The new syntax is [x, ..y]. If you run gleam format it will automatically update your code."
                        )
                        .unwrap();
                }
            },
        }
    }

    pub fn pretty_print(&self) {
        let buffer_writer = buffer_writer();
        let mut buffer = buffer_writer.buffer();
        self.pretty(&mut buffer);
        buffer_writer.print(&buffer).unwrap();
    }
}
