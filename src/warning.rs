use crate::{
    cli,
    diagnostic::{write, Diagnostic, Severity},
    typ::pretty::Printer,
};
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
                Todo { location, typ } => {
                    let diagnostic = Diagnostic {
                        title: "Todo found".to_string(),
                        label: "".to_string(),
                        file: path.to_str().unwrap().to_string(),
                        src: src.to_string(),
                        location: location.clone(),
                    };
                    write(buffer, diagnostic, Severity::Warning);
                    let mut printer = Printer::new();

                    writeln!(
                        buffer,
                        "I think this should be an `{}`.

This code will crash if it is run. Be sure to remove this todo before running
your program.",
                        printer.pretty_print(typ, 0)
                    )
                    .unwrap();
                }

                ImplicitlyDiscardedResult { location } => {
                    let diagnostic = Diagnostic {
                        title: "Unused result value".to_string(),
                        label: "".to_string(),
                        file: path.to_str().unwrap().to_string(),
                        src: src.to_string(),
                        location: location.clone(),
                    };
                    write(buffer, diagnostic, Severity::Warning);
                    writeln!(buffer,
"The Result value returned by this code is not being used, so any error is being
silently ignored. Check for an error with a case statement, or assign it to the
variable _ if you are sure the error does not matter.")
                    .unwrap();
                }

                NoFieldsRecordUpdate { location } => {
                    let diagnostic = Diagnostic {
                        title: "Fieldless record update".to_string(),
                        label: "".to_string(),
                        file: path.to_str().unwrap().to_string(),
                        src: src.to_string(),
                        location: location.clone(),
                    };
                    write(buffer, diagnostic, Severity::Warning);
                    writeln!(buffer,
"No fields have been changed in this record update, so it returns the original
record without modification. Add some fields or remove this update.")
                    .unwrap();
                }

                AllFieldsRecordUpdate { location } => {
                    let diagnostic = Diagnostic {
                        title: "Redundant record update".to_string(),
                        label: "".to_string(),
                        file: path.to_str().unwrap().to_string(),
                        src: src.to_string(),
                        location: location.clone(),
                    };
                    write(buffer, diagnostic, Severity::Warning);
                    writeln!(buffer,
"All fields have been given in this record update, so it does not need to be
be an update. Remove the update or remove fields that need to be copied.")
                    .unwrap();
                }

                UnusedType { location, name } => {
                    let diagnostic = Diagnostic {
                        title: "Unused type".to_string(),
                        label: "".to_string(),
                        file: path.to_str().unwrap().to_string(),
                        src: src.to_string(),
                        location: location.clone(),
                    };
                    write(buffer, diagnostic, Severity::Warning);
                    writeln!(
                        buffer,
                        "The type {} is never used, it can be safely removed.",
                        name
                    )
                    .unwrap();
                }
            },
        }
    }

    pub fn pretty_print(&self) {
        let buffer_writer = cli::stderr_buffer_writer();
        let mut buffer = buffer_writer.buffer();
        self.pretty(&mut buffer);
        buffer_writer.print(&buffer).unwrap();
    }
}

pub fn print_all(analysed: &[crate::project::Analysed]) {
    for a in analysed.iter() {
        for w in a.warnings.iter() {
            w.pretty_print()
        }
    }
}
