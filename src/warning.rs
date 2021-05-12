use crate::{
    cli,
    diagnostic::{write, Diagnostic, Severity},
    type_,
    type_::pretty::Printer,
};
use std::io::Write;
use std::path::PathBuf;
use termcolor::Buffer;

pub type Src = String;

#[derive(Debug, PartialEq)]
pub enum Warning {
    Type {
        path: PathBuf,
        src: Src,
        warning: crate::type_::Warning,
    },
}

impl Warning {
    pub fn to_diagnostic(&self) -> (Diagnostic, String) {
        #[allow(clippy::unwrap_used)]
        match self {
            Self::Type { path, src, warning } => match warning {
                type_::Warning::Todo { location, typ } => {
                    let mut printer = Printer::new();
                    (
                        Diagnostic {
                            title: "Todo found".to_string(),
                            label: "Todo found".to_string(),
                            file: path.to_str().unwrap().to_string(),
                            src: src.to_string(),
                            location: *location,
                        },
                        format!(
                            "Hint: I think its type is `{}`.

This code will crash if it is run. Be sure to remove this todo before running
your program.",
                            printer.pretty_print(typ, 0)
                        ),
                    )
                }

                type_::Warning::ImplicitlyDiscardedResult { location } => (
                    Diagnostic {
                        title: "Unused result value".to_string(),
                        label: "The Result value created here is unused".to_string(),
                        file: path.to_str().unwrap().to_string(),
                        src: src.to_string(),
                        location: *location,
                    },
                    "Hint: If you are sure you don't need it you can assign it to `_`".to_string(),
                ),

                type_::Warning::UnusedLiteral { location } => (
                    Diagnostic {
                        title: "Unused literal".to_string(),
                        label: "This value is never used".to_string(),
                        file: path.to_str().unwrap().to_string(),
                        src: src.to_string(),
                        location: *location,
                    },
                    "Hint: You can safely remove it.".to_string(),
                ),

                type_::Warning::NoFieldsRecordUpdate { location } => (
                    Diagnostic {
                        title: "Fieldless record update".to_string(),
                        label: "This record update doesn't change any fields.".to_string(),
                        file: path.to_str().unwrap().to_string(),
                        src: src.to_string(),
                        location: *location,
                    },
                    "Hint: Add some fields to change or replace it with the record itself. "
                        .to_string(),
                ),

                type_::Warning::AllFieldsRecordUpdate { location } => (
                    Diagnostic {
                        title: "Redundant record update".to_string(),
                        label: "This record update specifies all fields".to_string(),
                        file: path.to_str().unwrap().to_string(),
                        src: src.to_string(),
                        location: *location,
                    },
                    "Hint: It is better style to use the record creation syntax.".to_string(),
                ),

                type_::Warning::UnusedType {
                    location, imported, ..
                } => {
                    let title = if *imported {
                        "Unused imported type".to_string()
                    } else {
                        "Unused private type".to_string()
                    };
                    let label = if *imported {
                        "This imported type is never used.".to_string()
                    } else {
                        "This private type is never used.".to_string()
                    };

                    (
                        Diagnostic {
                            title,
                            label,
                            file: path.to_str().unwrap().to_string(),
                            src: src.to_string(),
                            location: *location,
                        },
                        "Hint: You can safely remove it.".to_string(),
                    )
                }

                type_::Warning::UnusedConstructor {
                    location, imported, ..
                } => {
                    let title = if *imported {
                        "Unused imported item".to_string()
                    } else {
                        "Unused private type constructor".to_string()
                    };
                    let label = if *imported {
                        "This imported type constructor is never used.".to_string()
                    } else {
                        "This private type constructor is never used.".to_string()
                    };

                    (
                        Diagnostic {
                            title,
                            label,
                            file: path.to_str().unwrap().to_string(),
                            src: src.to_string(),
                            location: *location,
                        },
                        "Hint: You can safely remove it.".to_string(),
                    )
                }

                type_::Warning::UnusedImportedValue { location, .. } => (
                    Diagnostic {
                        title: "Unused imported value".to_string(),
                        label: "This imported value is never used.".to_string(),
                        file: path.to_str().unwrap().to_string(),
                        src: src.to_string(),
                        location: *location,
                    },
                    "Hint: You can safely remove it.".to_string(),
                ),

                type_::Warning::UnusedPrivateModuleConstant { location, .. } => (
                    Diagnostic {
                        title: "Unused private constant".to_string(),
                        label: "This private constant is never used.".to_string(),
                        file: path.to_str().unwrap().to_string(),
                        src: src.to_string(),
                        location: *location,
                    },
                    "Hint: You can safely remove it.".to_string(),
                ),

                type_::Warning::UnusedPrivateFunction { location, .. } => (
                    Diagnostic {
                        title: "Unused private function".to_string(),
                        label: "This private function is never used.".to_string(),
                        file: path.to_str().unwrap().to_string(),
                        src: src.to_string(),
                        location: *location,
                    },
                    "Hint: You can safely remove it.".to_string(),
                ),

                type_::Warning::UnusedVariable { location, name, .. } => (
                    Diagnostic {
                        title: "Unused variable".to_string(),
                        label: "This variable is never used.".to_string(),
                        file: path.to_str().unwrap().to_string(),
                        src: src.to_string(),
                        location: *location,
                    },
                    format!("Hint: you can ignore it with an underscore: `_{}`.", name),
                ),
            },
        }
    }

    pub fn pretty(&self, buffer: &mut Buffer) {
        #[allow(clippy::expect_used)]
        buffer
            .write_all(b"\n")
            .expect("error pretty buffer write space before");
        let (diagnostic, extra) = self.to_diagnostic();
        write(buffer, diagnostic, Severity::Warning);
        if !extra.is_empty() {
            writeln!(buffer, "{}", extra).unwrap();
        }
    }

    pub fn pretty_print(&self) {
        let buffer_writer = cli::stderr_buffer_writer();
        let mut buffer = buffer_writer.buffer();
        self.pretty(&mut buffer);
        #[allow(clippy::unwrap_used)]
        buffer_writer.print(&buffer).unwrap();
    }
}

pub fn print_all(analysed: &[crate::project::Analysed]) -> usize {
    analysed
        .iter()
        .flat_map(|a| &a.warnings)
        .inspect(|w| w.pretty_print())
        .count()
}
