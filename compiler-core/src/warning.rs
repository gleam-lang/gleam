use crate::{
    diagnostic::{self, Diagnostic, Location},
    type_,
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
    pub fn to_diagnostic(&self) -> Diagnostic {
        #[allow(clippy::unwrap_used)]
        match self {
            Self::Type { path, warning, src } => match warning {
                type_::Warning::Todo { location, .. } => {
                    let text = "This code will crash if it is run. Be sure to remove this todo before running your program.".into();
                    Diagnostic {
                        title: "Todo found".into(),
                        text,
                        hint: None,
                        level: diagnostic::Level::Warning,
                        location: Some(Location {
                            src: "todo".into(),
                            path: path.to_path_buf(),
                            label: diagnostic::Label {
                                text: Some("Todo found".into()),
                                span: *location,
                            },
                            extra_labels: Vec::new(),
                        }),
                    }
                }

                type_::Warning::ImplicitlyDiscardedResult { location } => Diagnostic {
                    title: "Unused result value".into(),
                    text: "".into(),
                    hint: Some("If you are sure you don't need it you can assign it to `_`".into()),
                    level: diagnostic::Level::Warning,
                    location: Some(Location {
                        path: path.to_path_buf(),
                        src: src.to_string(),
                        label: diagnostic::Label {
                            text: Some("The Result value created here is unused".into()),
                            span: *location,
                        },
                        extra_labels: Vec::new(),
                    }),
                },

                type_::Warning::UnusedLiteral { location } => Diagnostic {
                    title: "Unused literal".into(),
                    text: "".into(),
                    hint: Some("You can safely remove it.".into()),
                    level: diagnostic::Level::Warning,
                    location: Some(Location {
                        path: path.to_path_buf(),
                        src: src.to_string(),
                        label: diagnostic::Label {
                            text: Some("This value is never used".into()),
                            span: *location,
                        },
                        extra_labels: Vec::new(),
                    }),
                },

                type_::Warning::NoFieldsRecordUpdate { location } => Diagnostic {
                    title: "Fieldless record update".into(),
                    text: "".into(),
                    hint: Some(
                        "Add some fields to change or replace it with the record itself.".into(),
                    ),
                    level: diagnostic::Level::Warning,
                    location: Some(Location {
                        path: path.to_path_buf(),
                        src: src.to_string(),
                        label: diagnostic::Label {
                            text: Some("This record update doesn't change any fields.".into()),
                            span: *location,
                        },
                        extra_labels: Vec::new(),
                    }),
                },

                type_::Warning::AllFieldsRecordUpdate { location } => Diagnostic {
                    title: "Redundant record update".into(),
                    text: "".into(),
                    hint: Some("It is better style to use the record creation syntax.".into()),
                    level: diagnostic::Level::Warning,
                    location: Some(Location {
                        src: src.to_string(),
                        path: path.to_path_buf(),
                        label: diagnostic::Label {
                            text: Some("This record update specifies all fields".into()),
                            span: *location,
                        },
                        extra_labels: Vec::new(),
                    }),
                },

                type_::Warning::UnusedType {
                    location, imported, ..
                } => {
                    let title = if *imported {
                        "Unused imported type".into()
                    } else {
                        "Unused private type".into()
                    };
                    let label = if *imported {
                        "This imported type is never used.".into()
                    } else {
                        "This private type is never used.".into()
                    };
                    Diagnostic {
                        title,
                        text: "".into(),
                        hint: Some("You can safely remove it.".into()),
                        level: diagnostic::Level::Warning,
                        location: Some(Location {
                            src: src.to_string(),
                            path: path.to_path_buf(),
                            label: diagnostic::Label {
                                text: Some(label),
                                span: *location,
                            },
                            extra_labels: Vec::new(),
                        }),
                    }
                }

                type_::Warning::UnusedConstructor {
                    location, imported, ..
                } => {
                    let title = if *imported {
                        "Unused imported item".into()
                    } else {
                        "Unused private type constructor".into()
                    };
                    let label = if *imported {
                        "This imported type constructor is never used.".into()
                    } else {
                        "This private type constructor is never used.".into()
                    };
                    Diagnostic {
                        title,
                        text: "".into(),
                        hint: Some("You can safely remove it.".into()),
                        level: diagnostic::Level::Warning,
                        location: Some(Location {
                            src: src.to_string(),
                            path: path.to_path_buf(),
                            label: diagnostic::Label {
                                text: Some(label),
                                span: *location,
                            },
                            extra_labels: Vec::new(),
                        }),
                    }
                }

                type_::Warning::UnusedImportedModule { location, .. } => Diagnostic {
                    title: "Unused imported module".into(),
                    text: "".into(),
                    hint: Some("You can safely remove it.".into()),
                    level: diagnostic::Level::Warning,
                    location: Some(Location {
                        src: src.to_string(),
                        path: path.to_path_buf(),
                        label: diagnostic::Label {
                            text: Some("This imported module is never used.".into()),
                            span: *location,
                        },
                        extra_labels: Vec::new(),
                    }),
                },

                type_::Warning::UnusedImportedValue { location, .. } => Diagnostic {
                    title: "Unused imported value".into(),
                    text: "".into(),
                    hint: Some("You can safely remove it.".into()),
                    level: diagnostic::Level::Warning,
                    location: Some(Location {
                        src: src.to_string(),
                        path: path.to_path_buf(),
                        label: diagnostic::Label {
                            text: Some("This imported value is never used.".into()),
                            span: *location,
                        },
                        extra_labels: Vec::new(),
                    }),
                },

                type_::Warning::UnusedPrivateModuleConstant { location, .. } => Diagnostic {
                    title: "Unused private constant".into(),
                    text: "".into(),
                    hint: Some("You can safely remove it.".into()),
                    level: diagnostic::Level::Warning,
                    location: Some(Location {
                        src: src.to_string(),
                        path: path.to_path_buf(),
                        label: diagnostic::Label {
                            text: Some("This private constant is never used.".into()),
                            span: *location,
                        },
                        extra_labels: Vec::new(),
                    }),
                },

                type_::Warning::UnusedPrivateFunction { location, .. } => Diagnostic {
                    title: "Unused private function".into(),
                    text: "".into(),
                    hint: Some("You can safely remove it.".into()),
                    level: diagnostic::Level::Warning,
                    location: Some(Location {
                        src: src.to_string(),
                        path: path.to_path_buf(),
                        label: diagnostic::Label {
                            text: Some("This private function is never used.".into()),
                            span: *location,
                        },
                        extra_labels: Vec::new(),
                    }),
                },

                type_::Warning::UnusedVariable { location, name, .. } => Diagnostic {
                    title: "Unused variable".into(),
                    text: "".into(),
                    hint: Some(format!(
                        "You can ignore it with an underscore: `_{}`.",
                        name
                    )),
                    level: diagnostic::Level::Warning,
                    location: Some(Location {
                        src: src.to_string(),
                        path: path.to_path_buf(),
                        label: diagnostic::Label {
                            text: Some("This variable is never used.".into()),
                            span: *location,
                        },
                        extra_labels: Vec::new(),
                    }),
                },
            },
        }
    }

    pub fn pretty(&self, buffer: &mut Buffer) {
        buffer
            .write_all(b"\n")
            .expect("error pretty buffer write space before");
        self.to_diagnostic().write(buffer);
    }
}
