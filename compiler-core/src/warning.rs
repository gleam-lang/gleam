use crate::{
    ast::TodoKind,
    diagnostic::{self, Diagnostic, Location},
    error::wrap,
    type_::{
        self,
        error::{LiteralCollectionKind, TodoOrPanic},
        pretty::Printer,
    },
};
use camino::Utf8PathBuf;
use debug_ignore::DebugIgnore;
use ecow::EcoString;
use std::sync::atomic::AtomicUsize;
use std::{
    io::Write,
    sync::{atomic::Ordering, Arc},
};
use termcolor::Buffer;

pub trait WarningEmitterIO {
    fn emit_warning(&self, warning: Warning);
}

#[derive(Debug, Clone, Copy)]
pub struct NullWarningEmitterIO;

impl WarningEmitterIO for NullWarningEmitterIO {
    fn emit_warning(&self, _warning: Warning) {}
}

#[derive(Debug, Clone, Default)]
pub struct VectorWarningEmitterIO {
    pub warnings: Arc<std::sync::RwLock<Vec<Warning>>>,
}

impl VectorWarningEmitterIO {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn take(&self) -> Vec<Warning> {
        let mut warnings = self.write_lock();
        std::mem::take(&mut *warnings)
    }

    pub fn reset(&self) {
        let mut warnings = self.write_lock();
        warnings.clear();
    }

    pub fn pop(&self) -> Option<Warning> {
        let mut warnings = self.write_lock();
        warnings.pop()
    }

    fn write_lock(&self) -> std::sync::RwLockWriteGuard<'_, Vec<Warning>> {
        self.warnings.write().expect("Vector lock poisoned")
    }
}

impl WarningEmitterIO for VectorWarningEmitterIO {
    fn emit_warning(&self, warning: Warning) {
        let mut warnings = self.write_lock();
        warnings.push(warning);
    }
}

#[derive(Debug, Clone)]
pub struct WarningEmitter {
    /// The number of warnings emitted.
    /// In the context of the project compiler this is the count for the root
    /// package only, the count is reset back to zero after the dependencies are
    /// compiled.
    count: Arc<AtomicUsize>,
    emitter: DebugIgnore<Arc<dyn WarningEmitterIO>>,
}

impl WarningEmitter {
    pub fn new(emitter: Arc<dyn WarningEmitterIO>) -> Self {
        Self {
            count: Arc::new(AtomicUsize::new(0)),
            emitter: DebugIgnore(emitter),
        }
    }

    pub fn null() -> Self {
        Self::new(Arc::new(NullWarningEmitterIO))
    }

    pub fn reset_count(&self) {
        self.count.store(0, Ordering::Relaxed);
    }

    pub fn count(&self) -> usize {
        self.count.load(Ordering::Relaxed)
    }

    pub fn emit(&self, warning: Warning) {
        _ = self.count.fetch_add(1, Ordering::Relaxed);
        self.emitter.emit_warning(warning);
    }

    pub fn vector() -> (Self, Arc<VectorWarningEmitterIO>) {
        let io = Arc::new(VectorWarningEmitterIO::default());
        let emitter = Self::new(io.clone());
        (emitter, Arc::clone(&io))
    }
}

#[derive(Debug, Clone)]
pub struct TypeWarningEmitter {
    module_path: Utf8PathBuf,
    module_src: EcoString,
    emitter: WarningEmitter,
}

impl TypeWarningEmitter {
    pub fn new(module_path: Utf8PathBuf, module_src: EcoString, emitter: WarningEmitter) -> Self {
        Self {
            module_path,
            module_src,
            emitter,
        }
    }

    pub fn null() -> Self {
        Self {
            module_path: Utf8PathBuf::new(),
            module_src: EcoString::from(""),
            emitter: WarningEmitter::new(Arc::new(NullWarningEmitterIO)),
        }
    }

    pub fn emit(&self, warning: type_::Warning) {
        self.emitter.emit(Warning::Type {
            path: self.module_path.clone(),
            src: self.module_src.clone(),
            warning,
        });
    }
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum Warning {
    Type {
        path: Utf8PathBuf,
        src: EcoString,
        warning: type_::Warning,
    },
    InvalidSource {
        path: Utf8PathBuf,
    },
}

impl Warning {
    pub fn to_diagnostic(&self) -> Diagnostic {
        match self {
            Warning::InvalidSource { path } => Diagnostic {
                title: "Invalid module name".into(),
                text: "\
Module names must begin with a lowercase letter and contain
only lowercase alphanumeric characters or underscores."
                    .into(),
                level: diagnostic::Level::Warning,
                location: None,
                hint: Some(format!(
                    "Rename `{path}` to be valid, or remove this file from the project source."
                )),
            },
            Self::Type { path, warning, src } => match warning {
                type_::Warning::Todo {
                    kind,
                    location,
                    typ,
                } => {
                    let mut text = String::new();
                    text.push_str(
                        "\
This code will crash if it is run. Be sure to finish it before
running your program.",
                    );
                    let title = match kind {
                        TodoKind::Keyword => "Todo found",
                        TodoKind::EmptyFunction => "Unimplemented function",
                        TodoKind::IncompleteUse => {
                            text.push_str(
                                "
A use expression must always be followed by at least one more
expression.",
                            );
                            "Incomplete use expression"
                        }
                    }
                    .into();
                    if !typ.is_variable() {
                        text.push_str(&format!(
                            "\n\nHint: I think its type is `{}`.\n",
                            Printer::new().pretty_print(typ, 0)
                        ));
                    }

                    Diagnostic {
                        title,
                        text,
                        level: diagnostic::Level::Warning,
                        location: Some(Location {
                            path: path.to_path_buf(),
                            src: src.clone(),
                            label: diagnostic::Label {
                                text: Some("This code is incomplete".into()),
                                span: *location,
                            },
                            extra_labels: Vec::new(),
                        }),
                        hint: None,
                    }
                }

                type_::Warning::ImplicitlyDiscardedResult { location } => Diagnostic {
                    title: "Unused result value".into(),
                    text: "".into(),
                    hint: Some(
                        "If you are sure you don't need it you can assign it to `_`.".into(),
                    ),
                    level: diagnostic::Level::Warning,
                    location: Some(Location {
                        path: path.to_path_buf(),
                        src: src.clone(),
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
                        src: src.clone(),
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
                        src: src.clone(),
                        label: diagnostic::Label {
                            text: Some("This record update doesn't change any fields".into()),
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
                        src: src.clone(),
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
                        "This imported type is never used".into()
                    } else {
                        "This private type is never used".into()
                    };
                    Diagnostic {
                        title,
                        text: "".into(),
                        hint: Some("You can safely remove it.".into()),
                        level: diagnostic::Level::Warning,
                        location: Some(Location {
                            src: src.clone(),
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
                        "Unused private constructor".into()
                    };
                    let label = if *imported {
                        "This imported constructor is never used".into()
                    } else {
                        "This private constructor is never used".into()
                    };
                    Diagnostic {
                        title,
                        text: "".into(),
                        hint: Some("You can safely remove it.".into()),
                        level: diagnostic::Level::Warning,
                        location: Some(Location {
                            src: src.clone(),
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
                        src: src.clone(),
                        path: path.to_path_buf(),
                        label: diagnostic::Label {
                            text: Some("This imported module is never used".into()),
                            span: *location,
                        },
                        extra_labels: Vec::new(),
                    }),
                },

                type_::Warning::UnusedImportedModuleAlias {
                    location,
                    module_name,
                    ..
                } => {
                    let text = format!(
                        "\
Hint: You can safely remove it.

    import {module_name} as _
"
                    );
                    Diagnostic {
                        title: "Unused imported module alias".into(),
                        text,
                        hint: None,
                        level: diagnostic::Level::Warning,
                        location: Some(Location {
                            src: src.clone(),
                            path: path.to_path_buf(),
                            label: diagnostic::Label {
                                text: Some("This alias is never used".into()),
                                span: *location,
                            },
                            extra_labels: Vec::new(),
                        }),
                    }
                }

                type_::Warning::UnusedImportedValue { location, .. } => Diagnostic {
                    title: "Unused imported value".into(),
                    text: "".into(),
                    hint: Some("You can safely remove it.".into()),
                    level: diagnostic::Level::Warning,
                    location: Some(Location {
                        src: src.clone(),
                        path: path.to_path_buf(),
                        label: diagnostic::Label {
                            text: Some("This imported value is never used".into()),
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
                        src: src.clone(),
                        path: path.to_path_buf(),
                        label: diagnostic::Label {
                            text: Some("This private constant is never used".into()),
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
                        src: src.clone(),
                        path: path.to_path_buf(),
                        label: diagnostic::Label {
                            text: Some("This private function is never used".into()),
                            span: *location,
                        },
                        extra_labels: Vec::new(),
                    }),
                },

                type_::Warning::UnusedVariable { location, name, .. } => Diagnostic {
                    title: "Unused variable".into(),
                    text: "".into(),
                    hint: Some(format!("You can ignore it with an underscore: `_{name}`.")),
                    level: diagnostic::Level::Warning,
                    location: Some(Location {
                        src: src.clone(),
                        path: path.to_path_buf(),
                        label: diagnostic::Label {
                            text: Some("This variable is never used".into()),
                            span: *location,
                        },
                        extra_labels: Vec::new(),
                    }),
                },
                type_::Warning::UnnecessaryDoubleIntNegation { location } => Diagnostic {
                    title: "Unnecessary double negation (--) on integer".into(),
                    text: "".into(),
                    hint: Some("You can safely remove this.".into()),
                    level: diagnostic::Level::Warning,
                    location: Some(Location {
                        src: src.clone(),
                        path: path.to_path_buf(),
                        label: diagnostic::Label {
                            text: None,
                            span: *location,
                        },
                        extra_labels: Vec::new(),
                    }),
                },
                type_::Warning::UnnecessaryDoubleBoolNegation { location } => Diagnostic {
                    title: "Unnecessary double negation (!!) on bool".into(),
                    text: "".into(),
                    hint: Some("You can safely remove this.".into()),
                    level: diagnostic::Level::Warning,
                    location: Some(Location {
                        src: src.clone(),
                        path: path.to_path_buf(),
                        label: diagnostic::Label {
                            text: None,
                            span: *location,
                        },
                        extra_labels: Vec::new(),
                    }),
                },
                type_::Warning::InefficientEmptyListCheck { location, kind } => {
                    use type_::error::EmptyListCheckKind;
                    let text = "The `list.length` function has to iterate across the whole
list to calculate the length, which is wasteful if you only
need to know if the list is empty or not.
"
                    .into();
                    let hint = Some(match kind {
                        EmptyListCheckKind::Empty => "You can use `the_list == []` instead.".into(),
                        EmptyListCheckKind::NonEmpty => {
                            "You can use `the_list != []` instead.".into()
                        }
                    });

                    Diagnostic {
                        title: "Inefficient use of `list.length`".into(),
                        text,
                        hint,
                        level: diagnostic::Level::Warning,
                        location: Some(Location {
                            src: src.clone(),
                            path: path.to_path_buf(),
                            label: diagnostic::Label {
                                text: None,
                                span: *location,
                            },
                            extra_labels: Vec::new(),
                        }),
                    }
                }

                type_::Warning::TransitiveDependencyImported {
                    location,
                    module,
                    package,
                } => {
                    let text = wrap(&format!(
                        "The module `{module}` is being imported, but \
`{package}`, the package it belongs to, is not a direct dependency of your \
package.
In a future version of Gleam this may become a compile error.

Run this command to add it to your dependencies:

    gleam add {package}
"
                    ));
                    Diagnostic {
                        title: "Transitive dependency imported".into(),
                        text,
                        hint: None,
                        level: diagnostic::Level::Warning,
                        location: Some(Location {
                            src: src.clone(),
                            path: path.to_path_buf(),
                            label: diagnostic::Label {
                                text: None,
                                span: *location,
                            },
                            extra_labels: Vec::new(),
                        }),
                    }
                }

                type_::Warning::DeprecatedItem {
                    location,
                    message,
                    layer,
                } => {
                    let text = wrap(&format!("It was deprecated with this message: {message}"));
                    let (title, diagnostic_label_text) = if layer.is_value() {
                        (
                            "Deprecated value used".into(),
                            Some("This value has been deprecated".into()),
                        )
                    } else {
                        (
                            "Deprecated type used".into(),
                            Some("This type has been deprecated".into()),
                        )
                    };

                    Diagnostic {
                        title,
                        text,
                        hint: None,
                        level: diagnostic::Level::Warning,
                        location: Some(Location {
                            src: src.clone(),
                            path: path.to_path_buf(),
                            label: diagnostic::Label {
                                text: diagnostic_label_text,
                                span: *location,
                            },
                            extra_labels: Vec::new(),
                        }),
                    }
                }

                type_::Warning::UnreachableCaseClause { location } => {
                    let text: String =
                        "This case clause cannot be reached as a previous clause matches
the same values.\n"
                            .into();
                    Diagnostic {
                        title: "Unreachable case clause".into(),
                        text,
                        hint: Some("It can be safely removed.".into()),
                        level: diagnostic::Level::Warning,
                        location: Some(Location {
                            src: src.clone(),
                            path: path.to_path_buf(),
                            label: diagnostic::Label {
                                text: None,
                                span: *location,
                            },
                            extra_labels: Vec::new(),
                        }),
                    }
                }

                type_::Warning::CaseMatchOnLiteralCollection { kind, location } => {
                    let kind = match kind {
                        LiteralCollectionKind::List => "list",
                        LiteralCollectionKind::Tuple => "tuple",
                        LiteralCollectionKind::Record => "record",
                    };

                    let title = format!("Redundant {kind}");
                    let text = wrap(&format!(
                        "Instead of building a {kind} and matching on it, \
you can match on its contents directly.
A case expression can take multiple subjects separated by commas like this:

    case one_subject, another_subject {{
      _, _ -> todo
    }}

See: https://tour.gleam.run/flow-control/multiple-subjects/"
                    ));

                    Diagnostic {
                        title,
                        text,
                        hint: None,
                        level: diagnostic::Level::Warning,
                        location: Some(Location {
                            src: src.clone(),
                            path: path.to_path_buf(),
                            label: diagnostic::Label {
                                text: Some(format!("You can remove this {kind} wrapper")),
                                span: *location,
                            },
                            extra_labels: Vec::new(),
                        }),
                    }
                }

                type_::Warning::CaseMatchOnLiteralValue { location } => Diagnostic {
                    title: "Match on a literal value".into(),
                    text: wrap(
                        "Matching on a literal value is redundant since you \
can already tell which branch is going to match with this value.",
                    ),
                    hint: None,
                    level: diagnostic::Level::Warning,
                    location: Some(Location {
                        src: src.clone(),
                        path: path.to_path_buf(),
                        label: diagnostic::Label {
                            text: Some("There's no need to pattern match on this value".into()),
                            span: *location,
                        },
                        extra_labels: Vec::new(),
                    }),
                },

                type_::Warning::OpaqueExternalType { location } => Diagnostic {
                    title: "Opaque external type".into(),
                    text: "This type has no constructors so making it opaque is redundant.".into(),
                    hint: Some("Remove the `opaque` qualifier from the type definition.".into()),
                    level: diagnostic::Level::Warning,
                    location: Some(Location {
                        src: src.clone(),
                        path: path.to_path_buf(),
                        label: diagnostic::Label {
                            text: None,
                            span: *location,
                        },
                        extra_labels: Vec::new(),
                    }),
                },

                type_::Warning::UnusedValue { location } => Diagnostic {
                    title: "Unused value".into(),
                    text: "".into(),
                    hint: None,
                    level: diagnostic::Level::Warning,
                    location: Some(Location {
                        path: path.to_path_buf(),
                        src: src.clone(),
                        label: diagnostic::Label {
                            text: Some("This value is never used".into()),
                            span: *location,
                        },
                        extra_labels: Vec::new(),
                    }),
                },

                type_::Warning::InternalTypeLeak { location, leaked } => {
                    let mut printer = Printer::new();

                    // TODO: be more precise.
                    // - is being returned by this public function
                    // - is taken as an argument by this public function
                    // - is taken as an argument by this public enum constructor
                    // etc
                    let text = format!(
                        "The following type is internal, but is being used by this public export.

{}

Internal types should not be used in a public facing function since they are
hidden from the package's documentation.",
                        printer.pretty_print(leaked, 4),
                    );
                    Diagnostic {
                        title: "Internal type used in public interface".into(),
                        text,
                        hint: None,
                        level: diagnostic::Level::Warning,
                        location: Some(Location {
                            label: diagnostic::Label {
                                text: None,
                                span: *location,
                            },
                            path: path.clone(),
                            src: src.clone(),
                            extra_labels: vec![],
                        }),
                    }
                }
                type_::Warning::RedundantAssertAssignment { location } => Diagnostic {
                    title: "Redundant assertion".into(),
                    text: "This assertion is redundant since the pattern covers all possibilities."
                        .into(),
                    hint: None,
                    level: diagnostic::Level::Warning,
                    location: Some(Location {
                        label: diagnostic::Label {
                            text: Some("You can remove this".into()),
                            span: *location,
                        },
                        path: path.clone(),
                        src: src.clone(),
                        extra_labels: vec![],
                    }),
                },

                type_::Warning::TodoOrPanicUsedAsFunction {
                    kind,
                    location,
                    args_location,
                    args,
                } => {
                    let title = match kind {
                        TodoOrPanic::Todo => "Todo used as a function".into(),
                        TodoOrPanic::Panic => "Panic used as a function".into(),
                    };
                    let label_location = match args_location {
                        None => location,
                        Some(location) => location,
                    };
                    let name = match kind {
                        TodoOrPanic::Todo => "todo",
                        TodoOrPanic::Panic => "panic",
                    };
                    let mut text = format!("`{name}` is not a function");
                    match args {
                        0 => text.push_str(&format!(
                            ", you can just write `{name}` instead of `{name}()`."
                        )),
                        1 => text.push_str(
                            " and will crash before it can do anything with this argument.",
                        ),
                        _ => text.push_str(
                            " and will crash before it can do anything with these arguments.",
                        ),
                    };

                    match args {
                        0 => {}
                        _ => text.push_str(&format!(
                            "\n\nHint: if you want to display an error message you should write
`{name} as \"my error message\"`
See: https://tour.gleam.run/advanced-features/{name}/"
                        )),
                    }

                    Diagnostic {
                        title,
                        text: wrap(&text),
                        hint: None,
                        level: diagnostic::Level::Warning,
                        location: Some(Location {
                            label: diagnostic::Label {
                                text: Some("".into()),
                                span: *label_location,
                            },
                            path: path.clone(),
                            src: src.clone(),
                            extra_labels: vec![],
                        }),
                    }
                }
            },
        }
    }

    pub fn pretty(&self, buffer: &mut Buffer) {
        buffer
            .write_all(b"\n")
            .expect("error pretty buffer write space before");
        self.to_diagnostic().write(buffer);
    }

    pub fn to_pretty_string(&self) -> String {
        let mut nocolor = Buffer::no_color();
        self.pretty(&mut nocolor);
        String::from_utf8(nocolor.into_inner()).expect("Warning printing produced invalid utf8")
    }
}
