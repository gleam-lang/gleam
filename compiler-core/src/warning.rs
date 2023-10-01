use crate::{
    ast::TodoKind,
    build::Target,
    diagnostic::{self, Diagnostic, Location},
    error::wrap,
    type_,
};
use camino::Utf8PathBuf;
use debug_ignore::DebugIgnore;
use smol_str::SmolStr;
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
    pub fn take(&self) -> Vec<Warning> {
        let mut warnings = self.write_lock();
        std::mem::take(&mut *warnings)
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
    module_src: SmolStr,
    emitter: WarningEmitter,
}

impl TypeWarningEmitter {
    pub fn new(module_path: Utf8PathBuf, module_src: SmolStr, emitter: WarningEmitter) -> Self {
        Self {
            module_path,
            module_src,
            emitter,
        }
    }

    pub fn null() -> Self {
        Self {
            module_path: Utf8PathBuf::new(),
            module_src: SmolStr::new(""),
            emitter: WarningEmitter::new(Arc::new(NullWarningEmitterIO)),
        }
    }

    pub fn emit(&self, warning: crate::type_::Warning) {
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
        src: SmolStr,
        warning: crate::type_::Warning,
    },
    Parse {
        path: Utf8PathBuf,
        src: SmolStr,
        warning: crate::parse::Warning,
    },
    InvalidSource {
        path: Utf8PathBuf,
    },
}

impl Warning {
    pub fn to_diagnostic(&self) -> Diagnostic {
        match self {
            Self::Parse { path, warning, src } => match warning {
                crate::parse::Warning::DeprecatedIf { location, target } => {
                    let target = match target {
                        Target::Erlang => "erlang",
                        Target::JavaScript => "javascript",
                    };
                    let text = format!(
                        "This syntax has been replaced by the `@target({target})` attribute.\n"
                    );
                    Diagnostic {
                        title: "Deprecated if syntax".into(),
                        text,
                        hint: Some("Run `gleam format` to auto-fix your code.".into()),
                        level: diagnostic::Level::Warning,
                        location: Some(Location {
                            path: path.to_path_buf(),
                            src: src.clone(),
                            label: diagnostic::Label {
                                text: None,
                                span: *location,
                            },
                            extra_labels: Vec::new(),
                        }),
                    }
                }

                crate::parse::Warning::DeprecatedExternalFn { location } => {
                    let text =
                        "This syntax has been replaced by the `@external` attribute.\n".into();
                    Diagnostic {
                        title: "Deprecated external fn syntax".into(),
                        text,
                        hint: Some("Run `gleam fix` to auto-fix your code.".into()),
                        level: diagnostic::Level::Warning,
                        location: Some(Location {
                            path: path.to_path_buf(),
                            src: src.clone(),
                            label: diagnostic::Label {
                                text: None,
                                span: *location,
                            },
                            extra_labels: Vec::new(),
                        }),
                    }
                }

                crate::parse::Warning::DeprecatedTodo { location, message } => {
                    let text = format!(
                        "The `todo()` syntax has been replaced by this syntax:
                        
    todo as \"{message}\"\n"
                    );
                    Diagnostic {
                        title: "Deprecated todo syntax".into(),
                        text,
                        hint: Some("Run `gleam format` to auto-fix your code.".into()),
                        level: diagnostic::Level::Warning,
                        location: Some(Location {
                            path: path.to_path_buf(),
                            src: src.clone(),
                            label: diagnostic::Label {
                                text: None,
                                span: *location,
                            },
                            extra_labels: Vec::new(),
                        }),
                    }
                }

                crate::parse::Warning::DeprecatedExternalType { location, name } => {
                    let text =
                        format!("This syntax has been replaced by the `type {name}` syntax.\n");
                    Diagnostic {
                        title: "Deprecated if syntax".into(),
                        text,
                        hint: Some("Run `gleam format` to auto-fix your code.".into()),
                        level: diagnostic::Level::Warning,
                        location: Some(Location {
                            path: path.to_path_buf(),
                            src: src.clone(),
                            label: diagnostic::Label {
                                text: None,
                                span: *location,
                            },
                            extra_labels: Vec::new(),
                        }),
                    }
                }

                crate::parse::Warning::DeprecatedOptionBitString { location } => {
                    let text = "This option has been replaced by the `bits` option.\n".into();
                    Diagnostic {
                        title: "Deprecated bit literal option".into(),
                        text,
                        hint: Some("Run `gleam format` to auto-fix your code.".into()),
                        level: diagnostic::Level::Warning,
                        location: Some(Location {
                            path: path.to_path_buf(),
                            src: src.clone(),
                            label: diagnostic::Label {
                                text: None,
                                span: *location,
                            },
                            extra_labels: Vec::new(),
                        }),
                    }
                }

                crate::parse::Warning::DeprecatedOptionBinary { location } => {
                    let text = "This option has been replaced by the `bytes` option.\n".into();
                    Diagnostic {
                        title: "Deprecated bit literal option".into(),
                        text,
                        hint: Some("Run `gleam format` to auto-fix your code.".into()),
                        level: diagnostic::Level::Warning,
                        location: Some(Location {
                            path: path.to_path_buf(),
                            src: src.clone(),
                            label: diagnostic::Label {
                                text: None,
                                span: *location,
                            },
                            extra_labels: Vec::new(),
                        }),
                    }
                }
            },

            Warning::InvalidSource { path } => Diagnostic {
                title: "Invalid module name.".into(),
                text: "Module names must begin with a lowercase letter and contain\
 only lowercase alphanumeric characters or underscores."
                    .into(),
                level: diagnostic::Level::Warning,
                location: None,
                hint: Some(format!(
                    "Rename `{}` to be valid, or remove this file from the project source.",
                    path
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
                            type_::pretty::Printer::new().pretty_print(typ, 0)
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
                    hint: Some("If you are sure you don't need it you can assign it to `_`".into()),
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
                        "This imported constructor is never used.".into()
                    } else {
                        "This private constructor is never used.".into()
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
                            text: Some("This imported module is never used.".into()),
                            span: *location,
                        },
                        extra_labels: Vec::new(),
                    }),
                },

                type_::Warning::UnusedImportedModuleAlias { location, .. } => Diagnostic {
                    title: "Unused imported module alias".into(),
                    text: "".into(),
                    hint: Some("You can safely remove it.".into()),
                    level: diagnostic::Level::Warning,
                    location: Some(Location {
                        src: src.clone(),
                        path: path.to_path_buf(),
                        label: diagnostic::Label {
                            text: Some("This imported module alias is never used.".into()),
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
                        src: src.clone(),
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
                        src: src.clone(),
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
                        src: src.clone(),
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
                    hint: Some(format!("You can ignore it with an underscore: `_{name}`.")),
                    level: diagnostic::Level::Warning,
                    location: Some(Location {
                        src: src.clone(),
                        path: path.to_path_buf(),
                        label: diagnostic::Label {
                            text: Some("This variable is never used.".into()),
                            span: *location,
                        },
                        extra_labels: Vec::new(),
                    }),
                },
                type_::Warning::UnnecessaryDoubleIntNegation { location } => Diagnostic {
                    title: "Unnecessary double negation (--) on integer.".into(),
                    text: "".into(),
                    hint: Some("You can safely remove this".into()),
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
                    title: "Unnecessary double negation (!!) on bool.".into(),
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
                        title: "Inefficient use of list.length".into(),
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
                        title: "Transative dependency imported".into(),
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

                type_::Warning::DeprecatedValue { location, message } => {
                    let text = wrap(&format!("It was deprecated with this message: {message}"));
                    Diagnostic {
                        title: "Deprecated value used".into(),
                        text,
                        hint: None,
                        level: diagnostic::Level::Warning,
                        location: Some(Location {
                            src: src.clone(),
                            path: path.to_path_buf(),
                            label: diagnostic::Label {
                                text: Some("This value has been deprecated".into()),
                                span: *location,
                            },
                            extra_labels: Vec::new(),
                        }),
                    }
                }

                type_::Warning::DeprecatedBitString { location } => {
                    let text = "The type BitString has been renamed to BitArray.\n".into();
                    Diagnostic {
                        title: "Deprecated BitString name used".into(),
                        text,
                        hint: Some("Run `gleam fix` to auto-fix your code.".into()),
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
