use crate::{
    ast::{BitArraySegmentTruncation, SrcSpan, TodoKind},
    build::Target,
    diagnostic::{self, Diagnostic, ExtraLabel, Location},
    error::wrap,
    type_::{
        self,
        error::{
            FeatureKind, LiteralCollectionKind, PanicPosition, TodoOrPanic,
            UnreachablePatternReason,
        },
        expression::ComparisonOutcome,
        pretty::Printer,
    },
};
use camino::Utf8PathBuf;
use debug_ignore::DebugIgnore;
use ecow::EcoString;
use std::{
    io::Write,
    sync::{Arc, atomic::Ordering},
};
use std::{rc::Rc, sync::atomic::AtomicUsize};
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
    emitter: DebugIgnore<Rc<dyn WarningEmitterIO>>,
}

impl WarningEmitter {
    pub fn new(emitter: Rc<dyn WarningEmitterIO>) -> Self {
        Self {
            count: Arc::new(AtomicUsize::new(0)),
            emitter: DebugIgnore(emitter),
        }
    }

    pub fn null() -> Self {
        Self::new(Rc::new(NullWarningEmitterIO))
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

    pub fn vector() -> (Self, Rc<VectorWarningEmitterIO>) {
        let io = Rc::new(VectorWarningEmitterIO::default());
        let emitter = Self::new(io.clone());
        (emitter, Rc::clone(&io))
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
            emitter: WarningEmitter::new(Rc::new(NullWarningEmitterIO)),
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

    DeprecatedSyntax {
        path: Utf8PathBuf,
        src: EcoString,
        warning: DeprecatedSyntaxWarning,
    },

    DeprecatedEnvironmentVariable {
        variable: DeprecatedEnvironmentVariable,
    },
}

#[derive(Debug, Clone, Eq, PartialEq, Copy)]
pub enum DeprecatedEnvironmentVariable {
    HexpmUser,
    HexpmPass,
}

impl DeprecatedEnvironmentVariable {
    fn name(&self) -> &'static str {
        match self {
            DeprecatedEnvironmentVariable::HexpmUser => "HEXPM_USER",
            DeprecatedEnvironmentVariable::HexpmPass => "HEXPM_PASS",
        }
    }

    fn message(&self) -> &'static str {
        match self {
            DeprecatedEnvironmentVariable::HexpmUser => {
                "Use the `{API_ENV_NAME}` environment variable instead."
            }
            DeprecatedEnvironmentVariable::HexpmPass => {
                "Use the `{API_ENV_NAME}` environment variable instead."
            }
        }
    }
}

#[derive(Debug, Clone, Eq, PartialEq, Copy)]
pub enum DeprecatedSyntaxWarning {
    /// If someone uses the deprecated syntax to append to a list:
    /// `["a"..rest]`, notice how there's no comma!
    DeprecatedListPrepend {
        location: SrcSpan,
    },

    /// If someone uses the deprecated syntax to pattern match on a list:
    /// ```gleam
    /// case list {
    ///   [first..rest] -> todo
    ///   //    ^^ notice there's no comma!
    ///   _ ->
    /// }
    /// ```
    ///
    DeprecatedListPattern {
        location: SrcSpan,
    },

    /// If someone uses the deprecated syntax to match on all lists instead of
    /// a common `_`:
    /// ```gleam
    /// case list {
    ///   [..] -> todo
    /// //^^^^ this matches on all lists so a `_` should be used instead!
    ///   _ ->
    /// }
    /// ```
    ///
    DeprecatedListCatchAllPattern {
        location: SrcSpan,
    },

    /// If a record pattern has a spread that is not preceded by a comma:
    /// ```gleam
    /// case wibble {
    ///   Wibble(arg1: name ..) -> todo
    /// //                  ^^ this should be preceded by a comma!
    /// }
    /// ```
    ///
    DeprecatedRecordSpreadPattern {
        location: SrcSpan,
    },

    DeprecatedTargetShorthand {
        target: Target,
        location: SrcSpan,
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

            Warning::DeprecatedSyntax {
                path,
                src,
                warning: DeprecatedSyntaxWarning::DeprecatedListPrepend { location },
            } => Diagnostic {
                title: "Deprecated prepend syntax".into(),
                text: wrap(
                    "This syntax for prepending to a list is deprecated.
When prepending an item to a list it should be preceded by a comma, \
like this: `[item, ..list]`.",
                ),

                hint: None,
                level: diagnostic::Level::Warning,
                location: Some(Location {
                    label: diagnostic::Label {
                        text: Some("This spread should be preceded by a comma".into()),
                        span: *location,
                    },
                    path: path.clone(),
                    src: src.clone(),
                    extra_labels: vec![],
                }),
            },

            Warning::DeprecatedSyntax {
                path,
                src,
                warning: DeprecatedSyntaxWarning::DeprecatedListPattern { location },
            } => Diagnostic {
                title: "Deprecated list pattern matching syntax".into(),
                text: wrap(
                    "This syntax for pattern matching on a list is deprecated.
When matching on the rest of a list it should always be preceded by a comma, \
like this: `[item, ..list]`.",
                ),
                hint: None,
                level: diagnostic::Level::Warning,
                location: Some(Location {
                    label: diagnostic::Label {
                        text: Some("This spread should be preceded by a comma".into()),
                        span: *location,
                    },
                    path: path.clone(),
                    src: src.clone(),
                    extra_labels: vec![],
                }),
            },

            Warning::DeprecatedSyntax {
                path,
                src,
                warning: DeprecatedSyntaxWarning::DeprecatedRecordSpreadPattern { location },
            } => Diagnostic {
                title: "Deprecated record pattern matching syntax".into(),
                text: wrap("This syntax for pattern matching on a record is deprecated."),
                hint: None,
                level: diagnostic::Level::Warning,
                location: Some(Location {
                    label: diagnostic::Label {
                        text: Some("This should be preceded by a comma".into()),
                        span: *location,
                    },
                    path: path.clone(),
                    src: src.clone(),
                    extra_labels: vec![],
                }),
            },

            Warning::DeprecatedSyntax {
                path,
                src,
                warning: DeprecatedSyntaxWarning::DeprecatedListCatchAllPattern { location },
            } => Diagnostic {
                title: "Deprecated list pattern matching syntax".into(),
                text: wrap(
                    "This syntax for pattern matching on lists is deprecated.
To match on all possible lists, use the `_` catch-all pattern instead.",
                ),
                hint: None,
                level: diagnostic::Level::Warning,
                location: Some(Location {
                    label: diagnostic::Label {
                        text: Some("This can be replaced with `_`".into()),
                        span: *location,
                    },
                    path: path.clone(),
                    src: src.clone(),
                    extra_labels: vec![],
                }),
            },

            Warning::DeprecatedSyntax {
                path,
                src,
                warning: DeprecatedSyntaxWarning::DeprecatedTargetShorthand { location, target },
            } => {
                let full_name = match target {
                    Target::Erlang => "erlang",
                    Target::JavaScript => "javascript",
                };

                Diagnostic {
                    title: "Deprecated target shorthand syntax".into(),
                    text: wrap(&format!(
                        "This shorthand target name is deprecated. Use the full name: `{full_name}` instead."
                    )),
                    hint: None,
                    level: diagnostic::Level::Warning,
                    location: Some(Location {
                        label: diagnostic::Label {
                            text: Some(format!("This should be replaced with `{full_name}`")),
                            span: *location,
                        },
                        path: path.clone(),
                        src: src.clone(),
                        extra_labels: vec![],
                    }),
                }
            }

            Warning::Type { path, warning, src } => match warning {
                type_::Warning::Todo {
                    kind,
                    location,
                    type_,
                } => {
                    let mut text = String::new();
                    text.push_str(
                        "\
This code will crash if it is run. Be sure to finish it before
running your program.",
                    );
                    let title = match kind {
                        TodoKind::Keyword => "Todo found",
                        TodoKind::EmptyBlock => {
                            text.push_str(
                                "
A block must always contain at least one expression.",
                            );
                            "Incomplete block"
                        }
                        TodoKind::EmptyFunction { .. } => "Unimplemented function",
                        TodoKind::IncompleteUse => {
                            text.push_str(
                                "
A use expression must always be followed by at least one expression.",
                            );
                            "Incomplete use expression"
                        }
                    }
                    .into();
                    if !type_.is_variable() {
                        text.push_str(&format!(
                            "\n\nHint: I think its type is `{}`.\n",
                            Printer::new().pretty_print(type_, 0)
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

                type_::Warning::UnusedVariable { location, origin } => Diagnostic {
                    title: "Unused variable".into(),
                    text: "".into(),
                    hint: origin.how_to_ignore(),
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
                    hint: None,
                    level: diagnostic::Level::Warning,
                    location: Some(Location {
                        src: src.clone(),
                        path: path.to_path_buf(),
                        label: diagnostic::Label {
                            text: Some("You can safely remove this.".into()),
                            span: *location,
                        },
                        extra_labels: Vec::new(),
                    }),
                },
                type_::Warning::UnnecessaryDoubleBoolNegation { location } => Diagnostic {
                    title: "Unnecessary double negation (!!) on bool".into(),
                    text: "".into(),
                    hint: None,
                    level: diagnostic::Level::Warning,
                    location: Some(Location {
                        src: src.clone(),
                        path: path.to_path_buf(),
                        label: diagnostic::Label {
                            text: Some("You can safely remove this.".into()),
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

                type_::Warning::UnreachableCasePattern { location, reason } => {
                    let text: String = match reason {
                        UnreachablePatternReason::DuplicatePattern => wrap(
                            "This pattern cannot be reached as a previous \
pattern matches the same values.\n",
                        ),
                        UnreachablePatternReason::ImpossibleVariant => wrap(
                            "This pattern cannot be reached as it matches on \
a variant of a type which is never present.\n",
                        ),
                    };
                    Diagnostic {
                        title: "Unreachable pattern".into(),
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
                    text: wrap(
                        "This expression computes a value without any side \
effects, but then the value isn't used at all. You might want to assign it to a \
variable, or delete the expression entirely if it's not needed.",
                    ),
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

                type_::Warning::AssertAssignmentOnInferredVariant { location } => Diagnostic {
                    title: "Assertion that will always fail".into(),
                    text: wrap(
                        "We can tell from the code above that the value will never match \
this pattern and that this code will always crash.

Either change the pattern or use `panic` to unconditionally fail.",
                    ),
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
                },

                type_::Warning::TodoOrPanicUsedAsFunction {
                    kind,
                    location,
                    arguments_location,
                    arguments,
                } => {
                    let title = match kind {
                        TodoOrPanic::Todo => "Todo used as a function".into(),
                        TodoOrPanic::Panic => "Panic used as a function".into(),
                    };
                    let label_location = match arguments_location {
                        None => location,
                        Some(location) => location,
                    };
                    let name = match kind {
                        TodoOrPanic::Todo => "todo",
                        TodoOrPanic::Panic => "panic",
                    };
                    let mut text = format!("`{name}` is not a function");
                    match arguments {
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

                    match arguments {
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
                                text: None,
                                span: *label_location,
                            },
                            path: path.clone(),
                            src: src.clone(),
                            extra_labels: vec![],
                        }),
                    }
                }

                type_::Warning::UnreachableCodeAfterPanic {
                    location,
                    panic_position: unreachable_code_kind,
                } => {
                    let text = match unreachable_code_kind {
                        PanicPosition::PreviousExpression => {
                            "This code is unreachable because it comes after a `panic`."
                        }
                        PanicPosition::PreviousFunctionArgument => {
                            "This argument is unreachable because the previous one always panics. \
Your code will crash before reaching this point."
                        }
                        PanicPosition::LastFunctionArgument => {
                            "This function call is unreachable because its last argument always panics. \
Your code will crash before reaching this point."
                        }
                        PanicPosition::EchoExpression => {
                            "This `echo` won't print anything because the expression it \
should be printing always panics."
                        }
                    };

                    Diagnostic {
                        title: "Unreachable code".into(),
                        text: wrap(text),
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

                type_::Warning::RedundantPipeFunctionCapture { location } => Diagnostic {
                    title: "Redundant function capture".into(),
                    text: wrap(
                        "This function capture is redundant since the value is already piped as \
the first argument of this call.

See: https://tour.gleam.run/functions/pipelines/",
                    ),
                    hint: None,
                    level: diagnostic::Level::Warning,
                    location: Some(Location {
                        label: diagnostic::Label {
                            text: Some("You can safely remove this".into()),
                            span: *location,
                        },
                        path: path.clone(),
                        src: src.clone(),
                        extra_labels: vec![],
                    }),
                },
                type_::Warning::FeatureRequiresHigherGleamVersion {
                    location,
                    minimum_required_version,
                    wrongfully_allowed_version,
                    feature_kind,
                } => {
                    let feature = match feature_kind {
                        FeatureKind::LabelShorthandSyntax => "The label shorthand syntax was",
                        FeatureKind::ConstantStringConcatenation => {
                            "Constant strings concatenation was"
                        }
                        FeatureKind::ArithmeticInGuards => "Arithmetic operations in guards were",
                        FeatureKind::UnannotatedUtf8StringSegment => {
                            "The ability to omit the `utf8` annotation for string segments was"
                        }
                        FeatureKind::UnannotatedFloatSegment => {
                            "The ability to omit the `float` annotation for float segments was"
                        }
                        FeatureKind::NestedTupleAccess => {
                            "The ability to access nested tuple fields was"
                        }
                        FeatureKind::InternalAnnotation => "The `@internal` annotation was",
                        FeatureKind::AtInJavascriptModules => {
                            "The ability to have `@` in a Javascript module's name was"
                        }
                        FeatureKind::RecordUpdateVariantInference => {
                            "Record updates for custom types when the variant is known was"
                        }
                        FeatureKind::RecordAccessVariantInference => {
                            "Field access on custom types when the variant is known was"
                        }
                        FeatureKind::LetAssertWithMessage => {
                            "Specifying a custom panic message when using let assert was"
                        }
                        FeatureKind::VariantWithDeprecatedAnnotation => {
                            "Deprecating individual custom type variants was"
                        }
                        FeatureKind::JavaScriptUnalignedBitArray => {
                            "Use of unaligned bit arrays on the JavaScript target was"
                        }
                        FeatureKind::BoolAssert => "The bool `assert` statement was",
                    };

                    Diagnostic {
                        title: "Incompatible gleam version range".into(),
                        text: wrap(&format!(
                        "{feature} introduced in version v{minimum_required_version}. But the Gleam version range \
                        specified in your `gleam.toml` would allow this code to run on an earlier \
                        version like v{wrongfully_allowed_version}, resulting in compilation errors!",
                    )),
                        hint: Some(format!(
                            "Remove the version constraint from your `gleam.toml` or update it to be:

    gleam = \">= {minimum_required_version}\""
                        )),
                        level: diagnostic::Level::Warning,
                        location: Some(Location {
                            label: diagnostic::Label {
                                text: Some(format!(
                                    "This requires a Gleam version >= {minimum_required_version}"
                                )),
                                span: *location,
                            },
                            path: path.clone(),
                            src: src.clone(),
                            extra_labels: vec![],
                        }),
                    }
                }

                type_::Warning::JavaScriptIntUnsafe { location } => Diagnostic {
                    title: "Int is outside JavaScript's safe integer range".into(),
                    text: wrap(
                        "This integer value is too large to be represented accurately by \
JavaScript's number type. To avoid this warning integer values must be in the range \
-(2^53 - 1) - (2^53 - 1).

See JavaScript's Number.MAX_SAFE_INTEGER and Number.MIN_SAFE_INTEGER properties for more \
information.",
                    ),
                    hint: None,
                    level: diagnostic::Level::Warning,
                    location: Some(Location {
                        path: path.to_path_buf(),
                        src: src.clone(),
                        label: diagnostic::Label {
                            text: Some("This is not a safe integer value on JavaScript".into()),
                            span: *location,
                        },
                        extra_labels: Vec::new(),
                    }),
                },

                type_::Warning::BitArraySegmentTruncatedValue {
                    location: _,
                    truncation:
                        BitArraySegmentTruncation {
                            truncated_value,
                            truncated_into,
                            segment_bits,
                            value_location,
                        },
                } => {
                    let (unit, segment_size, taken) = if segment_bits % 8 == 0 {
                        let bytes = segment_bits / 8;
                        let segment_size = pluralise(format!("{bytes} byte"), bytes);
                        let taken = if bytes == 1 {
                            "first byte".into()
                        } else {
                            format!("first {bytes} bytes")
                        };

                        ("bytes", segment_size, taken)
                    } else {
                        let segment_size = pluralise(format!("{segment_bits} bit"), *segment_bits);
                        let taken = if *segment_bits == 1 {
                            "first bit".into()
                        } else {
                            format!("first {segment_bits} bits")
                        };
                        ("bits", segment_size, taken)
                    };

                    let text = format!(
                        "This segment is {segment_size} long, but {truncated_value} \
doesn't fit in that many {unit}. It would be truncated by taking its {taken}, resulting in the value {truncated_into}."
                    );

                    Diagnostic {
                        title: "Truncated bit array segment".into(),
                        text: wrap(&text),
                        hint: None,
                        level: diagnostic::Level::Warning,
                        location: Some(Location {
                            path: path.to_path_buf(),
                            src: src.clone(),
                            label: diagnostic::Label {
                                text: Some(format!(
                                    "You can safely replace this with {truncated_into}"
                                )),
                                span: *value_location,
                            },
                            extra_labels: vec![],
                        }),
                    }
                }

                type_::Warning::AssertLiteralBool { location } => Diagnostic {
                    title: "Assertion of a literal value".into(),
                    text: wrap(
                        "Asserting on a literal bool is redundant since you \
can already tell whether it will be `True` or `False`.",
                    ),
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
                },

                type_::Warning::ModuleImportedTwice {
                    name,
                    first,
                    second,
                } => Diagnostic {
                    title: "Duplicate import".into(),
                    text: format!("The {name} module has been imported twice."),
                    hint: None,
                    level: diagnostic::Level::Warning,
                    location: Some(Location {
                        src: src.clone(),
                        path: path.to_path_buf(),
                        label: diagnostic::Label {
                            text: Some("Reimported here".into()),
                            span: *second,
                        },
                        extra_labels: vec![ExtraLabel {
                            src_info: None,
                            label: diagnostic::Label {
                                text: Some("First imported here".into()),
                                span: *first,
                            },
                        }],
                    }),
                },

                type_::Warning::UnusedDiscardPattern { location, name } => Diagnostic {
                    title: "Unused discard pattern".into(),
                    text: format!("`_ as {name}` can be written more concisely as `{name}`"),
                    level: diagnostic::Level::Warning,
                    location: Some(Location {
                        src: src.clone(),
                        path: path.to_path_buf(),
                        label: diagnostic::Label {
                            text: None,
                            span: SrcSpan {
                                start: location.start - "_ as ".len() as u32,
                                end: location.end,
                            },
                        },
                        extra_labels: vec![],
                    }),
                    hint: None,
                },

                type_::Warning::TopLevelDefinitionShadowsImport { location, name } => {
                    let text = format!(
                        "Definition of {name} shadows an imported value.
The imported value could not be used in this module anyway."
                    );
                    Diagnostic {
                        title: "Shadowed Import".into(),
                        text: wrap(&text),
                        level: diagnostic::Level::Warning,
                        location: Some(Location {
                            path: path.clone(),
                            src: src.clone(),
                            label: diagnostic::Label {
                                text: Some(wrap(&format!("`{name}` is defined here"))),
                                span: *location,
                            },
                            extra_labels: Vec::new(),
                        }),
                        hint: Some("Either rename the definition or remove the import.".into()),
                    }
                }

                type_::Warning::RedundantComparison { location, outcome } => Diagnostic {
                    title: "Redundant comparison".into(),
                    text: format!(
                        "This comparison is redundant since it always {}.",
                        match outcome {
                            ComparisonOutcome::AlwaysSucceeds => "succeeds",
                            ComparisonOutcome::AlwaysFails => "fails",
                        }
                    ),
                    hint: None,
                    level: diagnostic::Level::Warning,
                    location: Some(Location {
                        label: diagnostic::Label {
                            text: Some(format!(
                                "This is always `{}`",
                                match outcome {
                                    ComparisonOutcome::AlwaysSucceeds => "True",
                                    ComparisonOutcome::AlwaysFails => "False",
                                }
                            )),
                            span: *location,
                        },
                        path: path.clone(),
                        src: src.clone(),
                        extra_labels: vec![],
                    }),
                },
            },

            Warning::DeprecatedEnvironmentVariable { variable } => {
                let name = variable.name();
                let message = variable.message();

                let text = wrap(&format!(
                    "The environment variable `{name}` is deprecated.\n\n{message}"
                ));

                Diagnostic {
                    title: "Use of deprecated environment variable".into(),
                    text,
                    hint: None,
                    level: diagnostic::Level::Warning,
                    location: None,
                }
            }
        }
    }

    pub fn pretty(&self, buffer: &mut Buffer) {
        self.to_diagnostic().write(buffer);
        buffer
            .write_all(b"\n")
            .expect("error pretty buffer write space after");
    }

    pub fn to_pretty_string(&self) -> String {
        let mut nocolor = Buffer::no_color();
        self.pretty(&mut nocolor);
        String::from_utf8(nocolor.into_inner()).expect("Warning printing produced invalid utf8")
    }
}

fn pluralise(string: String, quantity: i64) -> String {
    if quantity == 1 {
        string
    } else {
        format!("{string}s")
    }
}
