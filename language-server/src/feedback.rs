use gleam_core::{Error, Warning, diagnostic::Diagnostic};
use std::collections::{HashMap, HashSet};

use camino::Utf8PathBuf;

use super::engine::Compilation;

#[derive(Debug, Default, PartialEq, Eq)]
pub struct Feedback {
    pub diagnostics: HashMap<Utf8PathBuf, Vec<Diagnostic>>,
    pub messages: Vec<Diagnostic>,
}

impl Feedback {
    /// Set the diagnostics for a file to an empty vector. This will overwrite
    /// any existing diagnostics on the client.
    pub fn unset_existing_diagnostics(&mut self, path: Utf8PathBuf) {
        _ = self.diagnostics.insert(path, vec![]);
    }

    pub fn append_diagnostic(&mut self, path: Utf8PathBuf, diagnostic: Diagnostic) {
        self.diagnostics.entry(path).or_default().push(diagnostic);
    }

    /// No feedback at all.
    ///
    pub fn none() -> Feedback {
        Default::default()
    }

    /// Add all the content of another feedback to this feedback.
    ///
    pub fn append_feedback(&mut self, feedback: Feedback) {
        for (path, diagnostics) in feedback.diagnostics {
            // Any new diagnostics for a file will overwrite any existing ones.
            _ = self.diagnostics.insert(path, diagnostics);
        }
        for diagnostic in feedback.messages {
            self.append_message(diagnostic);
        }
    }

    fn append_message(&mut self, diagnostic: Diagnostic) {
        self.messages.push(diagnostic);
    }
}

/// When an operation succeeds or fails we want to send diagnostics and
/// messages to the client for displaying to the user. This object converts
/// Gleam warnings, errors, etc to these feedback items.
///
/// Gleam has incremental compilation so we cannot erase all previous
/// diagnostics and replace each time new diagnostics are available; if a file
/// has not been recompiled then any diagnostics it had previously are still
/// valid and must not be erased.
/// To do this we keep track of which files have diagnostics and only overwrite
/// them if the file has been recompiled.
///
#[derive(Debug, Default)]
pub struct FeedbackBookKeeper {
    files_with_warnings: HashSet<Utf8PathBuf>,
    files_with_errors: HashSet<Utf8PathBuf>,
}

impl FeedbackBookKeeper {
    /// Send diagnostics for any warnings and remove any diagnostics for files
    /// that have compiled without warnings.
    ///
    pub fn response(&mut self, compilation: Compilation, warnings: Vec<Warning>) -> Feedback {
        let mut feedback = Feedback::default();

        if let Compilation::Yes(compiled_modules) = compilation {
            // Any existing diagnostics for files that have been compiled are no
            // longer valid so we set an empty vector of diagnostics for the files
            // to erase their diagnostics.
            for path in compiled_modules {
                let has_existing_diagnostics = self.files_with_warnings.remove(&path);
                if has_existing_diagnostics {
                    feedback.unset_existing_diagnostics(path);
                }
            }

            // Compilation was attempted and there is no error (which there is not
            // in this function) then it means that compilation has succeeded, so
            // there should be no error diagnostics.
            // We don't limit this to files that have been compiled as a previous
            // cached version could be used instead of a recompile.
            self.unset_errors(&mut feedback);
        }

        for warning in warnings {
            self.insert_warning(&mut feedback, warning);
        }

        feedback
    }

    fn unset_errors(&mut self, feedback: &mut Feedback) {
        // TODO: avoid clobbering warnings. They should be preserved rather than
        // removed with the errors here. We will need to store the warnings and
        // re-send them.
        for path in self.files_with_errors.drain() {
            feedback.unset_existing_diagnostics(path);
        }
    }

    /// Compilation failed, boo!
    ///
    /// Send diagnostics for any warnings and remove any diagnostics for files
    /// that have compiled without warnings, AND ALSO send diagnostics for the
    /// error that caused compilation to fail.
    ///
    pub fn build_with_error(
        &mut self,
        error: Error,
        compilation: Compilation,
        warnings: Vec<Warning>,
    ) -> Feedback {
        let diagnostics = error.to_diagnostics();
        let mut feedback = self.response(compilation, warnings);

        // A new error means that any existing errors are no longer valid. Unset them.
        self.unset_errors(&mut feedback);

        for diagnostic in diagnostics {
            match diagnostic.location.as_ref().map(|l| l.path.clone()) {
                Some(path) => {
                    _ = self.files_with_errors.insert(path.clone());
                    feedback.append_diagnostic(path, diagnostic);
                }

                None => {
                    feedback.append_message(diagnostic);
                }
            }
        }

        feedback
    }

    pub fn error(&mut self, error: Error) -> Feedback {
        self.build_with_error(error, Compilation::No, vec![])
    }

    fn insert_warning(&mut self, feedback: &mut Feedback, warning: Warning) {
        let diagnostic = warning.to_diagnostic();
        if let Some(path) = diagnostic.location.as_ref().map(|l| l.path.clone()) {
            _ = self.files_with_warnings.insert(path.clone());
            feedback.append_diagnostic(path, diagnostic);
        }
    }
}

#[cfg(test)]
mod tests {

    use std::assert_eq;

    use super::*;
    use gleam_core::{
        ast::SrcSpan,
        diagnostic::Level,
        parse::error::{ParseError, ParseErrorType},
        type_,
    };

    #[test]
    fn feedback() {
        let mut book_keeper = FeedbackBookKeeper::default();
        let file1 = Utf8PathBuf::from("src/file1.gleam");
        let file2 = Utf8PathBuf::from("src/file2.gleam");
        let file3 = Utf8PathBuf::from("src/file3.gleam");

        let warning1 = Warning::Type {
            path: file1.clone(),
            src: "src".into(),
            warning: type_::Warning::NoFieldsRecordUpdate {
                location: SrcSpan::new(1, 2),
            },
        };
        let warning2 = Warning::Type {
            path: file2.clone(),
            src: "src".into(),
            warning: type_::Warning::NoFieldsRecordUpdate {
                location: SrcSpan::new(1, 2),
            },
        };

        let feedback = book_keeper.response(
            Compilation::Yes(vec![file1.clone()]),
            vec![warning1.clone(), warning1.clone(), warning2.clone()],
        );

        assert_eq!(
            Feedback {
                diagnostics: HashMap::from([
                    (
                        file1.clone(),
                        vec![warning1.to_diagnostic(), warning1.to_diagnostic(),]
                    ),
                    (file2.clone(), vec![warning2.to_diagnostic(),])
                ]),
                messages: vec![],
            },
            feedback
        );

        let feedback = book_keeper.response(
            Compilation::Yes(vec![file1.clone(), file2.clone(), file3]),
            vec![],
        );

        assert_eq!(
            Feedback {
                diagnostics: HashMap::from([
                    // File 1 and 2 had diagnostics before so they have been unset
                    (file1, vec![]),
                    (file2, vec![]),
                    // File 3 had no diagnostics so does not need to to be unset
                ]),
                messages: vec![],
            },
            feedback
        );
    }

    #[test]
    fn locationless_error() {
        // The failed method sets an additional messages for errors without a
        // location.

        let mut book_keeper = FeedbackBookKeeper::default();
        let file1 = Utf8PathBuf::from("src/file1.gleam");

        let warning1 = Warning::Type {
            path: file1.clone(),
            src: "src".into(),
            warning: type_::Warning::NoFieldsRecordUpdate {
                location: SrcSpan::new(1, 2),
            },
        };

        let locationless_error = Error::Gzip("Hello!".into());

        let feedback = book_keeper.build_with_error(
            locationless_error.clone(),
            Compilation::Yes(vec![]),
            vec![warning1.clone()],
        );

        assert_eq!(
            Feedback {
                diagnostics: HashMap::from([(file1, vec![warning1.to_diagnostic()])]),
                messages: locationless_error.to_diagnostics(),
            },
            feedback
        );
    }

    #[test]
    fn error() {
        // The failed method sets an additional diagnostic if the error has a
        // location.

        let mut book_keeper = FeedbackBookKeeper::default();
        let file1 = Utf8PathBuf::from("src/file1.gleam");
        let file3 = Utf8PathBuf::from("src/file2.gleam");

        let warning1 = Warning::Type {
            path: file1.clone(),
            src: "src".into(),
            warning: type_::Warning::NoFieldsRecordUpdate {
                location: SrcSpan::new(1, 2),
            },
        };
        let error = Error::Parse {
            path: file3.clone(),
            src: "blah".into(),
            error: Box::new(ParseError {
                error: ParseErrorType::ConcatPatternVariableLeftHandSide,
                location: SrcSpan::new(1, 4),
            }),
        };

        let feedback = book_keeper.build_with_error(
            error.clone(),
            Compilation::Yes(vec![]),
            vec![warning1.clone()],
        );

        assert_eq!(
            Feedback {
                diagnostics: HashMap::from([
                    (file1, vec![warning1.to_diagnostic()]),
                    (file3.clone(), error.to_diagnostics()),
                ]),
                messages: vec![],
            },
            feedback
        );

        // The error diagnostic should be removed if the file compiles later.

        let feedback = book_keeper.response(Compilation::Yes(vec![file3.clone()]), vec![]);

        assert_eq!(
            Feedback {
                diagnostics: HashMap::from([(file3, vec![])]),
                messages: vec![],
            },
            feedback
        );
    }

    // https://github.com/gleam-lang/gleam/issues/2093
    #[test]
    fn successful_compilation_removes_error_diagnostic() {
        // It is possible for a compile error to be fixed but the module that
        // had the error to not actually be recompiled.
        //
        // 1. File is OK
        // 2. File is edited to an invalid state
        // 3. A compile error is emitted
        // 4. File is edited back to the earlier valid state
        // 5. File is not recompiled as the cache from step 1 is still valid
        //
        // Because of this the compiled files iterator does not contain the
        // file, so we need to make sure that the error is removed through other
        // means, such as tracking which files have errors and removing them all
        // when a successful compilation occurs.

        let mut book_keeper = FeedbackBookKeeper::default();
        let file1 = Utf8PathBuf::from("src/file1.gleam");
        let file2 = Utf8PathBuf::from("src/file2.gleam");

        let error = Error::Parse {
            path: file1.clone(),
            src: "blah".into(),
            error: Box::new(ParseError {
                error: ParseErrorType::ConcatPatternVariableLeftHandSide,
                location: SrcSpan::new(1, 4),
            }),
        };

        let feedback =
            book_keeper.build_with_error(error.clone(), Compilation::Yes(vec![]), vec![]);

        assert_eq!(
            Feedback {
                diagnostics: HashMap::from([(file1.clone(), error.to_diagnostics())]),
                messages: vec![],
            },
            feedback
        );

        // The error diagnostic should be removed on a successful compilation,
        // even though the file is not in the compiled files iterator.

        let feedback = book_keeper.response(Compilation::Yes(vec![file2]), vec![]);

        assert_eq!(
            Feedback {
                diagnostics: HashMap::from([(file1, vec![])]),
                messages: vec![],
            },
            feedback
        );
    }

    // https://github.com/gleam-lang/gleam/issues/2122
    #[test]
    fn second_failure_unsets_previous_error() {
        let mut book_keeper = FeedbackBookKeeper::default();
        let file1 = Utf8PathBuf::from("src/file1.gleam");
        let file2 = Utf8PathBuf::from("src/file2.gleam");

        let error = |file: &camino::Utf8Path| Error::Parse {
            path: file.to_path_buf(),
            src: "blah".into(),
            error: Box::new(ParseError {
                error: ParseErrorType::ConcatPatternVariableLeftHandSide,
                location: SrcSpan::new(1, 4),
            }),
        };

        let feedback =
            book_keeper.build_with_error(error(&file1), Compilation::Yes(vec![]), vec![]);

        assert_eq!(
            Feedback {
                diagnostics: HashMap::from([(file1.clone(), error(&file1).to_diagnostics())]),
                messages: vec![],
            },
            feedback
        );

        let feedback =
            book_keeper.build_with_error(error(&file2), Compilation::Yes(vec![]), vec![]);

        assert_eq!(
            Feedback {
                diagnostics: HashMap::from([
                    // Unset the previous error
                    (file1, vec![]),
                    // Set the new one
                    (file2.clone(), error(&file2).to_diagnostics()),
                ]),
                messages: vec![],
            },
            feedback
        );
    }

    // https://github.com/gleam-lang/gleam/issues/2105
    #[test]
    fn successful_non_compilation_does_not_remove_error_diagnostic() {
        let mut book_keeper = FeedbackBookKeeper::default();
        let file1 = Utf8PathBuf::from("src/file1.gleam");

        let error = Error::Parse {
            path: file1.clone(),
            src: "blah".into(),
            error: Box::new(ParseError {
                error: ParseErrorType::ConcatPatternVariableLeftHandSide,
                location: SrcSpan::new(1, 4),
            }),
        };

        let feedback =
            book_keeper.build_with_error(error.clone(), Compilation::Yes(vec![]), vec![]);

        assert_eq!(
            Feedback {
                diagnostics: HashMap::from([(file1, error.to_diagnostics())]),
                messages: vec![],
            },
            feedback
        );

        // The error diagnostic should not be removed, nothing has been
        // successfully compiled.

        let feedback = book_keeper.response(Compilation::No, vec![]);

        assert_eq!(
            Feedback {
                diagnostics: HashMap::new(),
                messages: vec![],
            },
            feedback
        );
    }

    #[test]
    fn append_feedback_new_file() {
        let mut feedback = Feedback {
            diagnostics: HashMap::from([(
                Utf8PathBuf::from("src/file1.gleam"),
                vec![Diagnostic {
                    location: None,
                    hint: None,
                    text: "Error 1".to_string(),
                    title: "Error 1".to_string(),
                    level: Level::Error,
                }],
            )]),
            messages: vec![Diagnostic {
                location: None,
                hint: None,
                text: "Error 2".to_string(),
                title: "Error 2".to_string(),
                level: Level::Error,
            }],
        };
        feedback.append_feedback(Feedback {
            diagnostics: HashMap::from([(
                Utf8PathBuf::from("src/file2.gleam"),
                vec![Diagnostic {
                    location: None,
                    hint: None,
                    text: "Error 3".to_string(),
                    title: "Error 3".to_string(),
                    level: Level::Error,
                }],
            )]),
            messages: vec![],
        });
        assert_eq!(
            feedback,
            Feedback {
                diagnostics: HashMap::from([
                    (
                        Utf8PathBuf::from("src/file1.gleam"),
                        vec![Diagnostic {
                            location: None,
                            hint: None,
                            text: "Error 1".to_string(),
                            title: "Error 1".to_string(),
                            level: Level::Error,
                        }],
                    ),
                    (
                        Utf8PathBuf::from("src/file2.gleam"),
                        vec![Diagnostic {
                            location: None,
                            hint: None,
                            text: "Error 3".to_string(),
                            title: "Error 3".to_string(),
                            level: Level::Error,
                        }],
                    ),
                ]),
                messages: vec![Diagnostic {
                    location: None,
                    hint: None,
                    text: "Error 2".to_string(),
                    title: "Error 2".to_string(),
                    level: Level::Error,
                },],
            }
        );
    }

    #[test]
    fn append_feedback_same_file() {
        let mut feedback = Feedback {
            diagnostics: HashMap::from([(
                Utf8PathBuf::from("src/file1.gleam"),
                vec![Diagnostic {
                    location: None,
                    hint: None,
                    text: "Error 1".to_string(),
                    title: "Error 1".to_string(),
                    level: Level::Error,
                }],
            )]),
            messages: vec![Diagnostic {
                location: None,
                hint: None,
                text: "Error 2".to_string(),
                title: "Error 2".to_string(),
                level: Level::Error,
            }],
        };
        feedback.append_feedback(Feedback {
            diagnostics: HashMap::from([(
                Utf8PathBuf::from("src/file1.gleam"),
                vec![Diagnostic {
                    location: None,
                    hint: None,
                    text: "Error 3".to_string(),
                    title: "Error 3".to_string(),
                    level: Level::Error,
                }],
            )]),
            messages: vec![],
        });
        assert_eq!(
            feedback,
            Feedback {
                diagnostics: HashMap::from([(
                    Utf8PathBuf::from("src/file1.gleam"),
                    vec![Diagnostic {
                        location: None,
                        hint: None,
                        text: "Error 3".to_string(),
                        title: "Error 3".to_string(),
                        level: Level::Error,
                    }],
                ),]),
                messages: vec![Diagnostic {
                    location: None,
                    hint: None,
                    text: "Error 2".to_string(),
                    title: "Error 2".to_string(),
                    level: Level::Error,
                },],
            }
        );
    }

    #[test]
    fn append_feedback_new_message() {
        let mut feedback = Feedback {
            diagnostics: HashMap::from([(
                Utf8PathBuf::from("src/file1.gleam"),
                vec![Diagnostic {
                    location: None,
                    hint: None,
                    text: "Error 1".to_string(),
                    title: "Error 1".to_string(),
                    level: Level::Error,
                }],
            )]),
            messages: vec![Diagnostic {
                location: None,
                hint: None,
                text: "Error 2".to_string(),
                title: "Error 2".to_string(),
                level: Level::Error,
            }],
        };
        feedback.append_feedback(Feedback {
            diagnostics: HashMap::from([]),
            messages: vec![Diagnostic {
                location: None,
                hint: None,
                text: "Error 3".to_string(),
                title: "Error 3".to_string(),
                level: Level::Error,
            }],
        });
        assert_eq!(
            feedback,
            Feedback {
                diagnostics: HashMap::from([(
                    Utf8PathBuf::from("src/file1.gleam"),
                    vec![Diagnostic {
                        location: None,
                        hint: None,
                        text: "Error 1".to_string(),
                        title: "Error 1".to_string(),
                        level: Level::Error,
                    },],
                ),]),
                messages: vec![
                    Diagnostic {
                        location: None,
                        hint: None,
                        text: "Error 2".to_string(),
                        title: "Error 2".to_string(),
                        level: Level::Error,
                    },
                    Diagnostic {
                        location: None,
                        hint: None,
                        text: "Error 3".to_string(),
                        title: "Error 3".to_string(),
                        level: Level::Error,
                    }
                ],
            }
        );
    }

    #[test]
    fn append_feedback_new_file_blank() {
        let mut feedback = Feedback {
            diagnostics: HashMap::from([(
                Utf8PathBuf::from("src/file1.gleam"),
                vec![Diagnostic {
                    location: None,
                    hint: None,
                    text: "Error 1".to_string(),
                    title: "Error 1".to_string(),
                    level: Level::Error,
                }],
            )]),
            messages: vec![Diagnostic {
                location: None,
                hint: None,
                text: "Error 2".to_string(),
                title: "Error 2".to_string(),
                level: Level::Error,
            }],
        };
        feedback.append_feedback(Feedback {
            diagnostics: HashMap::from([(Utf8PathBuf::from("src/file2.gleam"), vec![])]),
            messages: vec![],
        });
        assert_eq!(
            feedback,
            Feedback {
                diagnostics: HashMap::from([
                    (
                        Utf8PathBuf::from("src/file1.gleam"),
                        vec![Diagnostic {
                            location: None,
                            hint: None,
                            text: "Error 1".to_string(),
                            title: "Error 1".to_string(),
                            level: Level::Error,
                        },],
                    ),
                    (Utf8PathBuf::from("src/file2.gleam"), vec![],),
                ]),
                messages: vec![Diagnostic {
                    location: None,
                    hint: None,
                    text: "Error 2".to_string(),
                    title: "Error 2".to_string(),
                    level: Level::Error,
                },],
            }
        );
    }

    #[test]
    fn append_feedback_existing_file_blank() {
        let mut feedback = Feedback {
            diagnostics: HashMap::from([(
                Utf8PathBuf::from("src/file1.gleam"),
                vec![Diagnostic {
                    location: None,
                    hint: None,
                    text: "Error 1".to_string(),
                    title: "Error 1".to_string(),
                    level: Level::Error,
                }],
            )]),
            messages: vec![Diagnostic {
                location: None,
                hint: None,
                text: "Error 2".to_string(),
                title: "Error 2".to_string(),
                level: Level::Error,
            }],
        };
        feedback.append_feedback(Feedback {
            diagnostics: HashMap::from([(Utf8PathBuf::from("src/file1.gleam"), vec![])]),
            messages: vec![],
        });
        assert_eq!(
            feedback,
            Feedback {
                diagnostics: HashMap::from([(Utf8PathBuf::from("src/file1.gleam"), vec![],),]),
                messages: vec![Diagnostic {
                    location: None,
                    hint: None,
                    text: "Error 2".to_string(),
                    title: "Error 2".to_string(),
                    level: Level::Error,
                },],
            }
        );
    }
}
