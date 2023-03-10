use std::{
    collections::{HashMap, HashSet},
    path::PathBuf,
};

use gleam_core::{diagnostic::Diagnostic, Error, Warning};

#[derive(Debug, Default, PartialEq, Eq)]
pub struct Feedback {
    pub diagnostics: HashMap<PathBuf, Vec<Diagnostic>>,
    pub messages: Vec<Diagnostic>,
}

impl Feedback {
    /// Set the diagnostics for a file to an empty vector. This will overwrite
    /// any existing diagnostics on the client.
    pub fn unset_existing_diagnostics(&mut self, path: PathBuf) {
        _ = self.diagnostics.insert(path, vec![]);
    }

    pub fn append_diagnostic(&mut self, path: PathBuf, diagnostic: Diagnostic) {
        self.diagnostics
            .entry(path)
            .or_insert_with(Vec::new)
            .push(diagnostic);
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
    files_with_diagnostics: HashSet<PathBuf>,
}

impl FeedbackBookKeeper {
    // TODO: test
    /// Send diagnostics for any warnings and remove any diagnostics for files
    /// that have compiled without warnings.
    ///
    pub fn diagnostics(
        &mut self,
        compiled: impl Iterator<Item = PathBuf>,
        warnings: Vec<Warning>,
    ) -> Feedback {
        let mut feedback = Feedback::default();

        // Any existing diagnostics for files that have been compiled are no
        // longer valid so we set an empty vector of diagnostics for the files
        // to erase their diagnostics.
        for path in compiled {
            let has_existing_diagnostics = self.files_with_diagnostics.remove(&path);
            if has_existing_diagnostics {
                feedback.unset_existing_diagnostics(path);
            }
        }

        for warning in warnings {
            self.insert_warning(&mut feedback, warning);
        }

        feedback
    }

    // TODO: test
    /// Compilation failed, boo!
    ///
    /// Send diagnostics for any warnings and remove any diagnostics for files
    /// that have compiled without warnings, AND ALSO send diagnostics for the
    /// error that caused compilation to fail.
    ///
    pub fn diagnostics_with_error(
        &mut self,
        error: Error,
        compiled: impl Iterator<Item = PathBuf>,
        warnings: Vec<Warning>,
    ) -> Feedback {
        let diagnostic = error.to_diagnostic();
        let mut feedback = self.diagnostics(compiled, warnings);

        match diagnostic.location.as_ref().map(|l| l.path.clone()) {
            Some(path) => {
                _ = self.files_with_diagnostics.insert(path.clone());
                feedback.append_diagnostic(path, diagnostic);
            }

            None => {
                feedback.append_message(diagnostic);
            }
        }

        feedback
    }

    fn insert_warning(&mut self, feedback: &mut Feedback, warning: Warning) {
        let diagnostic = warning.to_diagnostic();
        if let Some(path) = diagnostic.location.as_ref().map(|l| l.path.clone()) {
            _ = self.files_with_diagnostics.insert(path.clone());
            feedback.append_diagnostic(path, diagnostic);
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use gleam_core::{
        ast::SrcSpan,
        parse::error::{ParseError, ParseErrorType},
        type_,
    };

    #[test]
    fn feedback() {
        let mut book_keeper = FeedbackBookKeeper::default();
        let file1 = PathBuf::from("src/file1.gleam");
        let file2 = PathBuf::from("src/file2.gleam");
        let file3 = PathBuf::from("src/file2.gleam");

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

        let feedback = book_keeper.diagnostics(
            vec![file1.clone()].into_iter(),
            vec![warning1.clone(), warning1.clone(), warning2.clone()],
        );

        assert_eq!(
            Feedback {
                diagnostics: vec![
                    (
                        file1.clone(),
                        vec![warning1.to_diagnostic(), warning1.to_diagnostic(),]
                    ),
                    (file2.clone(), vec![warning2.to_diagnostic(),])
                ]
                .into_iter()
                .collect(),
                messages: vec![],
            },
            feedback
        );

        let feedback = book_keeper.diagnostics(
            vec![file1.clone(), file2.clone(), file3.clone()].into_iter(),
            vec![],
        );

        assert_eq!(
            Feedback {
                diagnostics: vec![
                    // File 1 and 2 had diagnostics before so they have been unset
                    (file1.clone(), vec![]),
                    (file2.clone(), vec![]),
                    // File 3 had no diagnostics so does not need to to be unset
                ]
                .into_iter()
                .collect(),
                messages: vec![],
            },
            feedback
        );

        // The failed method sets an additional diagnostic or messages for the error

        let locationless_error = Error::Gzip("Hello!".into());

        let feedback = book_keeper.diagnostics_with_error(
            locationless_error.clone(),
            vec![].into_iter(),
            vec![warning1.clone()],
        );

        assert_eq!(
            Feedback {
                diagnostics: vec![(file1.clone(), vec![warning1.to_diagnostic()])]
                    .into_iter()
                    .collect(),
                messages: vec![locationless_error.to_diagnostic()],
            },
            feedback
        );

        let error = Error::Parse {
            path: file3.clone(),
            src: "blah".into(),
            error: ParseError {
                error: ParseErrorType::ConcatPatternVariableLeftHandSide,
                location: SrcSpan::new(1, 4),
            },
        };

        let feedback = book_keeper.diagnostics_with_error(
            error.clone(),
            vec![].into_iter(),
            vec![warning1.clone()],
        );

        assert_eq!(
            Feedback {
                diagnostics: vec![
                    (file1.clone(), vec![warning1.to_diagnostic()]),
                    (file3.clone(), vec![error.to_diagnostic()]),
                ]
                .into_iter()
                .collect(),
                messages: vec![],
            },
            feedback
        );
    }
}
