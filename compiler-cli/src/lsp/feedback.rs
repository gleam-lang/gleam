use std::{
    collections::{HashMap, HashSet},
    path::PathBuf,
};

use gleam_core::{diagnostic::Diagnostic, Error, Warning};

#[derive(Debug, Default)]
pub struct Feedback {
    pub diagnostics: HashMap<PathBuf, Vec<Diagnostic>>,
    pub notifications: Vec<Diagnostic>,
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

    fn append_notification(&mut self, diagnostic: Diagnostic) {
        self.notifications.push(diagnostic);
    }
}

/// When an operation succeeds or fails we want to send diagnostics and
/// notifications to the client for displaying to the user. This object converts
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
    /// Compilation succeeded, hooray!
    ///
    /// Send diagnostics for any warnings and remove any diagnostics for files
    /// that have compiled without warnings.
    ///
    pub fn succeeded(&mut self, compiled: Vec<PathBuf>, warnings: Vec<Warning>) -> Feedback {
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
    pub fn failed(
        &mut self,
        error: Error,
        compiled: Vec<PathBuf>,
        warnings: Vec<Warning>,
    ) -> Feedback {
        let diagnostic = error.to_diagnostic();
        let mut feedback = self.succeeded(compiled, warnings);

        match diagnostic.location.as_ref().map(|l| l.path.clone()) {
            Some(path) => {
                _ = self.files_with_diagnostics.insert(path.clone());
                feedback.append_diagnostic(path, diagnostic);
            }

            None => {
                feedback.append_notification(diagnostic);
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
