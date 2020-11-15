use crate::{
    ast::SrcSpan,
    config::{PackageConfig, Repository},
};
use codespan_reporting::files::Files;
use std::path::{Path, PathBuf};

pub trait SourceLinks {
    fn url(&self, location: &SrcSpan) -> Option<String>;
}

// Returns None for the urls which we can ignore in the template
struct NoSourceLinks {}

impl SourceLinks for NoSourceLinks {
    fn url(&self, _location: &SrcSpan) -> Option<String> {
        None
    }
}

// Creates links to the GitHub repository with the #LN-LM anchor syntax for linking directly to a
// range of line numbers
struct GitHubSourceLinks {
    codespan_file: codespan_reporting::files::SimpleFile<String, String>,
    url: String,
    version: String,
    relative_path: String,
}

impl SourceLinks for GitHubSourceLinks {
    fn url(&self, location: &SrcSpan) -> Option<String> {
        let start_line = self
            .codespan_file
            .line_index((), location.start)
            .unwrap_or_default()
            + 1;
        let end_line = self
            .codespan_file
            .line_index((), location.end)
            .unwrap_or_default()
            + 1;
        Some(format!(
            "{}/blob/{}/{}#L{}-L{}",
            &self.url, &self.version, &self.relative_path, start_line, end_line,
        ))
    }
}

pub fn build(
    project_root: impl AsRef<Path>,
    project_config: &PackageConfig,
    module_path: &PathBuf,
    name: &String,
) -> Box<dyn SourceLinks> {
    match (&project_config.version, &project_config.repository) {
        (Some(version), Repository::GitHub { user, repo }) => {
            let relative_path = module_path
                .strip_prefix(&project_root)
                .map(|path| path.to_str())
                .unwrap_or_default()
                .unwrap_or_default();

            let src = std::fs::read_to_string(&module_path).unwrap_or_default();
            let codespan_file = codespan_reporting::files::SimpleFile::new(name.clone(), src);

            Box::new(GitHubSourceLinks {
                codespan_file,
                relative_path: relative_path.to_string(),
                url: format!("https://github.com/{}/{}", &user, &repo),
                version: version.clone(),
            })
        }
        // If we either don't have a version or aren't dealing with a GitHub repository then we
        // can't generate source code links
        _ => Box::new(NoSourceLinks {}),
    }
}
