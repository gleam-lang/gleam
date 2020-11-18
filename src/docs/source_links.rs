use crate::{
    ast::SrcSpan,
    config::{PackageConfig, Repository},
    project::Analysed,
};
use codespan_reporting::files::Files;
use std::path::Path;

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
    user: String,
    repo: String,
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
            "https://github.com/{}/{}/blob/{}/{}#L{}-L{}",
            &self.user, &self.repo, &self.version, &self.relative_path, start_line, end_line,
        ))
    }
}

// Creates links to the GitLab repository with the #LN-M anchor syntax for linking directly to a
// range of line numbers
struct GitLabSourceLinks {
    codespan_file: codespan_reporting::files::SimpleFile<String, String>,
    user: String,
    repo: String,
    version: String,
    relative_path: String,
}

impl SourceLinks for GitLabSourceLinks {
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
            "https://gitlab.com/{}/{}/-/blob/{}/{}#L{}-{}",
            &self.user, &self.repo, &self.version, &self.relative_path, start_line, end_line,
        ))
    }
}

// Creates links to the BitBucket repository with the #lines-M:N anchor syntax for linking directly
// to a range of line numbers
struct BitBucketSourceLinks {
    codespan_file: codespan_reporting::files::SimpleFile<String, String>,
    user: String,
    repo: String,
    version: String,
    relative_path: String,
}

impl SourceLinks for BitBucketSourceLinks {
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
            "https://bitbucket.com/{}/{}/src/{}/{}#lines-{}:{}",
            &self.user, &self.repo, &self.version, &self.relative_path, start_line, end_line,
        ))
    }
}

pub fn build(
    project_root: impl AsRef<Path>,
    project_config: &PackageConfig,
    module: &Analysed,
) -> Box<dyn SourceLinks> {
    match &project_config.repository {
        Repository::GitHub { user, repo } => {
            let relative_path = module
                .path
                .strip_prefix(&project_root)
                .map(|path| path.to_str())
                .unwrap_or_default()
                .unwrap_or_default();

            let codespan_file = codespan_reporting::files::SimpleFile::new(
                module.name.join("/"),
                module.src.clone(),
            );

            Box::new(GitHubSourceLinks {
                codespan_file,
                relative_path: relative_path.to_string(),
                user: user.clone(),
                repo: repo.clone(),
                version: project_config.version.clone(),
            })
        }
        Repository::GitLab { user, repo } => {
            let relative_path = module
                .path
                .strip_prefix(&project_root)
                .map(|path| path.to_str())
                .unwrap_or_default()
                .unwrap_or_default();

            let codespan_file = codespan_reporting::files::SimpleFile::new(
                module.name.join("/"),
                module.src.clone(),
            );

            Box::new(GitLabSourceLinks {
                codespan_file,
                relative_path: relative_path.to_string(),
                user: user.clone(),
                repo: repo.clone(),
                version: project_config.version.clone(),
            })
        }
        Repository::BitBucket { user, repo } => {
            let relative_path = module
                .path
                .strip_prefix(&project_root)
                .map(|path| path.to_str())
                .unwrap_or_default()
                .unwrap_or_default();

            let codespan_file = codespan_reporting::files::SimpleFile::new(
                module.name.join("/"),
                module.src.clone(),
            );

            Box::new(BitBucketSourceLinks {
                codespan_file,
                relative_path: relative_path.to_string(),
                user: user.clone(),
                repo: repo.clone(),
                version: project_config.version.clone(),
            })
        }
        // If we either don't have a version or aren't dealing with a GitHub repository then we
        // can't generate source code links
        _ => Box::new(NoSourceLinks {}),
    }
}
