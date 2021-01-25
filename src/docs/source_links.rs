use crate::{
    ast::SrcSpan,
    config::{PackageConfig, Repository},
    project::Analysed,
};
use std::path::Path;

pub struct SourceLinker {
    line_starts: Vec<usize>,
    url_pattern: Option<(String, String)>,
}

impl SourceLinker {
    pub fn new(
        project_root: impl AsRef<Path>,
        project_config: &PackageConfig,
        module: &Analysed,
    ) -> Self {
        let path_in_repo = get_path_in_repo(project_root, &module.path);

        let url_pattern = match &project_config.repository {
            Repository::GitHub { user, repo } => Some((
                format!(
                    "https://github.com/{}/{}/blob/v{}/{}#L",
                    user, repo, project_config.version, path_in_repo
                ),
                "-L".to_string(),
            )),
            Repository::GitLab { user, repo } => Some((
                format!(
                    "https://gitlab.com/{}/{}/-/blob/v{}/{}#L",
                    user, repo, project_config.version, path_in_repo
                ),
                "-".to_string(),
            )),
            Repository::BitBucket { user, repo } => Some((
                format!(
                    "https://bitbucket.com/{}/{}/src/v{}/{}#lines-",
                    user, repo, project_config.version, path_in_repo
                ),
                ":".to_string(),
            )),
            Repository::Custom { .. } | Repository::None => None,
        };

        SourceLinker {
            line_starts: if url_pattern.is_some() {
                line_starts(&module.src)
            } else {
                vec![]
            },
            url_pattern,
        }
    }
    pub fn url(&self, span: &SrcSpan) -> String {
        match &self.url_pattern {
            Some((base, line_sep)) => {
                let start_line = line_no(&self.line_starts, span.start);
                let end_line = line_no(&self.line_starts, span.end);
                format!("{}{}{}{}", base, start_line, line_sep, end_line)
            }

            None => "".to_string(),
        }
    }
}

// get all the line start byte offsets for a str
fn line_starts(src: &str) -> Vec<usize> {
    std::iter::once(0)
        .chain(src.match_indices('\n').map(|(i, _)| i + 1))
        .collect()
}

// get the line number for a byte index
fn line_no(line_starts: &[usize], byte_index: usize) -> usize {
    line_starts
        .binary_search(&byte_index)
        .unwrap_or_else(|next_line| next_line - 1)
        + 1
}

fn get_path_in_repo(project_root: impl AsRef<Path>, path: &Path) -> String {
    path.strip_prefix(&project_root)
        .ok()
        .and_then(Path::to_str)
        .unwrap_or("")
        .to_string()
}
