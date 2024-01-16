use crate::{
    ast::SrcSpan,
    build,
    config::{PackageConfig, Repository},
    line_numbers::LineNumbers,
    paths::ProjectPaths,
};

use camino::{Utf8Component, Utf8Path};

pub struct SourceLinker {
    line_numbers: LineNumbers,
    url_pattern: Option<(String, String)>,
}

impl SourceLinker {
    pub fn new(
        paths: &ProjectPaths,
        project_config: &PackageConfig,
        module: &build::Module,
    ) -> Self {
        let path = paths
            .src_directory()
            .join(module.name.as_str())
            .strip_prefix(paths.root())
            .expect("path is not in root")
            .with_extension("gleam");
        let path_in_repo = to_url_path(&path).unwrap_or_default();

        let url_pattern = match &project_config.repository {
            Repository::GitHub { user, repo } => Some((
                format!(
                    "https://github.com/{}/{}/blob/v{}/{}#L",
                    user, repo, project_config.version, path_in_repo
                ),
                "-L".into(),
            )),
            Repository::GitLab { user, repo } => Some((
                format!(
                    "https://gitlab.com/{}/{}/-/blob/v{}/{}#L",
                    user, repo, project_config.version, path_in_repo
                ),
                "-".into(),
            )),
            Repository::BitBucket { user, repo } => Some((
                format!(
                    "https://bitbucket.com/{}/{}/src/v{}/{}#lines-",
                    user, repo, project_config.version, path_in_repo
                ),
                ":".into(),
            )),
            Repository::CodeBerg { user, repo } => Some((
                format!(
                    "https://codeberg.org/{}/{}/src/tag/{}/{}#L",
                    user, repo, project_config.version, path_in_repo
                ),
                "-".into(),
            )),
            Repository::SourceHut { user, repo } => Some((
                format!(
                    "https://git.sr.ht/~{}/{}/tree/{}/item/{}#L",
                    user, repo, project_config.version, path_in_repo
                ),
                "-".into(),
            )),
            Repository::Gitea { user, repo, host } => Some((
                format!(
                    "{host}/{user}/{repo}/src/tag/{}/{}#L",
                    project_config.version, path_in_repo
                ),
                "-".into(),
            )),
            Repository::Custom { .. } | Repository::None => None,
        };

        SourceLinker {
            line_numbers: LineNumbers::new(&module.code),
            url_pattern,
        }
    }
    pub fn url(&self, span: SrcSpan) -> String {
        match &self.url_pattern {
            Some((base, line_sep)) => {
                let start_line = self.line_numbers.line_number(span.start);
                let end_line = self.line_numbers.line_number(span.end);
                format!("{base}{start_line}{line_sep}{end_line}")
            }

            None => "".into(),
        }
    }
}

fn to_url_path(path: &Utf8Path) -> Option<String> {
    let mut buf = String::new();
    for c in path.components() {
        if let Utf8Component::Normal(s) = c {
            buf.push_str(s);
        }
        buf.push('/');
    }

    let _ = buf.pop();

    Some(buf)
}
