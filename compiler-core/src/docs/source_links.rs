use crate::{
    ast::SrcSpan,
    build,
    config::{PackageConfig, Repository},
    line_numbers::LineNumbers,
    paths::ProjectPaths,
};

use camino::{Utf8Component, Utf8Path, Utf8PathBuf};

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

        let path_in_repo = match project_config
            .repository
            .as_ref()
            .map(|r| r.path())
            .unwrap_or_default()
        {
            Some(repo_path) => to_url_path(&Utf8PathBuf::from(repo_path).join(path)),
            _ => to_url_path(&path),
        }
        .unwrap_or_default();

        let tag = project_config.tag_for_version(&project_config.version);

        let url_pattern = project_config
            .repository
            .as_ref()
            .map(|r| match r {
                Repository::GitHub { user, repo, .. } => Some((
                    format!("https://github.com/{user}/{repo}/blob/{tag}/{path_in_repo}#L"),
                    "-L".into(),
                )),
                Repository::GitLab { user, repo, .. } => Some((
                    format!("https://gitlab.com/{user}/{repo}/-/blob/{tag}/{path_in_repo}#L"),
                    "-".into(),
                )),
                Repository::BitBucket { user, repo, .. } => Some((
                    format!("https://bitbucket.com/{user}/{repo}/src/{tag}/{path_in_repo}#lines-"),
                    ":".into(),
                )),
                Repository::Codeberg { user, repo, .. } => Some((
                    format!("https://codeberg.org/{user}/{repo}/src/tag/{tag}/{path_in_repo}#L"),
                    "-".into(),
                )),
                Repository::SourceHut { user, repo, .. } => Some((
                    format!("https://git.sr.ht/~{user}/{repo}/tree/{tag}/item/{path_in_repo}#L"),
                    "-".into(),
                )),
                Repository::Gitea {
                    user, repo, host, ..
                }
                | Repository::Forgejo {
                    user, repo, host, ..
                } => {
                    let string_host = host.to_string();
                    let cleaned_host = string_host.trim_end_matches('/');
                    Some((
                        format!("{cleaned_host}/{user}/{repo}/src/tag/{tag}/{path_in_repo}#L",),
                        "-L".into(),
                    ))
                }
                Repository::Custom { .. } => None,
            })
            .unwrap_or_default();

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
                if start_line == end_line {
                    format!("{base}{start_line}")
                } else {
                    format!("{base}{start_line}{line_sep}{end_line}")
                }
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
