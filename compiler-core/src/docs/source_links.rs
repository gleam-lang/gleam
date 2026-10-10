// SPDX-License-Identifier: Apache-2.0
// SPDX-FileCopyrightText: 2020 The Gleam contributors
use crate::{
    build,
    config::{PackageConfig, Repository},
    paths::ProjectPaths,
};
use src_span::{LineNumbers, SrcSpan};

use camino::{Utf8Component, Utf8Path, Utf8PathBuf};

pub struct SourceLinker {
    line_numbers: LineNumbers,
    url_base: String,
    line_separator: Option<&'static str>,
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

        let (url_base, line_separator) = match project_config.repository.as_ref() {
            Some(Repository::GitHub { user, repo, .. }) => (
                format!("https://github.com/{user}/{repo}/blob/{tag}/{path_in_repo}#L"),
                Some("-L"),
            ),
            Some(Repository::GitLab { user, repo, .. }) => (
                format!("https://gitlab.com/{user}/{repo}/-/blob/{tag}/{path_in_repo}#L"),
                Some("-"),
            ),
            Some(Repository::BitBucket { user, repo, .. }) => (
                format!("https://bitbucket.com/{user}/{repo}/src/{tag}/{path_in_repo}#lines-"),
                Some(":"),
            ),
            Some(Repository::Codeberg { user, repo, .. }) => (
                format!("https://codeberg.org/{user}/{repo}/src/tag/{tag}/{path_in_repo}#L"),
                Some("-"),
            ),
            Some(Repository::SourceHut { user, repo, .. }) => (
                format!("https://git.sr.ht/~{user}/{repo}/tree/{tag}/item/{path_in_repo}#L"),
                Some("-"),
            ),
            Some(Repository::Tangled { user, repo, .. }) => (
                format!("https://tangled.org/{user}/{repo}/blob/{tag}/{path_in_repo}#L"),
                Some("-"),
            ),
            Some(
                Repository::Gitea {
                    user, repo, host, ..
                }
                | Repository::Forgejo {
                    user, repo, host, ..
                },
            ) => {
                let string_host = host.to_string();
                let cleaned_host = string_host.trim_end_matches('/');
                (
                    format!("{cleaned_host}/{user}/{repo}/src/tag/{tag}/{path_in_repo}#L"),
                    Some("-L"),
                )
            }
            Some(Repository::Custom { .. }) | None => (
                format!(
                    "https://hex.pm/packages/{}/{}/files/{path_in_repo}#L",
                    project_config.name, project_config.version,
                ),
                None,
            ),
        };

        SourceLinker {
            line_numbers: LineNumbers::new(&module.code),
            url_base,
            line_separator,
        }
    }

    pub fn url(&self, span: SrcSpan) -> String {
        let start_line = self.line_numbers.line_number(span.start);
        let end_line = self.line_numbers.line_number(span.end);
        match self.line_separator {
            Some(separator) if start_line != end_line => {
                format!("{}{start_line}{separator}{end_line}", self.url_base)
            }
            _ => format!("{}{start_line}", self.url_base),
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
