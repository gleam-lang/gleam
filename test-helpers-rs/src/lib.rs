use camino::{Utf8Path, Utf8PathBuf};
use gleam_core::{
    io::{Content, FileSystemWriter, memory::InMemoryFileSystem},
    version::COMPILER_VERSION,
};
use itertools::Itertools;
use regex::Regex;
use std::{collections::HashMap, fmt::Write, sync::LazyLock};

#[derive(Debug)]
pub struct TestCompileOutput {
    pub files: HashMap<Utf8PathBuf, Content>,
    pub warnings: Vec<gleam_core::Warning>,
}

impl TestCompileOutput {
    pub fn as_overview_text(&self) -> String {
        let mut buffer = String::new();
        for (path, content) in self.files.iter().sorted_by(|a, b| a.0.cmp(b.0)) {
            let normalised_path = if path.as_str().contains("cases") {
                path.as_str()
                    .split("cases")
                    .skip(1)
                    .collect::<String>()
                    .as_str()
                    .replace('\\', "/")
                    .split('/')
                    .skip(1)
                    .join("/")
            } else {
                path.as_str().replace('\\', "/")
            };
            buffer.push_str("//// ");
            buffer.push_str(&normalised_path);
            buffer.push('\n');

            let extension = path.extension();

            match content {
                _ if extension == Some("cache") => buffer.push_str("<.cache binary>"),
                Content::Binary(data) => write!(buffer, "<{} byte binary>", data.len()).unwrap(),

                Content::Text(_) if normalised_path.ends_with("@@main.erl") => {
                    write!(buffer, "<erlang entrypoint>").unwrap()
                }

                Content::Text(text) => {
                    let format_path = |caps: &regex::Captures| {
                        caps.get(1)
                            .expect("file path")
                            .as_str()
                            .replace("\\\\", "/")
                    };
                    let text = FILE_LINE_REGEX.replace_all(text, |caps: &regex::Captures| {
                        let path = format_path(caps);
                        let line_number = caps.get(2).expect("line number").as_str();
                        format!("-file(\"{path}\", {line_number}).")
                    });
                    let text = FILEPATH_MACRO_REGEX
                        .replace_all(text.to_string().as_str(), |caps: &regex::Captures| {
                            let path = format_path(caps);
                            format!("-define(FILEPATH, \"{path}\").")
                        })
                        .replace(COMPILER_VERSION, "<gleam compiler version string>");
                    buffer.push_str(&text)
                }
            };
            buffer.push('\n');
            buffer.push('\n');
        }

        for warning in self.warnings.iter().map(|w| w.to_pretty_string()).sorted() {
            write!(buffer, "//// Warning\n{}", normalise_diagnostic(&warning)).unwrap();
            buffer.push('\n');
            buffer.push('\n');
        }

        buffer
    }
}

pub fn to_in_memory_filesystem(path: &Utf8Path) -> InMemoryFileSystem {
    let fs = InMemoryFileSystem::new();

    let files = walkdir::WalkDir::new(path)
        .follow_links(true)
        .into_iter()
        .filter_map(Result::ok)
        .filter(|entry| entry.file_type().is_file())
        .map(|entry| entry.into_path());

    for fullpath in files {
        let content = std::fs::read(&fullpath).unwrap();
        let path = fullpath.strip_prefix(path).unwrap();
        fs.write_bytes(Utf8Path::from_path(path).unwrap(), &content)
            .unwrap();
    }

    fs
}

static FILE_LINE_REGEX: LazyLock<Regex> =
    LazyLock::new(|| Regex::new(r#"-file\("([^"]+)", (\d+)\)\."#).expect("Invalid regex"));

static FILEPATH_MACRO_REGEX: LazyLock<Regex> =
    LazyLock::new(|| Regex::new(r#"-define\(FILEPATH, "([^"]+)"\)\."#).expect("Invalid regex"));

pub fn normalise_diagnostic(text: &str) -> String {
    // There is an extra ^ on Windows in some error messages' code
    // snippets.
    // I've not managed to determine why this is yet (it is especially
    // tricky without a Windows computer) so for now we just squash them
    // in these cross-platform tests.
    Regex::new(r"\^+")
        .expect("^ sequence regex")
        .replace_all(text, "^")
        .replace('\\', "/")
}
