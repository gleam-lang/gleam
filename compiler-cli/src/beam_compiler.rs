// SPDX-License-Identifier: Apache-2.0
// SPDX-FileCopyrightText: 2024 The Gleam contributors

use gleam_core::{
    Result,
    error::{Error, ShellCommandFailureReason},
    io::{FileSystemWriter, Stdio},
    paths,
};

use crate::fs::get_os;

use std::{
    collections::HashSet,
    io::{self, BufRead, BufReader, Write},
    process::{Child, ChildStdin, ChildStdout},
};

use camino::{Utf8Path, Utf8PathBuf};
use itertools::Itertools;

#[derive(Debug)]
pub struct BeamCompilerInstance {
    process: Child,
    stdin: Option<ChildStdin>,
    stdout: BufReader<ChildStdout>,
    // A guard held for cleaning up the temporary file used to start the BEAM instance.
    _source: tempfile::NamedTempFile,
}

impl BeamCompilerInstance {
    pub fn compile(
        &mut self,
        out: &Utf8Path,
        lib: &Utf8Path,
        modules: &HashSet<Utf8PathBuf>,
        stdio: Stdio,
    ) -> Result<Vec<String>, Error> {
        // Check that the BEAM instance is still alive before attempting to use it.
        let exit_status = self
            .process
            .try_wait()
            .expect("access BEAM instance exit state");
        if let Some(status) = exit_status {
            panic!("BEAM compiler instance exited: {status}");
        }

        // Prepare work to send to the BEAM instance.
        let args = format!(
            "{{\"{}\", \"{}\", [\"{}\"]}}",
            escape_path(lib),
            escape_path(out.join("ebin")),
            modules
                .iter()
                .map(|module| escape_path(out.join(paths::ARTEFACT_DIRECTORY_NAME).join(module)))
                .join("\", \"")
        );

        tracing::debug!(args=?args, "call_beam_compiler");

        writeln!(self.stdin.as_ref().expect("stdin present"), "{args}.").map_err(|e| {
            Error::ShellCommand {
                program: "escript".into(),
                reason: ShellCommandFailureReason::IoError(e.kind()),
            }
        })?;

        let mut buf = String::new();
        let mut accumulated_modules: Vec<String> = Vec::new();
        while let (Ok(_), Ok(None)) = (self.stdout.read_line(&mut buf), self.process.try_wait()) {
            match buf.trim() {
                "gleam-compile-result-ok" => {
                    // Return Ok with the accumulated modules
                    return Ok(accumulated_modules);
                }
                "gleam-compile-result-error" => {
                    return Err(Error::ShellCommand {
                        program: "escript".into(),
                        reason: ShellCommandFailureReason::Unknown,
                    });
                }
                s if s.starts_with("gleam-compile-module:") => {
                    if let Some(module_content) = s.strip_prefix("gleam-compile-module:") {
                        accumulated_modules.push(module_content.to_string());
                    }
                }
                _ => match stdio {
                    Stdio::Inherit => print!("{buf}"),
                    Stdio::Null => {}
                },
            }

            buf.clear();
        }

        // if we get here, stdout got closed before we got an "ok" or "err".
        Err(Error::ShellCommand {
            program: "escript".into(),
            reason: ShellCommandFailureReason::Unknown,
        })
    }

    pub fn new<IO: FileSystemWriter>(io: &IO) -> Result<Self, Error> {
        let escript_source = std::include_str!("../templates/gleam@@compile.erl");
        let escript_file =
            tempfile::NamedTempFile::new().map_err(|e| Error::CouldNotCreateTempFile {
                error: e.to_string(),
            })?;
        let escript_path =
            Utf8PathBuf::from_path_buf(escript_file.path().to_path_buf()).expect("UTF8 temp");

        io.write(&escript_path, escript_source)?;

        tracing::trace!(escript_path=?escript_path, "spawn_beam_compiler");

        let mut process = std::process::Command::new("escript")
            .arg(escript_path)
            .stdin(std::process::Stdio::piped())
            .stdout(std::process::Stdio::piped())
            .spawn()
            .map_err(|e| match e.kind() {
                io::ErrorKind::NotFound => Error::ShellProgramNotFound {
                    program: "escript".into(),
                    os: get_os(),
                },
                other => Error::ShellCommand {
                    program: "escript".into(),
                    reason: ShellCommandFailureReason::IoError(other),
                },
            })?;

        let stdin = process.stdin.take().expect("could not get child stdin");
        let stdout = process.stdout.take().expect("could not get child stdout");

        Ok(Self {
            process,
            stdin: Some(stdin),
            stdout: BufReader::new(stdout),
            _source: escript_file,
        })
    }
}

impl Drop for BeamCompilerInstance {
    fn drop(&mut self) {
        // closing stdin will cause the BEAM instance to exit.
        drop(self.stdin.take());
        let _ = self.process.wait();
    }
}

fn escape_path<T: AsRef<str>>(path: T) -> String {
    path.as_ref().replace('\\', "\\\\").replace('"', "\\\"")
}

#[cfg(test)]
mod tests {
    use super::escape_path;

    #[test]
    fn escape_path_plain() {
        assert_eq!(
            escape_path("/build/packages/wibble/src/bar.erl"),
            "/build/packages/wibble/src/bar.erl"
        );
    }

    #[test]
    fn escape_path_backslash() {
        assert_eq!(
            escape_path("C:\\Users\\wibble\\bar.erl"),
            "C:\\\\Users\\\\wibble\\\\bar.erl"
        );
    }

    #[test]
    fn escape_path_double_quote() {
        assert_eq!(
            escape_path(r#"a", "/tmp/other.erl"#),
            r#"a\", \"/tmp/other.erl"#
        );
    }

    #[test]
    fn escape_path_double_quote_does_not_break_erlang_string() {
        // A double-quote in a path must not close the surrounding Erlang string
        // literal and produce extra elements in the module list.
        let path = r#"a", "/tmp/other"#;
        let escaped = escape_path(path);
        assert!(
            !escaped.contains("\", \""),
            "unescaped quote still present: {escaped}"
        );
    }
}
