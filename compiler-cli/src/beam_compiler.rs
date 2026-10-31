// SPDX-License-Identifier: Apache-2.0
// SPDX-FileCopyrightText: 2024 The Gleam contributors

use gleam_core::{
    Result,
    error::{Error, ShellCommandFailureReason},
    io::{CompileUnit, FileSystemWriter, Stdio},
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
    pub otp_version: u64,
    // A guard held for cleaning up the temporary file used to start the BEAM instance.
    _source: tempfile::NamedTempFile,
}

impl BeamCompilerInstance {
    pub fn compile(
        &mut self,
        out: &Utf8Path,
        lib: &Utf8Path,
        modules: &HashSet<CompileUnit>,
        stdio: Stdio,
    ) -> Result<(), Error> {
        // Check that the BEAM instance is still alive before attempting to use it.
        let exit_status = self
            .process
            .try_wait()
            .expect("access BEAM instance exit state");
        if let Some(status) = exit_status {
            panic!("BEAM compiler instance exited: {status}");
        }

        // Prepare work to send to the BEAM instance.
        let args = compile_request(lib, out, modules);

        tracing::debug!(args=?args, "call_beam_compiler");

        writeln!(self.stdin.as_ref().expect("stdin present"), "{args}.").map_err(|error| {
            Error::ShellCommand {
                program: "escript".into(),
                reason: ShellCommandFailureReason::IoError(error.kind()),
            }
        })?;

        let mut buf = String::new();
        while let (Ok(_), Ok(None)) = (self.stdout.read_line(&mut buf), self.process.try_wait()) {
            match buf.trim() {
                "gleam-compile-result-ok" => {
                    return Ok(());
                }
                "gleam-compile-result-error" => {
                    return Err(Error::ShellCommand {
                        program: "escript".into(),
                        reason: ShellCommandFailureReason::Unknown,
                    });
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
            tempfile::NamedTempFile::new().map_err(|error| Error::CouldNotCreateTempFile {
                error: error.to_string(),
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
            .map_err(|error| match error.kind() {
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
        let mut stdout = BufReader::new(stdout);

        let mut line = String::new();
        let otp_version = loop {
            line.clear();
            let bytes_read = stdout
                .read_line(&mut line)
                .map_err(|e| Error::ShellCommand {
                    program: "escript".into(),
                    reason: ShellCommandFailureReason::IoError(e.kind()),
                })?;
            if bytes_read == 0 {
                return Err(Error::ShellCommand {
                    program: "escript".into(),
                    reason: ShellCommandFailureReason::Unknown,
                });
            }

            match line.trim().strip_prefix("gleam-otp-version:") {
                Some(version) => {
                    break version
                        .parse()
                        .expect("BEAM compiler should report OTP version as an integer");
                }
                None => print!("{line}"),
            }
        };

        Ok(Self {
            process,
            stdin: Some(stdin),
            stdout,
            otp_version,
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

/// Builds the Erlang term sent to the BEAM instance for one compilation.
fn compile_request(lib: &Utf8Path, out: &Utf8Path, modules: &HashSet<CompileUnit>) -> String {
    let modules = modules
        .iter()
        .map(|module| match module {
            CompileUnit::Native(path) => {
                let path = escape_path(out.join(paths::ARTEFACT_DIRECTORY_NAME).join(path));
                format!("\"{path}\"")
            }
            CompileUnit::Gleam { path, source } => {
                let path = escape_path(out.join(paths::ARTEFACT_DIRECTORY_NAME).join(path));
                format!("{{\"{path}\", \"{}\"}}", escape_path(source))
            }
        })
        .join(", ");
    format!(
        "{{\"{}\", \"{}\", [{modules}]}}",
        escape_path(lib),
        escape_path(out.join("ebin")),
    )
}

fn escape_path<T: AsRef<str>>(path: T) -> String {
    path.as_ref().replace('\\', "\\\\").replace('"', "\\\"")
}

#[cfg(test)]
mod tests {
    use super::{compile_request, escape_path};
    use camino::{Utf8Path, Utf8PathBuf};
    use gleam_core::io::CompileUnit;
    use std::collections::HashSet;

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

    #[test]
    fn compile_request_sends_gleam_source() {
        let out = Utf8Path::new("/out");
        let modules = HashSet::from([CompileUnit::Gleam {
            path: Utf8PathBuf::from("app.abstr"),
            source: Utf8PathBuf::from(r#"/a "b"/src/app.gleam"#),
        }]);
        assert_eq!(
            compile_request(Utf8Path::new("/lib"), out, &modules),
            format!(
                r#"{{"/lib", "{}", [{{"{}", "/a \"b\"/src/app.gleam"}}]}}"#,
                escape_path(out.join("ebin")),
                escape_path(out.join("_gleam_artefacts").join("app.abstr")),
            )
        );
    }

    #[test]
    fn compile_request_sends_native_path_only() {
        let out = Utf8Path::new("/out");
        let modules = HashSet::from([CompileUnit::Native(Utf8PathBuf::from("ffi.erl"))]);
        assert_eq!(
            compile_request(Utf8Path::new("/lib"), out, &modules),
            format!(
                r#"{{"/lib", "{}", ["{}"]}}"#,
                escape_path(out.join("ebin")),
                escape_path(out.join("_gleam_artefacts").join("ffi.erl")),
            )
        );
    }
}
