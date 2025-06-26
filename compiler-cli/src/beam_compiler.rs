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
struct BeamCompilerInner {
    process: Child,
    stdin: ChildStdin,
    stdout: BufReader<ChildStdout>,
}

#[derive(Debug, Default)]
pub struct BeamCompiler {
    inner: Option<BeamCompilerInner>,
}

impl BeamCompiler {
    pub fn compile<IO: FileSystemWriter>(
        &mut self,
        io: &IO,
        out: &Utf8Path,
        lib: &Utf8Path,
        modules: &HashSet<Utf8PathBuf>,
        stdio: Stdio,
    ) -> Result<Vec<String>, Error> {
        let inner = match self.inner {
            Some(ref mut inner) => match inner.process.try_wait() {
                Ok(None) => inner,
                _ => self.inner.insert(self.spawn(io, out)?),
            },

            None => self.inner.insert(self.spawn(io, out)?),
        };

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

        writeln!(inner.stdin, "{args}.").map_err(|e| Error::ShellCommand {
            program: "escript".into(),
            reason: ShellCommandFailureReason::IoError(e.kind()),
        })?;

        let mut buf = String::new();
        let mut accumulated_modules: Vec<String> = Vec::new();
        while let (Ok(_), Ok(None)) = (inner.stdout.read_line(&mut buf), inner.process.try_wait()) {
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

            buf.clear()
        }

        // if we get here, stdout got closed before we got an "ok" or "err".
        Err(Error::ShellCommand {
            program: "escript".into(),
            reason: ShellCommandFailureReason::Unknown,
        })
    }

    fn spawn<IO: FileSystemWriter>(
        &self,
        io: &IO,
        out: &Utf8Path,
    ) -> Result<BeamCompilerInner, Error> {
        let escript_path = out
            .join(paths::ARTEFACT_DIRECTORY_NAME)
            .join("gleam@@compile.erl");

        let escript_source = std::include_str!("../templates/gleam@@compile.erl");
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

        Ok(BeamCompilerInner {
            process,
            stdin,
            stdout: BufReader::new(stdout),
        })
    }
}

impl Drop for BeamCompiler {
    fn drop(&mut self) {
        if let Some(mut inner) = self.inner.take() {
            // closing stdin will cause the erlang process to exit.
            drop(inner.stdin);
            let _ = inner.process.wait();
        }
    }
}

fn escape_path<T: AsRef<str>>(path: T) -> String {
    path.as_ref().replace("\\", "\\\\")
}
