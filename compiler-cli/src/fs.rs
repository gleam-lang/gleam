use gleam_core::{
    error::{Error, FileIoAction, FileKind},
    io::{
        CommandExecutor, DirEntry, FileSystemIO, FileSystemWriter, OutputFile, ReadDir,
        WrappedReader, WrappedWriter,
    },
    Result,
};
use lazy_static::lazy_static;
use std::{
    ffi::OsStr,
    fmt::Debug,
    fs::File,
    io::{self, BufRead, BufReader, Write},
    path::{Path, PathBuf},
    process::Stdio,
};

/// A `FileWriter` implementation that writes to the file system.
#[derive(Debug, Clone, Copy)]
pub struct ProjectIO;

impl ProjectIO {
    pub fn new() -> Self {
        Self
    }

    pub fn boxed() -> Box<Self> {
        Box::new(Self::new())
    }
}

impl gleam_core::io::FileSystemReader for ProjectIO {
    fn gleam_source_files(&self, dir: &Path) -> Box<dyn Iterator<Item = PathBuf>> {
        Box::new({
            let dir = dir.to_path_buf();
            walkdir::WalkDir::new(dir.clone())
                .follow_links(true)
                .into_iter()
                .filter_map(Result::ok)
                .filter(|e| e.file_type().is_file())
                .map(|d| d.into_path())
                .filter(move |d| is_gleam_path(d, dir.clone()))
        })
    }

    fn gleam_metadata_files(&self, dir: &Path) -> Box<dyn Iterator<Item = PathBuf>> {
        Box::new({
            let dir = dir.to_path_buf();
            walkdir::WalkDir::new(dir)
                .follow_links(true)
                .into_iter()
                .filter_map(Result::ok)
                .filter(|e| e.file_type().is_file())
                .map(|d| d.into_path())
                .filter(|p| p.extension().and_then(OsStr::to_str) == Some("gleam_module"))
        })
    }

    fn read(&self, path: &Path) -> Result<String, Error> {
        read(path)
    }

    fn is_file(&self, path: &Path) -> bool {
        path.is_file()
    }

    fn is_directory(&self, path: &Path) -> bool {
        path.is_dir()
    }

    fn reader(&self, path: &Path) -> Result<WrappedReader, Error> {
        reader(path)
    }

    fn read_dir(&self, path: &Path) -> Result<ReadDir> {
        read_dir(path).map(|entries| {
            entries
                .map(|result| result.map(|entry| DirEntry::from_path(entry.path())))
                .collect()
        })
    }
}

impl FileSystemWriter for ProjectIO {
    fn writer(&self, path: &Path) -> Result<WrappedWriter, Error> {
        writer(path)
    }

    fn delete(&self, path: &Path) -> Result<()> {
        delete_dir(path)
    }

    fn copy(&self, from: &Path, to: &Path) -> Result<()> {
        copy(from, to)
    }

    fn copy_dir(&self, from: &Path, to: &Path) -> Result<()> {
        copy_dir(from, to)
    }

    fn mkdir(&self, path: &Path) -> Result<(), Error> {
        mkdir(path)
    }

    fn hardlink(&self, from: &Path, to: &Path) -> Result<(), Error> {
        hardlink(from, to)
    }

    fn symlink_dir(&self, from: &Path, to: &Path) -> Result<(), Error> {
        symlink_dir(from, to)
    }

    fn delete_file(&self, path: &Path) -> Result<()> {
        delete_file(path)
    }
}

impl CommandExecutor for ProjectIO {
    fn exec(
        &self,
        program: &str,
        args: &[String],
        env: &[(&str, String)],
        cwd: Option<&Path>,
        quiet: bool,
    ) -> Result<i32, Error> {
        tracing::debug!(program=program, args=?args.join(" "), env=?env, cwd=?cwd, "command_exec");
        let stdout = if quiet {
            Stdio::null()
        } else {
            Stdio::inherit()
        };
        let result = std::process::Command::new(program)
            .args(args)
            .stdin(Stdio::null())
            .stdout(stdout)
            .envs(env.iter().map(|(a, b)| (a, b)))
            .current_dir(cwd.unwrap_or_else(|| Path::new("./")))
            .status();

        match result {
            Ok(status) => Ok(status.code().unwrap_or_default()),

            Err(error) => Err(match error.kind() {
                io::ErrorKind::NotFound => Error::ShellProgramNotFound {
                    program: program.to_string(),
                },

                other => Error::ShellCommand {
                    program: program.to_string(),
                    err: Some(other),
                },
            }),
        }
    }
}

impl FileSystemIO for ProjectIO {}

pub fn delete_dir(dir: &Path) -> Result<(), Error> {
    tracing::debug!(path=?dir, "deleting_directory");
    if dir.exists() {
        std::fs::remove_dir_all(&dir).map_err(|e| Error::FileIo {
            action: FileIoAction::Delete,
            kind: FileKind::Directory,
            path: dir.to_path_buf(),
            err: Some(e.to_string()),
        })?;
    } else {
        tracing::debug!(path=?dir, "directory_did_not_exist_for_deletion");
    }
    Ok(())
}

pub fn delete_file(file: &Path) -> Result<(), Error> {
    tracing::debug!("Deleting file {:?}", file);
    if file.exists() {
        std::fs::remove_file(&file).map_err(|e| Error::FileIo {
            action: FileIoAction::Delete,
            kind: FileKind::File,
            path: file.to_path_buf(),
            err: Some(e.to_string()),
        })?;
    } else {
        tracing::debug!("Did not exist for deletion: {:?}", file);
    }
    Ok(())
}

pub fn write_outputs_under(outputs: &[OutputFile], base: &Path) -> Result<(), Error> {
    for file in outputs {
        write_output_under(file, base)?;
    }
    Ok(())
}

pub fn write_outputs(outputs: &[OutputFile]) -> Result<(), Error> {
    for file in outputs {
        write_output(file)?;
    }
    Ok(())
}

pub fn write_output_under(file: &OutputFile, base: &Path) -> Result<(), Error> {
    let OutputFile { path, text } = file;
    write(&base.join(path), text)
}

pub fn write_output(file: &OutputFile) -> Result<(), Error> {
    let OutputFile { path, text } = file;
    write(path, text)
}

pub fn write(path: &Path, text: &str) -> Result<(), Error> {
    write_bytes(path, text.as_bytes())
}

pub fn writer(path: &Path) -> Result<WrappedWriter, Error> {
    tracing::debug!(path = ?path, "opening_file_writer");
    let dir_path = path.parent().ok_or_else(|| Error::FileIo {
        action: FileIoAction::FindParent,
        kind: FileKind::Directory,
        path: path.to_path_buf(),
        err: None,
    })?;
    std::fs::create_dir_all(dir_path).map_err(|e| Error::FileIo {
        action: FileIoAction::Create,
        kind: FileKind::Directory,
        path: dir_path.to_path_buf(),
        err: Some(e.to_string()),
    })?;
    let file = File::create(&path).map_err(|e| Error::FileIo {
        action: FileIoAction::Create,
        kind: FileKind::File,
        path: path.to_path_buf(),
        err: Some(e.to_string()),
    })?;
    Ok(WrappedWriter::new(path, Box::new(file)))
}

pub fn write_bytes(path: &Path, bytes: &[u8]) -> Result<(), Error> {
    tracing::debug!(path=?path, "deleting_directory");

    let dir_path = path.parent().ok_or_else(|| Error::FileIo {
        action: FileIoAction::FindParent,
        kind: FileKind::Directory,
        path: path.to_path_buf(),
        err: None,
    })?;

    std::fs::create_dir_all(dir_path).map_err(|e| Error::FileIo {
        action: FileIoAction::Create,
        kind: FileKind::Directory,
        path: dir_path.to_path_buf(),
        err: Some(e.to_string()),
    })?;

    let mut f = File::create(&path).map_err(|e| Error::FileIo {
        action: FileIoAction::Create,
        kind: FileKind::File,
        path: path.to_path_buf(),
        err: Some(e.to_string()),
    })?;

    f.write_all(bytes).map_err(|e| Error::FileIo {
        action: FileIoAction::WriteTo,
        kind: FileKind::File,
        path: path.to_path_buf(),
        err: Some(e.to_string()),
    })?;
    Ok(())
}

fn is_gleam_path(path: &Path, dir: impl AsRef<Path>) -> bool {
    use regex::Regex;
    lazy_static! {
        static ref RE: Regex = Regex::new(&format!(
            "^({module}{slash})*{module}\\.gleam$",
            module = "[a-z][_a-z0-9]*",
            slash = "(/|\\\\)",
        ))
        .expect("is_gleam_path() RE regex");
    }

    RE.is_match(
        path.strip_prefix(dir)
            .expect("is_gleam_path(): strip_prefix")
            .to_str()
            .expect("is_gleam_path(): to_str"),
    )
}

#[test]
fn is_gleam_path_test() {
    assert!(is_gleam_path(
        Path::new("/some-prefix/a.gleam"),
        Path::new("/some-prefix/")
    ));

    assert!(is_gleam_path(
        Path::new("/some-prefix/one_two/a.gleam"),
        Path::new("/some-prefix/")
    ));

    assert!(is_gleam_path(
        Path::new("/some-prefix/one_two/a123.gleam"),
        Path::new("/some-prefix/")
    ));

    assert!(is_gleam_path(
        Path::new("/some-prefix/one_2/a123.gleam"),
        Path::new("/some-prefix/")
    ));
}

pub fn gleam_files(dir: &Path) -> impl Iterator<Item = PathBuf> + '_ {
    walkdir::WalkDir::new(dir)
        .follow_links(true)
        .into_iter()
        .filter_map(Result::ok)
        .filter(|e| e.file_type().is_file())
        .map(|d| d.into_path())
        .filter(move |d| is_gleam_path(d, dir))
}

pub fn gleam_files_excluding_gitignore(dir: &Path) -> impl Iterator<Item = PathBuf> + '_ {
    ignore::WalkBuilder::new(dir)
        .follow_links(true)
        .require_git(false)
        .build()
        .into_iter()
        .filter_map(Result::ok)
        .filter(|e| e.file_type().map(|t| t.is_file()).unwrap_or(false))
        .map(ignore::DirEntry::into_path)
        .filter(move |d| is_gleam_path(d, dir))
}

pub fn native_files(dir: &Path) -> Result<impl Iterator<Item = PathBuf> + '_> {
    Ok(read_dir(dir)?
        .flat_map(Result::ok)
        .map(|e| e.path())
        .filter(|path| {
            let extension = path
                .extension()
                .unwrap_or_default()
                .to_str()
                .unwrap_or_default();
            extension == "erl" || extension == "hrl" || extension == "js" || extension == "mjs"
        }))
}

pub fn erlang_files(dir: &Path) -> Result<impl Iterator<Item = PathBuf> + '_> {
    Ok(read_dir(dir)?
        .flat_map(Result::ok)
        .map(|e| e.path())
        .filter(|path| {
            let extension = path
                .extension()
                .unwrap_or_default()
                .to_str()
                .unwrap_or_default();
            extension == "erl" || extension == "hrl"
        }))
}

pub fn create_tar_archive(outputs: Vec<OutputFile>) -> Result<Vec<u8>, Error> {
    tracing::debug!("creating_tar_archive");

    let encoder = flate2::write::GzEncoder::new(vec![], flate2::Compression::default());
    let mut builder = tar::Builder::new(encoder);

    for file in outputs {
        let mut header = tar::Header::new_gnu();
        header.set_path(&file.path).map_err(|e| Error::AddTar {
            path: file.path.clone(),
            err: e.to_string(),
        })?;
        header.set_size(file.text.as_bytes().len() as u64);
        header.set_cksum();
        builder
            .append(&header, file.text.as_bytes())
            .map_err(|e| Error::AddTar {
                path: file.path.clone(),
                err: e.to_string(),
            })?;
    }

    builder
        .into_inner()
        .map_err(|e| Error::TarFinish(e.to_string()))?
        .finish()
        .map_err(|e| Error::Gzip(e.to_string()))
}

pub fn mkdir(path: impl AsRef<Path> + Debug) -> Result<(), Error> {
    tracing::debug!(path=?path, "creating_directory");

    std::fs::create_dir_all(&path).map_err(|err| Error::FileIo {
        kind: FileKind::Directory,
        path: PathBuf::from(path.as_ref()),
        action: FileIoAction::Create,
        err: Some(err.to_string()),
    })
}

pub fn read_dir(path: impl AsRef<Path> + Debug) -> Result<std::fs::ReadDir, Error> {
    tracing::debug!(path=?path,"reading_directory");

    std::fs::read_dir(&path).map_err(|e| Error::FileIo {
        action: FileIoAction::Read,
        kind: FileKind::Directory,
        path: PathBuf::from(path.as_ref()),
        err: Some(e.to_string()),
    })
}

pub fn gleam_modules_metadata_paths(
    path: impl AsRef<Path> + Debug,
) -> Result<impl Iterator<Item = PathBuf>, Error> {
    Ok(read_dir(path)?
        .into_iter()
        .filter_map(Result::ok)
        .map(|f| f.path())
        .filter(|p| p.extension().and_then(OsStr::to_str) == Some("gleam_module")))
}

pub fn read(path: impl AsRef<Path> + Debug) -> Result<String, Error> {
    tracing::debug!(path=?path,"reading_file");

    std::fs::read_to_string(&path).map_err(|err| Error::FileIo {
        action: FileIoAction::Read,
        kind: FileKind::File,
        path: PathBuf::from(path.as_ref()),
        err: Some(err.to_string()),
    })
}

pub fn reader(path: impl AsRef<Path> + Debug) -> Result<WrappedReader, Error> {
    tracing::debug!(path=?path,"opening_file_reader");

    let reader = File::open(&path).map_err(|err| Error::FileIo {
        action: FileIoAction::Open,
        kind: FileKind::File,
        path: PathBuf::from(path.as_ref()),
        err: Some(err.to_string()),
    })?;

    Ok(WrappedReader::new(path.as_ref(), Box::new(reader)))
}

pub fn buffered_reader<P: AsRef<Path> + Debug>(path: P) -> Result<impl BufRead, Error> {
    tracing::debug!(path=?path,"opening_file_buffered_reader");
    let reader = File::open(&path).map_err(|err| Error::FileIo {
        action: FileIoAction::Open,
        kind: FileKind::File,
        path: PathBuf::from(path.as_ref()),
        err: Some(err.to_string()),
    })?;
    Ok(BufReader::new(reader))
}

pub fn copy(path: impl AsRef<Path> + Debug, to: impl AsRef<Path> + Debug) -> Result<(), Error> {
    tracing::debug!(from=?path, to=?to, "copying_file");

    // TODO: include the destination in the error message
    std::fs::copy(&path, &to)
        .map_err(|err| Error::FileIo {
            action: FileIoAction::Copy,
            kind: FileKind::File,
            path: PathBuf::from(path.as_ref()),
            err: Some(err.to_string()),
        })
        .map(|_| ())
}

pub fn copy_dir(path: impl AsRef<Path> + Debug, to: impl AsRef<Path> + Debug) -> Result<(), Error> {
    tracing::debug!(from=?path, to=?to, "copying_directory");

    // TODO: include the destination in the error message
    fs_extra::dir::copy(&path, &to, &fs_extra::dir::CopyOptions::new())
        .map_err(|err| Error::FileIo {
            action: FileIoAction::Copy,
            kind: FileKind::Directory,
            path: PathBuf::from(path.as_ref()),
            err: Some(err.to_string()),
        })
        .map(|_| ())
}

pub fn symlink_dir(
    src: impl AsRef<Path> + Debug,
    dest: impl AsRef<Path> + Debug,
) -> Result<(), Error> {
    tracing::debug!(src=?src, dest=?dest, "symlinking");
    symlink::symlink_dir(&canonicalise(src.as_ref())?, dest.as_ref()).map_err(|err| {
        Error::FileIo {
            action: FileIoAction::Link,
            kind: FileKind::File,
            path: PathBuf::from(dest.as_ref()),
            err: Some(err.to_string()),
        }
    })?;
    Ok(())
}

pub fn hardlink(from: impl AsRef<Path> + Debug, to: impl AsRef<Path> + Debug) -> Result<(), Error> {
    tracing::debug!(from=?from, to=?to, "hardlinking");
    std::fs::hard_link(&from, &to)
        .map_err(|err| Error::FileIo {
            action: FileIoAction::Link,
            kind: FileKind::File,
            path: PathBuf::from(from.as_ref()),
            err: Some(err.to_string()),
        })
        .map(|_| ())
}

pub fn git_init(path: &Path) -> Result<(), Error> {
    tracing::debug!(path=?path, "initializing git");

    let args = vec!["init".into(), "--quiet".into(), path.display().to_string()];

    match ProjectIO::new().exec("git", &args, &[], None, false) {
        Ok(_) => Ok(()),
        Err(err) => match err {
            Error::ShellProgramNotFound { .. } => Ok(()),
            _ => Err(Error::GitInitialization {
                error: err.to_string(),
            }),
        },
    }
}

fn canonicalise(path: &Path) -> Result<PathBuf, Error> {
    std::fs::canonicalize(path).map_err(|err| Error::FileIo {
        action: FileIoAction::Canonicalise,
        kind: FileKind::File,
        path: PathBuf::from(path),
        err: Some(err.to_string()),
    })
}
