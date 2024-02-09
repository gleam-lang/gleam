use gleam_core::{
    build::{NullTelemetry, Target},
    error::{Error, FileIoAction, FileKind},
    io::{
        CommandExecutor, Content, DirEntry, FileSystemReader, FileSystemWriter, OutputFile,
        ReadDir, Stdio, WrappedReader,
    },
    language_server::{DownloadDependencies, Locker, MakeLocker},
    manifest::Manifest,
    paths::ProjectPaths,
    warning::WarningEmitterIO,
    Result, Warning,
};
use std::{
    ffi::OsStr,
    fmt::Debug,
    fs::File,
    io::{self, BufRead, BufReader, Write},
    sync::OnceLock,
    time::SystemTime,
};

use camino::{ReadDirUtf8, Utf8Path, Utf8PathBuf};

use crate::{dependencies::UseManifest, lsp::LspLocker};

#[cfg(test)]
mod tests;

/// Return the current directory as a UTF-8 Path
pub fn get_current_directory() -> Result<Utf8PathBuf, Error> {
    let curr_dir = std::env::current_dir().map_err(|e| Error::FileIo {
        kind: FileKind::Directory,
        action: FileIoAction::Open,
        path: ".".into(),
        err: Some(e.to_string()),
    })?;
    Utf8PathBuf::from_path_buf(curr_dir.clone()).map_err(|_| Error::NonUtf8Path { path: curr_dir })
}

// Return the first directory with a gleam.toml as a UTF-8 Path
pub fn get_project_root(path: Utf8PathBuf) -> Result<Utf8PathBuf, Error> {
    fn walk(dir: Utf8PathBuf) -> Option<Utf8PathBuf> {
        match dir.join("gleam.toml").is_file() {
            true => Some(dir),
            false => match dir.parent() {
                Some(p) => walk(p.into()),
                None => None,
            },
        }
    }
    walk(path.clone()).ok_or(Error::UnableToFindProjectRoot {
        path: path.to_string(),
    })
}

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

impl FileSystemReader for ProjectIO {
    fn gleam_source_files(&self, dir: &Utf8Path) -> Vec<Utf8PathBuf> {
        if !dir.is_dir() {
            return vec![];
        }
        let dir = dir.to_path_buf();
        walkdir::WalkDir::new(dir)
            .follow_links(true)
            .into_iter()
            .filter_map(Result::ok)
            .filter(|e| e.file_type().is_file())
            .map(|d| d.into_path())
            .filter(move |d| d.extension() == Some(OsStr::new("gleam")))
            .map(|pb| Utf8PathBuf::from_path_buf(pb).expect("Non Utf-8 Path"))
            .collect()
    }

    fn gleam_cache_files(&self, dir: &Utf8Path) -> Vec<Utf8PathBuf> {
        if !dir.is_dir() {
            return vec![];
        }
        let dir = dir.to_path_buf();
        walkdir::WalkDir::new(dir)
            .follow_links(true)
            .into_iter()
            .filter_map(Result::ok)
            .filter(|e| e.file_type().is_file())
            .map(|d| d.into_path())
            .filter(|p| p.extension().and_then(OsStr::to_str) == Some("cache"))
            .map(|pb| Utf8PathBuf::from_path_buf(pb).expect("Non Utf-8 Path"))
            .collect()
    }

    fn read(&self, path: &Utf8Path) -> Result<String, Error> {
        read(path)
    }

    fn read_bytes(&self, path: &Utf8Path) -> Result<Vec<u8>, Error> {
        read_bytes(path)
    }

    fn is_file(&self, path: &Utf8Path) -> bool {
        path.is_file()
    }

    fn is_directory(&self, path: &Utf8Path) -> bool {
        path.is_dir()
    }

    fn reader(&self, path: &Utf8Path) -> Result<WrappedReader, Error> {
        reader(path)
    }

    fn read_dir(&self, path: &Utf8Path) -> Result<ReadDir> {
        read_dir(path).map(|entries| {
            entries
                .map(|result| result.map(|entry| DirEntry::from_path(entry.path())))
                .collect()
        })
    }

    fn modification_time(&self, path: &Utf8Path) -> Result<SystemTime, Error> {
        path.metadata()
            .map(|m| m.modified().unwrap_or_else(|_| SystemTime::now()))
            .map_err(|e| Error::FileIo {
                action: FileIoAction::ReadMetadata,
                kind: FileKind::File,
                path: path.to_path_buf(),
                err: Some(e.to_string()),
            })
    }

    fn canonicalise(&self, path: &Utf8Path) -> Result<Utf8PathBuf, Error> {
        canonicalise(path)
    }
}

impl FileSystemWriter for ProjectIO {
    fn delete_directory(&self, path: &Utf8Path) -> Result<()> {
        delete_directory(path)
    }

    fn copy(&self, from: &Utf8Path, to: &Utf8Path) -> Result<()> {
        copy(from, to)
    }

    fn copy_dir(&self, from: &Utf8Path, to: &Utf8Path) -> Result<()> {
        copy_dir(from, to)
    }

    fn mkdir(&self, path: &Utf8Path) -> Result<(), Error> {
        mkdir(path)
    }

    fn hardlink(&self, from: &Utf8Path, to: &Utf8Path) -> Result<(), Error> {
        hardlink(from, to)
    }

    fn symlink_dir(&self, from: &Utf8Path, to: &Utf8Path) -> Result<(), Error> {
        symlink_dir(from, to)
    }

    fn delete_file(&self, path: &Utf8Path) -> Result<()> {
        delete_file(path)
    }

    fn write(&self, path: &Utf8Path, content: &str) -> Result<(), Error> {
        write(path, content)
    }

    fn write_bytes(&self, path: &Utf8Path, content: &[u8]) -> Result<(), Error> {
        write_bytes(path, content)
    }
}

impl CommandExecutor for ProjectIO {
    fn exec(
        &self,
        program: &str,
        args: &[String],
        env: &[(&str, String)],
        cwd: Option<&Utf8Path>,
        stdio: Stdio,
    ) -> Result<i32, Error> {
        tracing::trace!(program=program, args=?args.join(" "), env=?env, cwd=?cwd, "command_exec");
        let result = std::process::Command::new(program)
            .args(args)
            .stdin(stdio.get_process_stdio())
            .stdout(stdio.get_process_stdio())
            .envs(env.iter().map(|pair| (pair.0, &pair.1)))
            .current_dir(cwd.unwrap_or_else(|| Utf8Path::new("./")))
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

impl MakeLocker for ProjectIO {
    fn make_locker(&self, paths: &ProjectPaths, target: Target) -> Result<Box<dyn Locker>> {
        let locker = LspLocker::new(paths, target)?;
        Ok(Box::new(locker))
    }
}

impl DownloadDependencies for ProjectIO {
    fn download_dependencies(&self, paths: &ProjectPaths) -> Result<Manifest> {
        crate::dependencies::download(paths, NullTelemetry, None, UseManifest::Yes)
    }
}

pub fn delete_directory(dir: &Utf8Path) -> Result<(), Error> {
    tracing::trace!(path=?dir, "deleting_directory");
    if dir.exists() {
        std::fs::remove_dir_all(dir).map_err(|e| Error::FileIo {
            action: FileIoAction::Delete,
            kind: FileKind::Directory,
            path: dir.to_path_buf(),
            err: Some(e.to_string()),
        })?;
    } else {
        tracing::trace!(path=?dir, "directory_did_not_exist_for_deletion");
    }
    Ok(())
}

pub fn delete_file(file: &Utf8Path) -> Result<(), Error> {
    tracing::trace!("Deleting file {:?}", file);
    if file.exists() {
        std::fs::remove_file(file).map_err(|e| Error::FileIo {
            action: FileIoAction::Delete,
            kind: FileKind::File,
            path: file.to_path_buf(),
            err: Some(e.to_string()),
        })?;
    } else {
        tracing::trace!("Did not exist for deletion: {:?}", file);
    }
    Ok(())
}

pub fn write_outputs_under(outputs: &[OutputFile], base: &Utf8Path) -> Result<(), Error> {
    for file in outputs {
        let path = base.join(&file.path);
        match &file.content {
            Content::Binary(buffer) => write_bytes(&path, buffer),
            Content::Text(buffer) => write(&path, buffer),
        }?;
    }
    Ok(())
}

pub fn write_output(file: &OutputFile) -> Result<(), Error> {
    let OutputFile { path, content } = file;
    match content {
        Content::Binary(buffer) => write_bytes(path, buffer),
        Content::Text(buffer) => write(path, buffer),
    }
}

pub fn write(path: &Utf8Path, text: &str) -> Result<(), Error> {
    write_bytes(path, text.as_bytes())
}

#[cfg(target_family = "unix")]
pub fn make_executable(path: impl AsRef<Utf8Path>) -> Result<(), Error> {
    use std::os::unix::fs::PermissionsExt;
    tracing::trace!(path = ?path.as_ref(), "setting_permissions");

    std::fs::set_permissions(path.as_ref(), std::fs::Permissions::from_mode(0o755)).map_err(
        |e| Error::FileIo {
            action: FileIoAction::UpdatePermissions,
            kind: FileKind::File,
            path: path.as_ref().to_path_buf(),
            err: Some(e.to_string()),
        },
    )?;
    Ok(())
}

#[cfg(not(target_family = "unix"))]
pub fn make_executable(_path: impl AsRef<Utf8Path>) -> Result<(), Error> {
    Ok(())
}

pub fn write_bytes(path: &Utf8Path, bytes: &[u8]) -> Result<(), Error> {
    tracing::trace!(path=?path, "writing_file");

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

    let mut f = File::create(path).map_err(|e| Error::FileIo {
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

fn is_gleam_path(path: &Utf8Path, dir: impl AsRef<Utf8Path>) -> bool {
    use regex::Regex;

    static RE: OnceLock<Regex> = OnceLock::new();

    RE.get_or_init(|| {
        Regex::new(&format!(
            "^({module}{slash})*{module}\\.gleam$",
            module = "[a-z][_a-z0-9]*",
            slash = "(/|\\\\)",
        ))
        .expect("is_gleam_path() RE regex")
    })
    .is_match(
        path.strip_prefix(dir.as_ref())
            .expect("is_gleam_path(): strip_prefix")
            .as_str(),
    )
}

fn is_gleam_build_dir(e: &ignore::DirEntry) -> bool {
    if !e.path().is_dir() || !e.path().ends_with("build") {
        return false;
    }

    let Some(parent_path) = e.path().parent() else {
        return false;
    };

    parent_path.join("gleam.toml").exists()
}

pub fn gleam_files_excluding_gitignore(dir: &Utf8Path) -> impl Iterator<Item = Utf8PathBuf> + '_ {
    ignore::WalkBuilder::new(dir)
        .follow_links(true)
        .require_git(false)
        .filter_entry(|e| !is_gleam_build_dir(e))
        .build()
        .filter_map(Result::ok)
        .filter(|e| e.file_type().map(|t| t.is_file()).unwrap_or(false))
        .map(ignore::DirEntry::into_path)
        .map(|pb| Utf8PathBuf::from_path_buf(pb).expect("Non Utf-8 Path"))
        .filter(move |d| is_gleam_path(d, dir))
}

pub fn native_files(dir: &Utf8Path) -> Result<impl Iterator<Item = Utf8PathBuf> + '_> {
    Ok(read_dir(dir)?
        .flat_map(Result::ok)
        .map(|e| e.into_path())
        .filter(|path| {
            let extension = path.extension().unwrap_or_default();
            matches!(extension, "erl" | "hrl" | "ex" | "js" | "mjs" | "ts")
        }))
}

pub fn private_files_excluding_gitignore(dir: &Utf8Path) -> impl Iterator<Item = Utf8PathBuf> + '_ {
    ignore::WalkBuilder::new(dir)
        .follow_links(true)
        .require_git(false)
        .build()
        .filter_map(Result::ok)
        .filter(|e| e.file_type().map(|t| t.is_file()).unwrap_or(false))
        .map(ignore::DirEntry::into_path)
        .map(|pb| Utf8PathBuf::from_path_buf(pb).expect("Non Utf-8 Path"))
}

pub fn erlang_files(dir: &Utf8Path) -> Result<impl Iterator<Item = Utf8PathBuf> + '_> {
    Ok(read_dir(dir)?
        .flat_map(Result::ok)
        .map(|e| e.into_path())
        .filter(|path| {
            let extension = path.extension().unwrap_or_default();
            extension == "erl" || extension == "hrl"
        }))
}

pub fn create_tar_archive(outputs: Vec<OutputFile>) -> Result<Vec<u8>, Error> {
    tracing::trace!("creating_tar_archive");

    let encoder = flate2::write::GzEncoder::new(vec![], flate2::Compression::default());
    let mut builder = tar::Builder::new(encoder);

    for file in outputs {
        let mut header = tar::Header::new_gnu();
        header.set_path(&file.path).map_err(|e| Error::AddTar {
            path: file.path.clone(),
            err: e.to_string(),
        })?;
        header.set_size(file.content.as_bytes().len() as u64);
        header.set_cksum();
        builder
            .append(&header, file.content.as_bytes())
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

pub fn mkdir(path: impl AsRef<Utf8Path> + Debug) -> Result<(), Error> {
    if path.as_ref().exists() {
        return Ok(());
    }

    tracing::trace!(path=?path, "creating_directory");

    std::fs::create_dir_all(path.as_ref()).map_err(|err| Error::FileIo {
        kind: FileKind::Directory,
        path: Utf8PathBuf::from(path.as_ref()),
        action: FileIoAction::Create,
        err: Some(err.to_string()),
    })
}

pub fn read_dir(path: impl AsRef<Utf8Path> + Debug) -> Result<ReadDirUtf8, Error> {
    tracing::trace!(path=?path,"reading_directory");

    Utf8Path::read_dir_utf8(path.as_ref()).map_err(|e| Error::FileIo {
        action: FileIoAction::Read,
        kind: FileKind::Directory,
        path: Utf8PathBuf::from(path.as_ref()),
        err: Some(e.to_string()),
    })
}

pub fn module_caches_paths(
    path: impl AsRef<Utf8Path> + Debug,
) -> Result<impl Iterator<Item = Utf8PathBuf>, Error> {
    Ok(read_dir(path)?
        .filter_map(Result::ok)
        .map(|f| f.into_path())
        .filter(|p| p.extension() == Some("cache")))
}

pub fn read(path: impl AsRef<Utf8Path> + Debug) -> Result<String, Error> {
    tracing::trace!(path=?path,"reading_file");

    std::fs::read_to_string(path.as_ref()).map_err(|err| Error::FileIo {
        action: FileIoAction::Read,
        kind: FileKind::File,
        path: Utf8PathBuf::from(path.as_ref()),
        err: Some(err.to_string()),
    })
}

pub fn read_bytes(path: impl AsRef<Utf8Path> + Debug) -> Result<Vec<u8>, Error> {
    tracing::trace!(path=?path,"reading_file");

    std::fs::read(path.as_ref()).map_err(|err| Error::FileIo {
        action: FileIoAction::Read,
        kind: FileKind::File,
        path: Utf8PathBuf::from(path.as_ref()),
        err: Some(err.to_string()),
    })
}

pub fn reader(path: impl AsRef<Utf8Path> + Debug) -> Result<WrappedReader, Error> {
    tracing::trace!(path=?path,"opening_file_reader");

    let reader = File::open(path.as_ref()).map_err(|err| Error::FileIo {
        action: FileIoAction::Open,
        kind: FileKind::File,
        path: Utf8PathBuf::from(path.as_ref()),
        err: Some(err.to_string()),
    })?;

    Ok(WrappedReader::new(path.as_ref(), Box::new(reader)))
}

pub fn buffered_reader<P: AsRef<Utf8Path> + Debug>(path: P) -> Result<impl BufRead, Error> {
    tracing::trace!(path=?path,"opening_file_buffered_reader");
    let reader = File::open(path.as_ref()).map_err(|err| Error::FileIo {
        action: FileIoAction::Open,
        kind: FileKind::File,
        path: Utf8PathBuf::from(path.as_ref()),
        err: Some(err.to_string()),
    })?;
    Ok(BufReader::new(reader))
}

pub fn copy(
    path: impl AsRef<Utf8Path> + Debug,
    to: impl AsRef<Utf8Path> + Debug,
) -> Result<(), Error> {
    tracing::trace!(from=?path, to=?to, "copying_file");

    // TODO: include the destination in the error message
    std::fs::copy(path.as_ref(), to.as_ref())
        .map_err(|err| Error::FileIo {
            action: FileIoAction::Copy,
            kind: FileKind::File,
            path: Utf8PathBuf::from(path.as_ref()),
            err: Some(err.to_string()),
        })
        .map(|_| ())
}

// pub fn rename(path: impl AsRef<Utf8Path> + Debug, to: impl AsRef<Utf8Path> + Debug) -> Result<(), Error> {
//     tracing::trace!(from=?path, to=?to, "renaming_file");

//     // TODO: include the destination in the error message
//     std::fs::rename(&path, &to)
//         .map_err(|err| Error::FileIo {
//             action: FileIoAction::Rename,
//             kind: FileKind::File,
//             path: Utf8PathBuf::from(path.as_ref()),
//             err: Some(err.to_string()),
//         })
//         .map(|_| ())
// }

pub fn copy_dir(
    path: impl AsRef<Utf8Path> + Debug,
    to: impl AsRef<Utf8Path> + Debug,
) -> Result<(), Error> {
    tracing::trace!(from=?path, to=?to, "copying_directory");

    // TODO: include the destination in the error message
    fs_extra::dir::copy(
        path.as_ref(),
        to.as_ref(),
        &fs_extra::dir::CopyOptions::new()
            .copy_inside(false)
            .content_only(true),
    )
    .map_err(|err| Error::FileIo {
        action: FileIoAction::Copy,
        kind: FileKind::Directory,
        path: Utf8PathBuf::from(path.as_ref()),
        err: Some(err.to_string()),
    })
    .map(|_| ())
}

pub fn symlink_dir(
    src: impl AsRef<Utf8Path> + Debug,
    dest: impl AsRef<Utf8Path> + Debug,
) -> Result<(), Error> {
    tracing::trace!(src=?src, dest=?dest, "symlinking");
    let src = canonicalise(src.as_ref())?;

    #[cfg(target_family = "windows")]
    let result = std::os::windows::fs::symlink_dir(src, dest.as_ref());
    #[cfg(not(target_family = "windows"))]
    let result = std::os::unix::fs::symlink(src, dest.as_ref());

    result.map_err(|err| Error::FileIo {
        action: FileIoAction::Link,
        kind: FileKind::File,
        path: Utf8PathBuf::from(dest.as_ref()),
        err: Some(err.to_string()),
    })?;
    Ok(())
}

pub fn hardlink(
    from: impl AsRef<Utf8Path> + Debug,
    to: impl AsRef<Utf8Path> + Debug,
) -> Result<(), Error> {
    tracing::trace!(from=?from, to=?to, "hardlinking");
    std::fs::hard_link(from.as_ref(), to.as_ref())
        .map_err(|err| Error::FileIo {
            action: FileIoAction::Link,
            kind: FileKind::File,
            path: Utf8PathBuf::from(from.as_ref()),
            err: Some(err.to_string()),
        })
        .map(|_| ())
}

/// Check if the given path is inside a git work tree.
/// This is done by running `git rev-parse --is-inside-work-tree --quiet` in the
/// given path. If git is not installed then we assume we're not in a git work
/// tree.
///
pub fn is_inside_git_work_tree(path: &Utf8Path) -> Result<bool, Error> {
    tracing::trace!(path=?path, "checking_for_git_repo");

    let args: Vec<&str> = vec!["rev-parse", "--is-inside-work-tree", "--quiet"];

    // Ignore all output, rely on the exit code instead.
    // git will display a fatal error on stderr if rev-parse isn't run inside of a git work tree,
    // so send stderr to /dev/null
    let result = std::process::Command::new("git")
        .args(args)
        .stdin(std::process::Stdio::null())
        .stdout(std::process::Stdio::null())
        .stderr(std::process::Stdio::null())
        .current_dir(path)
        .status();

    match result {
        Ok(status) => Ok(status.success()),
        Err(error) => match error.kind() {
            io::ErrorKind::NotFound => Ok(false),

            other => Err(Error::ShellCommand {
                program: "git".into(),
                err: Some(other),
            }),
        },
    }
}

/// Run `git init` in the given path.
/// If git is not installed then we do nothing.
pub fn git_init(path: &Utf8Path) -> Result<(), Error> {
    tracing::trace!(path=?path, "initializing git");

    if is_inside_git_work_tree(path)? {
        tracing::trace!(path=?path, "git_repo_already_exists");
        return Ok(());
    }

    let args = vec!["init".into(), "--quiet".into(), path.to_string()];

    match ProjectIO::new().exec("git", &args, &[], None, Stdio::Inherit) {
        Ok(_) => Ok(()),
        Err(err) => match err {
            Error::ShellProgramNotFound { .. } => Ok(()),
            _ => Err(Error::GitInitialization {
                error: err.to_string(),
            }),
        },
    }
}

pub fn canonicalise(path: &Utf8Path) -> Result<Utf8PathBuf, Error> {
    std::fs::canonicalize(path)
        .map_err(|err| Error::FileIo {
            action: FileIoAction::Canonicalise,
            kind: FileKind::File,
            path: Utf8PathBuf::from(path),
            err: Some(err.to_string()),
        })
        .map(|pb| Utf8PathBuf::from_path_buf(pb).expect("Non Utf8 Path"))
}

#[derive(Debug, Clone, Copy)]
pub struct ConsoleWarningEmitter;

impl WarningEmitterIO for ConsoleWarningEmitter {
    fn emit_warning(&self, warning: Warning) {
        let buffer_writer = crate::cli::stderr_buffer_writer();
        let mut buffer = buffer_writer.buffer();
        warning.pretty(&mut buffer);
        buffer_writer
            .print(&buffer)
            .expect("Writing warning to stderr");
    }
}
