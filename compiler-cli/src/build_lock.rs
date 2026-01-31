use camino::Utf8PathBuf;
use gleam_core::{
    Error, Result,
    build::{Mode, Target, Telemetry},
    error::{FileIoAction, FileKind},
    paths::ProjectPaths,
};
use strum::IntoEnumIterator;

#[derive(Debug)]
pub(crate) struct BuildLock {
    directory: Utf8PathBuf,
    filename: String,
}

impl BuildLock {
    /// Lock the build directory for the specified mode and target.
    pub fn new_target(paths: &ProjectPaths, mode: Mode, target: Target) -> Result<Self> {
        let directory = paths.build_directory();
        crate::fs::mkdir(&directory)?;
        let target = match target {
            Target::Erlang => "erlang",
            Target::JavaScript => "javascript",
        };
        Ok(Self {
            directory,
            filename: format!("gleam-{mode}-{target}.lock"),
        })
    }

    /// Lock the packages directory.
    pub fn new_packages(paths: &ProjectPaths) -> Result<Self> {
        let directory = paths.build_packages_directory();
        crate::fs::mkdir(&directory)?;
        Ok(Self {
            directory,
            filename: "gleam.lock".to_string(),
        })
    }

    /// Construct the lock file path
    pub fn lock_path(&self) -> Utf8PathBuf {
        self.directory.join(&self.filename)
    }

    /// Lock the directory specified by the lock
    pub fn lock<Telem: Telemetry + ?Sized>(&self, telemetry: &Telem) -> Result<Guard> {
        let lock_path = self.lock_path();
        tracing::debug!(path=?lock_path, "locking_directory");

        crate::fs::mkdir(&self.directory)?;

        let mut file = fslock::LockFile::open(lock_path.as_str()).map_err(|e| Error::FileIo {
            kind: FileKind::File,
            path: lock_path.clone(),
            action: FileIoAction::Create,
            err: Some(e.to_string()),
        })?;

        if !file.try_lock_with_pid().expect("Trying directory locking") {
            telemetry.waiting_for_build_directory_lock();
            file.lock_with_pid().expect("Directory locking")
        }

        Ok(Guard(file))
    }

    /// Lock all build directories. Does not lock the packages directory.
    pub fn lock_all_build<Telem: Telemetry>(
        paths: &ProjectPaths,
        telemetry: &Telem,
    ) -> Result<Vec<Guard>> {
        let mut locks = vec![];
        for mode in Mode::iter() {
            for target in Target::iter() {
                locks.push(Self::new_target(paths, mode, target)?.lock(telemetry)?);
            }
        }

        Ok(locks)
    }
}

#[derive(Debug)]
pub(crate) struct Guard(
    // False positive. This is used in `drop`. Presumably the lint error is a
    // bug in clippy.
    #[allow(dead_code)] fslock::LockFile,
);

#[test]
fn locking_global() {
    let paths = crate::project_paths_at_current_directory_without_toml();
    let lock = BuildLock::new_packages(&paths).expect("make lock");
    let _guard1: Guard = lock.lock(&gleam_core::build::NullTelemetry).unwrap();
    println!("Locked!")
}

#[test]
fn locking_dev_erlang() {
    let paths = crate::project_paths_at_current_directory_without_toml();
    let lock = BuildLock::new_target(&paths, Mode::Dev, Target::Erlang).expect("make lock");
    let _guard1: Guard = lock.lock(&gleam_core::build::NullTelemetry).unwrap();
    println!("Locked!")
}

#[test]
fn locking_prod_erlang() {
    let paths = crate::project_paths_at_current_directory_without_toml();
    let lock = BuildLock::new_target(&paths, Mode::Prod, Target::Erlang).expect("make lock");
    let _guard1: Guard = lock.lock(&gleam_core::build::NullTelemetry).unwrap();
    println!("Locked!")
}

#[test]
fn locking_lsp_erlang() {
    let paths = crate::project_paths_at_current_directory_without_toml();
    let lock = BuildLock::new_target(&paths, Mode::Lsp, Target::Erlang).expect("make lock");
    let _guard1: Guard = lock.lock(&gleam_core::build::NullTelemetry).unwrap();
    println!("Locked!")
}

#[test]
fn locking_dev_javascript() {
    let paths = crate::project_paths_at_current_directory_without_toml();
    let lock = BuildLock::new_target(&paths, Mode::Dev, Target::JavaScript).expect("make lock");
    let _guard1: Guard = lock.lock(&gleam_core::build::NullTelemetry).unwrap();
    println!("Locked!")
}

#[test]
fn locking_prod_javascript() {
    let paths = crate::project_paths_at_current_directory_without_toml();
    let lock = BuildLock::new_target(&paths, Mode::Prod, Target::JavaScript).expect("make lock");
    let _guard1: Guard = lock.lock(&gleam_core::build::NullTelemetry).unwrap();
    println!("Locked!")
}

#[test]
fn locking_lsp_javascript() {
    let paths = crate::project_paths_at_current_directory_without_toml();
    let lock = BuildLock::new_target(&paths, Mode::Lsp, Target::JavaScript).expect("make lock");
    let _guard1: Guard = lock.lock(&gleam_core::build::NullTelemetry).unwrap();
    println!("Locked!")
}
