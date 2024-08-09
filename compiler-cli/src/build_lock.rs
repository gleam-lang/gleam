use camino::Utf8PathBuf;
use gleam_core::{
    build::{Mode, Target, Telemetry},
    paths::ProjectPaths,
    Result,
};
use strum::IntoEnumIterator;

#[derive(Debug)]
pub(crate) struct BuildLock {
    directory: Utf8PathBuf,
}

impl BuildLock {
    /// Lock the build directory for the specified mode and target.
    pub fn new_target(paths: &ProjectPaths, mode: Mode, target: Target) -> Result<Self> {
        let build = paths.build_directory_for_target(mode, target);
        crate::fs::mkdir(&build)?;
        Ok(Self { directory: build })
    }

    /// Lock the packages directory.
    pub fn new_packages(paths: &ProjectPaths) -> Result<Self> {
        let packages = paths.build_packages_directory();
        crate::fs::mkdir(&packages)?;
        Ok(Self {
            directory: packages,
        })
    }

    /// Lock the specified directory
    pub fn lock(&self, telemetry: &dyn Telemetry) -> Result<Guard> {
        tracing::debug!(path=?self.directory, "locking_build_directory");

        crate::fs::mkdir(&self.directory)?;

        let lock_path = self.directory.join("gleam.lock");
        let mut file = fslock::LockFile::open(lock_path.as_str()).expect("LockFile creation");

        if !file.try_lock_with_pid().expect("Trying build locking") {
            telemetry.waiting_for_build_directory_lock();
            file.lock_with_pid().expect("Build locking")
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
    let _guard1 = lock.lock(&gleam_core::build::NullTelemetry);
    println!("Locked!")
}

#[test]
fn locking_dev_erlang() {
    let paths = crate::project_paths_at_current_directory_without_toml();
    let lock = BuildLock::new_target(&paths, Mode::Dev, Target::Erlang).expect("make lock");
    let _guard1 = lock.lock(&gleam_core::build::NullTelemetry);
    println!("Locked!")
}

#[test]
fn locking_prod_erlang() {
    let paths = crate::project_paths_at_current_directory_without_toml();
    let lock = BuildLock::new_target(&paths, Mode::Prod, Target::Erlang).expect("make lock");
    let _guard1 = lock.lock(&gleam_core::build::NullTelemetry);
    println!("Locked!")
}

#[test]
fn locking_lsp_erlang() {
    let paths = crate::project_paths_at_current_directory_without_toml();
    let lock = BuildLock::new_target(&paths, Mode::Lsp, Target::Erlang).expect("make lock");
    let _guard1 = lock.lock(&gleam_core::build::NullTelemetry);
    println!("Locked!")
}

#[test]
fn locking_dev_javascript() {
    let paths = crate::project_paths_at_current_directory_without_toml();
    let lock = BuildLock::new_target(&paths, Mode::Dev, Target::JavaScript).expect("make lock");
    let _guard1 = lock.lock(&gleam_core::build::NullTelemetry);
    println!("Locked!")
}

#[test]
fn locking_prod_javascript() {
    let paths = crate::project_paths_at_current_directory_without_toml();
    let lock = BuildLock::new_target(&paths, Mode::Prod, Target::JavaScript).expect("make lock");
    let _guard1 = lock.lock(&gleam_core::build::NullTelemetry);
    println!("Locked!")
}

#[test]
fn locking_lsp_javascript() {
    let paths = crate::project_paths_at_current_directory_without_toml();
    let lock = BuildLock::new_target(&paths, Mode::Lsp, Target::JavaScript).expect("make lock");
    let _guard1 = lock.lock(&gleam_core::build::NullTelemetry);
    println!("Locked!")
}
