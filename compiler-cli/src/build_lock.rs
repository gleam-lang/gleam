use std::path::PathBuf;

use gleam_core::build::{Mode, Target};
use gleam_core::{build::Telemetry, paths, Result};
use strum::IntoEnumIterator;

#[derive(Debug)]
pub(crate) struct BuildLock {
    path: PathBuf,
}

impl BuildLock {
    /// Lock the build directory for the specified mode and target.
    pub fn new_target(mode: Mode, target: Target) -> Result<Self> {
        let build = paths::build()
            .join(mode.to_string())
            .join(target.to_string());
        crate::fs::mkdir(&build)?;
        Ok(Self {
            path: build.join("build.lock"),
        })
    }

    /// Lock the packages directory.
    pub fn new_packages() -> Result<Self> {
        let packages = paths::packages();
        crate::fs::mkdir(&packages)?;
        Ok(Self {
            path: packages.join("build.lock"),
        })
    }

    /// Lock the specified directory
    pub fn lock<Telem: Telemetry>(&self, telemetry: &Telem) -> Guard {
        tracing::info!("locking_build_directory");
        let mut file = fslock::LockFile::open(&self.path).expect("LockFile creation");
        if !file.try_lock_with_pid().expect("Trying build locking") {
            telemetry.waiting_for_build_directory_lock();
            file.lock_with_pid().expect("Build locking")
        }
        Guard(file)
    }

    /// Lock all build directories. Does not lock the packages directory.
    pub fn lock_all_build<Telem: Telemetry>(telemetry: &Telem) -> Result<Vec<Guard>> {
        let mut locks = vec![];
        for mode in Mode::iter() {
            for target in Target::iter() {
                locks.push(BuildLock::new_target(mode, target)?.lock(telemetry));
            }
        }
        Ok(locks)
    }
}

#[derive(Debug)]
pub(crate) struct Guard(fslock::LockFile);

#[test]
fn locking_global() {
    let lock = BuildLock::new_packages().expect("make lock");
    let _guard1 = lock.lock(&crate::telemetry::NullTelemetry);
    println!("Locked!")
}

#[test]
fn locking_dev_erlang() {
    let lock = BuildLock::new_target(Mode::Dev, Target::Erlang).expect("make lock");
    let _guard1 = lock.lock(&crate::telemetry::NullTelemetry);
    println!("Locked!")
}

#[test]
fn locking_prod_erlang() {
    let lock = BuildLock::new_target(Mode::Prod, Target::Erlang).expect("make lock");
    let _guard1 = lock.lock(&crate::telemetry::NullTelemetry);
    println!("Locked!")
}

#[test]
fn locking_lsp_erlang() {
    let lock = BuildLock::new_target(Mode::Lsp, Target::Erlang).expect("make lock");
    let _guard1 = lock.lock(&crate::telemetry::NullTelemetry);
    println!("Locked!")
}

#[test]
fn locking_dev_javascript() {
    let lock = BuildLock::new_target(Mode::Dev, Target::JavaScript).expect("make lock");
    let _guard1 = lock.lock(&crate::telemetry::NullTelemetry);
    println!("Locked!")
}

#[test]
fn locking_prod_javascript() {
    let lock = BuildLock::new_target(Mode::Prod, Target::JavaScript).expect("make lock");
    let _guard1 = lock.lock(&crate::telemetry::NullTelemetry);
    println!("Locked!")
}

#[test]
fn locking_lsp_javascript() {
    let lock = BuildLock::new_target(Mode::Lsp, Target::JavaScript).expect("make lock");
    let _guard1 = lock.lock(&crate::telemetry::NullTelemetry);
    println!("Locked!")
}
