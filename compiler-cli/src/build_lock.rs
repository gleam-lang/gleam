use std::path::PathBuf;

use gleam_core::build::{Mode, Target};
use gleam_core::{build::Telemetry, paths, Result};

#[derive(Debug)]
pub(crate) struct BuildLock {
    path: PathBuf,
}

// TODO: return errors rather than crashing.
impl BuildLock {
    pub fn new_scoped(mode: Mode, target: Target) -> Result<Self> {
        let build = paths::build()
            .join(mode.to_string())
            .join(target.to_string());
        crate::fs::mkdir(&build)?;
        Ok(Self {
            path: build.join("build.lock"),
        })
    }

    pub fn new_global() -> Result<Self> {
        let build = paths::build();
        crate::fs::mkdir(&build)?;
        Ok(Self {
            path: build.join("build.lock"),
        })
    }

    /// Lock the build directory
    pub fn lock<Telem: Telemetry>(&self, telemetry: &Telem) -> Guard {
        tracing::info!("locking_build_directory");
        let mut file = fslock::LockFile::open(&self.path).expect("LockFile creation");
        if !file.try_lock_with_pid().expect("Trying build locking") {
            telemetry.waiting_for_build_directory_lock();
            file.lock_with_pid().expect("Build locking")
        }
        Guard(file)
    }
}

#[derive(Debug)]
pub(crate) struct Guard(fslock::LockFile);

#[test]
fn locking_global() {
    let lock = BuildLock::new_global().expect("make lock");
    let _guard1 = lock.lock(&crate::telemetry::NullTelemetry);
    println!("Locked!")
}

#[test]
fn locking_dev_erlang() {
    let lock = BuildLock::new_scoped(Mode::Dev, Target::Erlang).expect("make lock");
    let _guard1 = lock.lock(&crate::telemetry::NullTelemetry);
    println!("Locked!")
}

#[test]
fn locking_prod_erlang() {
    let lock = BuildLock::new_scoped(Mode::Prod, Target::Erlang).expect("make lock");
    let _guard1 = lock.lock(&crate::telemetry::NullTelemetry);
    println!("Locked!")
}

#[test]
fn locking_lsp_erlang() {
    let lock = BuildLock::new_scoped(Mode::Lsp, Target::Erlang).expect("make lock");
    let _guard1 = lock.lock(&crate::telemetry::NullTelemetry);
    println!("Locked!")
}

#[test]
fn locking_dev_javascript() {
    let lock = BuildLock::new_scoped(Mode::Dev, Target::JavaScript).expect("make lock");
    let _guard1 = lock.lock(&crate::telemetry::NullTelemetry);
    println!("Locked!")
}

#[test]
fn locking_prod_javascript() {
    let lock = BuildLock::new_scoped(Mode::Prod, Target::JavaScript).expect("make lock");
    let _guard1 = lock.lock(&crate::telemetry::NullTelemetry);
    println!("Locked!")
}

#[test]
fn locking_lsp_javascript() {
    let lock = BuildLock::new_scoped(Mode::Lsp, Target::JavaScript).expect("make lock");
    let _guard1 = lock.lock(&crate::telemetry::NullTelemetry);
    println!("Locked!")
}
