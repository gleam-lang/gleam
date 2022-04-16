use std::path::PathBuf;

use gleam_core::{build::Telemetry, paths, Result};

#[derive(Debug)]
pub(crate) struct BuildLock {
    path: PathBuf,
}

// TODO: return errors rather than crashing.
impl BuildLock {
    pub fn new() -> Result<Self> {
        let build = paths::build();
        crate::fs::mkdir(&build)?;
        Ok(Self {
            path: build.join("gleam-compile.lock"),
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
fn locking() {
    let lock = BuildLock::new().expect("make lock");
    let _guard1 = lock.lock(&crate::telemetry::NullTelemetry);
    println!("Locked!")
}
