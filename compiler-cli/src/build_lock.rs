use gleam_core::build::Telemetry;

#[derive(Debug)]
pub(crate) struct BuildLock(named_lock::NamedLock);

// TODO: return errors rather than crashing.
impl BuildLock {
    pub fn new() -> Self {
        let dir = std::env::current_dir()
            .expect("Determining current directory")
            .join("build")
            .join("gleam-compile");
        // We replace any `\` with `/` because on Windows the mutex's name must
        // not contain them.
        let name = dir.to_string_lossy().replace('\\', "/");
        let lock = named_lock::NamedLock::create(&name).expect("Lock creation");
        Self(lock)
    }

    /// Lock the build directory
    pub fn lock<Telem: Telemetry>(&self, telemetry: &Telem) -> Guard<'_> {
        tracing::info!("locking_build_directory");
        let guard = match self.0.try_lock() {
            Ok(guard) => guard,
            Err(_) => {
                telemetry.waiting_for_build_directory_lock();
                self.0.lock().expect("Build locking")
            }
        };
        Guard(guard)
    }
}

#[derive(Debug)]
pub(crate) struct Guard<'a>(named_lock::NamedLockGuard<'a>);

#[test]
fn locking() {
    let lock = BuildLock::new();
    let _guard1 = lock.lock(&crate::telemetry::NullTelemetry);
    println!("Locked!")
}
