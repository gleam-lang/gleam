#[derive(Debug)]
pub(crate) struct BuildLock(named_lock::NamedLock);

// TODO: return errors rather than crashing.
impl BuildLock {
    pub fn new() -> Self {
        let dir = std::env::current_dir()
            .expect("Determining current directory")
            .join("gleam-compile");
        // We replace any `\` with `/` because on Windows the mutex's name must
        // not contain them.
        let name = dir.to_string_lossy().replace('\\', "/");
        let lock = named_lock::NamedLock::create(&name).expect("Lock creation");
        Self(lock)
    }

    /// Lock the build directory
    pub fn lock(&self) -> Guard<'_> {
        let guard = self.0.lock().expect("Build locking");
        Guard(guard)
    }
}

#[derive(Debug)]
pub(crate) struct Guard<'a>(named_lock::NamedLockGuard<'a>);

#[test]
fn locking() {
    let lock = BuildLock::new();
    let _guard1 = lock.lock();
    println!("Locked!")
}
