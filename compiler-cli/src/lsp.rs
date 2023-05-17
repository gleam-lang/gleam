use crate::{build_lock::BuildLock, fs::ProjectIO};
use gleam_core::{
    build::{Mode, NullTelemetry, Target},
    language_server::{LanguageServer, LockGuard, Locker},
    paths::ProjectPaths,
    Result,
};

pub fn main() -> Result<()> {
    eprintln!("Starting language server...");
    tracing::info!("language_server_starting");

    // Create the transport. Includes the stdio (stdin and stdout) versions but this could
    // also be implemented to use sockets or HTTP.
    let (connection, io_threads) = lsp_server::Connection::stdio();

    // Run the server and wait for the two threads to end, typically by trigger
    // LSP Exit event.
    LanguageServer::new(&connection, ProjectIO::new())?.run()?;

    // Shut down gracefully.
    io_threads.join().expect("joining_lsp_threads");

    tracing::info!("language_server_stopped");
    Ok(())
}

#[derive(Debug)]
pub struct LspLocker(BuildLock);

impl LspLocker {
    pub fn new(paths: &ProjectPaths, target: Target) -> Result<Self> {
        let build_lock = BuildLock::new_target(paths, Mode::Lsp, target)?;
        Ok(Self(build_lock))
    }
}

impl Locker for LspLocker {
    fn lock_for_build(&self) -> LockGuard {
        LockGuard(Box::new(self.0.lock(&NullTelemetry)))
    }
}
