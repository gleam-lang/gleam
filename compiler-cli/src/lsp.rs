use crate::{build_lock::BuildLock, fs::ProjectIO};
use gleam_core::{
    build::{Mode, NullTelemetry, Target},
    language_server::{LanguageServer, LockGuard, Locker},
    paths::ProjectPaths,
    Result,
};

pub fn main() -> Result<()> {
    tracing::info!("language_server_starting");

    eprintln!(
        "Hello human!

This command is intended to be run by language server clients such
as a text editor rather than being run directly in the console.

Many editors will automatically start the language server for you
when you open a Gleam project. If yours does not you may need to
look up how to configure your editor to use a language server.

You can exit this program by pressing ctrl+c.
"
    );

    // Create the transport. Includes the stdio (stdin and stdout) versions but this could
    // also be implemented to use sockets or HTTP.
    let (connection, io_threads) = lsp_server::Connection::stdio();

    // Run the server and wait for the two threads to end, typically by trigger
    // LSP Exit event.
    LanguageServer::new(&connection, ProjectIO::new())?.run()?;

    // Shut down gracefully.
    drop(connection);
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
