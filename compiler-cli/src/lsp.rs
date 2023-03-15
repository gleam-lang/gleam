// This module is a prototype-y mess. It has lots of TODO comments in it. Let's
// resolve them all, inject all the IO, wrap a bunch of tests around it, and
// move it into the `gleam_core` package.

// TODO: Make a new router class which finds the root of the project a message
// is for and dispatches to the correct language server, making one for that
// root if it does not exist. This will require the compiler to be modified so
// that it can run on projects where the root is not the cwd.

mod protocol_adapter;

use crate::{
    build_lock::BuildLock, dependencies::UseManifest, fs::ProjectIO,
    lsp::protocol_adapter::LanguageServerProtocolAdapter,
};
use gleam_core::{
    build::{Mode, NullTelemetry, Target},
    language_server::{LockGuard, Locker},
    manifest::Manifest,
    paths::ProjectPaths,
    Result,
};

pub fn main() -> Result<()> {
    tracing::info!("language_server_starting");

    // Read the project config. If we are running in the context of a Gleam
    // fall back to a non-compiling mode that can only do formatting.
    let paths = crate::project_paths_at_current_directory();
    let config = if paths.root_config().exists() {
        tracing::info!("gleam_project_detected");
        Some(crate::config::root_config()?)
    } else {
        tracing::info!("gleam_project_not_found");
        None
    };

    // Create the transport. Includes the stdio (stdin and stdout) versions but this could
    // also be implemented to use sockets or HTTP.
    let (connection, io_threads) = lsp_server::Connection::stdio();

    // Run the server and wait for the two threads to end (typically by trigger LSP Exit event).
    let io = ProjectIO::new();
    LanguageServerProtocolAdapter::new(
        &connection,
        config,
        dependencies_downloader,
        paths,
        io,
        make_locker,
    )?
    .run()?;
    io_threads.join().expect("joining_lsp_threads");

    // Shut down gracefully.
    tracing::info!("language_server_stopped");
    Ok(())
}

fn make_locker(paths: &ProjectPaths, target: Target) -> Result<Box<dyn Locker>> {
    let locker = LspLocker::new(paths, target)?;
    Ok(Box::new(locker))
}

fn dependencies_downloader(paths: &ProjectPaths) -> Result<Manifest> {
    crate::dependencies::download(paths, NullTelemetry, None, UseManifest::Yes)
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
