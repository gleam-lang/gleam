use std::{
    collections::HashSet,
    path::{Path, PathBuf},
};

use crate::{
    io::{CommandExecutor, FileSystemIO},
    Error, Result,
};

pub(crate) struct CopiedNativeFiles {
    pub any_elixir: bool,
    pub to_compile: Vec<PathBuf>,
}

pub(crate) struct NativeFileCopier<'a, IO> {
    io: IO,
    root: &'a Path,
    destination_dir: &'a Path,
    copied: HashSet<PathBuf>,
    to_compile: Vec<PathBuf>,
    elixir_files_copied: bool,
}

impl<'a, IO> NativeFileCopier<'a, IO>
where
    IO: FileSystemIO + CommandExecutor + Clone,
{
    pub(crate) fn new(io: IO, root: &'a Path, out: &'a Path) -> Self {
        Self {
            io,
            root,
            destination_dir: out,
            to_compile: Vec::new(),
            copied: HashSet::new(),
            elixir_files_copied: false,
        }
    }

    /// Copy native files from the given directory to the build directory.
    ///
    /// Errors if any duplicate files are found.
    ///
    /// Returns a list of files that need to be compiled (Elixir and Erlang).
    ///
    pub fn run(mut self) -> Result<CopiedNativeFiles> {
        self.io.mkdir(&self.destination_dir)?;

        let src = self.root.join("src");
        self.copy_files(&src)?;

        let test = self.root.join("test");
        if self.io.is_directory(&test) {
            self.copy_files(&test)?;
        }

        Ok(CopiedNativeFiles {
            to_compile: self.to_compile,
            any_elixir: self.elixir_files_copied,
        })
    }

    fn copy_files(&mut self, src_root: &Path) -> Result<()> {
        let mut check_elixir_libs = true;

        for entry in self.io.read_dir(src_root)? {
            let path = entry.expect("copy_native_files dir_entry").pathbuf;
            self.copy(path, src_root)?;
        }
        Ok(())
    }

    fn copy(&mut self, path: PathBuf, src_root: &Path) -> Result<()> {
        let extension = path
            .extension()
            .unwrap_or_default()
            .to_str()
            .unwrap_or_default();

        // Skip unknown file formats that are not supported native files
        if !matches!(extension, "mjs" | "js" | "ts" | "hrl" | "erl" | "ex") {
            return Ok(());
        }

        let relative_path = path
            .strip_prefix(src_root)
            .expect("copy_native_files strip prefix")
            .to_path_buf();
        let destination = self.destination_dir.join(&relative_path);

        self.io.copy(&path, &destination)?;

        self.elixir_files_copied = self.elixir_files_copied || extension == "ex";

        // BEAM native modules need to be compiled
        if matches!(extension, "erl" | "ex") {
            _ = self.to_compile.push(relative_path.clone());
        }

        // TODO: test
        if !self.copied.insert(relative_path.clone()) {
            return Err(Error::DuplicateSourceFile {
                file: relative_path.to_string_lossy().to_string(),
            });
        }

        Ok(())
    }
}
