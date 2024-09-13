#[cfg(test)]
mod tests;

use std::collections::{HashMap, HashSet};

use camino::{Utf8Path, Utf8PathBuf};

use crate::{
    io::{FileSystemReader, FileSystemWriter},
    Error, Result,
};

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct CopiedNativeFiles {
    pub any_elixir: bool,
    pub to_compile: Vec<Utf8PathBuf>,
}

pub(crate) struct NativeFileCopier<'a, IO> {
    io: IO,
    root: &'a Utf8Path,
    destination_dir: &'a Utf8Path,
    seen_native_files: HashSet<Utf8PathBuf>,
    seen_erlang_modules: HashMap<String, Utf8PathBuf>,
    to_compile: Vec<Utf8PathBuf>,
    elixir_files_copied: bool,
}

impl<'a, IO> NativeFileCopier<'a, IO>
where
    IO: FileSystemReader + FileSystemWriter + Clone,
{
    pub(crate) fn new(io: IO, root: &'a Utf8Path, out: &'a Utf8Path) -> Self {
        Self {
            io,
            root,
            destination_dir: out,
            to_compile: Vec::new(),
            seen_native_files: HashSet::new(),
            seen_erlang_modules: HashMap::new(),
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
        self.copy_files(&src, None)?;

        let test = self.root.join("test");
        if self.io.is_directory(&test) {
            self.copy_files(&test, None)?;
        }

        Ok(CopiedNativeFiles {
            to_compile: self.to_compile,
            any_elixir: self.elixir_files_copied,
        })
    }

    fn copy_files(&mut self, src_root: &Utf8Path, nested_path: Option<&Utf8Path>) -> Result<()> {
        let mut check_elixir_libs = true;

        for entry in self
            .io
            .read_dir(nested_path.as_deref().unwrap_or(src_root))?
        {
            let path = entry.expect("copy_native_files dir_entry").pathbuf;

            if self.io.is_directory(&path) {
                // Recursively copy files in subdirectories as well.
                self.copy_files(src_root, Some(&path))?;
            } else {
                self.copy(path, src_root, nested_path.is_some())?;
            }
        }
        Ok(())
    }

    fn copy(&mut self, file: Utf8PathBuf, src_root: &Utf8Path, in_subdir: bool) -> Result<()> {
        let extension = file.extension().unwrap_or_default();

        let relative_path = file
            .strip_prefix(src_root)
            .expect("copy_native_files strip prefix")
            .to_path_buf();

        // Check for Erlang modules conflicting between each other anywhere in
        // the tree. We do this beforehand as '.gleam' files can also cause
        // a conflict, despite not being native files.
        self.check_for_conflicting_erlang_modules(&relative_path)?;

        // Skip unknown file formats that are not supported native files
        if !matches!(extension, "mjs" | "js" | "ts" | "hrl" | "erl" | "ex") {
            return Ok(());
        }

        let destination = self.destination_dir.join(&relative_path);

        // Check that this native file was not already copied
        self.check_for_duplicate(&relative_path)?;

        // If the source file's mtime is older than the destination file's mtime
        // then it has not changed and as such does not need to be copied.
        //
        // This makes no practical difference for JavaScript etc files, but for
        // Erlang and Elixir files it mean we can skip compiling them.
        if self.io.is_file(&destination)
            && self.io.modification_time(&file)? <= self.io.modification_time(&destination)?
        {
            tracing::debug!(?file, "skipping_unchanged_native_file_unchanged");
            return Ok(());
        }

        tracing::debug!(?file, "copying_native_file");

        // Ensure destination exists (subdir might not exist yet in the output)
        if in_subdir {
            if let Some(parent) = destination.parent() {
                self.io.mkdir(parent)?;
            }
        }

        self.io.copy(&file, &destination)?;
        self.elixir_files_copied = self.elixir_files_copied || extension == "ex";

        // BEAM native modules need to be compiled
        if matches!(extension, "erl" | "ex") {
            _ = self.to_compile.push(relative_path.clone());
        }

        Ok(())
    }

    fn check_for_duplicate(&mut self, relative_path: &Utf8PathBuf) -> Result<(), Error> {
        if !self.seen_native_files.insert(relative_path.clone()) {
            return Err(Error::DuplicateSourceFile {
                file: relative_path.to_string(),
            });
        }
        Ok(())
    }

    /// Erlang module files cannot have the same name regardless of their
    /// relative positions within the project. Ensure we raise an error if the
    /// user attempts to create `.erl` files with the same name.
    fn check_for_conflicting_erlang_modules(
        &mut self,
        relative_path: &Utf8PathBuf,
    ) -> Result<(), Error> {
        let erl_name = match relative_path.extension() {
            Some("gleam") => relative_path
                .with_extension("erl")
                .as_str()
                .replace("/", "@"),
            Some("erl") => relative_path
                .file_name()
                .expect("path has file name")
                .to_owned(),
            _ => return Ok(()),
        };

        if let Some(first) = self
            .seen_erlang_modules
            .insert(erl_name.clone(), relative_path.clone())
        {
            // TODO: Dedicated error
            return Err(Error::DuplicateModule {
                module: ecow::eco_format!("{}", erl_name),
                first,
                second: relative_path.clone(),
            });
        }

        Ok(())
    }
}
