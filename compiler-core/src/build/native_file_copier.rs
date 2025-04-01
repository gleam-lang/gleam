#[cfg(test)]
mod tests;

use std::collections::{HashMap, HashSet};

use camino::{Utf8Path, Utf8PathBuf};
use ecow::{EcoString, eco_format};

use crate::{
    Error, Result,
    io::{DirWalker, FileSystemReader, FileSystemWriter},
    paths::ProjectPaths,
};

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct CopiedNativeFiles {
    pub any_elixir: bool,
    pub to_compile: Vec<Utf8PathBuf>,
}

pub(crate) struct NativeFileCopier<'a, IO> {
    io: IO,
    paths: ProjectPaths,
    destination_dir: &'a Utf8Path,
    seen_native_files: HashSet<Utf8PathBuf>,
    seen_modules: HashMap<EcoString, Utf8PathBuf>,
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
            paths: ProjectPaths::new(root.into()),
            destination_dir: out,
            to_compile: Vec::new(),
            seen_native_files: HashSet::new(),
            seen_modules: HashMap::new(),
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

        let src = self.paths.src_directory();
        self.copy_files(&src)?;

        let dev = self.paths.test_or_dev_directory(&self.io);
        if self.io.is_directory(&dev) {
            self.copy_files(&dev)?;
        }

        // Sort for deterministic output
        self.to_compile.sort_unstable();

        Ok(CopiedNativeFiles {
            to_compile: self.to_compile,
            any_elixir: self.elixir_files_copied,
        })
    }

    fn copy_files(&mut self, src_root: &Utf8Path) -> Result<()> {
        let mut dir_walker = DirWalker::new(src_root.to_path_buf());
        while let Some(path) = dir_walker.next_file(&self.io)? {
            self.copy(path, &src_root)?;
        }
        Ok(())
    }

    fn copy(&mut self, file: Utf8PathBuf, src_root: &Utf8Path) -> Result<()> {
        let extension = file.extension().unwrap_or_default();

        let relative_path = file
            .strip_prefix(src_root)
            .expect("copy_native_files strip prefix")
            .to_path_buf();

        // No need to run duplicate native file checks for .gleam files, but we
        // still need to check for conflicting `.gleam` and `.mjs` files, so we
        // add a special case for `.gleam`.
        if extension == "gleam" {
            self.check_for_conflicting_javascript_modules(&relative_path)?;

            return Ok(());
        }

        // Skip unknown file formats that are not supported native files
        if !crate::io::is_native_file_extension(extension) {
            return Ok(());
        }

        let destination = self.destination_dir.join(&relative_path);

        // Check that this native file was not already copied
        self.check_for_duplicate(&relative_path)?;

        // Check for JavaScript modules conflicting between each other within
        // the same relative path. We need to do this as '.gleam' files can
        // also cause a conflict, despite not being native files, as they are
        // compiled to `.mjs`.
        self.check_for_conflicting_javascript_modules(&relative_path)?;

        // Check for Erlang modules conflicting between each other anywhere in
        // the tree.
        self.check_for_conflicting_erlang_modules(&relative_path)?;

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
        if let Some(parent) = destination.parent() {
            self.io.mkdir(parent)?;
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

    /// Gleam files are compiled to `.mjs` files, which must not conflict with
    /// an FFI `.mjs` file with the same name, so we check for this case here.
    fn check_for_conflicting_javascript_modules(
        &mut self,
        relative_path: &Utf8PathBuf,
    ) -> Result<(), Error> {
        let mjs_path = match relative_path.extension() {
            Some("gleam") => eco_format!("{}", relative_path.with_extension("mjs")),
            Some("mjs") => eco_format!("{}", relative_path),
            _ => return Ok(()),
        };

        // Insert the full relative `.mjs` path in `seen_modules` as there is
        // no conflict if two `.mjs` files have the same name but are in
        // different subpaths, unlike Erlang files.
        let existing = self
            .seen_modules
            .insert(mjs_path.clone(), relative_path.clone());

        // If there was no already existing one then there's no problem.
        let Some(existing) = existing else {
            return Ok(());
        };

        let existing_is_gleam = existing.extension() == Some("gleam");
        if existing_is_gleam || relative_path.extension() == Some("gleam") {
            let (gleam_file, native_file) = if existing_is_gleam {
                (&existing, relative_path)
            } else {
                (relative_path, &existing)
            };
            return Err(Error::ClashingGleamModuleAndNativeFileName {
                module: eco_format!("{}", gleam_file.with_extension("")),
                gleam_file: gleam_file.clone(),
                native_file: native_file.clone(),
            });
        }

        // The only way for two `.mjs` files to clash is by having
        // the exact same path.
        assert_eq!(&existing, relative_path);
        return Err(Error::DuplicateSourceFile {
            file: existing.to_string(),
        });
    }

    /// Erlang module files cannot have the same name regardless of their
    /// relative positions within the project. Ensure we raise an error if the
    /// user attempts to create `.erl` files with the same name.
    fn check_for_conflicting_erlang_modules(
        &mut self,
        relative_path: &Utf8PathBuf,
    ) -> Result<(), Error> {
        // Ideally we'd check for `.gleam` files here as well. However, it is
        // actually entirely legitimate to receive precompiled `.erl` files for
        // each `.gleam` file from Hex, so this would prompt an error for every
        // package downloaded from Hex, which we do not want.
        if !matches!(relative_path.extension(), Some("erl")) {
            return Ok(());
        }

        // Insert just the `.erl` module filename in `seen_modules` instead of
        // its full relative path, because `.erl` files with the same name
        // cause a conflict when targetting Erlang regardless of subpath.
        let erl_file = relative_path.file_name().expect("path has file name");
        let erl_string = eco_format!("{}", erl_file);

        if let Some(first) = self.seen_modules.insert(erl_string, relative_path.clone()) {
            return Err(Error::DuplicateNativeErlangModule {
                module: eco_format!("{}", relative_path.file_stem().expect("path has file stem")),
                first,
                second: relative_path.clone(),
            });
        }

        Ok(())
    }
}
