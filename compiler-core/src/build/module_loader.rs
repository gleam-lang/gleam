#[cfg(test)]
mod tests;

use std::{
    collections::HashMap,
    path::{Path, PathBuf},
    time::SystemTime,
};

use super::{
    package_compiler::{module_name, CacheMetadata, CachedModule, Input, UncompiledModule},
    package_loader::CodegenRequired,
    Mode, Origin, Target,
};
use crate::{
    error::{FileIoAction, FileKind},
    io::{CommandExecutor, FileSystemIO},
    Error, Result,
};

#[derive(Debug)]
pub(crate) struct ModuleLoader<'a, IO> {
    pub io: IO,
    pub mode: Mode,
    pub target: Target,
    pub codegen: CodegenRequired,
    pub package_name: &'a str,
    pub source_directory: &'a Path,
    pub artefact_directory: &'a Path,
    pub origin: Origin,
}

impl<'a, IO> ModuleLoader<'a, IO>
where
    IO: FileSystemIO + CommandExecutor + Clone,
{
    /// Load a module from the given path.
    ///
    /// If the module has been compiled before and the source file has not been
    /// changed since then, load the precompiled data instead.
    ///
    /// Whether the module has changed or not is determined by comparing the
    /// modification time of the source file with the value recorded in the
    /// `.timestamp` file in the artefact directory.
    pub fn load(&self, path: PathBuf) -> Result<Input> {
        let name = module_name(self.source_directory, &path);
        let artefact = name.replace("/", "@");
        let source_mtime = self.io.modification_time(&path)?;

        let read_source = |name| Ok(Input::New(self.read_source(path, name, source_mtime)?));

        let meta = match self.read_cache_metadata(&artefact)? {
            Some(meta) => meta,
            None => return read_source(name),
        };

        // If there's a timestamp and it's newer or the same than the source
        // file modification time then we read the cached data.
        if meta.mtime < source_mtime {
            tracing::debug!(?name, "cache_stale");
            return read_source(name);
        }

        // The cache currently does not contain enough data to perform codegen,
        // so if codegen is required in this compiler run then we must check
        // that codegen has already been performed before using a cache.
        if self.codegen.is_required() && !meta.codegen_performed {
            tracing::debug!(?name, "codegen_required_cache_insufficient");
            return read_source(name);
        }

        Ok(Input::Cached(self.cached(name, meta)))
    }

    /// Read the timestamp file from the artefact directory for the given
    /// artefact slug. If the file does not exist, return `None`.
    fn read_cache_metadata(&self, artefact: &str) -> Result<Option<CacheMetadata>> {
        let meta_path = self
            .artefact_directory
            .join(artefact)
            .with_extension("cache_meta");

        if !self.io.is_file(&meta_path) {
            return Ok(None);
        }

        let binary = self.io.read_bytes(&meta_path)?;
        let cache_metadata = CacheMetadata::from_binary(&binary).map_err(|e| -> Error {
            Error::FileIo {
                action: FileIoAction::Parse,
                kind: FileKind::File,
                path: meta_path,
                err: Some(e),
            }
        })?;
        Ok(Some(cache_metadata))
    }

    fn read_source(
        &self,
        path: PathBuf,
        name: String,
        mtime: SystemTime,
    ) -> Result<UncompiledModule, Error> {
        read_source(
            self.io.clone(),
            self.target,
            self.origin,
            path,
            name,
            &self.package_name,
            mtime,
        )
    }

    fn cached(&self, name: String, meta: CacheMetadata) -> CachedModule {
        CachedModule {
            dependencies: meta.dependencies,
            source_path: self.source_directory.join(format!("{}.gleam", name)),
            origin: self.origin,
            name,
        }
    }
}

pub(crate) fn read_source<IO: FileSystemIO + CommandExecutor + Clone>(
    io: IO,
    target: Target,
    origin: Origin,
    path: PathBuf,
    name: String,
    package_name: &str,
    mtime: SystemTime,
) -> Result<UncompiledModule> {
    let code = io.read(&path)?;

    let (mut ast, extra) = crate::parse::parse_module(&code).map_err(|error| Error::Parse {
        path: path.clone(),
        src: code.clone(),
        error,
    })?;

    let dependencies = ast.dependencies(target);

    ast.name = name.as_str().into();
    let module = UncompiledModule {
        package: package_name.to_string(),
        dependencies,
        origin,
        extra,
        mtime,
        path,
        name,
        code,
        ast,
    };
    Ok(module)
}
