#[cfg(test)]
mod tests;

use std::time::SystemTime;

use camino::{Utf8Path, Utf8PathBuf};

use ecow::EcoString;
use serde::{Deserialize, Serialize};

use super::{
    package_compiler::{module_name, CacheMetadata, CachedModule, Input, UncompiledModule},
    package_loader::CodegenRequired,
    Mode, Origin, Target,
};
use crate::{
    error::{FileIoAction, FileKind},
    io::{CommandExecutor, FileSystemReader, FileSystemWriter},
    warning::WarningEmitter,
    Error, Result,
};

#[derive(Debug, PartialEq, Clone, Serialize, Deserialize)]
pub(crate) struct SourceFingerprint(u64);

impl SourceFingerprint {
    pub(crate) fn new(source: &str) -> Self {
        SourceFingerprint(xxhash_rust::xxh3::xxh3_64(source.as_bytes()))
    }
}

#[derive(Debug)]
pub(crate) struct ModuleLoader<'a, IO> {
    pub io: IO,
    pub warnings: &'a WarningEmitter,
    pub mode: Mode,
    pub target: Target,
    pub codegen: CodegenRequired,
    pub package_name: &'a EcoString,
    pub source_directory: &'a Utf8Path,
    pub artefact_directory: &'a Utf8Path,
    pub origin: Origin,
}

impl<'a, IO> ModuleLoader<'a, IO>
where
    IO: FileSystemReader + FileSystemWriter + CommandExecutor + Clone,
{
    /// Load a module from the given path.
    ///
    /// If the module has been compiled before and the source file has not been
    /// changed since then, load the precompiled data instead.
    ///
    /// Whether the module has changed or not is determined by comparing the
    /// modification time of the source file with the value recorded in the
    /// `.timestamp` file in the artefact directory.
    pub fn load(&self, path: Utf8PathBuf) -> Result<Input> {
        let name = module_name(self.source_directory, &path);
        let artefact = name.replace("/", "@");
        let source_mtime = self.io.modification_time(&path)?;

        let read_source = |name| self.read_source(path, name, source_mtime);

        let meta = match self.read_cache_metadata(&artefact)? {
            Some(meta) => meta,
            None => return read_source(name).map(Input::New),
        };

        // The cache currently does not contain enough data to perform codegen,
        // so if codegen is required in this compiler run then we must check
        // that codegen has already been performed before using a cache.
        if self.codegen.is_required() && !meta.codegen_performed {
            tracing::debug!(?name, "codegen_required_cache_insufficient");
            return read_source(name).map(Input::New);
        }

        // If the timestamp of the source is newer than the cache entry and
        // the hash of the source differs from the one in the cache entry,
        // then we need to recompile.
        if meta.mtime < source_mtime {
            let source_module = read_source(name.clone())?;
            if meta.fingerprint != SourceFingerprint::new(&source_module.code) {
                tracing::debug!(?name, "cache_stale");
                return Ok(Input::New(source_module));
            }
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
        path: Utf8PathBuf,
        name: EcoString,
        mtime: SystemTime,
    ) -> Result<UncompiledModule, Error> {
        read_source(
            self.io.clone(),
            self.target,
            self.origin,
            path,
            name,
            self.package_name.clone(),
            mtime,
        )
    }

    fn cached(&self, name: EcoString, meta: CacheMetadata) -> CachedModule {
        CachedModule {
            dependencies: meta.dependencies,
            source_path: self.source_directory.join(format!("{}.gleam", name)),
            origin: self.origin,
            name,
        }
    }
}

pub(crate) fn read_source<IO>(
    io: IO,
    target: Target,
    origin: Origin,
    path: Utf8PathBuf,
    name: EcoString,
    package_name: EcoString,
    mtime: SystemTime,
) -> Result<UncompiledModule>
where
    IO: FileSystemReader + FileSystemWriter + CommandExecutor + Clone,
{
    let code: EcoString = io.read(&path)?.into();

    let parsed = crate::parse::parse_module(&code).map_err(|error| Error::Parse {
        path: path.clone(),
        src: code.clone(),
        error,
    })?;
    let mut ast = parsed.module;
    let extra = parsed.extra;
    let dependencies = ast.dependencies(target);

    ast.name = name.clone();
    let module = UncompiledModule {
        package: package_name,
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
