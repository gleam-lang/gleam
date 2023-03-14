use std::path::{Path, PathBuf};

use crate::io::FileSystemReader;

/// Given a path, find the nearest parent directory containing a `gleam.toml`
/// file.
pub fn find_gleam_project_parent<IO>(io: &IO, path: &Path) -> Option<PathBuf>
where
    IO: FileSystemReader,
{
    let mut current = path.to_path_buf();
    while current.pop() {
        if io.is_file(&current.join("gleam.toml")) {
            return Some(current);
        }
    }
    None
}

#[cfg(test)]
mod find_gleam_project_parent_tests {
    use super::*;
    use crate::io::{memory::InMemoryFileSystem, FileSystemWriter};

    #[test]
    fn root() {
        let io = InMemoryFileSystem::new();
        assert_eq!(find_gleam_project_parent(&io, Path::new("/")), None);
    }

    #[test]
    fn outside_a_project() {
        let io = InMemoryFileSystem::new();
        assert_eq!(
            find_gleam_project_parent(&io, Path::new("/app/src/one.gleam")),
            None
        );
    }

    #[test]
    fn gleam_toml_itself() {
        let io = InMemoryFileSystem::new();
        io.write(Path::new("/app/gleam.toml"), "").unwrap();
        assert_eq!(
            find_gleam_project_parent(&io, Path::new("/app/gleam.toml")),
            Some(PathBuf::from("/app"))
        );
    }

    #[test]
    fn test_module() {
        let io = InMemoryFileSystem::new();
        io.write(Path::new("/app/gleam.toml"), "").unwrap();
        assert_eq!(
            find_gleam_project_parent(&io, Path::new("/app/test/one/two/three.gleam")),
            Some(PathBuf::from("/app"))
        );
    }

    #[test]
    fn src_module() {
        let io = InMemoryFileSystem::new();
        io.write(Path::new("/app/gleam.toml"), "").unwrap();
        assert_eq!(
            find_gleam_project_parent(&io, Path::new("/app/src/one/two/three.gleam")),
            Some(PathBuf::from("/app"))
        );
    }

    #[test]
    fn nested_projects() {
        let io = InMemoryFileSystem::new();
        io.write(Path::new("/app/gleam.toml"), "").unwrap();
        io.write(Path::new("/app/examples/wibble/gleam.toml"), "")
            .unwrap();
        assert_eq!(
            find_gleam_project_parent(&io, Path::new("/app/src/one.gleam")),
            Some(PathBuf::from("/app"))
        );
        assert_eq!(
            find_gleam_project_parent(&io, Path::new("/app/examples/wibble/src/one.gleam")),
            Some(PathBuf::from("/app/examples/wibble"))
        );
    }
}
