use crate::{
    error::ShellCommandFailureReason,
    io::{Command, CommandExecutor, FileSystemReader, FileSystemWriter, Stdio},
    Error,
};
use camino::Utf8PathBuf;

#[cfg(not(target_os = "windows"))]
const ELIXIR_EXECUTABLE: &str = "elixir";
#[cfg(target_os = "windows")]
const ELIXIR_EXECUTABLE: &str = "elixir.bat";

// These Elixir core libs will be loaded with the current project
const ELIXIR_LIBS: [&str; 4] = ["eex", "elixir", "logger", "mix"];

pub struct ElixirLibraries<'a, IO> {
    io: &'a IO,
    build_dir: &'a Utf8PathBuf,
    subprocess_stdio: Stdio,
}

impl<'a, IO> ElixirLibraries<'a, IO> {
    fn new(io: &'a IO, build_dir: &'a Utf8PathBuf, subprocess_stdio: Stdio) -> Self {
        Self {
            io,
            build_dir,
            subprocess_stdio,
        }
    }
}

impl<'a, IO> ElixirLibraries<'a, IO>
where
    IO: CommandExecutor + FileSystemReader + FileSystemWriter + Clone,
{
    pub fn make_available(
        io: &'a IO,
        build_dir: &'a Utf8PathBuf,
        subprocess_stdio: Stdio,
    ) -> Result<(), Error> {
        let it = Self::new(io, build_dir, subprocess_stdio);
        let result = it.run();

        if result.is_err() {
            it.cleanup();
        }

        result
    }

    fn cleanup(&self) {
        self.io
            .delete_file(&self.paths_cache_path())
            .expect("deleting paths cache in cleanup");
    }

    fn paths_cache_filename(&self) -> &'static str {
        "gleam_elixir_paths"
    }

    fn paths_cache_path(&self) -> Utf8PathBuf {
        self.build_dir.join(self.paths_cache_filename())
    }

    fn run(&self) -> Result<(), Error> {
        // The pathfinder is a file in build/{target}/erlang
        // It contains the full path for each Elixir core lib we need, new-line delimited
        // The pathfinder saves us from repeatedly loading Elixir to get this info
        let mut update_links = false;
        let cache = self.paths_cache_path();
        if !self.io.is_file(&cache) {
            // The pathfinder must be written
            // Any existing core lib links will get updated
            update_links = true;
            // TODO: test
            let env = vec![("TERM".to_string(), "dumb".to_string())];
            // Prepare the libs for Erlang's code:lib_dir function
            let elixir_atoms: Vec<String> =
                ELIXIR_LIBS.iter().map(|lib| format!(":{}", lib)).collect();
            // Use Elixir to find its core lib paths and write the pathfinder file
            let args = vec![
            "--eval".to_string(),
            format!(
                ":ok = File.write(~s({}), [{}] |> Stream.map(fn(lib) -> lib |> :code.lib_dir |> Path.expand end) |> Enum.join(~s(\\n)))",
                self.paths_cache_filename(),
                elixir_atoms.join(", "),
            ),
        ];
            tracing::debug!("writing_elixir_paths_to_build");
            let status = self.io.exec(Command {
                program: ELIXIR_EXECUTABLE.into(),
                args,
                env,
                cwd: Some(self.build_dir.clone()),
                stdio: self.subprocess_stdio,
            })?;
            if status != 0 {
                return Err(Error::ShellCommand {
                    program: "elixir".into(),
                    reason: ShellCommandFailureReason::Unknown,
                });
            }
        }

        // Each pathfinder line is a system path for an Elixir core library
        let read_pathfinder = self.io.read(&cache)?;
        for lib_path in read_pathfinder.split('\n') {
            let source = Utf8PathBuf::from(lib_path);
            let name = source.as_path().file_name().expect(&format!(
                "Unexpanded path in {}",
                self.paths_cache_filename()
            ));
            let dest = self.build_dir.join(name);
            let ebin = dest.join("ebin");
            if !update_links || self.io.is_directory(&ebin) {
                // Either links don't need updating
                // Or this library is already linked
                continue;
            }
            // TODO: unit test
            if self.io.is_directory(&dest) {
                // Delete the existing link
                self.io.delete_directory(&dest)?;
            }
            tracing::debug!("linking_{}_to_build", name,);
            self.io.symlink_dir(&source, &dest)?;
        }

        Ok(())
    }
}
