import gleam/erlang/atom.{Atom}
import glisten/socket.{Socket}

pub external type FileDescriptor

pub external fn size(path: BitString) -> Int =
  "filelib" "file_size"

pub external fn sendfile(
  file_descriptor: FileDescriptor,
  socket: Socket,
  offset: Int,
  bytes: Int,
  options: List(a),
) -> Result(Int, Atom) =
  "file" "sendfile"

pub type FileError {
  IsDir
  NoAccess
  NoEntry
  UnknownFileError
}

pub external fn open(file: BitString) -> Result(FileDescriptor, FileError) =
  "mist_ffi" "file_open"
