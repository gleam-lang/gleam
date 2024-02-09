import gleam/erlang/process.{Pid}
import gleam/erlang/file
import ream/storage/file/read
import ream/storage/file/close
import ream/storage/file/write

pub type Endian {
  Big
  Little
}

pub type EncodingType {
  Unicode
  Utf8
  Utf16(Endian)
  Utf32(Endian)
  Latin1
}

pub type Mode {
  Read
  Write
  Append
  Exclusive
  Raw
  Binary
  DelayedWrite(size: Int, delay: Int)
  ReadAhead(size: Int)
  Compressed
  CompressedOne
  Encoding(encoding: EncodingType)
  Ram
  Sync
  Directory
}

pub type Location {
  Bof(Int)
  Cur(Int)
  Eof(Int)
}

pub fn open(filename: String, mode: List(Mode)) -> Result(Pid, file.Reason) {
  do_open(filename, [Binary, ..mode])
}

pub external fn do_open(
  filename: String,
  mode: List(Mode),
) -> Result(Pid, file.Reason) =
  "file" "open"

pub external fn read(io_device: Pid, bytes: Int) -> read.Result =
  "file" "read"

pub fn close(io_device: Pid) -> Result(Bool, file.Reason) {
  case do_close(io_device) {
    close.Ok -> Ok(True)
    close.Error(reason) -> Error(reason)
  }
}

external fn do_close(io_device: Pid) -> close.Result =
  "file" "close"

pub fn write(io_device: Pid, data: BitString) -> Result(Bool, file.Reason) {
  case do_write(io_device, data) {
    write.Ok -> Ok(True)
    write.Error(reason) -> Error(reason)
  }
}

external fn do_write(io_device: Pid, data: BitString) -> write.Result =
  "file" "write"

pub external fn dirname(filename: String) -> String =
  "filename" "dirname"

pub external fn basename(filename: String) -> String =
  "filename" "basename"

pub external fn join(parts: List(String)) -> String =
  "filename" "join"

pub external fn position(
  io_device: Pid,
  location: Location,
) -> Result(Int, file.Reason) =
  "file" "position"

pub fn recursive_make_directory(path: String) -> Result(Bool, file.Reason) {
  case file.is_directory(path) {
    Error(file.Enoent) -> {
      let prev_dir = dirname(path)
      let assert Ok(True) = recursive_make_directory(prev_dir)
      let assert Ok(_) = file.make_directory(path)
      Ok(True)
    }
    Error(file.Eexist) -> Ok(True)
    Ok(True) -> Ok(True)
    _ -> Error(file.Einval)
  }
}
