import gleam/erlang/file
import gleam/erlang/process.{Pid}
import gleam/int
import gleam/result.{try}
import ream/storage/file as fs
import ream/storage/file/read

/// the size of the offset in bytes, it's 6 bytes or 48 bits
/// letting us store offsets of 2^48 bytes (256TB) of data
pub const offset_size_bits = 48

/// event size (in bits) is storing letting us store events 
/// of 2^24 bits (16MB) of data
pub const event_size_bits = 24

/// the size of the file id in bytes, it's 16 bytes or 128 bits
/// representing a 128 bit integer (UUID)
pub const file_id_size_bits = 128

/// the size of the index entry in bytes, it's 25 bytes because
/// we're storing the offset (6 bytes), the event size (3 bytes)
/// and the file id (16 bytes).
pub const index_size_bytes = 25

pub type Index {
  Index(offset: Int, size: Int, file_id: Int)
}

pub type IndexFile {
  IndexFile(handler: Pid, size: Int, file_path: String)
}

pub fn open(path: String) -> Result(IndexFile, file.Reason) {
  let assert Ok(True) = fs.recursive_make_directory(path)
  let index = fs.join([path, "index"])
  use index_pid <- try(fs.open(index, [fs.Read, fs.Append]))
  use index_info <- try(file.file_info(index))
  Ok(IndexFile(index_pid, index_info.size, index))
}

pub fn close(index_file: IndexFile) -> Result(Nil, file.Reason) {
  let assert Ok(_) = fs.close(index_file.handler)
  Ok(Nil)
}

pub fn add(
  index_file: IndexFile,
  event_size: Int,
  file_id: Int,
) -> #(Index, IndexFile) {
  let event_size_bytes = event_size_bits / 8
  let #(index_content, index) = case index_file.size {
    0 -> {
      let index = Index(0, event_size + event_size_bytes, file_id)
      #(index_binary(index), index)
    }
    _ -> {
      let assert Ok(_) =
        fs.position(index_file.handler, fs.Eof(-index_size_bytes))
      // FIXME: https://github.com/gleam-lang/gleam/issues/2166
      let offset_size_bits = offset_size_bits
      let event_size_bits = event_size_bits
      let file_id_size_bits = file_id_size_bits
      // end FIXME
      let assert Ok(<<
        offset:size(offset_size_bits),
        prev_size:size(event_size_bits),
        _file_id:size(file_id_size_bits),
      >>) = last_entry_for_file(index_file.handler, file_id)
      let offset = offset + prev_size
      let index = Index(offset, event_size + event_size_bytes, file_id)
      #(index_binary(index), index)
    }
  }
  let assert Ok(_) = fs.write(index_file.handler, index_content)
  let index_file =
    IndexFile(..index_file, size: index_file.size + index_size_bytes)
  #(index, index_file)
}

fn index_binary(index: Index) -> BitString {
  // FIXME: https://github.com/gleam-lang/gleam/issues/2166
  let offset_size_bits = offset_size_bits
  let event_size_bits = event_size_bits
  let file_id_size_bits = file_id_size_bits
  // end FIXME
  <<
    index.offset:size(offset_size_bits),
    index.size:size(event_size_bits),
    index.file_id:size(file_id_size_bits),
  >>
}

fn last_entry_for_file(
  index_file: Pid,
  file_id: Int,
) -> Result(BitString, file.Reason) {
  // FIXME: https://github.com/gleam-lang/gleam/issues/2166
  let offset_size_bits = offset_size_bits
  let event_size_bits = event_size_bits
  let file_id_size_bits = file_id_size_bits
  // end FIXME
  let assert read.Ok(<<
    offset:size(offset_size_bits),
    size:size(event_size_bits),
    new_file_id:size(file_id_size_bits),
  >>) = fs.read(index_file, index_size_bytes)
  case new_file_id == file_id {
    True -> Ok(index_binary(Index(offset, size, file_id)))
    False -> {
      case fs.position(index_file, fs.Cur(-2 * index_size_bytes)) {
        Ok(_) -> last_entry_for_file(index_file, file_id)
        Error(file.Einval) -> Ok(index_binary(Index(0, 0, file_id)))
      }
    }
  }
}

pub fn count(index_file: IndexFile) -> Int {
  let assert Ok(result) = int.divide(index_file.size, index_size_bytes)
  result
}

pub fn set_pos(index_file: IndexFile, index: Int) -> Result(Int, file.Reason) {
  fs.position(index_file.handler, fs.Bof(index * index_size_bytes))
}

pub fn get_next(index_file: IndexFile) -> Result(Index, file.Reason) {
  // FIXME: https://github.com/gleam-lang/gleam/issues/2166
  let offset_size_bits = offset_size_bits
  let event_size_bits = event_size_bits
  let file_id_size_bits = file_id_size_bits
  // end FIXME
  case fs.read(index_file.handler, index_size_bytes) {
    read.Ok(<<
      offset:size(offset_size_bits),
      size:size(event_size_bits),
      file_id:size(file_id_size_bits),
    >>) -> Ok(Index(offset, size, file_id))
    read.Eof -> Error(file.Espipe)
    _ -> Error(file.Einval)
  }
}

pub fn get(index_file: IndexFile, index: Int) -> Result(Index, file.Reason) {
  case count(index_file) > index {
    True -> {
      let assert Ok(_) = set_pos(index_file, index)
      get_next(index_file)
    }
    False -> Error(file.Einval)
  }
}
