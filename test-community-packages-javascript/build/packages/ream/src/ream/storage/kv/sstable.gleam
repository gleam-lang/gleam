import gleam/erlang/file
import gleam/erlang/process.{Pid}
import gleam/map.{Map}
import ream/storage/file as fs
import ream/storage/file/read
import ream/storage/kv/memtable.{MemTable, MemTableEntry}

const header_size = 38

pub fn flush(mem_table: MemTable, path: String) -> Result(Bool, file.Reason) {
  let assert Ok(True) = fs.recursive_make_directory(fs.dirname(path))
  let assert Ok(file) = fs.open(path, [fs.Write])
  // TODO maybe suggest the inclusion of `map.each/2` for gleam/stdlib
  map.filter(
    mem_table.entries,
    fn(_key, entry) {
      let content = memtable.entry_to_bitstring(entry)
      let assert Ok(_) = fs.write(file, content)
      False
    },
  )
  let assert Ok(_) = fs.close(file)
  Ok(True)
}

pub fn load(path: String, max_size: Int) -> Result(MemTable, file.Reason) {
  let assert Ok(file) = fs.open(path, [fs.Read])
  let assert Ok(entries) = read_entries(file, map.new())
  let assert Ok(_) = fs.close(file)
  Ok(memtable.from_entries(entries, max_size))
}

fn read_entries(
  file: Pid,
  entries: Map(Int, MemTableEntry),
) -> Result(Map(Int, MemTableEntry), file.Reason) {
  case fs.read(file, header_size) {
    read.Ok(<<key_hash:128, key_size:16, file_id:128, offset:32>>) -> {
      let assert read.Ok(key_string) = fs.read(file, key_size)
      let #(_key, entry) =
        memtable.bitstring_to_entry(<<
          key_hash:128,
          key_size:16,
          file_id:128,
          offset:32,
          key_string:bit_string,
        >>)
      let entries = map.insert(entries, key_hash, entry)
      read_entries(file, entries)
    }
    read.Eof -> Ok(entries)
    read.Error(reason) -> Error(reason)
  }
}
