//// MemTable is an in-memory data structure that holds the latest written
//// records. It is a sorted list of records sorted by the key hash. When the
//// MemTable reaches its capacity, it is flushed to disk as a Table (Sorted
//// String Table or SSTable). See the sstable module for more details.

import gleam/bit_string
import gleam/list
import gleam/map.{Map}
import gleam/option.{None}
import ream/storage/kv/value.{Value}

/// MemTable holds a sorted list of the latest written records. Generally, we
/// could implement a sorted list algorithm like skip list, but for simplicity
/// we use a map instead.
///
/// MemTables have a max capacity and when that is reached, we flush the MemTable
/// to disk as a Table (Sorted String Table or SSTable). See the sstable module
/// for more details.
///
/// The MemTable structure is as follows:
/// - `entries`: a map of key hash to MemTableEntry. The key hash is the hash of
///   the key string.
/// - `size`: the total size of the MemTable in bytes.
/// - `max_size`: the maximum size of the MemTable in bytes.
pub type MemTable {
  MemTable(entries: Map(Int, MemTableEntry), size: Int, max_size: Int)
}

/// MemTableEntry is a single entry in the MemTable.
pub type MemTableEntry {
  MemTableEntry(key: String, value: Value)
}

/// Reason is the reason why a MemTable operation failed.
pub type Reason {
  /// The MemTable is full and cannot accept more entries.
  CapacityExceeded
}

/// The key hash size is 32-bit integer
pub const key_hash_size_bytes = 4

/// The key string size data is 16-bit integer
pub const key_size_bytes = 2

/// The file ID size is 128-bit integer corresponding to the UUID
/// of the file in binary format.
pub const file_id_size_bytes = 16

/// The file offset size is 32-bit integer.
pub const file_offset_size_bytes = 4

/// The payload size for the memtable entry, it is the sum of the
/// key hash size, file ID size, file offset size and key size.
pub const payload_size_bytes = 26

pub fn new(max_size: Int) -> MemTable {
  MemTable(entries: map.new(), size: 0, max_size: max_size)
}

/// calculate_entries_size calculates the total size of the entries in the
/// MemTable. It is mainly used when we load the MemTable from disk.
pub fn from_entries(entries: Map(Int, MemTableEntry), max_size: Int) -> MemTable {
  let size = calculate_entries_size(entries)
  MemTable(entries: entries, size: size, max_size: max_size)
}

/// entry_to_bitstring converts a MemTableEntry to a bitstring. It is mainly used
/// when we need to convert the MemTableEntry to its binary representation.
pub fn entry_to_bitstring(entry: MemTableEntry) -> BitString {
  let key_hash = hash(entry.key)
  let key_string = bit_string.from_string(entry.key)
  let key_size = bit_string.byte_size(key_string)
  let file_id = entry.value.file_id
  let file_offset = entry.value.offset
  <<
    key_hash:128,
    key_size:16,
    file_id:128,
    file_offset:32,
    key_string:bit_string,
  >>
}

/// bitstring_to_entry converts a bitstring to a MemTableEntry. It is mainly used
/// when we need to convert the binary representation of a MemTableEntry to a
/// MemTableEntry.
pub fn bitstring_to_entry(bitstring: BitString) -> #(Int, MemTableEntry) {
  let <<
    key_hash:128,
    _key_size:16,
    file_id:128,
    file_offset:32,
    key_string:bit_string,
  >> = bitstring
  let assert Ok(key) = bit_string.to_string(key_string)
  let value =
    Value(data: None, deleted: False, file_id: file_id, offset: file_offset)
  #(key_hash, MemTableEntry(key: key, value: value))
}

/// contains checks if the MemTable contains the given key.
pub fn contains(mem_table: MemTable, key: String) -> Bool {
  map.has_key(mem_table.entries, hash(key))
}

/// generates a hash for the given key.
pub external fn hash(key: String) -> Int =
  "erlang" "phash2"

/// set sets the given key to the given value in the MemTable. If the MemTable
/// reaches its capacity, it returns an error. Otherwise, it returns the updated
/// MemTable.
pub fn set(
  mem_table: MemTable,
  key: String,
  value: Value,
) -> Result(MemTable, Reason) {
  let key_hash = hash(key)
  case map.get(mem_table.entries, key_hash) {
    Error(Nil) -> {
      let entry = MemTableEntry(key, value)
      let entry_size = calculate_size(entry)
      let current_size = mem_table.size + entry_size
      case current_size > mem_table.max_size {
        True -> Error(CapacityExceeded)
        False ->
          Ok(
            MemTable(
              ..mem_table,
              entries: map.insert(mem_table.entries, key_hash, entry),
              size: current_size,
            ),
          )
      }
    }
    Ok(old_entry) -> {
      let old_entry_size = calculate_size(old_entry)
      let entry = MemTableEntry(..old_entry, value: value)
      let current_entry_size = calculate_size(entry)
      let mem_table_size = mem_table.size + current_entry_size - old_entry_size
      case mem_table_size > mem_table.max_size {
        True -> Error(CapacityExceeded)
        False ->
          Ok(
            MemTable(
              ..mem_table,
              entries: map.insert(mem_table.entries, key_hash, entry),
              size: mem_table_size,
            ),
          )
      }
    }
  }
}

/// deletes the given key from the MemTable. If the key does not exist, it
/// returns the original MemTable. Otherwise, it returns the updated MemTable.
pub fn delete(mem_table: MemTable, key: String) -> MemTable {
  let key_hash = hash(key)
  case map.get(mem_table.entries, key_hash) {
    Error(Nil) -> mem_table
    Ok(entry) -> {
      MemTable(
        ..mem_table,
        entries: map.delete(mem_table.entries, key_hash),
        size: mem_table.size - calculate_size(entry),
      )
    }
  }
}

/// gets the value of the given key from the MemTable. If the key does not
/// exist, it returns an error.
pub fn get(mem_table: MemTable, key: String) -> Result(Value, Nil) {
  case map.get(mem_table.entries, hash(key)) {
    Ok(MemTableEntry(key: stored_key, value: value)) if key == stored_key ->
      Ok(value)
    Ok(MemTableEntry(key: _stored_key, value: _)) -> {
      // TODO better panic messages when https://github.com/gleam-lang/gleam/issues/2176 is fixed
      // let errmsg =
      //   "collision hash function between " <> key <> " and " <> stored_key
      panic
    }
    Error(Nil) -> Error(Nil)
  }
}

/// search for a pivot in the MemTable. The pivot is the middle key in the
/// MemTable.
pub fn search_pivot(mem_table: MemTable) -> Int {
  let entries = mem_table.entries
  let keys = map.keys(entries)
  let entries_count = map.size(entries)
  let #(_, [pivot, ..]) = list.split(keys, at: entries_count / 2)
  pivot
}

/// splits the MemTable into two MemTables. The first MemTable contains
/// MemTableEntries with keys less than the pivot. The second MemTable contains
/// MemTableEntries with keys greater than or equal to the pivot. The pivot is
/// the middle key in the MemTable.
pub fn split(mem_table: MemTable, pivot: Int) -> #(MemTable, MemTable, Int) {
  let #(low_entries, high_entries) =
    mem_table.entries
    |> map.to_list()
    |> list.partition(fn(entry) { entry.0 < pivot })

  let low_entries = map.from_list(low_entries)

  let low =
    MemTable(
      ..mem_table,
      entries: low_entries,
      size: calculate_entries_size(low_entries),
    )

  let high_entries = map.from_list(high_entries)

  let high =
    MemTable(
      ..mem_table,
      entries: high_entries,
      size: calculate_entries_size(high_entries),
    )

  case low.size >= high.size, map.get(high.entries, pivot) {
    True, _ -> #(low, high, pivot)
    False, Error(Nil) -> #(low, high, pivot + 1)
    False, Ok(entry) -> {
      let high =
        MemTable(
          ..high,
          entries: map.delete(high.entries, pivot),
          size: high.size - calculate_size(entry),
        )
      let low =
        MemTable(
          ..low,
          entries: map.insert(low.entries, pivot, entry),
          size: low.size + calculate_size(entry),
        )
      #(low, high, pivot + 1)
    }
  }
}

fn calculate_entries_size(entries: Map(Int, MemTableEntry)) -> Int {
  map.fold(entries, 0, fn(acc, _key, entry) { acc + calculate_size(entry) })
}

fn calculate_size(entry: MemTableEntry) -> Int {
  bit_string.byte_size(entry_to_bitstring(entry))
}

/// get_bounds returns the lower and higher bounds of the MemTable. If the
/// MemTable is empty, it returns `#(0, 0)`.
pub fn get_bounds(mem_table: MemTable) -> #(Int, Int) {
  case map.to_list(mem_table.entries) {
    [] -> #(0, 0)
    [#(k, _), ..entries] -> {
      list.fold(
        entries,
        #(k, k),
        fn(acc: #(Int, Int), entry) {
          #(get_lower(acc.0, entry.0), get_higher(acc.1, entry.0))
        },
      )
    }
  }
}

fn get_lower(lower_bound: Int, key: Int) -> Int {
  case lower_bound {
    0 -> key
    lower_bound if key < lower_bound -> key
    lower_bound -> lower_bound
  }
}

fn get_higher(higher_bound: Int, key: Int) -> Int {
  case higher_bound {
    0 -> key
    higher_bound if key > higher_bound -> key
    higher_bound -> higher_bound
  }
}
