import gleam/erlang/process.{Pid}
import gleam/list
import gleam/map.{Map}
import gleam/option.{None, Option, Some}
import gleam/result.{try}
import ream/storage/file as fs
import ream/storage/file/read
import ream/storage/kv/memtable.{CapacityExceeded, MemTable}
import ream/storage/kv/sstable
import ream/storage/kv/value.{Value, ValueFile}
import ream/uuid

pub type MemTableRange {
  MemTableRange(lower: Int, upper: Int, memtable: Option(MemTable))
}

pub type KV {
  KV(
    base_path: String,
    name: String,
    memtable_ranges: Map(Int, MemTableRange),
    active_value_file: Option(Int),
    values: Map(Int, ValueFile),
    memtables_loaded: Int,
    max_memtables_loaded: Int,
    max_memtable_size: Int,
    max_value_size: Int,
  )
}

pub type KVInfo {
  KVInfo(
    base_path: String,
    name: String,
    values: Int,
    values_size_bytes: Int,
    memtables_total: Int,
    memtables_loaded: Int,
    memtables_loaded_size_bytes: Int,
    max_memtables_loaded: Int,
    max_memtable_size: Int,
    max_value_size: Int,
  )
}

const min_bound = 0

const max_bound = 340_282_366_920_938_463_463_374_607_431_768_211_455

pub fn open(
  path: String,
  name: String,
  max_memtables_loaded: Int,
  max_memtable_size: Int,
  max_value_size: Int,
) -> KV {
  let path = fs.join([path, "kv", name])

  let key_index_file = fs.join([path, "key", "index"])
  let assert Ok(True) = fs.recursive_make_directory(fs.dirname(key_index_file))
  let assert Ok(kv) = fs.open(key_index_file, [fs.Read, fs.Write])
  let ranges =
    read_memtable_ranges(
      kv,
      fs.join([path, "key"]),
      max_memtable_size,
      map.new(),
    )
  let #(memtables_loaded, ranges) = case map.size(ranges) == 0 {
    True -> {
      let memtable = memtable.new(max_memtable_size)
      let ranges =
        [#(new_id(), MemTableRange(min_bound, max_bound, Some(memtable)))]
        |> map.from_list()
      #(1, ranges)
    }
    False -> #(0, ranges)
  }
  let assert Ok(_) = fs.close(kv)

  let value_index_file = fs.join([path, "value", "index"])
  let assert Ok(True) =
    fs.recursive_make_directory(fs.dirname(value_index_file))
  let assert Ok(kv) = fs.open(value_index_file, [fs.Read, fs.Write])
  let #(active_value_file, values) =
    read_values(kv, fs.join([path, "value"]), None, max_value_size, map.new())
  let assert Ok(_) = fs.close(kv)

  KV(
    path,
    name,
    ranges,
    active_value_file,
    values,
    memtables_loaded,
    max_memtables_loaded,
    max_memtable_size,
    max_value_size,
  )
}

fn new_id() -> Int {
  uuid.to_int(uuid.new())
}

fn read_memtable_ranges(
  kv: Pid,
  path: String,
  max_size: Int,
  acc: Map(Int, MemTableRange),
) -> Map(Int, MemTableRange) {
  case fs.read(kv, 48) {
    read.Ok(<<lower:size(128), upper:size(128), id:size(128)>>) -> {
      let range = MemTableRange(lower, upper, None)
      read_memtable_ranges(kv, path, max_size, map.insert(acc, id, range))
    }
    read.Eof -> acc
    read.Error(_err) -> {
      let assert Ok(_) = fs.close(kv)
      // TODO better panic messages when https://github.com/gleam-lang/gleam/issues/2176 is fixed
      panic
    }
  }
}

fn read_values(
  kv: Pid,
  path: String,
  last_file_id: Option(Int),
  max_value_size: Int,
  acc: Map(Int, ValueFile),
) -> #(Option(Int), Map(Int, ValueFile)) {
  case fs.read(kv, 16) {
    read.Ok(<<file_id:size(128)>>) -> {
      let assert Ok(value_file) = value.open(path, file_id, max_value_size)
      read_values(
        kv,
        path,
        Some(file_id),
        max_value_size,
        map.insert(acc, file_id, value_file),
      )
    }
    read.Eof -> #(last_file_id, acc)
    read.Error(_err) -> {
      let assert Ok(_) = fs.close(kv)
      // TODO better panic messages when https://github.com/gleam-lang/gleam/issues/2176 is fixed
      panic
    }
  }
}

fn sstable_path(path: String, id: Int) -> String {
  let parts = uuid.parts(uuid.from_int(id))
  fs.join([path, "key", ..parts])
}

fn find_range(kv: KV, key_hash: Int, max_size: Int) -> #(Int, KV) {
  let max_memtables_loaded = kv.max_memtables_loaded
  let assert #(#(loaded, Some(range_id)), range_list) =
    kv.memtable_ranges
    |> map.to_list()
    |> list.map_fold(
      #(kv.memtables_loaded, None),
      fn(acc, entry) {
        let #(id, range) = entry
        let #(loaded, range_id) = acc
        case
          key_hash >= range.lower && key_hash <= range.upper,
          range.memtable
        {
          True, Some(_) -> #(#(loaded, Some(id)), #(id, range))
          True, None -> {
            let assert Ok(memtable) =
              sstable.load(sstable_path(kv.base_path, id), max_size)
            #(
              #(loaded + 1, Some(id)),
              #(id, MemTableRange(..range, memtable: Some(memtable))),
            )
          }
          False, Some(memtable) if loaded >= max_memtables_loaded -> {
            let assert Ok(_) =
              sstable.flush(memtable, sstable_path(kv.base_path, id))
            #(
              #(loaded - 1, range_id),
              #(id, MemTableRange(..range, memtable: None)),
            )
          }
          False, _ -> #(acc, #(id, range))
        }
      },
    )

  #(
    range_id,
    KV(
      ..kv,
      memtable_ranges: map.from_list(range_list),
      memtables_loaded: loaded,
    ),
  )
}

pub fn close(kv: KV) -> Result(Nil, Nil) {
  let assert Ok(_) = flush(kv)

  map.values(kv.values)
  |> list.each(fn(v) { value.close(v) })

  Ok(Nil)
}

pub fn flush(kv: KV) -> Result(Nil, Nil) {
  let key_index_file = fs.join([kv.base_path, "key", "index"])
  let assert Ok(True) = fs.recursive_make_directory(fs.dirname(key_index_file))
  let assert Ok(kv_file) = fs.open(key_index_file, [fs.Write])
  let memtable_ranges = map.to_list(kv.memtable_ranges)
  let assert Ok(_) =
    write_memtable_ranges(kv_file, kv.base_path, memtable_ranges)
  let assert Ok(_) = fs.close(kv_file)

  let value_index_file = fs.join([kv.base_path, "value", "index"])
  let assert Ok(True) =
    fs.recursive_make_directory(fs.dirname(value_index_file))
  let assert Ok(kv_file) = fs.open(value_index_file, [fs.Write])
  let assert Ok(_) = write_values(kv_file, map.keys(kv.values))
  let assert Ok(_) = fs.close(kv_file)

  Ok(Nil)
}

fn write_memtable_ranges(
  kv_file: Pid,
  base_path: String,
  memtable_ranges: List(#(Int, MemTableRange)),
) -> Result(Nil, Nil) {
  case memtable_ranges {
    [#(id, MemTableRange(lower, upper, Some(memtable))), ..rest] -> {
      let assert Ok(True) = sstable.flush(memtable, sstable_path(base_path, id))
      let assert Ok(_) = fs.write(kv_file, <<lower:128, upper:128, id:128>>)
      write_memtable_ranges(kv_file, base_path, rest)
    }
    [#(id, MemTableRange(lower, upper, None)), ..rest] -> {
      let assert Ok(_) = fs.write(kv_file, <<lower:128, upper:128, id:128>>)
      write_memtable_ranges(kv_file, base_path, rest)
    }
    [] -> Ok(Nil)
  }
}

fn write_values(kv: Pid, values: List(Int)) -> Result(Nil, Nil) {
  case values {
    [id, ..rest] -> {
      let assert Ok(_) = fs.write(kv, <<id:128>>)
      write_values(kv, rest)
    }
    [] -> Ok(Nil)
  }
}

pub fn get(kv: KV, key: String) -> #(Result(BitString, Nil), KV) {
  let key_hash = memtable.hash(key)
  let #(range_id, kv) = find_range(kv, key_hash, kv.max_memtable_size)
  let assert Ok(range) = map.get(kv.memtable_ranges, range_id)
  let assert Some(memtable) = range.memtable
  case memtable.get(memtable, key) {
    Ok(value) -> {
      let assert Ok(vfile) = map.get(kv.values, value.file_id)
      case value.read(vfile, value.offset) {
        Ok(Value(deleted: False, file_id: _, offset: _, data: Some(data))) -> #(
          Ok(data),
          kv,
        )
        _ -> #(Error(Nil), kv)
      }
    }
    Error(_err) -> #(Error(Nil), kv)
  }
}

pub fn set(kv: KV, key: String, value: BitString) -> KV {
  let key_hash = memtable.hash(key)
  let #(range_id, kv) = find_range(kv, key_hash, kv.max_memtable_size)
  let assert Ok(range) = map.get(kv.memtable_ranges, range_id)
  let assert Some(memtable) = range.memtable
  let kv = case kv.active_value_file {
    Some(_file_id) -> kv
    None -> {
      let assert Ok(vfile) =
        value.create(fs.join([kv.base_path, "value"]), kv.max_value_size)
      KV(
        ..kv,
        active_value_file: Some(vfile.id),
        values: map.insert(kv.values, vfile.id, vfile),
      )
    }
  }
  case memtable.get(memtable, key) {
    Ok(old_value) -> {
      // key is in the index, we have to replace it
      case store_value(kv, key, range_id, range, memtable, value) {
        Ok(kv) -> kv
        Error(CapacityExceeded) -> {
          let kv = split(kv, key_hash, range_id, range, memtable)
          set(kv, key, value)
        }
      }
      let assert Ok(kv) = delete_value(kv, old_value)
      kv
    }
    Error(Nil) -> {
      // key isn't in the index yet, insert it as a new key
      case store_value(kv, key, range_id, range, memtable, value) {
        Ok(kv) -> kv
        Error(CapacityExceeded) -> {
          let kv = split(kv, key_hash, range_id, range, memtable)
          set(kv, key, value)
        }
      }
    }
  }
}

fn split(
  kv: KV,
  key_hash: Int,
  range_id: Int,
  range: MemTableRange,
  memtable: MemTable,
) -> KV {
  let #(memtable_low, memtable_high, pivot) = memtable.split(memtable, key_hash)
  let assert MemTableRange(lower, upper, _) = range
  let #(memtable_range_low, memtable_range_high) = case
    memtable_low.size >= memtable_high.size
  {
    False -> #(
      MemTableRange(lower, pivot - 1, Some(memtable_low)),
      MemTableRange(pivot, upper, None),
    )
    True -> #(
      MemTableRange(lower, pivot - 1, None),
      MemTableRange(pivot, upper, Some(memtable_high)),
    )
  }
  let memtable_high_id = new_id()
  let memtable_ranges =
    kv.memtable_ranges
    |> map.insert(range_id, memtable_range_low)
    |> map.insert(memtable_high_id, memtable_range_high)

  let assert Ok(True) =
    sstable.flush(memtable_high, sstable_path(kv.base_path, memtable_high_id))

  let assert Ok(True) =
    sstable.flush(memtable_low, sstable_path(kv.base_path, range_id))

  KV(..kv, memtable_ranges: memtable_ranges)
}

fn store_value(
  kv: KV,
  key: String,
  range_id: Int,
  range: MemTableRange,
  memtable: MemTable,
  value_data: BitString,
) -> Result(KV, memtable.Reason) {
  let assert Some(file_id) = kv.active_value_file
  let assert Ok(vfile) = map.get(kv.values, file_id)
  case value.write(vfile, value_data) {
    Ok(#(vfile, value)) -> {
      use memtable <- try(memtable.set(memtable, key, value))
      let range = MemTableRange(..range, memtable: Some(memtable))
      Ok(
        KV(
          ..kv,
          active_value_file: Some(vfile.id),
          values: map.insert(kv.values, vfile.id, vfile),
          memtable_ranges: map.insert(kv.memtable_ranges, range_id, range),
        ),
      )
    }
    Error(value.CapacityExceeded) -> {
      let assert Ok(vfile) =
        value.create(fs.join([kv.base_path, "value"]), kv.max_value_size)
      KV(
        ..kv,
        active_value_file: Some(vfile.id),
        values: map.insert(kv.values, vfile.id, vfile),
      )
      |> store_value(key, range_id, range, memtable, value_data)
    }
  }
}

fn delete_value(kv: KV, value: Value) -> Result(KV, Nil) {
  let file_id = value.file_id
  let assert Ok(vfile) = map.get(kv.values, file_id)
  let assert Ok(vfile) = value.delete(vfile, value)
  let values = map.insert(kv.values, file_id, vfile)
  Ok(KV(..kv, values: values))
}

pub fn info(kv: KV) -> KVInfo {
  KVInfo(
    base_path: kv.base_path,
    name: kv.name,
    values: map.size(kv.values),
    values_size_bytes: map.fold(
      kv.values,
      0,
      fn(acc, _key, value_file) { acc + value_file.size },
    ),
    memtables_total: map.size(kv.memtable_ranges),
    memtables_loaded: kv.memtables_loaded,
    memtables_loaded_size_bytes: map.fold(
      kv.memtable_ranges,
      0,
      fn(acc, _key, memtable_range) {
        case memtable_range.memtable {
          Some(memtable) -> acc + memtable.size
          None -> acc
        }
      },
    ),
    max_memtables_loaded: kv.max_memtables_loaded,
    max_memtable_size: kv.max_memtable_size,
    max_value_size: kv.max_value_size,
  )
}
