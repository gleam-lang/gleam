import { Error, throwError, isEqual } from "../gleam.js";
import * as $list from "../gleam/list.js";
import * as $option from "../gleam/option.js";
import * as $pair from "../gleam/pair.js";
import * as $result from "../gleam/result.js";
import {
  map_size as do_size,
  map_to_list as do_to_list,
  new_map as do_new,
  map_get as do_get,
  map_insert as do_insert,
  map_remove as do_delete,
} from "../gleam_stdlib.js";

export function size(map) {
  return do_size(map);
}

export function to_list(map) {
  return do_to_list(map);
}

export function from_list(list) {
  return do_from_list(list);
}

function do_from_list(list) {
  return $list.fold(list, new$(), insert_pair);
}

export function has_key(map, key) {
  return do_has_key(key, map);
}

function do_has_key(key, map) {
  return !isEqual(get(map, key), new Error(undefined));
}

export function new$() {
  return do_new();
}

export function get(from, get) {
  return do_get(from, get);
}

export function insert(map, key, value) {
  return do_insert(key, value, map);
}

export function map_values(map, fun) {
  return do_map_values(fun, map);
}

function do_map_values(f, map) {
  let f$1 = (map, k, v) => { return insert(map, k, f(k, v)); };
  let _pipe = map;
  return fold(_pipe, new$(), f$1);
}

export function keys(map) {
  return do_keys(map);
}

function do_keys(map) {
  let _pipe = map;
  let _pipe$1 = to_list(_pipe);
  return $list.map(_pipe$1, $pair.first);
}

export function values(map) {
  return do_values(map);
}

function do_values(map) {
  let _pipe = map;
  let _pipe$1 = to_list(_pipe);
  return $list.map(_pipe$1, $pair.second);
}

export function filter(map, property) {
  return do_filter(property, map);
}

function do_filter(f, map) {
  let insert$1 = (map, k, v) => {
    let $ = f(k, v);
    if ($) {
      return insert(map, k, v);
    } else {
      return map;
    }
  };
  let _pipe = map;
  return fold(_pipe, new$(), insert$1);
}

export function take(map, desired_keys) {
  return do_take(desired_keys, map);
}

function do_take(desired_keys, map) {
  let insert$1 = (taken, key) => {
    let $ = get(map, key);
    if ($.isOk()) {
      let value = $[0];
      return insert(taken, key, value);
    } else {
      return taken;
    }
  };
  return $list.fold(desired_keys, new$(), insert$1);
}

export function merge(map, new_entries) {
  return do_merge(map, new_entries);
}

function insert_pair(map, pair) {
  return insert(map, pair[0], pair[1]);
}

function do_merge(map, new_entries) {
  let _pipe = new_entries;
  let _pipe$1 = to_list(_pipe);
  return $list.fold(_pipe$1, map, insert_pair);
}

export function delete$(map, key) {
  return do_delete(key, map);
}

export function drop(map, disallowed_keys) {
  return $list.fold(disallowed_keys, map, delete$);
}

export function update(map, key, fun) {
  let _pipe = map;
  let _pipe$1 = get(_pipe, key);
  let _pipe$2 = $option.from_result(_pipe$1);
  let _pipe$3 = fun(_pipe$2);
  return ((_capture) => { return insert(map, key, _capture); })(_pipe$3);
}

function do_fold(loop$list, loop$initial, loop$fun) {
  let list = loop$list;
  let initial = loop$initial;
  let fun = loop$fun;
  while (true) {
    if (list.hasLength(0)) {
      return initial;
    } else if (list.atLeastLength(1)) {
      let k = list.head[0];
      let v = list.head[1];
      let tail = list.tail;
      list = tail;
      initial = fun(initial, k, v);
      fun = fun;
    } else {
      throwError(
        "case_no_match",
        "gleam/map",
        459,
        "do_fold",
        "No case clause matched",
        { values: [list] }
      );
    }
  }
}

export function fold(map, initial, fun) {
  let _pipe = map;
  let _pipe$1 = to_list(_pipe);
  return do_fold(_pipe$1, initial, fun);
}
