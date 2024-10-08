import { Error, toList, prepend as listPrepend, isEqual } from "../gleam.mjs";
import * as $option from "../gleam/option.mjs";
import {
  map_size as size,
  map_to_list as to_list,
  new_map as do_new,
  map_get as do_get,
  map_insert as do_insert,
  map_remove as do_delete,
} from "../gleam_stdlib.mjs";

export { size, to_list };

export function new$() {
  return do_new();
}

export function is_empty(dict) {
  return isEqual(dict, new$());
}

export function get(from, get) {
  return do_get(from, get);
}

function do_has_key(key, dict) {
  return !isEqual(get(dict, key), new Error(undefined));
}

export function has_key(dict, key) {
  return do_has_key(key, dict);
}

export function insert(dict, key, value) {
  return do_insert(key, value, dict);
}

function fold_list_of_pair(loop$list, loop$initial) {
  while (true) {
    let list = loop$list;
    let initial = loop$initial;
    if (list.hasLength(0)) {
      return initial;
    } else {
      let x = list.head;
      let rest = list.tail;
      loop$list = rest;
      loop$initial = insert(initial, x[0], x[1]);
    }
  }
}

export function from_list(list) {
  return fold_list_of_pair(list, new$());
}

function reverse_and_concat(loop$remaining, loop$accumulator) {
  while (true) {
    let remaining = loop$remaining;
    let accumulator = loop$accumulator;
    if (remaining.hasLength(0)) {
      return accumulator;
    } else {
      let item = remaining.head;
      let rest = remaining.tail;
      loop$remaining = rest;
      loop$accumulator = listPrepend(item, accumulator);
    }
  }
}

function do_keys_acc(loop$list, loop$acc) {
  while (true) {
    let list = loop$list;
    let acc = loop$acc;
    if (list.hasLength(0)) {
      return reverse_and_concat(acc, toList([]));
    } else {
      let first = list.head;
      let rest = list.tail;
      loop$list = rest;
      loop$acc = listPrepend(first[0], acc);
    }
  }
}

function do_keys(dict) {
  let list_of_pairs = to_list(dict);
  return do_keys_acc(list_of_pairs, toList([]));
}

export function keys(dict) {
  return do_keys(dict);
}

function do_values_acc(loop$list, loop$acc) {
  while (true) {
    let list = loop$list;
    let acc = loop$acc;
    if (list.hasLength(0)) {
      return reverse_and_concat(acc, toList([]));
    } else {
      let first = list.head;
      let rest = list.tail;
      loop$list = rest;
      loop$acc = listPrepend(first[1], acc);
    }
  }
}

function do_values(dict) {
  let list_of_pairs = to_list(dict);
  return do_values_acc(list_of_pairs, toList([]));
}

export function values(dict) {
  return do_values(dict);
}

function insert_taken(loop$dict, loop$desired_keys, loop$acc) {
  while (true) {
    let dict = loop$dict;
    let desired_keys = loop$desired_keys;
    let acc = loop$acc;
    let insert$1 = (taken, key) => {
      let $ = get(dict, key);
      if ($.isOk()) {
        let value = $[0];
        return insert(taken, key, value);
      } else {
        return taken;
      }
    };
    if (desired_keys.hasLength(0)) {
      return acc;
    } else {
      let first = desired_keys.head;
      let rest = desired_keys.tail;
      loop$dict = dict;
      loop$desired_keys = rest;
      loop$acc = insert$1(acc, first);
    }
  }
}

function do_take(desired_keys, dict) {
  return insert_taken(dict, desired_keys, new$());
}

export function take(dict, desired_keys) {
  return do_take(desired_keys, dict);
}

function insert_pair(dict, pair) {
  return insert(dict, pair[0], pair[1]);
}

function fold_inserts(loop$new_entries, loop$dict) {
  while (true) {
    let new_entries = loop$new_entries;
    let dict = loop$dict;
    if (new_entries.hasLength(0)) {
      return dict;
    } else {
      let first = new_entries.head;
      let rest = new_entries.tail;
      loop$new_entries = rest;
      loop$dict = insert_pair(dict, first);
    }
  }
}

function do_merge(dict, new_entries) {
  let _pipe = new_entries;
  let _pipe$1 = to_list(_pipe);
  return fold_inserts(_pipe$1, dict);
}

export function merge(dict, new_entries) {
  return do_merge(dict, new_entries);
}

export function delete$(dict, key) {
  return do_delete(key, dict);
}

export function drop(loop$dict, loop$disallowed_keys) {
  while (true) {
    let dict = loop$dict;
    let disallowed_keys = loop$disallowed_keys;
    if (disallowed_keys.hasLength(0)) {
      return dict;
    } else {
      let first = disallowed_keys.head;
      let rest = disallowed_keys.tail;
      loop$dict = delete$(dict, first);
      loop$disallowed_keys = rest;
    }
  }
}

export function upsert(dict, key, fun) {
  let _pipe = dict;
  let _pipe$1 = get(_pipe, key);
  let _pipe$2 = $option.from_result(_pipe$1);
  let _pipe$3 = fun(_pipe$2);
  return ((_capture) => { return insert(dict, key, _capture); })(_pipe$3);
}

function do_fold(loop$list, loop$initial, loop$fun) {
  while (true) {
    let list = loop$list;
    let initial = loop$initial;
    let fun = loop$fun;
    if (list.hasLength(0)) {
      return initial;
    } else {
      let k = list.head[0];
      let v = list.head[1];
      let rest = list.tail;
      loop$list = rest;
      loop$initial = fun(initial, k, v);
      loop$fun = fun;
    }
  }
}

export function fold(dict, initial, fun) {
  let _pipe = dict;
  let _pipe$1 = to_list(_pipe);
  return do_fold(_pipe$1, initial, fun);
}

function do_map_values(f, dict) {
  let f$1 = (dict, k, v) => { return insert(dict, k, f(k, v)); };
  return fold(dict, new$(), f$1);
}

export function map_values(dict, fun) {
  return do_map_values(fun, dict);
}

function do_filter(f, dict) {
  let insert$1 = (dict, k, v) => {
    let $ = f(k, v);
    if ($) {
      return insert(dict, k, v);
    } else {
      return dict;
    }
  };
  let _pipe = dict;
  return fold(_pipe, new$(), insert$1);
}

export function filter(dict, predicate) {
  return do_filter(predicate, dict);
}

export function each(dict, fun) {
  return fold(
    dict,
    undefined,
    (nil, k, v) => {
      fun(k, v);
      return nil;
    },
  );
}

export function combine(dict, other, fun) {
  return fold(
    dict,
    other,
    (acc, key, value) => {
      let $ = get(acc, key);
      if ($.isOk()) {
        let other_value = $[0];
        return insert(acc, key, fun(value, other_value));
      } else {
        return insert(acc, key, value);
      }
    },
  );
}
