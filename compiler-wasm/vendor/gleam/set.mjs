import { CustomType as $CustomType, makeError } from "../gleam.mjs";
import * as $dict from "../gleam/dict.mjs";
import * as $list from "../gleam/list.mjs";
import * as $result from "../gleam/result.mjs";

class Set extends $CustomType {
  constructor(map) {
    super();
    this.map = map;
  }
}

const token = undefined;

export function new$() {
  return new Set($dict.new$());
}

export function size(set) {
  return $dict.size(set.map);
}

export function insert(set, member) {
  return new Set($dict.insert(set.map, member, token));
}

export function contains(set, member) {
  let _pipe = set.map;
  let _pipe$1 = $dict.get(_pipe, member);
  return $result.is_ok(_pipe$1);
}

export function delete$(set, member) {
  return new Set($dict.delete$(set.map, member));
}

export function to_list(set) {
  return $dict.keys(set.map);
}

export function from_list(members) {
  let map = $list.fold(
    members,
    $dict.new$(),
    (m, k) => { return $dict.insert(m, k, token); },
  );
  return new Set(map);
}

export function fold(set, initial, reducer) {
  return $dict.fold(set.map, initial, (a, k, _) => { return reducer(a, k); });
}

export function filter(set, predicate) {
  return new Set($dict.filter(set.map, (m, _) => { return predicate(m); }));
}

export function drop(set, disallowed) {
  return $list.fold(disallowed, set, delete$);
}

export function take(set, desired) {
  return new Set($dict.take(set.map, desired));
}

function order(first, second) {
  let $ = $dict.size(first.map) > $dict.size(second.map);
  if ($) {
    return [first, second];
  } else if (!$) {
    return [second, first];
  } else {
    throw makeError(
      "case_no_match",
      "gleam/set",
      225,
      "order",
      "No case clause matched",
      { values: [$] }
    )
  }
}

export function union(first, second) {
  let $ = order(first, second);
  let larger = $[0];
  let smaller = $[1];
  return fold(smaller, larger, insert);
}

export function intersection(first, second) {
  let $ = order(first, second);
  let larger = $[0];
  let smaller = $[1];
  return take(larger, to_list(smaller));
}
