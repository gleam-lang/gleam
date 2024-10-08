import { CustomType as $CustomType, isEqual } from "../gleam.mjs";
import * as $dict from "../gleam/dict.mjs";
import * as $list from "../gleam/list.mjs";
import * as $result from "../gleam/result.mjs";

class Set extends $CustomType {
  constructor(dict) {
    super();
    this.dict = dict;
  }
}

export function new$() {
  return new Set($dict.new$());
}

export function size(set) {
  return $dict.size(set.dict);
}

export function is_empty(set) {
  return isEqual(set, new$());
}

export function contains(set, member) {
  let _pipe = set.dict;
  let _pipe$1 = $dict.get(_pipe, member);
  return $result.is_ok(_pipe$1);
}

export function delete$(set, member) {
  return new Set($dict.delete$(set.dict, member));
}

export function to_list(set) {
  return $dict.keys(set.dict);
}

export function fold(set, initial, reducer) {
  return $dict.fold(set.dict, initial, (a, k, _) => { return reducer(a, k); });
}

export function filter(set, predicate) {
  return new Set($dict.filter(set.dict, (m, _) => { return predicate(m); }));
}

export function drop(set, disallowed) {
  return $list.fold(disallowed, set, delete$);
}

export function take(set, desired) {
  return new Set($dict.take(set.dict, desired));
}

function order(first, second) {
  let $ = $dict.size(first.dict) > $dict.size(second.dict);
  if ($) {
    return [first, second];
  } else {
    return [second, first];
  }
}

export function intersection(first, second) {
  let $ = order(first, second);
  let larger = $[0];
  let smaller = $[1];
  return take(larger, to_list(smaller));
}

export function difference(first, second) {
  return drop(first, to_list(second));
}

export function is_subset(first, second) {
  return isEqual(intersection(first, second), first);
}

export function is_disjoint(first, second) {
  return isEqual(intersection(first, second), new$());
}

export function each(set, fun) {
  return fold(
    set,
    undefined,
    (nil, member) => {
      fun(member);
      return nil;
    },
  );
}

const token = undefined;

export function insert(set, member) {
  return new Set($dict.insert(set.dict, member, token));
}

export function from_list(members) {
  let dict = $list.fold(
    members,
    $dict.new$(),
    (m, k) => { return $dict.insert(m, k, token); },
  );
  return new Set(dict);
}

export function map(set, fun) {
  return fold(
    set,
    new$(),
    (acc, member) => { return insert(acc, fun(member)); },
  );
}

export function union(first, second) {
  let $ = order(first, second);
  let larger = $[0];
  let smaller = $[1];
  return fold(smaller, larger, insert);
}

export function symmetric_difference(first, second) {
  return difference(union(first, second), intersection(first, second));
}
