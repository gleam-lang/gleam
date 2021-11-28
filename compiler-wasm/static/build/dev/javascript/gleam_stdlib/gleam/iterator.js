import { Ok, Error, toList, CustomType, throwError, isEqual } from "../gleam.js";
import * as $list from "../gleam/list.js";
import * as $map from "../gleam/map.js";
import * as $option from "../gleam/option.js";
import { None, Some } from "../gleam/option.js";

class Stop extends CustomType {}

class Continue extends CustomType {
  constructor(x0,
  x1) {
    super();
    this[0] = x0;
    this[1] = x1;
  }
}

class Iterator extends CustomType {
  constructor(continuation) {
    super();
    this.continuation = continuation;
  }
}

export class Next extends CustomType {
  constructor(element,
  accumulator) {
    super();
    this.element = element;
    this.accumulator = accumulator;
  }
}

export class Done extends CustomType {}

function stop() {
  return new Stop();
}

function do_unfold(initial, f) {
  return () => {
    let $ = f(initial);
    if ($ instanceof Next) {
      let x = $.element;
      let acc = $.accumulator;
      return new Continue(x, do_unfold(acc, f));
    } else if ($ instanceof Done) {
      return new Stop();
    } else {
      throwError(
        "case_no_match",
        "gleam/iterator",
        43,
        "",
        "No case clause matched",
        { values: [$] }
      );
    }
  };
}

export function unfold(initial, f) {
  let _pipe = initial;
  let _pipe$1 = do_unfold(_pipe, f);
  return new Iterator(_pipe$1);
}

export function repeatedly(f) {
  return unfold(undefined, (_) => { return new Next(f(), undefined); });
}

export function repeat(x) {
  return repeatedly(() => { return x; });
}

export function from_list(list) {
  let yield$ = (acc) => {
    if (acc.hasLength(0)) {
      return new Done();
    } else if (acc.atLeastLength(1)) {
      let head = acc.head;
      let tail = acc.tail;
      return new Next(head, tail);
    } else {
      throwError(
        "case_no_match",
        "gleam/iterator",
        108,
        "",
        "No case clause matched",
        { values: [acc] }
      );
    }
  };
  return unfold(list, yield$);
}

function do_fold(loop$continuation, loop$f, loop$accumulator) {
  let continuation = loop$continuation;
  let f = loop$f;
  let accumulator = loop$accumulator;
  while (true) {
    let $ = continuation();
    if ($ instanceof Continue) {
      let elem = $[0];
      let next = $[1];
      continuation = next;
      f = f;
      accumulator = f(accumulator, elem);
    } else if ($ instanceof Stop) {
      return accumulator;
    } else {
      throwError(
        "case_no_match",
        "gleam/iterator",
        122,
        "do_fold",
        "No case clause matched",
        { values: [$] }
      );
    }
  }
}

export function fold(iterator, initial, f) {
  let _pipe = iterator.continuation;
  return do_fold(_pipe, f, initial);
}

export function run(iterator) {
  return fold(iterator, undefined, (_, _1) => { return undefined; });
}

export function to_list(iterator) {
  let _pipe = iterator;
  let _pipe$1 = fold(
    _pipe,
    toList([]),
    (acc, e) => { return toList([e], acc); },
  );
  return $list.reverse(_pipe$1);
}

export function step(iterator) {
  let $ = iterator.continuation();
  if ($ instanceof Stop) {
    return new Done();
  } else if ($ instanceof Continue) {
    let e = $[0];
    let a = $[1];
    return new Next(e, new Iterator(a));
  } else {
    throwError(
      "case_no_match",
      "gleam/iterator",
      199,
      "step",
      "No case clause matched",
      { values: [$] }
    );
  }
}

function do_take(continuation, desired) {
  return () => {
    let $ = desired > 0;
    if (!$) {
      return new Stop();
    } else if ($) {
      let $1 = continuation();
      if ($1 instanceof Stop) {
        return new Stop();
      } else if ($1 instanceof Continue) {
        let e = $1[0];
        let next = $1[1];
        return new Continue(e, do_take(next, desired - 1));
      } else {
        throwError(
          "case_no_match",
          "gleam/iterator",
          210,
          "",
          "No case clause matched",
          { values: [$1] }
        );
      }
    } else {
      throwError(
        "case_no_match",
        "gleam/iterator",
        207,
        "",
        "No case clause matched",
        { values: [$] }
      );
    }
  };
}

export function take(iterator, desired) {
  let _pipe = iterator.continuation;
  let _pipe$1 = do_take(_pipe, desired);
  return new Iterator(_pipe$1);
}

function do_drop(loop$continuation, loop$desired) {
  let continuation = loop$continuation;
  let desired = loop$desired;
  while (true) {
    let $ = continuation();
    if ($ instanceof Stop) {
      return new Stop();
    } else if ($ instanceof Continue) {
      let e = $[0];
      let next = $[1];
      let $1 = desired > 0;
      if ($1) {
        continuation = next;
        desired = desired - 1;
      } else if (!$1) {
        return new Continue(e, next);
      } else {
        throwError(
          "case_no_match",
          "gleam/iterator",
          240,
          "do_drop",
          "No case clause matched",
          { values: [$1] }
        );
      }
    } else {
      throwError(
        "case_no_match",
        "gleam/iterator",
        237,
        "do_drop",
        "No case clause matched",
        { values: [$] }
      );
    }
  }
}

export function drop(iterator, desired) {
  let _pipe = () => { return do_drop(iterator.continuation, desired); };
  return new Iterator(_pipe);
}

function do_map(continuation, f) {
  return () => {
    let $ = continuation();
    if ($ instanceof Stop) {
      return new Stop();
    } else if ($ instanceof Continue) {
      let e = $[0];
      let continuation$1 = $[1];
      return new Continue(f(e), do_map(continuation$1, f));
    } else {
      throwError(
        "case_no_match",
        "gleam/iterator",
        271,
        "",
        "No case clause matched",
        { values: [$] }
      );
    }
  };
}

export function map(iterator, f) {
  let _pipe = iterator.continuation;
  let _pipe$1 = do_map(_pipe, f);
  return new Iterator(_pipe$1);
}

function do_append(first, second) {
  let $ = first();
  if ($ instanceof Continue) {
    let e = $[0];
    let first$1 = $[1];
    return new Continue(e, () => { return do_append(first$1, second); });
  } else if ($ instanceof Stop) {
    return second();
  } else {
    throwError(
      "case_no_match",
      "gleam/iterator",
      298,
      "do_append",
      "No case clause matched",
      { values: [$] }
    );
  }
}

export function append(first, second) {
  let _pipe = () => {
    return do_append(first.continuation, second.continuation);
  };
  return new Iterator(_pipe);
}

function do_flatten(flattened) {
  let $ = flattened();
  if ($ instanceof Stop) {
    return new Stop();
  } else if ($ instanceof Continue) {
    let it = $[0];
    let next_iterator = $[1];
    return do_append(
      it.continuation,
      () => { return do_flatten(next_iterator); },
    );
  } else {
    throwError(
      "case_no_match",
      "gleam/iterator",
      320,
      "do_flatten",
      "No case clause matched",
      { values: [$] }
    );
  }
}

export function flatten(iterator) {
  let _pipe = () => { return do_flatten(iterator.continuation); };
  return new Iterator(_pipe);
}

export function flat_map(iterator, f) {
  let _pipe = iterator;
  let _pipe$1 = map(_pipe, f);
  return flatten(_pipe$1);
}

function do_filter(loop$continuation, loop$predicate) {
  let continuation = loop$continuation;
  let predicate = loop$predicate;
  while (true) {
    let $ = continuation();
    if ($ instanceof Stop) {
      return new Stop();
    } else if ($ instanceof Continue) {
      let e = $[0];
      let iterator = $[1];
      let $1 = predicate(e);
      if ($1) {
        return new Continue(e, () => { return do_filter(iterator, predicate); });
      } else if (!$1) {
        continuation = iterator;
        predicate = predicate;
      } else {
        throwError(
          "case_no_match",
          "gleam/iterator",
          372,
          "do_filter",
          "No case clause matched",
          { values: [$1] }
        );
      }
    } else {
      throwError(
        "case_no_match",
        "gleam/iterator",
        369,
        "do_filter",
        "No case clause matched",
        { values: [$] }
      );
    }
  }
}

export function filter(iterator, predicate) {
  let _pipe = () => { return do_filter(iterator.continuation, predicate); };
  return new Iterator(_pipe);
}

export function cycle(iterator) {
  let _pipe = repeat(iterator);
  return flatten(_pipe);
}

export function range(start, stop) {
  let increment = (() => {
    let $ = start < stop;
    if ($) {
      return 1;
    } else if (!$) {
      return -1;
    } else {
      throwError(
        "case_no_match",
        "gleam/iterator",
        428,
        "range",
        "No case clause matched",
        { values: [$] }
      );
    }
  })();
  let next_step = (current) => {
    let $ = current === stop;
    if ($) {
      return new Done();
    } else if (!$) {
      return new Next(current, current + increment);
    } else {
      throwError(
        "case_no_match",
        "gleam/iterator",
        434,
        "",
        "No case clause matched",
        { values: [$] }
      );
    }
  };
  return unfold(start, next_step);
}

function do_find(loop$continuation, loop$f) {
  let continuation = loop$continuation;
  let f = loop$f;
  while (true) {
    let $ = continuation();
    if ($ instanceof Stop) {
      return new Error(undefined);
    } else if ($ instanceof Continue) {
      let e = $[0];
      let next = $[1];
      let $1 = f(e);
      if ($1) {
        return new Ok(e);
      } else if (!$1) {
        continuation = next;
        f = f;
      } else {
        throwError(
          "case_no_match",
          "gleam/iterator",
          447,
          "do_find",
          "No case clause matched",
          { values: [$1] }
        );
      }
    } else {
      throwError(
        "case_no_match",
        "gleam/iterator",
        444,
        "do_find",
        "No case clause matched",
        { values: [$] }
      );
    }
  }
}

export function find(haystack, is_desired) {
  let _pipe = haystack.continuation;
  return do_find(_pipe, is_desired);
}

function do_index(continuation, next) {
  return () => {
    let $ = continuation();
    if ($ instanceof Stop) {
      return new Stop();
    } else if ($ instanceof Continue) {
      let e = $[0];
      let continuation$1 = $[1];
      return new Continue([next, e], do_index(continuation$1, next + 1));
    } else {
      throwError(
        "case_no_match",
        "gleam/iterator",
        484,
        "",
        "No case clause matched",
        { values: [$] }
      );
    }
  };
}

export function index(iterator) {
  let _pipe = iterator.continuation;
  let _pipe$1 = do_index(_pipe, 0);
  return new Iterator(_pipe$1);
}

export function iterate(initial, f) {
  return unfold(initial, (element) => { return new Next(element, f(element)); });
}

function do_take_while(continuation, predicate) {
  return () => {
    let $ = continuation();
    if ($ instanceof Stop) {
      return new Stop();
    } else if ($ instanceof Continue) {
      let e = $[0];
      let next = $[1];
      let $1 = predicate(e);
      if (!$1) {
        return new Stop();
      } else if ($1) {
        return new Continue(e, do_take_while(next, predicate));
      } else {
        throwError(
          "case_no_match",
          "gleam/iterator",
          527,
          "",
          "No case clause matched",
          { values: [$1] }
        );
      }
    } else {
      throwError(
        "case_no_match",
        "gleam/iterator",
        524,
        "",
        "No case clause matched",
        { values: [$] }
      );
    }
  };
}

export function take_while(iterator, predicate) {
  let _pipe = iterator.continuation;
  let _pipe$1 = do_take_while(_pipe, predicate);
  return new Iterator(_pipe$1);
}

function do_drop_while(loop$continuation, loop$predicate) {
  let continuation = loop$continuation;
  let predicate = loop$predicate;
  while (true) {
    let $ = continuation();
    if ($ instanceof Stop) {
      return new Stop();
    } else if ($ instanceof Continue) {
      let e = $[0];
      let next = $[1];
      let $1 = predicate(e);
      if (!$1) {
        return new Continue(e, next);
      } else if ($1) {
        continuation = next;
        predicate = predicate;
      } else {
        throwError(
          "case_no_match",
          "gleam/iterator",
          558,
          "do_drop_while",
          "No case clause matched",
          { values: [$1] }
        );
      }
    } else {
      throwError(
        "case_no_match",
        "gleam/iterator",
        555,
        "do_drop_while",
        "No case clause matched",
        { values: [$] }
      );
    }
  }
}

export function drop_while(iterator, predicate) {
  let _pipe = () => { return do_drop_while(iterator.continuation, predicate); };
  return new Iterator(_pipe);
}

function do_scan(continuation, f, accumulator) {
  return () => {
    let $ = continuation();
    if ($ instanceof Stop) {
      return new Stop();
    } else if ($ instanceof Continue) {
      let el = $[0];
      let next = $[1];
      let accumulated = f(accumulator, el);
      return new Continue(accumulated, do_scan(next, f, accumulated));
    } else {
      throwError(
        "case_no_match",
        "gleam/iterator",
        587,
        "",
        "No case clause matched",
        { values: [$] }
      );
    }
  };
}

export function scan(iterator, initial, f) {
  let _pipe = iterator.continuation;
  let _pipe$1 = do_scan(_pipe, f, initial);
  return new Iterator(_pipe$1);
}

function do_zip(left, right) {
  return () => {
    let $ = left();
    if ($ instanceof Stop) {
      return new Stop();
    } else if ($ instanceof Continue) {
      let el_left = $[0];
      let next_left = $[1];
      let $1 = right();
      if ($1 instanceof Stop) {
        return new Stop();
      } else if ($1 instanceof Continue) {
        let el_right = $1[0];
        let next_right = $1[1];
        return new Continue([el_left, el_right], do_zip(next_left, next_right));
      } else {
        throwError(
          "case_no_match",
          "gleam/iterator",
          625,
          "",
          "No case clause matched",
          { values: [$1] }
        );
      }
    } else {
      throwError(
        "case_no_match",
        "gleam/iterator",
        622,
        "",
        "No case clause matched",
        { values: [$] }
      );
    }
  };
}

export function zip(left, right) {
  let _pipe = do_zip(left.continuation, right.continuation);
  return new Iterator(_pipe);
}

class AnotherBy extends CustomType {
  constructor(x0,
  x1,
  x2,
  x3) {
    super();
    this[0] = x0;
    this[1] = x1;
    this[2] = x2;
    this[3] = x3;
  }
}

class LastBy extends CustomType {
  constructor(x0) {
    super();
    this[0] = x0;
  }
}

function next_chunk(
  loop$continuation,
  loop$f,
  loop$previous_key,
  loop$current_chunk
) {
  let continuation = loop$continuation;
  let f = loop$f;
  let previous_key = loop$previous_key;
  let current_chunk = loop$current_chunk;
  while (true) {
    let $ = continuation();
    if ($ instanceof Stop) {
      return new LastBy($list.reverse(current_chunk));
    } else if ($ instanceof Continue) {
      let e = $[0];
      let next = $[1];
      let key = f(e);
      let $1 = isEqual(key, previous_key);
      if ($1) {
        continuation = next;
        f = f;
        previous_key = key;
        current_chunk = toList([e], current_chunk);
      } else if (!$1) {
        return new AnotherBy($list.reverse(current_chunk), key, e, next);
      } else {
        throwError(
          "case_no_match",
          "gleam/iterator",
          663,
          "next_chunk",
          "No case clause matched",
          { values: [$1] }
        );
      }
    } else {
      throwError(
        "case_no_match",
        "gleam/iterator",
        659,
        "next_chunk",
        "No case clause matched",
        { values: [$] }
      );
    }
  }
}

function do_chunk(continuation, f, previous_key, previous_element) {
  let $ = next_chunk(continuation, f, previous_key, toList([previous_element]));
  if ($ instanceof LastBy) {
    let chunk = $[0];
    return new Continue(chunk, stop);
  } else if ($ instanceof AnotherBy) {
    let chunk = $[0];
    let key = $[1];
    let el = $[2];
    let next = $[3];
    return new Continue(chunk, () => { return do_chunk(next, f, key, el); });
  } else {
    throwError(
      "case_no_match",
      "gleam/iterator",
      677,
      "do_chunk",
      "No case clause matched",
      { values: [$] }
    );
  }
}

export function chunk(iterator, f) {
  let _pipe = () => {
    let $ = iterator.continuation();
    if ($ instanceof Stop) {
      return new Stop();
    } else if ($ instanceof Continue) {
      let e = $[0];
      let next = $[1];
      return do_chunk(next, f, f(e), e);
    } else {
      throwError(
        "case_no_match",
        "gleam/iterator",
        697,
        "",
        "No case clause matched",
        { values: [$] }
      );
    }
  };
  return new Iterator(_pipe);
}

class Another extends CustomType {
  constructor(x0,
  x1) {
    super();
    this[0] = x0;
    this[1] = x1;
  }
}

class Last extends CustomType {
  constructor(x0) {
    super();
    this[0] = x0;
  }
}

class NoMore extends CustomType {}

function next_sized_chunk(loop$continuation, loop$left, loop$current_chunk) {
  let continuation = loop$continuation;
  let left = loop$left;
  let current_chunk = loop$current_chunk;
  while (true) {
    let $ = continuation();
    if ($ instanceof Stop) {
      if (current_chunk.hasLength(0)) {
        return new NoMore();
      } else {
        let remaining = current_chunk;
        return new Last($list.reverse(remaining));
      }
    } else if ($ instanceof Continue) {
      let e = $[0];
      let next = $[1];
      let chunk$1 = toList([e], current_chunk);
      let $1 = left > 1;
      if (!$1) {
        return new Another($list.reverse(chunk$1), next);
      } else if ($1) {
        continuation = next;
        left = left - 1;
        current_chunk = chunk$1;
      } else {
        throwError(
          "case_no_match",
          "gleam/iterator",
          725,
          "next_sized_chunk",
          "No case clause matched",
          { values: [$1] }
        );
      }
    } else {
      throwError(
        "case_no_match",
        "gleam/iterator",
        717,
        "next_sized_chunk",
        "No case clause matched",
        { values: [$] }
      );
    }
  }
}

function do_sized_chunk(continuation, count) {
  return () => {
    let $ = next_sized_chunk(continuation, count, toList([]));
    if ($ instanceof NoMore) {
      return new Stop();
    } else if ($ instanceof Last) {
      let chunk$1 = $[0];
      return new Continue(chunk$1, stop);
    } else if ($ instanceof Another) {
      let chunk$1 = $[0];
      let next_element = $[1];
      return new Continue(chunk$1, do_sized_chunk(next_element, count));
    } else {
      throwError(
        "case_no_match",
        "gleam/iterator",
        738,
        "",
        "No case clause matched",
        { values: [$] }
      );
    }
  };
}

export function sized_chunk(iterator, count) {
  let _pipe = iterator.continuation;
  let _pipe$1 = do_sized_chunk(_pipe, count);
  return new Iterator(_pipe$1);
}

function do_intersperse(continuation, separator) {
  let $ = continuation();
  if ($ instanceof Stop) {
    return new Stop();
  } else if ($ instanceof Continue) {
    let e = $[0];
    let next = $[1];
    let next_interspersed = () => { return do_intersperse(next, separator); };
    return new Continue(
      separator,
      () => { return new Continue(e, next_interspersed); },
    );
  } else {
    throwError(
      "case_no_match",
      "gleam/iterator",
      775,
      "do_intersperse",
      "No case clause matched",
      { values: [$] }
    );
  }
}

export function intersperse(iterator, elem) {
  let _pipe = () => {
    let $ = iterator.continuation();
    if ($ instanceof Stop) {
      return new Stop();
    } else if ($ instanceof Continue) {
      let e = $[0];
      let next = $[1];
      return new Continue(e, () => { return do_intersperse(next, elem); });
    } else {
      throwError(
        "case_no_match",
        "gleam/iterator",
        803,
        "",
        "No case clause matched",
        { values: [$] }
      );
    }
  };
  return new Iterator(_pipe);
}

function do_any(continuation, predicate) {
  let $ = continuation();
  if ($ instanceof Stop) {
    return false;
  } else if ($ instanceof Continue) {
    let e = $[0];
    let next = $[1];
    return predicate(e) || do_any(next, predicate);
  } else {
    throwError(
      "case_no_match",
      "gleam/iterator",
      815,
      "do_any",
      "No case clause matched",
      { values: [$] }
    );
  }
}

export function any(iterator, predicate) {
  let _pipe = iterator.continuation;
  return do_any(_pipe, predicate);
}

function do_all(continuation, predicate) {
  let $ = continuation();
  if ($ instanceof Stop) {
    return true;
  } else if ($ instanceof Continue) {
    let e = $[0];
    let next = $[1];
    return predicate(e) && do_all(next, predicate);
  } else {
    throwError(
      "case_no_match",
      "gleam/iterator",
      851,
      "do_all",
      "No case clause matched",
      { values: [$] }
    );
  }
}

export function all(iterator, predicate) {
  let _pipe = iterator.continuation;
  return do_all(_pipe, predicate);
}

function update_group_with(el) {
  return (maybe_group) => {
    if (maybe_group instanceof Some) {
      let group = maybe_group[0];
      return toList([el], group);
    } else if (maybe_group instanceof None) {
      return toList([el]);
    } else {
      throwError(
        "case_no_match",
        "gleam/iterator",
        885,
        "",
        "No case clause matched",
        { values: [maybe_group] }
      );
    }
  };
}

function group_updater(f) {
  return (groups, elem) => {
    let _pipe = groups;
    return $map.update(_pipe, f(elem), update_group_with(elem));
  };
}

export function group(iterator, key) {
  let _pipe = iterator;
  let _pipe$1 = fold(_pipe, $map.new$(), group_updater(key));
  return $map.map_values(
    _pipe$1,
    (_, group) => { return $list.reverse(group); },
  );
}

export function reduce(iterator, f) {
  let $ = iterator.continuation();
  if ($ instanceof Stop) {
    return new Error(undefined);
  } else if ($ instanceof Continue) {
    let e = $[0];
    let next = $[1];
    let _pipe = do_fold(next, f, e);
    return new Ok(_pipe);
  } else {
    throwError(
      "case_no_match",
      "gleam/iterator",
      939,
      "reduce",
      "No case clause matched",
      { values: [$] }
    );
  }
}

export function last(iterator) {
  let _pipe = iterator;
  return reduce(_pipe, (_, elem) => { return elem; });
}

export function empty() {
  return new Iterator(stop);
}

export function once(f) {
  let _pipe = () => { return new Continue(f(), stop); };
  return new Iterator(_pipe);
}

export function single(elem) {
  return once(() => { return elem; });
}

function do_interleave(current, next) {
  let $ = current();
  if ($ instanceof Stop) {
    return next();
  } else if ($ instanceof Continue) {
    let e = $[0];
    let next_other = $[1];
    return new Continue(e, () => { return do_interleave(next, next_other); });
  } else {
    throwError(
      "case_no_match",
      "gleam/iterator",
      1004,
      "do_interleave",
      "No case clause matched",
      { values: [$] }
    );
  }
}

export function interleave(left, right) {
  let _pipe = () => {
    return do_interleave(left.continuation, right.continuation);
  };
  return new Iterator(_pipe);
}

function do_fold_until(loop$continuation, loop$f, loop$accumulator) {
  let continuation = loop$continuation;
  let f = loop$f;
  let accumulator = loop$accumulator;
  while (true) {
    let $ = continuation();
    if ($ instanceof Stop) {
      return accumulator;
    } else if ($ instanceof Continue) {
      let elem = $[0];
      let next = $[1];
      let $1 = f(accumulator, elem);
      if ($1 instanceof $list.Continue) {
        let accumulator$1 = $1[0];
        continuation = next;
        f = f;
        accumulator = accumulator$1;
      } else if ($1 instanceof $list.Stop) {
        let accumulator$1 = $1[0];
        return accumulator$1;
      } else {
        throwError(
          "case_no_match",
          "gleam/iterator",
          1038,
          "do_fold_until",
          "No case clause matched",
          { values: [$1] }
        );
      }
    } else {
      throwError(
        "case_no_match",
        "gleam/iterator",
        1035,
        "do_fold_until",
        "No case clause matched",
        { values: [$] }
      );
    }
  }
}

export function fold_until(iterator, initial, f) {
  let _pipe = iterator.continuation;
  return do_fold_until(_pipe, f, initial);
}

function do_try_fold(loop$continuation, loop$f, loop$accumulator) {
  let continuation = loop$continuation;
  let f = loop$f;
  let accumulator = loop$accumulator;
  while (true) {
    let $ = continuation();
    if ($ instanceof Stop) {
      return new Ok(accumulator);
    } else if ($ instanceof Continue) {
      let elem = $[0];
      let next = $[1];
      let $1 = f(accumulator, elem);
      if (!$1.isOk()) return $1;
      let accumulator$1 = $1[0];

      continuation = next;
      f = f;
      accumulator = accumulator$1;
    } else {
      throwError(
        "case_no_match",
        "gleam/iterator",
        1080,
        "do_try_fold",
        "No case clause matched",
        { values: [$] }
      );
    }
  }
}

export function try_fold(iterator, initial, f) {
  let _pipe = iterator.continuation;
  return do_try_fold(_pipe, f, initial);
}

export function first(iterator) {
  let $ = iterator.continuation();
  if ($ instanceof Stop) {
    return new Error(undefined);
  } else if ($ instanceof Continue) {
    let e = $[0];
    return new Ok(e);
  } else {
    throwError(
      "case_no_match",
      "gleam/iterator",
      1130,
      "first",
      "No case clause matched",
      { values: [$] }
    );
  }
}

export function at(iterator, index) {
  let _pipe = iterator;
  let _pipe$1 = drop(_pipe, index);
  return first(_pipe$1);
}
