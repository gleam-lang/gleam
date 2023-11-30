import { Ok, Error, toList, CustomType as $CustomType, makeError, isEqual } from "../gleam.mjs";
import * as $list from "../gleam/list.mjs";

class Queue extends $CustomType {
  constructor(in$, out) {
    super();
    this.in = in$;
    this.out = out;
  }
}

export function new$() {
  return new Queue(toList([]), toList([]));
}

export function from_list(list) {
  return new Queue(toList([]), list);
}

export function to_list(queue) {
  let _pipe = queue.out;
  return $list.append(_pipe, $list.reverse(queue.in));
}

export function is_empty(queue) {
  return (isEqual(queue.in, toList([]))) && (isEqual(queue.out, toList([])));
}

export function length(queue) {
  return $list.length(queue.in) + $list.length(queue.out);
}

export function push_back(queue, item) {
  return new Queue(toList([item], queue.in), queue.out);
}

export function push_front(queue, item) {
  return new Queue(queue.in, toList([item], queue.out));
}

export function pop_back(loop$queue) {
  while (true) {
    let queue = loop$queue;
    if (queue instanceof Queue &&
    queue.in.hasLength(0) &&
    queue.out.hasLength(0)) {
      return new Error(undefined);
    } else if (queue instanceof Queue && queue.in.hasLength(0)) {
      let out = queue.out;
      loop$queue = new Queue($list.reverse(out), toList([]));
    } else if (queue instanceof Queue && queue.in.atLeastLength(1)) {
      let first = queue.in.head;
      let rest = queue.in.tail;
      let out = queue.out;
      let queue$1 = new Queue(rest, out);
      return new Ok([first, queue$1]);
    } else {
      throw makeError(
        "case_no_match",
        "gleam/queue",
        166,
        "pop_back",
        "No case clause matched",
        { values: [queue] }
      )
    }
  }
}

export function pop_front(loop$queue) {
  while (true) {
    let queue = loop$queue;
    if (queue instanceof Queue &&
    queue.in.hasLength(0) &&
    queue.out.hasLength(0)) {
      return new Error(undefined);
    } else if (queue instanceof Queue && queue.out.hasLength(0)) {
      let in$ = queue.in;
      loop$queue = new Queue(toList([]), $list.reverse(in$));
    } else if (queue instanceof Queue && queue.out.atLeastLength(1)) {
      let in$ = queue.in;
      let first = queue.out.head;
      let rest = queue.out.tail;
      let queue$1 = new Queue(in$, rest);
      return new Ok([first, queue$1]);
    } else {
      throw makeError(
        "case_no_match",
        "gleam/queue",
        206,
        "pop_front",
        "No case clause matched",
        { values: [queue] }
      )
    }
  }
}

export function reverse(queue) {
  return new Queue(queue.out, queue.in);
}

function check_equal(loop$xs, loop$x_tail, loop$ys, loop$y_tail, loop$eq) {
  while (true) {
    let xs = loop$xs;
    let x_tail = loop$x_tail;
    let ys = loop$ys;
    let y_tail = loop$y_tail;
    let eq = loop$eq;
    if (xs.hasLength(0) &&
    x_tail.hasLength(0) &&
    ys.hasLength(0) &&
    y_tail.hasLength(0)) {
      return true;
    } else if (xs.atLeastLength(1) && ys.atLeastLength(1)) {
      let x = xs.head;
      let xs$1 = xs.tail;
      let y = ys.head;
      let ys$1 = ys.tail;
      let $ = eq(x, y);
      if (!$) {
        return false;
      } else if ($) {
        loop$xs = xs$1;
        loop$x_tail = x_tail;
        loop$ys = ys$1;
        loop$y_tail = y_tail;
        loop$eq = eq;
      } else {
        throw makeError(
          "case_no_match",
          "gleam/queue",
          252,
          "check_equal",
          "No case clause matched",
          { values: [$] }
        )
      }
    } else if (xs.hasLength(0) && x_tail.atLeastLength(1)) {
      loop$xs = $list.reverse(x_tail);
      loop$x_tail = toList([]);
      loop$ys = ys;
      loop$y_tail = y_tail;
      loop$eq = eq;
    } else if (ys.hasLength(0) && y_tail.atLeastLength(1)) {
      loop$xs = xs;
      loop$x_tail = x_tail;
      loop$ys = $list.reverse(y_tail);
      loop$y_tail = toList([]);
      loop$eq = eq;
    } else {
      return false;
    }
  }
}

export function is_logically_equal(a, b, element_is_equal) {
  return check_equal(a.out, a.in, b.out, b.in, element_is_equal);
}

export function is_equal(a, b) {
  return check_equal(
    a.out,
    a.in,
    b.out,
    b.in,
    (a, b) => { return isEqual(a, b); },
  );
}
