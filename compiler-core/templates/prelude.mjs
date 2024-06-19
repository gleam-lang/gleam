// Values marked with @internal are not part of the public API and may change
// without notice.

export class CustomType {
  withFields(fields) {
    let properties = Object.keys(this).map((label) =>
      label in fields ? fields[label] : this[label],
    );
    return new this.constructor(...properties);
  }
}

export class List {
  static fromArray(array, tail) {
    let t = tail || new Empty();
    for (let i = array.length - 1; i >= 0; --i) {
      t = new NonEmpty(array[i], t);
    }
    return t;
  }

  [Symbol.iterator]() {
    return new ListIterator(this);
  }

  toArray() {
    return [...this];
  }

  // @internal
  atLeastLength(desired) {
    for (let _ of this) {
      if (desired <= 0) return true;
      desired--;
    }
    return desired <= 0;
  }

  // @internal
  hasLength(desired) {
    for (let _ of this) {
      if (desired <= 0) return false;
      desired--;
    }
    return desired === 0;
  }

  countLength() {
    let length = 0;
    for (let _ of this) length++;
    return length;
  }
}

// @internal
export function prepend(element, tail) {
  return new NonEmpty(element, tail);
}

export function toList(elements, tail) {
  return List.fromArray(elements, tail);
}

// @internal
class ListIterator {
  #current;

  constructor(current) {
    this.#current = current;
  }

  next() {
    if (this.#current instanceof Empty) {
      return { done: true };
    } else {
      let { head, tail } = this.#current;
      this.#current = tail;
      return { value: head, done: false };
    }
  }
}

export class Empty extends List {}

export class NonEmpty extends List {
  constructor(head, tail) {
    super();
    this.head = head;
    this.tail = tail;
  }
}

export class BitArray {
  constructor(buffer) {
    if (!(buffer instanceof Uint8Array)) {
      throw "BitArray can only be constructed from a Uint8Array";
    }
    this.buffer = buffer;
  }

  // @internal
  get length() {
    return this.buffer.length;
  }

  // @internal
  byteAt(index) {
    return this.buffer[index];
  }

  // @internal
  floatFromSlice(start, end, isBigEndian) {
    return byteArrayToFloat(this.buffer, start, end, isBigEndian);
  }

  // @internal
  intFromSlice(start, end, isBigEndian, isSigned) {
    return byteArrayToInt(this.buffer, start, end, isBigEndian, isSigned);
  }

  // @internal
  binaryFromSlice(start, end) {
    return new BitArray(this.buffer.slice(start, end));
  }

  // @internal
  sliceAfter(index) {
    return new BitArray(this.buffer.slice(index));
  }
}

export class UtfCodepoint {
  constructor(value) {
    this.value = value;
  }
}

// @internal
export function toBitArray(segments) {
  let size = (segment) =>
    segment instanceof Uint8Array ? segment.byteLength : 1;
  let bytes = segments.reduce((acc, segment) => acc + size(segment), 0);
  let view = new DataView(new ArrayBuffer(bytes));
  let cursor = 0;
  for (let segment of segments) {
    if (segment instanceof Uint8Array) {
      new Uint8Array(view.buffer).set(segment, cursor);
      cursor += segment.byteLength;
    } else {
      view.setInt8(cursor, segment);
      cursor++;
    }
  }
  return new BitArray(new Uint8Array(view.buffer));
}

// @internal
// Derived from this answer https://stackoverflow.com/questions/8482309/converting-javascript-integer-to-byte-array-and-back
export function sizedInt(value, size, isBigEndian) {
  if (size < 0) {
    return new Uint8Array();
  }
  if (size % 8 != 0) {
    const msg = `Bit arrays must be byte aligned on JavaScript, got size of ${size} bits`;
    throw new globalThis.Error(msg);
  }

  const byteArray = new Uint8Array(size / 8);

  // Convert negative number to two's complement representation
  if (value < 0) {
    value = (2 ** size) + value;
  }

  if (isBigEndian) {
    for (let i = 0; i < byteArray.length; i++) {
      const byte = value % 256
      byteArray[i] = byte;
      value = (value - byte) / 256;
    }
  } else {
    for (let i = byteArray.length - 1; i >= 0; i--) {
      const byte = value % 256
      byteArray[i] = byte;
      value = (value - byte) / 256;
    }
  }

  return byteArray.reverse();
}

// @internal
export function byteArrayToInt(byteArray, start, end, isBigEndian, isSigned) {
  let value = 0;

  // Read bytes as an unsigned integer value
  if (isBigEndian) {
    for (let i = start; i < end; i++) {
      value = value * 256 + byteArray[i];
    }
  } else {
    for (let i = end - 1; i >= start; i--) {
      value = value * 256 + byteArray[i];
    }
  }

  if (isSigned) {
    const byteSize = end - start;

    const highBit = 2 ** (byteSize * 8 - 1);

    // If the high bit is set and this is a signed integer, reinterpret as
    // two's complement
    if (value >= highBit) {
      value -= highBit * 2;
    }
  }

  return value;
}

// @internal
export function byteArrayToFloat(byteArray, start, end, isBigEndian) {
  const view = new DataView(byteArray.buffer);

  const byteSize = end - start;

  if (byteSize === 8) {
    return view.getFloat64(start, !isBigEndian)
  } else if (byteSize === 4) {
    return view.getFloat32(start, !isBigEndian)
  } else {
    const msg = `Sized floats must be 32-bit or 64-bit on JavaScript, got size of ${byteSize * 8} bits`;
    throw new globalThis.Error(msg);
  }
}

// @internal
export function stringBits(string) {
  return new TextEncoder().encode(string);
}

// @internal
export function codepointBits(codepoint) {
  return stringBits(String.fromCodePoint(codepoint.value));
}

// @internal
export function sizedFloat(float, size, isBigEndian) {
  if (size !== 32 && size !== 64) {
    const msg = `Sized floats must be 32-bit or 64-bit on JavaScript, got size of ${size} bits`;
    throw new globalThis.Error(msg);
  }

  const byteArray = new Uint8Array(size / 8);

  const view = new DataView(byteArray.buffer);

  if (size == 64) {
    view.setFloat64(0, float, !isBigEndian);
  } else if (size === 32) {
    view.setFloat32(0, float, !isBigEndian);
  }
  
  return byteArray;
}

export class Result extends CustomType {
  // @internal
  static isResult(data) {
    return data instanceof Result;
  }
}

export class Ok extends Result {
  constructor(value) {
    super();
    this[0] = value;
  }

  // @internal
  isOk() {
    return true;
  }
}

export class Error extends Result {
  constructor(detail) {
    super();
    this[0] = detail;
  }

  // @internal
  isOk() {
    return false;
  }
}

export function isEqual(x, y) {
  let values = [x, y];

  while (values.length) {
    let a = values.pop();
    let b = values.pop();
    if (a === b) continue;

    if (!isObject(a) || !isObject(b)) return false;
    let unequal =
      !structurallyCompatibleObjects(a, b) ||
      unequalDates(a, b) ||
      unequalBuffers(a, b) ||
      unequalArrays(a, b) ||
      unequalMaps(a, b) ||
      unequalSets(a, b) ||
      unequalRegExps(a, b);
    if (unequal) return false;

    const proto = Object.getPrototypeOf(a);
    if (proto !== null && typeof proto.equals === "function") {
      try {
        if (a.equals(b)) continue;
        else return false;
      } catch {}
    }

    let [keys, get] = getters(a);
    for (let k of keys(a)) {
      values.push(get(a, k), get(b, k));
    }
  }

  return true;
}

function getters(object) {
  if (object instanceof Map) {
    return [(x) => x.keys(), (x, y) => x.get(y)];
  } else {
    let extra = object instanceof globalThis.Error ? ["message"] : [];
    return [(x) => [...extra, ...Object.keys(x)], (x, y) => x[y]];
  }
}

function unequalDates(a, b) {
  return a instanceof Date && (a > b || a < b);
}

function unequalBuffers(a, b) {
  return (
    a.buffer instanceof ArrayBuffer &&
    a.BYTES_PER_ELEMENT &&
    !(a.byteLength === b.byteLength && a.every((n, i) => n === b[i]))
  );
}

function unequalArrays(a, b) {
  return Array.isArray(a) && a.length !== b.length;
}

function unequalMaps(a, b) {
  return a instanceof Map && a.size !== b.size;
}

function unequalSets(a, b) {
  return (
    a instanceof Set && (a.size != b.size || [...a].some((e) => !b.has(e)))
  );
}

function unequalRegExps(a, b) {
  return a instanceof RegExp && (a.source !== b.source || a.flags !== b.flags);
}

function isObject(a) {
  return typeof a === "object" && a !== null;
}

function structurallyCompatibleObjects(a, b) {
  if (typeof a !== "object" && typeof b !== "object" && (!a || !b))
    return false;

  let nonstructural = [Promise, WeakSet, WeakMap, Function];
  if (nonstructural.some((c) => a instanceof c)) return false;

  return a.constructor === b.constructor;
}

// @internal
export function remainderInt(a, b) {
  if (b === 0) {
    return 0;
  } else {
    return a % b;
  }
}

// @internal
export function divideInt(a, b) {
  return Math.trunc(divideFloat(a, b));
}

// @internal
export function divideFloat(a, b) {
  if (b === 0) {
    return 0;
  } else {
    return a / b;
  }
}

// @internal
export function makeError(variant, module, line, fn, message, extra) {
  let error = new globalThis.Error(message);
  error.gleam_error = variant;
  error.module = module;
  error.line = line;
  error.fn = fn;
  for (let k in extra) error[k] = extra[k];
  return error;
}
