export class CustomType {
  inspect() {
    let field = (label) => {
      let value = inspect(this[label]);
      return isNaN(parseInt(label)) ? `${label}: ${value}` : value;
    };
    let props = Object.keys(this).map(field).join(", ");
    return props ? `${this.constructor.name}(${props})` : this.constructor.name;
  }

  withFields(fields) {
    let properties = Object.keys(this).map((label) =>
      label in fields ? fields[label] : this[label]
    );
    return new this.constructor(...properties);
  }
}

export class List {
  static fromArray(array, tail) {
    let t = tail || new Empty();
    return array.reduceRight((xs, x) => new NonEmpty(x, xs), t);
  }

  static isList(data) {
    let variant = data?.__gleam_prelude_variant__;
    return variant === "EmptyList" || variant === "NonEmptyList";
  }

  [Symbol.iterator]() {
    return new ListIterator(this);
  }

  inspect() {
    return `[${this.toArray().map(inspect).join(", ")}]`;
  }

  toArray() {
    return [...this];
  }

  atLeastLength(desired) {
    for (let _ of this) {
      if (desired <= 0) return true;
      desired--;
    }
    return desired <= 0;
  }

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

export function toList(elements, tail) {
  return List.fromArray(elements, tail);
}

class ListIterator {
  #current;

  constructor(current) {
    this.#current = current;
  }

  next() {
    if (this.#current.isEmpty()) {
      return { done: true };
    } else {
      let { head, tail } = this.#current;
      this.#current = tail;
      return { value: head, done: false };
    }
  }
}

export class Empty extends List {
  get __gleam_prelude_variant__() {
    return "EmptyList";
  }

  isEmpty() {
    return true;
  }
}

export class NonEmpty extends List {
  constructor(head, tail) {
    super();
    this.head = head;
    this.tail = tail;
  }

  get __gleam_prelude_variant__() {
    return "NonEmptyList";
  }

  isEmpty() {
    return false;
  }
}

export class BitString {
  static isBitString(data) {
    return data?.__gleam_prelude_variant__ === "BitString";
  }

  constructor(buffer) {
    this.buffer = buffer;
  }

  get __gleam_prelude_variant__() {
    return "BitString";
  }

  inspect() {
    return `<<${Array.from(this.buffer).join(", ")}>>`;
  }

  get length() {
    return this.buffer.length;
  }

  byteAt(index) {
    return this.buffer[index];
  }

  floatAt(index) {
    return byteArrayToFloat(this.buffer.slice(index, index + 8));
  }

  intFromSlice(start, end) {
    return byteArrayToInt(this.buffer.slice(start, end));
  }

  sliceAfter(index) {
    return new BitString(this.buffer.slice(index));
  }
}

export class UtfCodepoint {
  constructor(value) {
    this.value = value;
  }

  get __gleam_prelude_variant__() {
    return "UtfCodepoint";
  }

  inspect() {
    return `//utfcodepoint(${String.fromCodePoint(this.value)})`;
  }
}

export function toBitString(segments) {
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
  return new BitString(new Uint8Array(view.buffer));
}

// Derrived from this answer https://stackoverflow.com/questions/8482309/converting-javascript-integer-to-byte-array-and-back
export function sizedInteger(value, size) {
  if (size < 0) {
    return new Uint8Array();
  }
  if (size % 8 != 0) {
    throw "Needs to be a byte size" + size;
  }
  var byteArray = new Uint8Array(size / 8);

  for (var index = 0; index < byteArray.length; index++) {
    var byte = value & 0xff;
    byteArray[index] = byte;
    value = (value - byte) / 256;
  }
  return byteArray.reverse();
}

export function byteArrayToInt(byteArray) {
  byteArray = byteArray.reverse();
  var value = 0;
  for (var i = byteArray.length - 1; i >= 0; i--) {
    value = value * 256 + byteArray[i];
  }

  return value;
}

export function byteArrayToFloat(byteArray) {
  return new Float64Array(byteArray.reverse().buffer)[0];
}

export function stringBits(string) {
  return new TextEncoder().encode(string);
}

export function codepointBits(codepoint) {
  return stringBits(String.fromCodePoint(codepoint.value));
}

export function float64Bits(float) {
  return new Uint8Array(Float64Array.from([float]).buffer).reverse();
}

export class Result extends CustomType {
  static isResult(data) {
    let variant = data?.__gleam_prelude_variant__;
    return variant === "Ok" || variant === "Error";
  }
}

export class Ok extends Result {
  constructor(value) {
    super();
    this[0] = value;
  }

  get __gleam_prelude_variant__() {
    return "Ok";
  }

  isOk() {
    return true;
  }
}

export class Error extends Result {
  constructor(detail) {
    super();
    this[0] = detail;
  }

  get __gleam_prelude_variant__() {
    return "Error";
  }

  isOk() {
    return false;
  }
}

export function inspect(v) {
  let t = typeof v;
  if (v === true) return "True";
  if (v === false) return "False";
  if (v === null) return "//js(null)";
  if (v === undefined) return "Nil";
  if (t === "string") return JSON.stringify(v);
  if (t === "bigint" || t === "number") return v.toString();
  if (Array.isArray(v)) return `#(${v.map(inspect).join(", ")})`;
  if (v instanceof Set) return `//js(Set(${[...v].map(inspect).join(", ")}))`;
  if (v instanceof RegExp) return `//js(${v})`;
  if (v instanceof Date) return `//js(Date("${v.toISOString()}"))`;
  if (v instanceof Function) {
    let args = [];
    for (let i of Array(v.length).keys())
      args.push(String.fromCharCode(i + 97));
    return `//fn(${args.join(", ")}) { ... }`;
  }
  try {
    return v.inspect();
  } catch (_) {
    return inspectObject(v);
  }
}

function inspectObject(v) {
  let [keys, get] = getters(v);
  let name = v.constructor.name;
  let props = [];
  for (let k of keys(v)) {
    props.push(`${inspect(k)}: ${inspect(get(v, k))}`);
  }
  let body = props.length ? " " + props.join(", ") + " " : "";
  let head = name === "Object" ? "" : name + " ";
  return `//js(${head}{${body}})`;
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
      unequalSets(a, b);
    if (unequal) return false;

    let [keys, get] = getters(a);
    for (const k of keys(a)) {
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

function isObject(a) {
  return typeof a === "object" && a !== null;
}

function structurallyCompatibleObjects(a, b) {
  if (typeof a !== "object" && typeof b !== "object" && (!a || !b))
    return false;

  let nonstructural = [Promise, WeakSet, WeakMap, Function];
  if (nonstructural.some((c) => a instanceof c)) return false;

  return (
    a.constructor === b.constructor ||
    (a.__gleam_prelude_variant__ &&
      a.__gleam_prelude_variant__ === b.__gleam_prelude_variant__)
  );
}

export function divideInt(a, b) {
  return divideFloat(a, b) | 0;
}

export function divideFloat(a, b) {
  if (b === 0) {
    return 0;
  } else {
    return a / b;
  }
}

export function throwError(variant, module, line, fn, message, extra) {
  let error = new globalThis.Error(message);
  error.gleam_error = variant;
  error.module = module;
  error.line = line;
  error.fn = fn;
  for (let k in extra) error[k] = extra[k];
  throw error;
}
