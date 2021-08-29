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
  inspect() {
    return `[${this.toArray().map(inspect).join(", ")}]`;
  }

  static fromArray(array, tail) {
    let t = tail || new Empty();
    return array.reduceRight((xs, x) => new NonEmpty(x, xs), t);
  }

  toArray() {
    let current = this;
    let array = [];
    while (!current.isEmpty()) {
      array.push(current.head);
      current = current.tail;
    }
    return array;
  }

  atLeastLength(desired) {
    let current = this;
    while (!current.isEmpty()) {
      if (desired <= 0) return true;
      desired--;
      current = current.tail;
    }
    return desired <= 0;
  }

  hasLength(desired) {
    let current = this;
    while (!current.isEmpty()) {
      if (desired <= 0) return false;
      desired--;
      current = current.tail;
    }
    return desired == 0;
  }
}

export function toList(elements, tail) {
  return List.fromArray(elements, tail);
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

export function stringBits(string) {
  return new TextEncoder().encode(string);
}

export function codepointBits(codepoint) {
  return stringBits(String.fromCodePoint(codepoint.value));
}

export class Result extends CustomType {}

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
  if (v instanceof RegExp) return `//js(${v})`;
  if (v instanceof Date) return `//js(Date("${v.toISOString()}"))`;
  try {
    return v.inspect();
  } catch (_) {
    return inspectObject(v);
  }
}

function inspectObject(v) {
  let property = (k) => `${k}: ${inspect(v[k])}`;
  let name = v.constructor.name;
  let names = Object.getOwnPropertyNames(v);
  let props = names.length ? " " + names.map(property).join(", ") + " " : "";
  let head = name === "Object" ? "" : name + " ";
  return `//js(${head}{${props}})`;
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
      unequalArrays(a, b);
    if (unequal) return false;

    for (const k of Object.keys(a)) {
      values.push(a[k], b[k]);
    }
  }

  return true;
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

function isObject(a) {
  return typeof a === "object" && a !== null;
}

function structurallyCompatibleObjects(a, b) {
  if (typeof a !== "object" && typeof b !== "object" && (!a || !b))
    return false;

  let nonstructural = [Promise, Set, Map, WeakSet, WeakMap];
  if (nonstructural.some((c) => a instanceof c)) return false;

  return (
    a.constructor === b.constructor ||
    (a.__gleam_prelude_variant__ &&
      a.__gleam_prelude_variant__ === b.__gleam_prelude_variant__)
  );
}

export function divideInt(a, b) {
  if (b === 0) {
    return 0 | 0;
  } else {
    return (a / b) | 0;
  }
}

export function divideFloat(a, b) {
  if (b === 0) {
    return 0;
  } else {
    return a / b;
  }
}
