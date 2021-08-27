function define(object, name, fallback) {
  return (object[name] = globalThis[name] || fallback);
}

export const symbols = define(globalThis, "__gleam", {});
define(symbols, "variant", Symbol("variant"));
define(symbols, "inspect", Symbol("inspect"));

export class CustomType {
  [symbols.inspect]() {
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
  [symbols.inspect]() {
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

  isEmpty() {
    return "EmptyList" == this[symbols.variant];
  }
}

export function toList(elements, tail) {
  return List.fromArray(elements, tail);
}

export class Empty extends List {
  get [symbols.variant]() {
    return "EmptyList";
  }
}

export class NonEmpty extends List {
  constructor(head, tail) {
    super();
    this.head = head;
    this.tail = tail;
  }

  get [symbols.variant]() {
    return "NonEmptyList";
  }
}

export class BitString {
  constructor(buffer) {
    this.buffer = buffer;
  }

  get [symbols.variant]() {
    return "BitString";
  }

  [symbols.inspect]() {
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

  get [symbols.variant]() {
    return "UtfCodepoint";
  }

  [symbols.inspect]() {
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

export class Result extends CustomType {
  isOk() {
    return "Ok" === this[symbols.variant];
  }
}

export class Ok extends Result {
  constructor(value) {
    super();
    this[0] = value;
  }

  get [symbols.variant]() {
    return "Ok";
  }
}

export class Error extends Result {
  constructor(detail) {
    super();
    this[0] = detail;
  }

  get [symbols.variant]() {
    return "Error";
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
  if (v[symbols.inspect]) return v[symbols.inspect]();
  return inspectObject(v);
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

    let unequal =
      !sameTypeObjects(a, b) ||
      unequalDates(a, b) ||
      unequalArrayBuffers(a, b) ||
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

function unequalArrayBuffers(a, b) {
  return (
    a.buffer instanceof ArrayBuffer &&
    a.BYTES_PER_ELEMENT &&
    !(a.byteLength === b.byteLength && a.every((n, i) => n === b[i]))
  );
}

function unequalArrays(a, b) {
  return Array.isArray(a) && a.length !== b.length;
}

function sameTypeObjects(a, b) {
  return (
    typeof a === "object" &&
    typeof b === "object" &&
    a !== null &&
    b !== null &&
    (a.constructor === b.constructor ||
      (a[symbols.variant] && a[symbols.variant] === b[symbols.variant]))
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
