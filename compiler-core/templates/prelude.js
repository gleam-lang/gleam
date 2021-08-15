function define(object, name, fallback) {
  return (object[name] = globalThis[name] || fallback);
}

export const symbols = define(globalThis, "__gleam", {});
define(symbols, "variant", Symbol("variant"));
define(symbols, "inspect", Symbol("inspect"));

export class Record {
  [symbols.inspect]() {
    let field = (label) => {
      let value = inspect(this[label]);
      return isNaN(parseInt(label)) ? `${label}: ${value}` : value;
    };
    let props = Object.getOwnPropertyNames(this).map(field).join(", ");
    return props ? `${this.constructor.name}(${props})` : this.constructor.name;
  }
}

export class List {
  [symbols.inspect]() {
    return `[${this.toArray().map(inspect).join(", ")}]`;
  }

  static fromArray(array) {
    return array.reduceRight(
      (list, element) => new NonEmpty(element, list),
      new Empty()
    );
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

  isEmpty() {
    return "EmptyList" == this[symbols.variant];
  }
}

export class Empty extends List {
  [symbols.variant] = "EmptyList";
}

export class NonEmpty extends List {
  [symbols.variant] = "NonEmptyList";

  constructor(head, tail) {
    super();
    this.head = head;
    this.tail = tail;
  }
}

export class BitString {
  [symbols.variant] = "BitString";

  constructor(buffer) {
    this.buffer = buffer;
  }

  [symbols.inspect]() {
    return `<<${Array.from(this.buffer).join(", ")}>>`;
  }

  size() {
    return this.buffer.length;
  }
}

export class UtfCodepoint {
  [symbols.variant] = "UtfCodepoint";

  constructor(value) {
    this.value = value;
  }

  toBuffer() {
    return new Uint8Array(String.fromCodePoint(this.value));
  }

  [symbols.inspect]() {
    return `//utfcodepoint(${String.fromCodePoint(this.value)})`;
  }
}

export function stringBits(string) {
  return new TextEncoder().encode(string);
}

export function codepointBits(codepoint) {
  return utf8Bits(String.fromCodePoint(codepoint));
}

export class Result extends Record {
  isOk() {
    return "Ok" === this[symbols.variant];
  }
}

export class Ok extends Result {
  [symbols.variant] = "Ok";

  constructor(value) {
    super();
    this[0] = value;
  }
}

export class Error extends Result {
  [symbols.variant] = "Error";

  constructor(detail) {
    super();
    this[0] = detail;
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
  if (v instanceof globalThis.Error)
    return `//js(new ${v.constructor.name}(${inspect(v.message)}))`;
  if (v[symbols.inspect]) return v[symbols.inspect]();
  let entries = Object.entries(v);
  if (entries.length) {
    let properties = entries.map(([k, v]) => `${k}: ${inspect(v)}`).join(", ");
    return `//js({ ${properties} })`;
  } else {
    return `//js({})`;
  }
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
