const variant = globalThis.__gleam_prelude_variant || Symbol("gleam");
globalThis.__gleam_prelude_variant = variant;

export class Record {
  inspect() {
    let field = (label) => {
      let value = inspect(this[label]);
      return isNaN(parseInt(label)) ? `${label}: ${value}` : value;
    };
    let props = Object.getOwnPropertyNames(this).map(field).join(", ");
    return props ? `${this.constructor.name}(${props})` : this.constructor.name;
  }
}

export class List {
  inspect() {
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
    return "EmptyList" == this[variant];
  }
}

export class Empty extends List {
  [variant] = "EmptyList";
}

export class NonEmpty extends List {
  [variant] = "NonEmptyList";

  constructor(head, tail) {
    super();
    this.head = head;
    this.tail = tail;
  }
}

export class BitString {
  [variant] = "BitString";

  constructor(buffer) {
    this.buffer = buffer;
  }

  inspect() {
    return `<<${Array.from(this.buffer).join(", ")}>>`;
  }

  size() {
    return this.buffer.length;
  }
}

export class UtfCodepoint {
  [variant] = "UtfCodepoint";

  constructor(value) {
    this.value = value;
  }

  toBuffer() {
    return new Uint8Array(String.fromCodePoint(this.value));
  }

  inspect() {
    return `//utfcodepoint(${String.fromCodePoint(this.value)})`;
  }
}

export class Result extends Record {
  isOk() {
    return "Ok" === this[variant];
  }
}

export class Ok extends Result {
  [variant] = "Ok";

  constructor(value) {
    super();
    this[0] = value;
  }
}

export class Error extends Result {
  [variant] = "Error";

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
  try {
    if (typeof v.inspect === "function") return v.inspect();
  } catch (error) {}
  let entries = Object.entries(v);
  if (entries.length) {
    let properties = entries.map(([k, v]) => `${k}: ${inspect(v)}`).join(", ");
    return `//js({ ${properties} })`;
  } else {
    return `//js({})`;
  }
}

export function equal(x, y) {
  let values = [x, y];

  while (values.length) {
    let a = values.pop();
    let b = values.pop();
    if (a === b) continue;

    let unequal =
      !sameTypeObjects(a, b) || unequalDates(a, b) || unequalArrayBuffers(a, b);
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

function sameTypeObjects(a, b) {
  return (
    typeof a === "object" &&
    typeof b === "object" &&
    a !== null &&
    b !== null &&
    (a.constructor === b.constructor ||
      (a[variant] && a[variant] === b[variant]))
  );
}
