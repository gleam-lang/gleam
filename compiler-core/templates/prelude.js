// TODO: make equality work structurally with non-global Gleam prelude classes

class Record {
  inspect() {
    let field = (label) => {
      let value = inspect(this[label]);
      return isNaN(parseInt(label)) ? `${label}: ${value}` : value;
    };
    let props = Object.getOwnPropertyNames(this).map(field).join(", ");
    return props ? `${this.constructor.name}(${props})` : this.constructor.name;
  }
}

class List {
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
}

class Empty extends List {
  isEmpty() {
    return true;
  }
}

class NonEmpty extends List {
  constructor(head, tail) {
    super();
    this.head = head;
    this.tail = tail;
  }

  isEmpty() {
    return false;
  }
}

class BitString {
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

class UtfCodepoint {
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

class Result extends Record {}

class Ok extends Result {
  constructor(value) {
    super();
    this[0] = value;
  }

  isOk() {
    return true;
  }
}

class Error extends Result {
  constructor(detail) {
    super();
    this[0] = detail;
  }

  isOk() {
    return false;
  }
}

function inspect(v) {
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

function equal(x, y) {
  let values = [x, y];

  while (values.length) {
    let a = values.pop();
    let b = values.pop();
    if (a === b) continue;

    let unequal =
      !sameTypeObjects(a, b) || unequalDates(a, b) || unequalArrayBuffers(a, b);
    if (unequal) return false;

    for (const k of Object.getOwnPropertyNames(a)) {
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
    a.constructor === b.constructor
  );
}

// Tests

function assertEqual(a, b) {
  process.stdout.write(".");
  console.assert(equal(a, b), `\n\t${inspect(a)}\n\t  !=\n\t${inspect(b)}`);
}

function assertNotEqual(a, b) {
  process.stdout.write(".");
  console.assert(!equal(a, b), `\n\t${inspect(a)}\n\t  ==\n\t${inspect(b)}`);
}

class ExampleRecordImpl extends Record {
  constructor(first, detail, boop) {
    super();
    this[0] = first;
    this.detail = detail;
    this.boop = boop;
  }
}

let fmt = new Intl.DateTimeFormat("en-GB", { timeStyle: "medium" });
console.log(`\nRunning tests at ${fmt.format(new Date())}`);

// Equality of Gleam values

assertEqual(true, true);
assertEqual(false, false);
assertEqual(undefined, undefined);
assertNotEqual(true, false);
assertNotEqual(false, true);
assertNotEqual(undefined, false);
assertNotEqual(undefined, true);
assertNotEqual(true, undefined);
assertNotEqual(false, undefined);

assertEqual(1, 1);
assertNotEqual(1, 2);
assertEqual(1.1, 1.1);
assertNotEqual(2.1, 1.1);
assertEqual(-1, -1);
assertNotEqual(-1, 1);
assertEqual(-1.1, -1.1);
assertNotEqual(-1.1, 1.1);

assertEqual("", "");
assertEqual("123", "123");
assertEqual("👽", "👽");
assertNotEqual("👽", "👾");

assertEqual(new Ok(1), new Ok(1));
assertEqual(new Ok(2), new Ok(2));
assertEqual(new Ok(new Ok(2)), new Ok(new Ok(2)));
assertNotEqual(new Ok(1), new Ok(2));
assertNotEqual(new Ok(new Ok(2)), new Ok(new Ok(3)));

assertEqual(new Error(1), new Error(1));
assertEqual(new Error(2), new Error(2));
assertEqual(new Error(new Error(2)), new Error(new Error(2)));
assertNotEqual(new Error(2), new Error(3));
assertNotEqual(new Error(new Error(2)), new Error(new Error(3)));

assertEqual(
  new ExampleRecordImpl(undefined, 1, new Ok(2.1)),
  new ExampleRecordImpl(undefined, 1, new Ok(2.1))
);
assertNotEqual(
  new ExampleRecordImpl(undefined, 1, new Ok("2.1")),
  new ExampleRecordImpl(undefined, 1, new Ok(2.1))
);

assertEqual(List.fromArray([]), List.fromArray([]));
assertEqual(
  List.fromArray([1, 2, new Ok(1)]),
  List.fromArray([1, 2, new Ok(1)])
);
assertNotEqual(
  List.fromArray([1, 2, new Ok(1)]),
  List.fromArray([1, 2, new Ok(2)])
);
assertNotEqual(List.fromArray([1, 2]), List.fromArray([1, 2, new Ok(2)]));

assertEqual(
  new BitString(new Uint8Array([])),
  new BitString(new Uint8Array([]))
);
assertEqual(
  new BitString(new Uint8Array([1, 2, 3])),
  new BitString(new Uint8Array([1, 2, 3]))
);
assertNotEqual(
  new BitString(new Uint8Array([1, 2])),
  new BitString(new Uint8Array([1, 2, 3]))
);

assertEqual(new UtfCodepoint(128013), new UtfCodepoint(128013));
assertNotEqual(new UtfCodepoint(128013), new UtfCodepoint(128014));

// Equality of JavaScript values

assertEqual([], []);
assertEqual([1, 2], [1, 2]);
assertEqual([new Ok([1, 2])], [new Ok([1, 2])]);
assertNotEqual([], [[]]);

assertEqual({}, {});
assertEqual({ a: 1 }, { a: 1 });
assertEqual({ a: 1, b: 2 }, { b: 2, a: 1 });
assertEqual({ a: new Ok(1) }, { a: new Ok(1) });
assertNotEqual({ a: new Ok(2) }, { a: new Ok(1) });

assertEqual(new Date(0), new Date(0));
assertNotEqual(new Date(1), new Date(0));

assertEqual(new Uint8Array([1, 2]), new Uint8Array([1, 2]));
assertEqual(new Uint16Array([1, 2]), new Uint16Array([1, 2]));
assertEqual(new Uint32Array([1, 2]), new Uint32Array([1, 2]));
assertNotEqual(new Uint8Array([1, 3]), new Uint8Array([1, 2]));
assertNotEqual(new Uint16Array([1, 3]), new Uint16Array([1, 2]));
assertNotEqual(new Uint32Array([1, 3]), new Uint32Array([1, 2]));

class ExampleA {
  constructor(x) {
    this.x = x;
  }
}

class ExampleB {
  constructor(x) {
    this.x = x;
  }
}

assertEqual(new ExampleA(1), new ExampleA(1));
assertEqual(new ExampleB(1), new ExampleB(1));
assertNotEqual(new ExampleA(1), new ExampleA(2));
assertNotEqual(new ExampleA(1), new ExampleB(1));

// Inspecting Gleam values

assertEqual(inspect(true), "True");
assertEqual(inspect(false), "False");
assertEqual(inspect(undefined), "Nil");

assertEqual(inspect(0), "0");
assertEqual(inspect(1), "1");
assertEqual(inspect(2), "2");
assertEqual(inspect(-1), "-1");
assertEqual(inspect(-2), "-2");

assertEqual(inspect(0.23), "0.23");
assertEqual(inspect(1.23), "1.23");
assertEqual(inspect(2.23), "2.23");
assertEqual(inspect(-1.23), "-1.23");
assertEqual(inspect(-2.23), "-2.23");

assertEqual(inspect(new Ok(1)), "Ok(1)");
assertEqual(inspect(new Ok(true)), "Ok(True)");
assertEqual(inspect(new Ok(false)), "Ok(False)");
assertEqual(inspect(new Ok(undefined)), "Ok(Nil)");

assertEqual(inspect(new Error(2)), "Error(2)");
assertEqual(inspect(new Error(true)), "Error(True)");
assertEqual(inspect(new Error(false)), "Error(False)");
assertEqual(inspect(new Error(undefined)), "Error(Nil)");

assertEqual(
  inspect(new ExampleRecordImpl(undefined, 1, 2.1)),
  "ExampleRecordImpl(Nil, detail: 1, boop: 2.1)"
);
assertEqual(
  inspect(new ExampleRecordImpl(new Ok(1), 1, 2.1)),
  "ExampleRecordImpl(Ok(1), detail: 1, boop: 2.1)"
);

assertEqual(inspect([]), "#()");
assertEqual(inspect([1, 2, 3]), "#(1, 2, 3)");
assertEqual(inspect([new Ok(1), new Ok(2)]), "#(Ok(1), Ok(2))");

assertEqual(inspect(List.fromArray([])), "[]");
assertEqual(inspect(List.fromArray([1, 2, 3])), "[1, 2, 3]");
assertEqual(inspect(List.fromArray([new Ok(1), new Ok(2)])), "[Ok(1), Ok(2)]");

assertEqual(inspect(new BitString(new Uint8Array([]))), "<<>>");
assertEqual(inspect(new BitString(new Uint8Array([1, 2, 3]))), "<<1, 2, 3>>");

assertEqual(inspect(new UtfCodepoint(128013)), "//utfcodepoint(🐍)");

// Inspecting JavaScript values

assertEqual(inspect(null), "//js(null)");
assertEqual(inspect({}), "//js({})");
assertEqual(inspect({ a: 1 }), "//js({ a: 1 })");
assertEqual(inspect({ a: 1, b: 2 }), "//js({ a: 1, b: 2 })");
assertEqual(inspect({ a: 1, b: new Ok(1) }), "//js({ a: 1, b: Ok(1) })");
assertEqual(inspect(new globalThis.Error("stuff")), '//js(new Error("stuff"))');

console.log("\nDone.");
