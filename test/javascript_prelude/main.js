import {
  BitString,
  Empty,
  Error,
  List,
  NonEmpty,
  Ok,
  Record,
  UtfCodepoint,
  equal,
  inspect,
} from "./prelude.js";

// TODO: test methods to be used by generated code
// - result.isOk
// - list.atLeastLength

let failures = 0;
let passes = 0;

function pass() {
  process.stdout.write(`\u001b[${32}m.\u001b[${0}m`);
  passes++;
}

function fail(message) {
  console.log("");
  console.assert(false, message);
  failures++;
}

function assertEqual(a, b) {
  if (equal(a, b)) {
    pass();
  } else {
    fail(`\n\t${inspect(a)}\n\t!=\n\t${inspect(b)}`);
  }
}

function assertNotEqual(a, b) {
  if (equal(a, b)) {
    fail(`\n\t${inspect(a)}\n\t==\n\t${inspect(b)}`);
  } else {
    pass();
  }
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
console.log(`\nRunning tests at ${fmt.format(new Date())}\n`);

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
assertNotEqual(List.fromArray([1]), List.fromArray([]));
assertNotEqual(List.fromArray([]), List.fromArray([1]));

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

// Equality between Gleam prelude types from different packages
//
// The prelude is not global, each package gets one to fit into the JavaScript
// module system. We work around this by having a single global
// `globalThis.__gleam_prelude_variant` symbol and using this in place of the
// constructor to check if Gleam values are of the same kind.
//
// In these tests we simulate a different prelude by creating new version of the
// prelude types through inheritance.

class AnotherEmpty extends Empty {}
class AnotherNonEmpty extends NonEmpty {}
class AnotherBitString extends BitString {}
class AnotherOk extends Ok {}
class AnotherError extends Error {}
class AnotherUtfCodepoint extends UtfCodepoint {}

assertEqual(List.fromArray([]), new AnotherEmpty());
assertEqual(List.fromArray([1]), new AnotherNonEmpty(1, new AnotherEmpty()));
assertNotEqual(List.fromArray([1]), new AnotherEmpty());
assertEqual(new AnotherEmpty(), List.fromArray([]));
assertEqual(new AnotherNonEmpty(1, new AnotherEmpty()), List.fromArray([1]));
assertNotEqual(new AnotherEmpty(), List.fromArray([1]));

assertEqual(new Ok(1), new AnotherOk(1));
assertEqual(new AnotherOk(1), new Ok(1));
assertNotEqual(new Ok(2), new AnotherOk(1));
assertNotEqual(new AnotherOk(2), new Ok(1));

assertEqual(new Error(1), new AnotherError(1));
assertEqual(new AnotherError(1), new Error(1));
assertNotEqual(new Error(2), new AnotherError(1));
assertNotEqual(new AnotherError(2), new Error(1));

assertEqual(
  new BitString(new Uint8Array([1, 2])),
  new AnotherBitString(new Uint8Array([1, 2]))
);
assertEqual(
  new AnotherBitString(new Uint8Array([1, 2])),
  new BitString(new Uint8Array([1, 2]))
);
assertNotEqual(
  new BitString(new Uint8Array([2, 2])),
  new AnotherBitString(new Uint8Array([1, 2]))
);
assertNotEqual(
  new AnotherBitString(new Uint8Array([2, 2])),
  new BitString(new Uint8Array([1, 2]))
);

assertEqual(new UtfCodepoint(128013), new AnotherUtfCodepoint(128013));
assertEqual(new AnotherUtfCodepoint(128013), new UtfCodepoint(128013));
assertNotEqual(new UtfCodepoint(128014), new AnotherUtfCodepoint(128013));
assertNotEqual(new AnotherUtfCodepoint(128014), new UtfCodepoint(128013));

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

console.log(`

${passes + failures} tests
${passes} passes
${failures} failures
`);

if (failures) process.exit(1);
