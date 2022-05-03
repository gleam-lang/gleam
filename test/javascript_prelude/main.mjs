import {
  BitString,
  CustomType,
  Empty,
  Error,
  List,
  NonEmpty,
  Ok,
  UtfCodepoint,
  codepointBits,
  divideFloat,
  divideInt,
  inspect,
  isEqual,
  stringBits,
  toBitString,
  toList,
} from "./prelude.mjs";

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
  if (isEqual(a, b)) {
    pass();
  } else {
    fail(`\n\t${inspect(a)}\n\t!=\n\t${inspect(b)}`);
  }
}

function assertNotEqual(a, b) {
  if (isEqual(a, b)) {
    fail(`\n\t${inspect(a)}\n\t==\n\t${inspect(b)}`);
  } else {
    pass();
  }
}

class ExampleRecordImpl extends CustomType {
  constructor(first, detail, boop) {
    super();
    this[0] = first;
    this.detail = detail;
    this.boop = boop;
  }
}

let fmt = new Intl.DateTimeFormat("en-GB", { timeStyle: "medium" });
console.log(`Running tests at ${fmt.format(new Date())}\n`);

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

// toBitString

assertEqual(new BitString(new Uint8Array([])), toBitString([]));

assertEqual(
  new BitString(new Uint8Array([97, 98, 99])),
  toBitString([stringBits("abc")])
);

assertEqual(
  new BitString(new Uint8Array([97])),
  toBitString([codepointBits(new UtfCodepoint(97))])
);

assertEqual(
  new BitString(new Uint8Array([240, 159, 144, 141])),
  toBitString([codepointBits(new UtfCodepoint(128013))])
);

// toList

assertEqual(toList([]), List.fromArray([]));
assertEqual(toList([1, 2, 3]), List.fromArray([1, 2, 3]));
assertEqual(toList([1, 2], toList([3, 4])), List.fromArray([1, 2, 3, 4]));
assertEqual(toList([1, 2, 3], toList([4, 5])), List.fromArray([1, 2, 3, 4, 5]));

// Equality of JavaScript values

assertEqual([], []);
assertEqual([1, 2], [1, 2]);
assertEqual([new Ok([1, 2])], [new Ok([1, 2])]);
assertNotEqual([], [[]]);
assertNotEqual([], [1, []]);
assertNotEqual([1, []], []);

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

// Promises are not equal unless they have reference equality
let promise = Promise.resolve(1);
assertEqual(promise, promise);
assertNotEqual(Promise.resolve(1), Promise.resolve(1));

// Functions are not equal unless they have reference equality
let fun = () => 1;
assertEqual(fun, fun);
assertNotEqual(
  () => 1,
  () => 1
);

// Maps are compared structurally
let map = new Map([["a", 1]]);
assertEqual(map, map);
assertEqual(new Map([["a", 1]]), new Map([["a", 1]]));
assertNotEqual(
  new Map([
    ["a", 1],
    ["b", 2],
  ]),
  new Map([["a", 1]])
);
assertNotEqual(
  new Map([["a", 1]]),
  new Map([
    ["a", 1],
    ["b", 2],
  ])
);
assertNotEqual(new Map([["a", 1]]), new Map([["b", 1]]));
assertEqual(
  new Map([["a", new Map([["a", []]])]]),
  new Map([["a", new Map([["a", []]])]])
);

// Sets are compared structurally
let set = new Set(["a", 1]);
assertEqual(set, set);
assertEqual(new Set(["a", 1]), new Set(["a", 1]));
assertNotEqual(new Set(["a", 1]), new Set(["b", 1]));
assertNotEqual(new Set(["a", 1, "b"]), new Set(["a", 1]));
assertNotEqual(new Set(["a", 1]), new Set(["a", 1, "b"]));
assertNotEqual(
  new Set(["a", new Map([["a", []]])]),
  new Set(["a", new Map([["a", []]])])
);

// WeakMaps are not equal unless they have reference equality
let weak_map = new WeakMap([[map, 1]]);
assertEqual(weak_map, weak_map);
assertNotEqual(new WeakMap([[map, 1]]), new WeakMap([[map, 1]]));

// WeakSets are not equal unless they have reference equality
let weak_set = new WeakSet([map, set]);
assertEqual(weak_set, weak_set);
assertNotEqual(new WeakSet([map, set]), new WeakSet([map, set]));

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

assertEqual(new BitString(new Uint8Array([1, 2, 3])).byteAt(0), 1);
assertEqual(new BitString(new Uint8Array([1, 2, 3])).byteAt(2), 3);
assertEqual(
  new BitString(new Uint8Array([63, 240, 0, 0, 0, 0, 0, 0])).floatAt(0),
  1.0
);
assertEqual(new BitString(new Uint8Array([1, 2, 3])).intFromSlice(0, 1), 1);
assertEqual(new BitString(new Uint8Array([1, 2, 3])).intFromSlice(0, 2), 258);
assertEqual(
  new BitString(new Uint8Array([1, 2, 3])).sliceAfter(1),
  new BitString(new Uint8Array([2, 3]))
);

assertEqual(inspect(new UtfCodepoint(128013)), "//utfcodepoint(🐍)");

assertEqual(
  inspect(() => undefined),
  "//fn() { ... }"
);

assertEqual(
  inspect((a) => undefined),
  "//fn(a) { ... }"
);

assertEqual(
  inspect((x, y) => undefined),
  "//fn(a, b) { ... }"
);

assertEqual(
  inspect((x, y, z) => undefined),
  "//fn(a, b, c) { ... }"
);

// Inspecting JavaScript values

assertEqual(inspect(null), "//js(null)");
assertEqual(inspect({}), "//js({})");
assertEqual(inspect({ a: 1 }), '//js({ "a": 1 })');
assertEqual(inspect({ a: 1, b: 2 }), '//js({ "a": 1, "b": 2 })');
assertEqual(inspect({ a: 1, b: new Ok(1) }), '//js({ "a": 1, "b": Ok(1) })');
assertEqual(
  inspect(new globalThis.Error("Oh no")),
  '//js(Error { "message": "Oh no" })'
);
assertEqual(
  inspect(
    (() => {
      let error = new globalThis.Error("Oh no");
      error.other = new Ok(1);
      return error;
    })()
  ),
  '//js(Error { "message": "Oh no", "other": Ok(1) })'
);

// Generic JS objects
assertEqual(inspect(Promise.resolve(1)), "//js(Promise {})");

// Inspecting Dates
assertEqual(
  inspect(new Date("1991-01-05")),
  '//js(Date("1991-01-05T00:00:00.000Z"))'
);

// Inspecting RegExps
assertEqual(inspect(/1[23]/g), "//js(/1[23]/g)");

// Inspecting Maps
assertEqual(
  inspect(
    new Map([
      [1, 2],
      [3, new Ok([1, 2])],
    ])
  ),
  "//js(Map { 1: 2, 3: Ok(#(1, 2)) })"
);

// Inspecting Sets
assertEqual(
  inspect(new Set([1, 2, new Ok([1, 2])])),
  "//js(Set(1, 2, Ok(#(1, 2))))"
);

// Result.isOk

assertEqual(new Ok(1).isOk(), true);
assertEqual(new Error(1).isOk(), false);

// List.atLeastLength

assertEqual(List.fromArray([]).atLeastLength(0), true);
assertEqual(List.fromArray([]).atLeastLength(1), false);
assertEqual(List.fromArray([]).atLeastLength(-1), true);
assertEqual(List.fromArray([1]).atLeastLength(0), true);
assertEqual(List.fromArray([1]).atLeastLength(1), true);
assertEqual(List.fromArray([1]).atLeastLength(2), false);
assertEqual(List.fromArray([1]).atLeastLength(-1), true);

// List.hasLength

assertEqual(toList([]).hasLength(0), true);
assertEqual(toList([]).hasLength(1), false);
assertEqual(toList([]).hasLength(-1), false);
assertEqual(toList([1]).hasLength(0), false);
assertEqual(toList([1]).hasLength(1), true);
assertEqual(toList([1]).hasLength(2), false);
assertEqual(toList([1, 1]).hasLength(1), false);
assertEqual(toList([1, 1]).hasLength(2), true);
assertEqual(toList([1, 1]).hasLength(3), false);

// List iterable interface

assertEqual([...toList([])], []);
assertEqual([...toList([1, 2, 3])], [1, 2, 3]);

// BitString.length

assertEqual(new BitString(new Uint8Array([])).length, 0);
assertEqual(new BitString(new Uint8Array([1, 2])).length, 2);
assertEqual(new BitString(new Uint8Array([1, 2, 3, 4])).length, 4);

//
// Division
//

assertEqual(divideInt(1, 0), 0);
assertEqual(divideInt(1, 1), 1);
assertEqual(divideInt(1, 2), 0);
assertEqual(divideInt(3, 2), 1);
assertEqual(divideInt(11, 3), 3);
assertEqual(divideInt(-1, 0), 0);
assertEqual(divideInt(-1, 1), -1);
assertEqual(divideInt(-1, 2), -0);
assertEqual(divideInt(-3, 2), -1);
assertEqual(divideInt(-11, 3), -3);
assertEqual(divideInt(1, -1), -1);
assertEqual(divideInt(1, -2), 0);
assertEqual(divideInt(3, -2), -1);
assertEqual(divideInt(11, -3), -3);
assertEqual(divideInt(-1, -1), 1);
assertEqual(divideInt(-1, -2), 0);
assertEqual(divideInt(-3, -2), 1);
assertEqual(divideInt(-11, -3), 3);

assertEqual(divideFloat(1.5, 0.0), 0.0);
assertEqual(divideFloat(1.5, 2.0), 0.75);
assertEqual(divideFloat(1.5, 2.5), 0.6);
assertEqual(divideFloat(-1.5, 0.0), -0.0);
assertEqual(divideFloat(-1.5, 2.0), -0.75);
assertEqual(divideFloat(-1.5, 2.5), -0.6);
assertEqual(divideFloat(1.5, -0.0), -0.0);
assertEqual(divideFloat(1.5, -2.0), -0.75);
assertEqual(divideFloat(1.5, -2.5), -0.6);
assertEqual(divideFloat(-1.5, -0.0), 0.0);
assertEqual(divideFloat(-1.5, -2.0), 0.75);
assertEqual(divideFloat(-1.5, -2.5), 0.6);

// Record updates

assertEqual(new Ok(1).withFields({ 0: 2 }), new Ok(2));
assertEqual(new Error(1).withFields({ 0: 2 }), new Error(2));

assertEqual(
  new ExampleRecordImpl(1, 2, 3).withFields({}),
  new ExampleRecordImpl(1, 2, 3)
);
assertEqual(
  new ExampleRecordImpl(1, 2, 3).withFields({ boop: 6, 0: 40 }),
  new ExampleRecordImpl(40, 2, 6)
);
assertEqual(
  new ExampleRecordImpl(1, 2, 3).withFields({ boop: 4, detail: 5, 0: 6 }),
  new ExampleRecordImpl(6, 5, 4)
);

//
// Summary
//

console.log(`

${passes + failures} tests
${passes} passes
${failures} failures
`);

if (failures) process.exit(1);
