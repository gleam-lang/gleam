import {
  BitArray,
  CustomType,
  Error,
  List,
  Ok,
  UtfCodepoint,
  codepointBits,
  divideFloat,
  divideInt,
  isEqual,
  stringBits,
  toBitArray,
  toList,
  sizedInt,
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

function inspect(a) {
  if (typeof a === "object" && a !== null && typeof a.inspect === "function") {
    return a.inspect();
  } else {
    return JSON.stringify(a);
  }
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

function assertThrows(msg, callable) {
  try {
    callable();
    fail(msg);
  } catch (error) {
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
  new ExampleRecordImpl(undefined, 1, new Ok(2.1)),
);
assertNotEqual(
  new ExampleRecordImpl(undefined, 1, new Ok("2.1")),
  new ExampleRecordImpl(undefined, 1, new Ok(2.1)),
);

assertEqual(List.fromArray([]), List.fromArray([]));
assertEqual(
  List.fromArray([1, 2, new Ok(1)]),
  List.fromArray([1, 2, new Ok(1)]),
);
assertNotEqual(
  List.fromArray([1, 2, new Ok(1)]),
  List.fromArray([1, 2, new Ok(2)]),
);
assertNotEqual(List.fromArray([1, 2]), List.fromArray([1, 2, new Ok(2)]));
assertNotEqual(List.fromArray([1]), List.fromArray([]));
assertNotEqual(List.fromArray([]), List.fromArray([1]));

assertEqual(new BitArray(new Uint8Array([])), new BitArray(new Uint8Array([])));
assertEqual(
  new BitArray(new Uint8Array([1, 2, 3])),
  new BitArray(new Uint8Array([1, 2, 3])),
);
assertNotEqual(
  new BitArray(new Uint8Array([1, 2])),
  new BitArray(new Uint8Array([1, 2, 3])),
);

assertEqual(new UtfCodepoint(128013), new UtfCodepoint(128013));
assertNotEqual(new UtfCodepoint(128013), new UtfCodepoint(128014));

// toBitArray

assertEqual(new BitArray(new Uint8Array([])), toBitArray([]));

const testValues = [
  { input: 0, u8: 0 },
  { input: 1, u8: 1 },
  { input: 127, u8: 127 },
  { input: 128, u8: 128 },
  { input: 129, u8: 129 },
  { input: 255, u8: 255 },
  { input: 256, u8: 0 },
  { input: 257, u8: 1 },
  { input: 2000, u8: 208 },
  { input: 0, u8: 0 },
  { input: -1, u8: 255 },
  { input: -127, u8: 129 },
  { input: -128, u8: 128 },
  { input: -129, u8: 127 },
  { input: -255, u8: 1 },
  { input: -256, u8: 0 },
  { input: -257, u8: 255 },
  { input: -2000, u8: 48 },
];

for (const { input, u8 } of testValues) {
  assertEqual(new BitArray(new Uint8Array([u8])), toBitArray([input]));
}

assertEqual(new BitArray(new Uint8Array([])), toBitArray([new Uint8Array([])]));
assertEqual(
  new BitArray(new Uint8Array([1, 2, 4, 8])),
  toBitArray([new Uint8Array([1, 2, 4, 8])])
);

assertEqual(
  new BitArray(new Uint8Array(testValues.map((t) => t.u8))),
  toBitArray(testValues.map((t) => t.input))
);

assertEqual(
  new BitArray(
    new Uint8Array([1, 2, 4, 8, ...testValues.map((t) => t.u8), 80, 90, 100])
  ),
  toBitArray([
    new Uint8Array([]),
    new Uint8Array([1, 2, 4, 8]),
    ...testValues.map((t) => t.input),
    new Uint8Array([80, 90]),
    new Uint8Array([]),
    new Uint8Array([100]),
  ])
);

assertEqual(
  new BitArray(new Uint8Array([97, 98, 99])),
  toBitArray([stringBits("abc")]),
);

assertEqual(
  new BitArray(new Uint8Array([97])),
  toBitArray([codepointBits(new UtfCodepoint(97))]),
);

assertEqual(
  new BitArray(new Uint8Array([240, 159, 144, 141])),
  toBitArray([codepointBits(new UtfCodepoint(128013))]),
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
  () => 1,
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
  new Map([["a", 1]]),
);
assertNotEqual(
  new Map([["a", 1]]),
  new Map([
    ["a", 1],
    ["b", 2],
  ]),
);
assertNotEqual(new Map([["a", 1]]), new Map([["b", 1]]));
assertEqual(
  new Map([["a", new Map([["a", []]])]]),
  new Map([["a", new Map([["a", []]])]]),
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
  new Set(["a", new Map([["a", []]])]),
);

// WeakMaps are not equal unless they have reference equality
let weak_map = new WeakMap([[map, 1]]);
assertEqual(weak_map, weak_map);
assertNotEqual(new WeakMap([[map, 1]]), new WeakMap([[map, 1]]));

// WeakSets are not equal unless they have reference equality
let weak_set = new WeakSet([map, set]);
assertEqual(weak_set, weak_set);
assertNotEqual(new WeakSet([map, set]), new WeakSet([map, set]));

// RegExp are compared structurally
let re = new RegExp("test", "g");
let re_literal = /test/g;
assertEqual(re, re);
assertEqual(re_literal, re_literal);
assertEqual(re, re_literal);
assertNotEqual(re, new RegExp("test", "i"));
assertNotEqual(re, new RegExp("test"));
assertNotEqual(re_literal, new RegExp("test", "i"));
assertNotEqual(re_literal, /test/);
assertNotEqual(re_literal, new RegExp("test", "i"));
assertNotEqual(re, /test/i);

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

// Custom .equals() method
class NoCustomEquals {
  constructor(id, notImportant) {
    this.id = id;
    this.notImportant = notImportant;
  }
}
class HasCustomEqualsThatThrows {
  constructor(id, notImportant) {
    this.id = id;
    this.notImportant = notImportant;
  }
  equals() {
    throw "not today";
  }
}
class HasCustomEquals {
  constructor(id, notImportant) {
    this.id = id;
    this.notImportant = notImportant;
  }
  equals(o) {
    return this.id === o.id;
  }
}
function testCustomEquals(o) {
  this.id === o.id;
}
const hasEqualsField = {
  id: 1,
  notImportant: 2,
  equals: testCustomEquals,
};
const hasEqualsField2 = {
  id: 1,
  notImportant: 3,
  equals: testCustomEquals,
};
// no custom equals, use structural equality
assertEqual(new NoCustomEquals(1, 1), new NoCustomEquals(1, 1));
assertNotEqual(new NoCustomEquals(1, 1), new NoCustomEquals(1, 2));
// custom equals throws, fallback to structural equality
assertEqual(
  new HasCustomEqualsThatThrows(1, 1),
  new HasCustomEqualsThatThrows(1, 1),
);
assertNotEqual(
  new HasCustomEqualsThatThrows(1, 1),
  new HasCustomEqualsThatThrows(1, 2),
);
// custom equals works, use it
assertEqual(new HasCustomEquals(1, 1), new HasCustomEquals(1, 1));
assertEqual(new HasCustomEquals(1, 1), new HasCustomEquals(1, 2));
assertNotEqual(new HasCustomEquals(1, 1), new HasCustomEquals(2, 1));
// custom equals defined on object instead of prototype, don't use it
assertEqual(hasEqualsField, { ...hasEqualsField });
assertNotEqual(hasEqualsField, hasEqualsField2);

// BitArray

assertEqual(new BitArray(new Uint8Array([1, 2, 3])).byteAt(0), 1);
assertEqual(new BitArray(new Uint8Array([1, 2, 3])).byteAt(2), 3);
assertEqual(new BitArray(new Uint8Array([1, 2, 3])).intFromSlice(0, 1, true, false), 1);
assertEqual(new BitArray(new Uint8Array([160, 2, 3])).intFromSlice(0, 1, false, true), -96);
assertEqual(new BitArray(new Uint8Array([1, 2, 3])).intFromSlice(0, 2, true, false), 258);
assertEqual(new BitArray(new Uint8Array([1, 2, 3])).intFromSlice(0, 2, false, false), 513);
assertEqual(new BitArray(new Uint8Array([1, 160, 3])).intFromSlice(0, 2, false, true), -24575);
assertEqual(new BitArray(new Uint8Array([160, 2, 3])).intFromSlice(0, 2, true, false), 40962);
assertEqual(new BitArray(new Uint8Array([160, 2, 3])).intFromSlice(0, 2, true, true), -24574);
assertEqual(
  new BitArray(new Uint8Array([255, 255, 255, 255, 255, 255, 255])).intFromSlice(0, 7, true, true),
  -1,
);
assertEqual(
  new BitArray(new Uint8Array([255, 255, 255, 255, 255, 255, 254])).intFromSlice(0, 7, true, false),
  Number(0xFFFFFFFFFFFFFEn),
);
assertEqual(
  new BitArray(new Uint8Array([63, 240, 0, 0, 0, 0, 0, 0])).floatFromSlice(0, 8, true),
  1.0,
);
assertEqual(
  new BitArray(new Uint8Array([0, 0, 0, 0, 0, 0, 240, 63])).floatFromSlice(0, 8, false),
  1.0,
);
assertEqual(
  new BitArray(new Uint8Array([0xC9, 0x74, 0x24, 0x00])).floatFromSlice(0, 4, true),
  -1000000.0,
);
assertEqual(
  new BitArray(new Uint8Array([0x00, 0x24, 0x74, 0xC9])).floatFromSlice(0, 4, false),
  -1000000.0,
);
assertEqual(
  new BitArray(new Uint8Array([1, 2, 3, 4, 5])).binaryFromSlice(1, 4),
  new BitArray(new Uint8Array([2, 3, 4])),
);
assertEqual(
  new BitArray(new Uint8Array([1, 2, 3])).sliceAfter(1),
  new BitArray(new Uint8Array([2, 3])),
);
assertEqual(
  new BitArray(new Uint8Array([1, 2, 3, 4, 5]))
    .binaryFromSlice(1, 4)
    .sliceAfter(1),
  new BitArray(new Uint8Array([3, 4]))
);

// sizedInt()

assertEqual(
  sizedInt(100, 0, true),
  new Uint8Array([]),
);
assertEqual(
  sizedInt(0, 32, true),
  new Uint8Array([0, 0, 0, 0]),
);
assertEqual(
  sizedInt(1, 24, true),
  new Uint8Array([0, 0, 1]),
);
assertEqual(
  sizedInt(-1, 32, true),
  new Uint8Array([255, 255, 255, 255]),
);
assertEqual(
  sizedInt(80000, 16, true),
  new Uint8Array([56, 128]),
);
assertEqual(
  sizedInt(-80000, 16, true),
  new Uint8Array([199, 128]),
);
assertEqual(
  sizedInt(-489_391_639_457_909_760, 56, true),
  new Uint8Array([53, 84, 229, 150, 16, 180, 0]),
);
assertEqual(
  sizedInt(-1, 64, true),
  new Uint8Array([255, 255, 255, 255, 255, 255, 255, 255]),
);
assertEqual(
  sizedInt(Number.MAX_SAFE_INTEGER, 64, true),
  new Uint8Array([0, 31, 255, 255, 255, 255, 255, 255]),
);
assertEqual(
  sizedInt(Number.MIN_SAFE_INTEGER, 64, true),
  new Uint8Array([255, 224, 0, 0, 0, 0, 0, 1]),
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

// BitArray.length

assertEqual(new BitArray(new Uint8Array([])).length, 0);
assertEqual(new BitArray(new Uint8Array([1, 2])).length, 2);
assertEqual(new BitArray(new Uint8Array([1, 2, 3, 4])).length, 4);

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
  new ExampleRecordImpl(1, 2, 3),
);
assertEqual(
  new ExampleRecordImpl(1, 2, 3).withFields({ boop: 6, 0: 40 }),
  new ExampleRecordImpl(40, 2, 6),
);
assertEqual(
  new ExampleRecordImpl(1, 2, 3).withFields({ boop: 4, detail: 5, 0: 6 }),
  new ExampleRecordImpl(6, 5, 4),
);

// Test BitArray can only be constructed from Uint8Array, not ArrayBuffer
const bs1 = new BitArray(new Uint8Array(new ArrayBuffer(8)));
assertThrows("Should only construct BitArray from Uint8Array", () => {
  const bs = new BitArray(new ArrayBuffer(8));
});

//
// Summary
//

console.log(`

${passes + failures} tests
${passes} passes
${failures} failures
`);

if (failures) process.exit(1);
