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
  sizedFloat,
  bitArraySlice,
  bitArraySliceToInt,
  bitArraySliceToFloat,
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

assertEqual(new UtfCodepoint(128013), new UtfCodepoint(128013));
assertNotEqual(new UtfCodepoint(128013), new UtfCodepoint(128014));

// new BitArray()

assertEqual(
  new BitArray(new Uint8Array([1, 2, 3])),
  new BitArray(new Uint8Array([1, 2, 3]), 24),
);

assertThrows("`new BitArray()` throws with an ArrayBuffer", () => {
  new BitArray(new ArrayBuffer(8));
});

assertThrows(
  "`new BitArray()` throws with a raw array",
  () => new BitArray([1, 2]),
);

assertThrows(
  "`new BitArray()` throws with invalid bit size",
  () => new BitArray(new Uint8Array([2]), -1),
);

assertThrows(
  "`new BitArray()` throws with too many bytes for the bit size",
  () => new BitArray(new Uint8Array([1, 2]), 7),
);

assertThrows(
  "`new BitArray()` throws with too few bytes for the bit size",
  () => new BitArray(new Uint8Array([1, 2]), 17),
);

assertThrows(
  "`new BitArray()` throws with an invalid bit offset",
  () => new BitArray(new Uint8Array([1]), 0, -1),
);

assertThrows(
  "`new BitArray()` throws with an invalid bit offset",
  () => new BitArray(new Uint8Array([1]), 0, 8),
);

// toBitArray()

assertEqual(
  new BitArray(new Uint8Array([1, 2])),
  toBitArray([new BitArray(new Uint8Array([1, 2]))]),
);

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
  toBitArray([new Uint8Array([1, 2, 4, 8])]),
);

assertEqual(
  new BitArray(new Uint8Array(testValues.map((t) => t.u8))),
  toBitArray(testValues.map((t) => t.input)),
);

assertEqual(
  new BitArray(
    new Uint8Array([1, 2, 4, 8, ...testValues.map((t) => t.u8), 80, 90, 100]),
  ),
  toBitArray([
    new Uint8Array([]),
    new Uint8Array([1, 2, 4, 8]),
    ...testValues.map((t) => t.input),
    new Uint8Array([80, 90]),
    new Uint8Array([]),
    new Uint8Array([100]),
  ]),
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

assertEqual(
  new BitArray(new Uint8Array([240, 159, 144, 141, 0xfe]), 39),
  toBitArray([
    new BitArray(new Uint8Array([240, 159, 144])),
    new BitArray(new Uint8Array([141])),
    new BitArray(new Uint8Array([0xfe]), 7),
  ]),
);

assertEqual(
  new BitArray(
    new Uint8Array([240, 159, 144, 0xa9, 0b11101010, 0xf7, 0x39, 0xae]),
    64,
  ),
  toBitArray([
    new BitArray(new Uint8Array([240, 159, 144])),
    new BitArray(new Uint8Array([0b10110101]), 4, 3),
    new BitArray(new Uint8Array([0x9f]), 7),
    new BitArray(new Uint8Array([0b00010100]), 4, 2),
    new BitArray(new Uint8Array([0]), 1),
    new BitArray(new Uint8Array([0xaf, 0x73, 0x9a, 0xee]), 24, 4),
  ]),
);

assertEqual(
  new BitArray(
    new Uint8Array([
      129, 145, 57, 255, 255, 255, 255, 255, 191, 157, 243, 182, 246, 62, 104,
      49, 62, 31, 110, 200, 120, 13, 88,
    ]),
    181,
  ),
  toBitArray([
    sizedInt(2, 2, false),
    sizedInt(0, 2, false),
    sizedInt(200, 11, true),
    sizedInt(-100, 49, false),
    sizedFloat(-1.234, 32, true),
    sizedFloat(-8.2e40, 64, false),
    sizedInt(0xf, 5, false),
    new Uint8Array([1]),
    0xab,
  ]),
);

// BitArray.equals()

assertEqual(new BitArray(new Uint8Array([])), new BitArray(new Uint8Array([])));
assertEqual(
  new BitArray(new Uint8Array([1, 2, 3])),
  new BitArray(new Uint8Array([1, 2, 3])),
);
assertNotEqual(
  new BitArray(new Uint8Array([1, 2])),
  new BitArray(new Uint8Array([1, 2, 3])),
);
assertNotEqual(
  new BitArray(new Uint8Array([1, 0xf0]), 12),
  new BitArray(new Uint8Array([2, 0xf0]), 12),
);
assertNotEqual(
  new BitArray(new Uint8Array([1, 0xf0]), 12),
  new BitArray(new Uint8Array([1, 0xf0]), 13),
);
assertNotEqual(
  new BitArray(new Uint8Array([0x12, 0x30]), 12),
  new BitArray(new Uint8Array([0x12, 0x4f]), 12),
);
assertEqual(
  new BitArray(new Uint8Array([0x12, 0x30]), 12),
  new BitArray(new Uint8Array([0x12, 0x3f]), 12),
);
assertEqual(
  new BitArray(new Uint8Array([0b10110110, 0b01101001]), 8, 3),
  new BitArray(new Uint8Array([0b10110011]), 8),
);
assertEqual(
  new BitArray(new Uint8Array([0b10110110, 0b01101001, 0b10011010]), 17, 5),
  new BitArray(new Uint8Array([0b11001101, 0b00110011, 0b01000001]), 17),
);
assertEqual(
  new BitArray(new Uint8Array([0b11001101, 0b00110011]), 14, 1),
  new BitArray(new Uint8Array([0b11100110, 0b10011001]), 14, 2),
);
assertEqual(
  new BitArray(new Uint8Array([0b10110110]), 4, 2),
  new BitArray(new Uint8Array([0b10111011]), 4, 3),
);
assertNotEqual(
  new BitArray(new Uint8Array([0b10110110, 0b10110110]), 9, 2),
  new BitArray(new Uint8Array([0b10110111, 0b10100110]), 9, 2),
);
assertNotEqual(
  new BitArray(new Uint8Array([0b10110110, 0b10110110]), 9, 2),
  new BitArray(new Uint8Array([0b10110110, 0b10000110]), 9, 2),
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

assertNotEqual(new Map([]), new Map([["b", 1]]));

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

// Objects
assertEqual({ a: 1 }, { a: 1 });
assertEqual({ a: 1, b: 2 }, { b: 2, a: 1 });

assertEqual({ a: {} }, { a: {} });
assertEqual({ a: 1, b: { c: 3 } }, { a: 1, b: { c: 3 } });

assertNotEqual({ a: 1 }, {});
assertNotEqual({ a: 1, b: 2 }, { b: 2 });
assertNotEqual({}, { a: 1 });
assertNotEqual({ b: 2 }, { a: 1, b: 2 });

// BitArray

assertEqual(new BitArray(new Uint8Array([1, 2, 3])).byteAt(0), 1);
assertEqual(new BitArray(new Uint8Array([1, 2, 3])).byteAt(2), 3);

// bitArraySlice

assertThrows("`bitArraySlice()` throws if start is less than zero", () =>
  bitArraySlice(new BitArray(new Uint8Array([1])), -1, 8),
);

assertThrows(
  "`bitArraySlice()` throws if start is greater than the bit size",
  () => bitArraySlice(new BitArray(new Uint8Array([1])), 9, 8),
);

assertThrows("`bitArraySlice()` throws if end is before start", () =>
  bitArraySlice(new BitArray(new Uint8Array([1])), 4, 2),
);

assertThrows(
  "`bitArraySlice()` throws if end is greater than the bit size",
  () => bitArraySlice(new Uint8Array([1]), 0, 10),
);

assertEqual(
  bitArraySlice(new BitArray(new Uint8Array([1, 2, 3])), 0, 0),
  new BitArray(new Uint8Array([])),
);

assertEqual(
  bitArraySlice(new BitArray(new Uint8Array([0xbe, 0xff])), 0, 2),
  new BitArray(new Uint8Array([0xbe]), 2),
);

assertEqual(
  bitArraySlice(new BitArray(new Uint8Array([0x12, 0b10101101, 0xff])), 10, 14),
  new BitArray(new Uint8Array([0b10110100]), 4),
);

assertEqual(
  bitArraySlice(new BitArray(new Uint8Array([1, 2, 3])), 8, 24),
  new BitArray(new Uint8Array([2, 3])),
);

assertEqual(
  bitArraySlice(new BitArray(new Uint8Array([1, 2, 3, 4, 5])), 8, 32),
  new BitArray(new Uint8Array([2, 3, 4])),
);

assertEqual(
  bitArraySlice(new BitArray(new Uint8Array([1, 0xfe, 0xa1])), 8, 19),
  new BitArray(new Uint8Array([0xfe, 0xa1]), 11),
);

assertEqual(
  bitArraySlice(
    new BitArray(new Uint8Array([17, 79, 190, 151, 98, 222, 101])),
    19,
    51,
  ),
  new BitArray(new Uint8Array([244, 187, 22, 243]), 32),
);

assertEqual(
  bitArraySlice(new BitArray(new Uint8Array([0, 0, 0, 0, 0xff, 0xe0])), 37, 41),
  new BitArray(new Uint8Array([0b11111100]), 4),
);

assertEqual(
  bitArraySliceToFloat(
    bitArraySlice(
      new BitArray(new Uint8Array([0xaa, 0xbb, 0xff, 0, 0, 0])),
      8,
      48,
    ),
    8,
    40,
    false,
  ),
  3.5733110840282835e-43,
);

assertEqual(
  bitArraySlice(
    bitArraySlice(new BitArray(new Uint8Array([1, 2, 3, 4, 5])), 8, 32),
    8,
  ),
  new BitArray(new Uint8Array([3, 4])),
);

assertEqual(
  bitArraySlice(
    new BitArray(new Uint8Array([0b00000001, 0b00000010, 0b00000011]), 20, 2),
    4,
    10,
  ),
  new BitArray(new Uint8Array([0b00000001, 0b00000010]), 6, 6),
);

assertEqual(
  bitArraySlice(new BitArray(new Uint8Array([1])), 1),
  new BitArray(new Uint8Array([0b00000010]), 7),
);

// sizedFloat()

assertEqual(sizedFloat(0.0, 16, true), new Uint8Array([0x00, 0x00]));
assertEqual(sizedFloat(1.0, 16, true), new Uint8Array([0x3c, 0x00]));
assertEqual(sizedFloat(-1.0, 16, false), new Uint8Array([0x00, 0xbc]));
assertEqual(sizedFloat(1.234375, 16, true), new Uint8Array([0x3c, 0xf0]));
assertEqual(sizedFloat(-65_504.0, 16, false), new Uint8Array([0xff, 0xfb]));
assertEqual(
  sizedFloat(-0.00001519918441772461, 16, true),
  new Uint8Array([0x80, 0xff]),
);
assertEqual(sizedFloat(Infinity, 16, true), new Uint8Array([0x7c, 0x00]));
assertEqual(sizedFloat(-Infinity, 16, false), new Uint8Array([0x00, 0xfc]));
assertEqual(sizedFloat(NaN, 16, true), new Uint8Array([0x7e, 0x00]));
assertEqual(sizedFloat(1_000_000.0, 16, true), new Uint8Array([0x7c, 0x00]));
assertEqual(sizedFloat(-1_000_000.0, 16, true), new Uint8Array([0xfc, 0x00]));

assertEqual(sizedFloat(16.25, 32, true), new Uint8Array([65, 130, 0, 0]));
assertEqual(sizedFloat(-16.25, 32, false), new Uint8Array([0, 0, 130, 193]));
assertEqual(
  sizedFloat(1000.5, 64, true),
  new Uint8Array([64, 143, 68, 0, 0, 0, 0, 0]),
);
assertEqual(
  sizedFloat(-1000.5, 64, false),
  new Uint8Array([0, 0, 0, 0, 0, 68, 143, 192]),
);

// sizedInt()

assertEqual(sizedInt(100, -8, true), new Uint8Array([]));
assertEqual(sizedInt(100, 0, true), new Uint8Array([]));
assertEqual(sizedInt(100, 8, true), new Uint8Array([100]));
assertEqual(sizedInt(-10, 8, true), new Uint8Array([246]));

assertEqual(sizedInt(1, 1, true), new BitArray(new Uint8Array([0x80]), 1));
assertEqual(sizedInt(2, 2, true), new BitArray(new Uint8Array([0x80]), 2));
assertEqual(sizedInt(15, 4, true), new BitArray(new Uint8Array([0xf0]), 4));
assertEqual(sizedInt(100, 7, true), new BitArray(new Uint8Array([200]), 7));
assertEqual(sizedInt(-44, 7, true), new BitArray(new Uint8Array([168]), 7));
assertEqual(sizedInt(-1, 6, true), new BitArray(new Uint8Array([252]), 6));
assertEqual(sizedInt(-1231, 3, true), new BitArray(new Uint8Array([32]), 3));
assertEqual(sizedInt(1231, 5, true), new BitArray(new Uint8Array([120]), 5));

assertEqual(
  sizedInt(773, 10, true),
  new BitArray(new Uint8Array([193, 64]), 10),
);
assertEqual(
  sizedInt(-276, 10, false),
  new BitArray(new Uint8Array([236, 128]), 10),
);
assertEqual(sizedInt(80000, 16, true), new Uint8Array([56, 128]));
assertEqual(sizedInt(-80000, 16, true), new Uint8Array([199, 128]));
assertEqual(sizedInt(1, 24, true), new Uint8Array([0, 0, 1]));
assertEqual(
  sizedInt(-100, 31, false),
  new BitArray(new Uint8Array([156, 255, 255, 254]), 31),
);
assertEqual(sizedInt(0, 32, true), new Uint8Array([0, 0, 0, 0]));
assertEqual(sizedInt(-1, 32, true), new Uint8Array([255, 255, 255, 255]));
assertEqual(
  sizedInt(-10, 33, true),
  new BitArray(new Uint8Array([255, 255, 255, 251, 0]), 33),
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
assertEqual(
  sizedInt(Number.MAX_SAFE_INTEGER, 77, true),
  new BitArray(
    new Uint8Array([0, 0, 0, 255, 255, 255, 255, 255, 255, 248]),
    77,
  ),
);
assertEqual(
  sizedInt(Number.MIN_SAFE_INTEGER, 75, false),
  new BitArray(new Uint8Array([1, 0, 0, 0, 0, 0, 224, 255, 255, 224]), 75),
);
assertEqual(
  sizedInt(Number(9444732965739289353650176n), 75, true),
  new BitArray(new Uint8Array([255, 255, 255, 255, 255, 248, 0, 0, 0, 0]), 75),
);

// bitArraySliceToFloat()

assertEqual(
  bitArraySliceToFloat(toBitArray([63, 240, 0, 0, 0, 0, 0, 0]), 0, 64, true),
  1.0,
);
assertEqual(
  bitArraySliceToFloat(toBitArray([0, 0, 0, 0, 0, 0, 240, 63]), 0, 64, false),
  1.0,
);
assertEqual(
  bitArraySliceToFloat(toBitArray([0xff, 0xc9, 0x74, 0x24, 0x00]), 8, 40, true),
  -1000000.0,
);
assertEqual(
  bitArraySliceToFloat(toBitArray([0x00, 0x24, 0x74, 0xc9]), 0, 32, false),
  -1000000.0,
);
assertEqual(
  bitArraySliceToFloat(
    new BitArray(new Uint8Array([112, 152, 127, 244, 0, 7, 192]), 50, 0),
    11,
    43,
    true,
  ),
  -511.25,
);
assertEqual(
  bitArraySliceToFloat(
    new BitArray(
      new Uint8Array([8, 0, 0, 0, 1, 129, 39, 103, 129, 254]),
      79,
      0,
    ),
    7,
    71,
    false,
  ),
  -5011.75,
);
assertEqual(
  bitArraySliceToFloat(
    new BitArray(new Uint8Array([212, 152, 127, 244, 0, 7, 192]), 50, 3),
    11,
    43,
    true,
  ),
  1.0714967429001753e-19,
);

assertEqual(
  bitArraySliceToFloat(new BitArray(new Uint8Array([0x00, 0x00])), 0, 16, true),
  0.0,
);
assertEqual(
  bitArraySliceToFloat(new BitArray(new Uint8Array([0x3c, 0x00])), 0, 16, true),
  1.0,
);
assertEqual(
  bitArraySliceToFloat(
    new BitArray(new Uint8Array([0x00, 0xbc])),
    0,
    16,
    false,
  ),
  -1.0,
);
assertEqual(
  bitArraySliceToFloat(
    new BitArray(new Uint8Array([0xf0, 0x3c])),
    0,
    16,
    false,
  ),
  1.234375,
);
assertEqual(
  bitArraySliceToFloat(new BitArray(new Uint8Array([0xfb, 0xff])), 0, 16, true),
  -65_504.0,
);
assertEqual(
  bitArraySliceToFloat(
    new BitArray(new Uint8Array([0xff, 0x80])),
    0,
    16,
    false,
  ),
  -0.00001519918441772461,
);
assertEqual(
  bitArraySliceToFloat(new BitArray(new Uint8Array([0x7c, 0x00])), 0, 16, true),
  Infinity,
);
assertEqual(
  bitArraySliceToFloat(
    new BitArray(new Uint8Array([0x00, 0xfc])),
    0,
    16,
    false,
  ),
  -Infinity,
);
assertEqual(
  isNaN(
    bitArraySliceToFloat(
      new BitArray(new Uint8Array([0x7e, 0x00])),
      0,
      16,
      true,
    ),
  ),
  true,
);

// bitArraySliceToInt()

assertEqual(
  bitArraySliceToInt(toBitArray([0b10011110]), 0, 4, true, false),
  0b1001,
);
assertEqual(
  bitArraySliceToInt(
    new BitArray(new Uint8Array([0b11001110]), 7, 1),
    0,
    4,
    true,
    false,
  ),
  0b1001,
);
assertEqual(
  bitArraySliceToInt(toBitArray([0b10011110]), 4, 8, true, false),
  0b1110,
);
assertEqual(
  bitArraySliceToInt(toBitArray([0b10011110]), 1, 6, true, true),
  0b00111,
);
assertEqual(bitArraySliceToInt(toBitArray([0b10011010]), 3, 8, true, true), -6);
assertEqual(
  bitArraySliceToInt(toBitArray([0b10011010]), 0, 7, true, true),
  -51,
);
assertEqual(
  bitArraySliceToInt(
    new BitArray(new Uint8Array([0b11001100, 0b00100000]), 11),
    1,
    11,
    false,
    false,
  ),
  408,
);
assertEqual(
  bitArraySliceToInt(toBitArray([0xb6, 0xe3]), 0, 12, true, false),
  0xb6e,
);
assertEqual(
  bitArraySliceToInt(toBitArray([0xb6, 0xe3]), 0, 12, false, false),
  0xeb6,
);
assertEqual(
  bitArraySliceToInt(toBitArray([0xff, 0xb6, 0xe3]), 8, 20, true, false),
  0xb6e,
);
assertEqual(
  bitArraySliceToInt(toBitArray([0xff, 0xb6, 0xe3]), 20, 24, true, false),
  0x03,
);
assertEqual(
  bitArraySliceToInt(toBitArray([0xff, 0xb6, 0xe3]), 8, 20, false, false),
  0xeb6,
);
assertEqual(
  bitArraySliceToInt(toBitArray([0xa5, 0x6c, 0xaa]), 5, 18, true, false),
  5554,
);
assertEqual(
  bitArraySliceToInt(toBitArray([0xa5, 0x6c, 0xaa]), 5, 18, false, true),
  -3411,
);
assertEqual(bitArraySliceToInt(toBitArray([1, 2, 3]), 0, 8, true, false), 1);
assertEqual(
  bitArraySliceToInt(toBitArray([160, 2, 3]), 0, 8, false, true),
  -96,
);
assertEqual(bitArraySliceToInt(toBitArray([1, 2, 3]), 0, 16, true, false), 258);
assertEqual(
  bitArraySliceToInt(toBitArray([1, 2, 3]), 0, 16, false, false),
  513,
);
assertEqual(
  bitArraySliceToInt(toBitArray([1, 160, 3]), 0, 16, false, true),
  -24575,
);
assertEqual(
  bitArraySliceToInt(toBitArray([160, 2, 3]), 0, 16, true, false),
  40962,
);
assertEqual(
  bitArraySliceToInt(toBitArray([3, 160, 2]), 8, 24, true, true),
  -24574,
);
assertEqual(
  bitArraySliceToInt(
    new BitArray(new Uint8Array([146, 192, 70, 25, 128]), 33),
    1,
    24,
    true,
    true,
  ),
  1_228_870,
);
assertEqual(
  bitArraySliceToInt(toBitArray([255, 255, 255, 255]), 0, 32, false, true),
  -1,
);
assertEqual(
  bitArraySliceToInt(
    new BitArray(new Uint8Array([217, 150, 209, 191, 0]), 33),
    1,
    33,
    true,
    false,
  ),
  3_006_112_638,
);
assertEqual(
  bitArraySliceToInt(
    new BitArray(new Uint8Array([146, 192, 70, 25, 128]), 33),
    1,
    33,
    true,
    true,
  ),
  629_181_491,
);
assertEqual(
  bitArraySliceToInt(
    new BitArray(new Uint8Array([251, 24, 47, 227, 128]), 33),
    1,
    33,
    false,
    false,
  ),
  3_344_904_438,
);
assertEqual(
  bitArraySliceToInt(
    new BitArray(new Uint8Array([240, 102, 91, 101, 128]), 33),
    0,
    33,
    false,
    false,
  ),
  5_995_456_240,
);
assertEqual(
  bitArraySliceToInt(
    toBitArray([231, 255, 255, 255, 254, 123]),
    0,
    40,
    true,
    true,
  ),
  -103_079_215_106,
);
assertEqual(
  bitArraySliceToInt(
    toBitArray([0, 231, 255, 255, 253, 123, 17]),
    1,
    55,
    true,
    false,
  ),
  127_543_348_739_464,
);
assertEqual(
  bitArraySliceToInt(
    toBitArray([142, 231, 255, 255, 253, 123, 17, 139]),
    8,
    62,
    false,
    true,
  ),
  -8425025061257241,
);
assertEqual(
  bitArraySliceToInt(
    toBitArray([142, 231, 255, 255, 253, 123, 17, 139]),
    7,
    62,
    false,
    true,
  ),
  -8293899692933261,
);
assertEqual(
  bitArraySliceToInt(
    toBitArray([142, 231, 255, 255, 253, 123, 17]),
    8,
    48,
    true,
    true,
  ),
  -103_079_215_749,
);
assertEqual(
  bitArraySliceToInt(
    toBitArray([255, 255, 255, 255, 255, 255, 255]),
    0,
    56,
    true,
    true,
  ),
  -1,
);
assertEqual(
  bitArraySliceToInt(
    toBitArray([0x00, 0xaa, 255, 255, 255, 255, 255]),
    0,
    56,
    true,
    false,
  ),
  0xaaffffffffff,
);
assertEqual(
  bitArraySliceToInt(
    toBitArray([255, 255, 255, 255, 255, 0xaa, 0x00]),
    0,
    56,
    false,
    false,
  ),
  0xaaffffffffff,
);
assertEqual(
  bitArraySliceToInt(
    toBitArray([255, 255, 255, 255, 255, 255, 255]),
    0,
    56,
    true,
    false,
  ),
  Number(0xfffffffffffffen),
);
assertEqual(
  bitArraySliceToInt(toBitArray([0xfe, 0x3f]), 4, 12, true, false),
  0xe3,
);
assertEqual(bitArraySliceToInt(toBitArray([253, 94]), 3, 11, true, true), -22);
assertEqual(
  bitArraySliceToInt(toBitArray([233, 164]), 3, 15, true, false),
  1234,
);
assertEqual(
  bitArraySliceToInt(toBitArray([250, 72]), 3, 15, false, false),
  1234,
);
assertEqual(
  bitArraySliceToInt(toBitArray([250, 72, 223, 189]), 7, 29, true, false),
  596983,
);
assertEqual(
  bitArraySliceToInt(
    toBitArray([250, 72, 223, 189, 41, 97, 165, 0, 0, 0, 0, 177]),
    14,
    85,
    false,
    true,
  ),
  70821197049655,
);
assertEqual(
  bitArraySliceToInt(
    toBitArray([250, 72, 223, 189, 41, 97, 165, 0, 0, 0, 0, 177]),
    14,
    85,
    true,
    true,
  ),
  Number(515_906_807_693_217_628_160n),
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

// BitArray.byteSize

assertEqual(new BitArray(new Uint8Array([])).byteSize, 0);
assertEqual(new BitArray(new Uint8Array([1, 2])).byteSize, 2);
assertEqual(new BitArray(new Uint8Array([1, 2, 3, 4])).byteSize, 4);
assertEqual(new BitArray(new Uint8Array([1, 2, 3, 4], 28)).byteSize, 4);

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

//
// Summary
//

console.log(`

${passes + failures} tests
${passes} passes
${failures} failures
`);

if (failures) process.exit(1);
