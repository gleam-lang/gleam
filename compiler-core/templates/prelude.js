// TODO: bit string

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

class List extends Record {
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
    while (current instanceof NonEmpty) {
      array.push(current.head);
      current = current.tail;
    }
    return array;
  }
}

class Empty extends List {}

class NonEmpty extends List {
  constructor(head, tail) {
    super();
    this.head = head;
    this.tail = tail;
  }
}

class Result extends Record {}

class Ok extends Result {
  constructor(value) {
    super();
    this[0] = value;
  }
}

class Error extends Result {
  constructor(detail) {
    super();
    this[0] = detail;
  }
}

class Thing extends Record {
  constructor(first, detail, boop) {
    super();
    this[0] = first;
    this.detail = detail;
    this.boop = boop;
  }
}

function inspect(value) {
  if (value === true) return "True";
  if (value === false) return "False";
  if (value === undefined) return "Nil";
  try {
    if (typeof value.inspect === "function") return value.inspect();
  } catch (error) {}
  if (Array.isArray(value)) return `#(${value.map(inspect).join(", ")})`;
  // TODO: different syntax for js stuff
  // TODO: bigints
  return JSON.stringify(value);
}

function equal(x, y) {
  let values = [x, y];
  while (values.length !== 0) {
    let a = values.pop();
    let b = values.pop();
    if (a === b || uintArrayEqual(a, b)) continue;
    if (objectType(a) !== objectType(b)) return false;
    for (let k of Object.getOwnPropertyNames(a)) values.push(a[k], b[k]);
  }
  return true;
}

function uintArrayEqual(a, b) {
  return (
    a instanceof Uint8Array &&
    b instanceof Uint8Array &&
    a.byteLength === b.byteLength &&
    a.every((x, i) => x === b[i])
  );
}

function objectType(object) {
  if (object !== null && typeof object === "object") {
    return object.constructor.name;
  } else {
    return typeof object;
  }
}

console.log("");
console.log("");

let x = new Record();
console.log(x.inspect());

console.log(inspect(true));
console.log(inspect(false));
console.log(inspect(undefined));

let ok = new Ok(1);
console.log(ok.inspect());

let error = new Error("err");
console.log(error.inspect());

let thing = new Thing([1, 2, "hello"], "ok", { a: 1 });
console.log(thing.inspect());

console.log(List.fromArray([]).inspect());
console.log(List.fromArray([1, 2, new Ok([1])]).inspect());
