function echo(value, message, file, line) {
  const grey = "\u001b[90m";
  const reset_color = "\u001b[39m";
  const file_line = `${file}:${line}`;
  const inspector = new Echo$Inspector();
  const string_value = inspector.inspect(value);
  const string_message = message === undefined ? "" : " " + message;

  if (globalThis.process?.stderr?.write) {
    // If we're in Node.js, use `stderr`
    const string = `${grey}${file_line}${reset_color}${string_message}\n${string_value}\n`;
    globalThis.process.stderr.write(string);
  } else if (globalThis.Deno) {
    // If we're in Deno, use `stderr`
    const string = `${grey}${file_line}${reset_color}${string_message}\n${string_value}\n`;
    globalThis.Deno.stderr.writeSync(new TextEncoder().encode(string));
  } else {
    // Otherwise, use `console.log`
    // The browser's console.log doesn't support ansi escape codes
    const string = `${file_line}\n${string_value}`;
    globalThis.console.log(string);
  }

  return value;
}

class Echo$Inspector {
  #references = new Set();

  #isDict(value) {
    try {
      // We can only check if an object is a stdlib Dict if it is one of the
      // project's dependencies.
      // The `Dict` class is the default export of `stdlib/dict.mjs`
      // that we import as `$stdlib$dict`.
      return value instanceof $stdlib$dict.default;
    } catch {
      // If stdlib is not one of the project's dependencies then `$stdlib$dict`
      // will not have been imported and the check will throw an exception meaning
      // we can't check if something is actually a `Dict`.
      return false;
    }
  }

  #float(float) {
    const string = float.toString().replace("+", "");
    if (string.indexOf(".") >= 0) {
      return string;
    } else {
      const index = string.indexOf("e");
      if (index >= 0) {
        return string.slice(0, index) + ".0" + string.slice(index);
      } else {
        return string + ".0";
      }
    }
  }

  inspect(v) {
    const t = typeof v;
    if (v === true) return "True";
    if (v === false) return "False";
    if (v === null) return "//js(null)";
    if (v === undefined) return "Nil";
    if (t === "string") return this.#string(v);
    if (t === "bigint" || Number.isInteger(v)) return v.toString();
    if (t === "number") return this.#float(v);
    if (v instanceof $UtfCodepoint) return this.#utfCodepoint(v);
    if (v instanceof $BitArray) return this.#bit_array(v);
    if (v instanceof RegExp) return `//js(${v})`;
    if (v instanceof Date) return `//js(Date("${v.toISOString()}"))`;
    if (v instanceof globalThis.Error) return `//js(${v.toString()})`;
    if (v instanceof Function) {
      const args = [];
      for (const i of Array(v.length).keys())
        args.push(String.fromCharCode(i + 97));
      return `//fn(${args.join(", ")}) { ... }`;
    }

    if (this.#references.size === this.#references.add(v).size) {
      return "//js(circular reference)";
    }

    let printed;
    if (Array.isArray(v)) {
      printed = `#(${v.map((v) => this.inspect(v)).join(", ")})`;
    } else if (v instanceof $List) {
      printed = this.#list(v);
    } else if (v instanceof $CustomType) {
      printed = this.#customType(v);
    } else if (this.#isDict(v)) {
      printed = this.#dict(v);
    } else if (v instanceof Set) {
      return `//js(Set(${[...v].map((v) => this.inspect(v)).join(", ")}))`;
    } else {
      printed = this.#object(v);
    }
    this.#references.delete(v);
    return printed;
  }

  #object(v) {
    const name = Object.getPrototypeOf(v)?.constructor?.name || "Object";
    const props = [];
    for (const k of Object.keys(v)) {
      props.push(`${this.inspect(k)}: ${this.inspect(v[k])}`);
    }
    const body = props.length ? " " + props.join(", ") + " " : "";
    const head = name === "Object" ? "" : name + " ";
    return `//js(${head}{${body}})`;
  }

  #dict(map) {
    let body = "dict.from_list([";
    let first = true;

    let key_value_pairs = [];
    map.forEach((value, key) => {
      key_value_pairs.push([key, value]);
    });
    key_value_pairs.sort();
    key_value_pairs.forEach(([key, value]) => {
      if (!first) body = body + ", ";
      body = body + "#(" + this.inspect(key) + ", " + this.inspect(value) + ")";
      first = false;
    });
    return body + "])";
  }

  #customType(record) {
    const props = Object.keys(record)
      .map((label) => {
        const value = this.inspect(record[label]);
        return isNaN(parseInt(label)) ? `${label}: ${value}` : value;
      })
      .join(", ");
    return props
      ? `${record.constructor.name}(${props})`
      : record.constructor.name;
  }

  #list(list) {
    if (list instanceof $Empty) {
      return "[]";
    }

    let char_out = 'charlist.from_string("';
    let list_out = "[";

    let current = list;
    while (current instanceof $NonEmpty) {
      let element = current.head;
      current = current.tail;

      if (list_out !== "[") {
        list_out += ", ";
      }
      list_out += this.inspect(element);

      if (char_out) {
        if (Number.isInteger(element) && element >= 32 && element <= 126) {
          char_out += String.fromCharCode(element);
        } else {
          char_out = null;
        }
      }
    }

    if (char_out) {
      return char_out + '")';
    } else {
      return list_out + "]";
    }
  }

  #string(str) {
    let new_str = '"';
    for (let i = 0; i < str.length; i++) {
      const char = str[i];
      switch (char) {
        case "\n":
          new_str += "\\n";
          break;
        case "\r":
          new_str += "\\r";
          break;
        case "\t":
          new_str += "\\t";
          break;
        case "\f":
          new_str += "\\f";
          break;
        case "\\":
          new_str += "\\\\";
          break;
        case '"':
          new_str += '\\"';
          break;
        default:
          if (char < " " || (char > "~" && char < "\u{00A0}")) {
            new_str +=
              "\\u{" +
              char.charCodeAt(0).toString(16).toUpperCase().padStart(4, "0") +
              "}";
          } else {
            new_str += char;
          }
      }
    }
    new_str += '"';
    return new_str;
  }

  #utfCodepoint(codepoint) {
    return `//utfcodepoint(${String.fromCodePoint(codepoint.value)})`;
  }

  #bit_array(bits) {
    if (bits.bitSize === 0) {
      return "<<>>";
    }

    let acc = "<<";

    for (let i = 0; i < bits.byteSize - 1; i++) {
      acc += bits.byteAt(i).toString();
      acc += ", ";
    }

    if (bits.byteSize * 8 === bits.bitSize) {
      acc += bits.byteAt(bits.byteSize - 1).toString();
    } else {
      const trailingBitsCount = bits.bitSize % 8;
      acc += bits.byteAt(bits.byteSize - 1) >> (8 - trailingBitsCount);
      acc += `:size(${trailingBitsCount})`;
    }

    acc += ">>";
    return acc;
  }
}
