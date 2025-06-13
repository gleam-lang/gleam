function echo(value, file, line) {
  const grey = "\u001b[90m";
  const reset_color = "\u001b[39m";
  const file_line = `${file}:${line}`;
  const string_value = new Echo$Inspector.inspect(value);

  if (globalThis.process?.stderr?.write) {
    // If we're in Node.js, use `stderr`
    const string = `${grey}${file_line}${reset_color}\n${string_value}\n`;
    process.stderr.write(string);
  } else if (globalThis.Deno) {
    // If we're in Deno, use `stderr`
    const string = `${grey}${file_line}${reset_color}\n${string_value}\n`;
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

  inspect(v) {
    const t = typeof v;
    if (v === true) return "True";
    if (v === false) return "False";
    if (v === null) return "//js(null)";
    if (v === undefined) return "Nil";
    if (t === "string") return this.#string(v);
    if (t === "bigint" || Number.isInteger(v)) return v.toString();
    if (t === "number") return float_to_string(v);
    if (v instanceof UtfCodepoint) return this.#utfCodepoint(v);
    if (v instanceof BitArray) return this.#bit_array(v);
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

    if (Array.isArray(v))
      return `#(${v.map((v) => this.inspect(v)).join(", ")})`;
    if (v instanceof List) return this.#list(v);
    if (v instanceof CustomType) return this.#customType(v);
    if (v instanceof Dict) return this.#dict(v);
    if (v instanceof Set)
      return `//js(Set(${[...v].map((v) => this.inspect(v)).join(", ")}))`;
    return this.#object(v);
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
    map.forEach((value, key) => {
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
    if (list instanceof Empty) {
      return "[]";
    }

    let char_out = 'charlist.from_string("';
    let list_out = "[";

    let current = list;
    while (current instanceof NonEmpty) {
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
    let acc = "<<";
    if (bits.bitSize === 0) {
      return acc;
    }

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
