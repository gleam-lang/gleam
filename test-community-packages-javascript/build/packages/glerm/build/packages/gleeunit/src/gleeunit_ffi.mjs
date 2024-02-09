async function* gleamFiles(directory) {
  for (let entry of await read_dir(directory)) {
    let path = join_path(directory, entry);
    if (path.endsWith(".gleam")) {
      yield path;
    } else {
      try {
        yield* gleamFiles(path);
      } catch (error) {
        // Could not read directory, assume it's a file
      }
    }
  }
}

async function readRootPackageName() {
  let toml = await read_file("gleam.toml", "utf-8");
  for (let line of toml.split("\n")) {
    let matches = line.match(/\s*name\s*=\s*"([a-z][a-z0-9_]*)"/); // Match regexp in compiler-cli/src/new.rs in validate_name()
    if (matches) return matches[1];
  }
  throw new Error("Could not determine package name from gleam.toml");
}

export async function main() {
  let passes = 0;
  let failures = 0;

  let packageName = await readRootPackageName();
  let dist = `../${packageName}/`;

  for await (let path of await gleamFiles("test")) {
    let js_path = path.slice("test/".length).replace(".gleam", ".mjs");
    let module = await import(join_path(dist, js_path));
    for (let fnName of Object.keys(module)) {
      if (!fnName.endsWith("_test")) continue;
      try {
        await module[fnName]();
        write(`\u001b[32m.\u001b[0m`);
        passes++;
      } catch (error) {
        let moduleName = "\n" + js_path.slice(0, -4);
        let line = error.line ? `:${error.line}` : "";
        write(`\n❌ ${moduleName}.${fnName}${line}: ${error}\n`);
        failures++;
      }
    }
  }

  console.log(`
${passes + failures} tests, ${failures} failures`);
  exit(failures ? 1 : 0);
}

export function crash(message) {
  throw new Error(message);
}

function write(message) {
  if (globalThis.Deno) {
    Deno.stdout.writeSync(new TextEncoder().encode(message));
  } else {
    process.stdout.write(message);
  }
}

function exit(code) {
  if (globalThis.Deno) {
    Deno.exit(code);
  } else {
    process.exit(code);
  }
}

async function read_dir(path) {
  if (globalThis.Deno) {
    let items = [];
    for await (let item of Deno.readDir(path, { withFileTypes: true })) {
      items.push(item.name);
    }
    return items;
  } else {
    let { readdir } = await import("fs/promises");
    return readdir(path);
  }
}

function join_path(a, b) {
  if (a.endsWith("/")) return a + b;
  return a + "/" + b;
}

async function read_file(path) {
  if (globalThis.Deno) {
    return Deno.readTextFile(path);
  } else {
    let { readFile } = await import("fs/promises");
    let contents = await readFile(path);
    return contents.toString();
  }
}
