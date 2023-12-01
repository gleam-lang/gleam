let compiler;

export default async function initGleamCompiler() {
  const wasm = await import("/compiler/gleam_wasm.js");
  await wasm.default();
  wasm.initialise_panic_hook();
  if (!compiler) {
    compiler = new Compiler(wasm);
  }
  return compiler;
}

class Compiler {
  #wasm;
  #nextId = 0;
  #projects = new Map();

  constructor(wasm) {
    this.#wasm = wasm;
  }

  get wasm() {
    return this.#wasm;
  }

  newProject() {
    const id = this.#nextId++;
    const project = new Project(id);
    this.#projects.set(id, new WeakRef(project));
    return project;
  }

  garbageCollectProjects() {
    const gone = [];
    for (const [id, project] of this.#projects) {
      if (!project.deref()) gone.push(id);
    }
    for (const id of gone) {
      this.#projects.delete(id);
      this.#wasm.delete_project(id);
    }
  }
}

class Project {
  #id;

  constructor(id) {
    this.#id = id;
  }

  get projectId() {
    return this.#id;
  }

  writeModule(moduleName, code) {
    compiler.wasm.write_module(this.#id, moduleName, code);
  }

  compilePackage(target) {
    compiler.garbageCollectProjects();
    compiler.wasm.reset_warnings(this.#id);
    compiler.wasm.compile_package(this.#id, target);
  }

  readCompiledJavaScript(moduleName) {
    return compiler.wasm.read_compiled_javascript(this.#id, moduleName);
  }

  readCompiledErlang(moduleName) {
    return compiler.wasm.read_compiled_erlang(this.#id, moduleName);
  }

  resetFilesystem() {
    compiler.wasm.reset_filesystem(this.#id);
  }

  delete() {
    compiler.wasm.delete_project(this.#id);
  }

  takeWarnings() {
    const warnings = [];
    while (true) {
      const warning = compiler.wasm.pop_warning(this.#id);
      if (!warning) return warnings;
      warnings.push(warning.trimStart());
    }
  }
}
