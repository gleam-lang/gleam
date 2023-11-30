import CodeFlask from "https://cdn.jsdelivr.net/npm/codeflask@1.4.1/+esm";
import initGleamCompiler from "./compiler.js";
import stdlib from "./stdlib.js";

const output = document.querySelector("#output");
const initialCode = document.querySelector("#code").textContent;

const prismGrammar = {
  comment: {
    pattern: /\/\/.*/,
    greedy: true,
  },
  function: /([a-z_][a-z0-9_]+)(?=\()/,
  keyword:
    /\b(use|case|if|external|fn|import|let|assert|try|pub|type|opaque|const|todo|as)\b/,
  symbol: {
    pattern: /([A-Z][A-Za-z0-9_]+)/,
    greedy: true,
  },
  operator: {
    pattern:
      /(<<|>>|<-|->|\|>|<>|\.\.|<=\.?|>=\.?|==\.?|!=\.?|<\.?|>\.?|&&|\|\||\+\.?|-\.?|\/\.?|\*\.?|%\.?|=)/,
    greedy: true,
  },
  string: {
    pattern: /"(?:\\(?:\r\n|[\s\S])|(?!")[^\\\r\n])*"/,
    greedy: true,
  },
  module: {
    pattern: /([a-z][a-z0-9_]*)\./,
    inside: {
      punctuation: /\./,
    },
    alias: "keyword",
  },
  punctuation: /[.\\:,{}()]/,
  number:
    /\b(?:0b[0-1]+|0o[0-7]+|[[:digit:]][[:digit:]_]*(\\.[[:digit:]]*)?|0x[[:xdigit:]]+)\b/,
};

// Monkey patch console.log to keep a copy of the output
let logged = "";
const log = console.log;
console.log = (...args) => {
  log(...args);
  logged += args.map((e) => `${e}`).join(" ") + "\n";
};

function resetLogCapture() {
  logged = "";
}

async function compileEval(project, code) {
  try {
    project.writeModule("main", code);
    project.compilePackage("javascript");
    const js = project.readCompiledJavaScript("main");
    const main = await loadProgram(js);
    resetLogCapture();
    if (main) main();
    replaceOutput(logged, "log");
  } catch (error) {
    replaceOutput(error.toString(), "error");
  }
  for (const warning of project.takeWarnings()) {
    appendOutput(warning, "warning");
  }
}

async function loadProgram(js) {
  const url = new URL(import.meta.url);
  url.pathname = "";
  url.hash = "";
  url.search = "";
  const href = url.toString();
  const js1 = js.replaceAll(
    /from\s+"\.\/(.+)"/g,
    `from "${href}precompiled/$1"`
  );
  const js2 = btoa(unescape(encodeURIComponent(js1)));
  const module = await import("data:text/javascript;base64," + js2);
  return module.main;
}

function clearOutput() {
  while (output.firstChild) {
    output.removeChild(output.firstChild);
  }
}

function replaceOutput(content, className) {
  clearOutput();
  appendOutput(content, className);
}

function appendOutput(content, className) {
  const element = document.createElement("pre");
  element.textContent = content;
  element.className = className;
  output.appendChild(element);
}

const editor = new CodeFlask("#editor-target", {
  language: "gleam",
});
editor.addLanguage("gleam", prismGrammar);
editor.updateCode(initialCode);

const compiler = await initGleamCompiler();
const project = compiler.newProject();
for (const [name, code] of Object.entries(stdlib)) {
  project.writeModule(name, code);
}

function debounce(fn, delay) {
  let timer = null;
  return (...args) => {
    clearTimeout(timer);
    timer = setTimeout(() => fn(...args), delay);
  };
}

editor.onUpdate(debounce((code) => compileEval(project, code), 200));
compileEval(project, initialCode);
