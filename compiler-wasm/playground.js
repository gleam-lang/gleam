import CodeFlask from "https://cdn.jsdelivr.net/npm/codeflask@1.4.1/+esm";
import initGleamCompiler from "./compiler.js";

const output = document.querySelector("#output");
const initialCode = `pub fn main() {
  greet("Joe")
}

fn greet(name: String) -> String {
  "Hello, " <> name <> "!"
}
`;

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

async function compileEval(project, code) {
  clearOutput();
  try {
    project.writeModule("main", code);
    project.compilePackage("javascript");
    const js = project.readCompiledJavaScript("main");
    const main = await loadProgram(js);
    if (main) appendOutput(main(), "success");
  } catch (error) {
    appendOutput(error.toString(), "error");
  }
  for (const warning of project.takeWarnings()) {
    appendOutput(warning, "warning");
  }
}

async function loadProgram(js) {
  const href = import.meta.url.toString();
  const js1 = js.replaceAll(/from\s+"(.+)"/g, `from "${href}/../vendor/$1"`);
  const js2 = btoa(unescape(encodeURIComponent(js1)));
  const module = await import("data:text/javascript;base64," + js2);
  return module.main;
}

function clearOutput() {
  while (output.firstChild) {
    output.removeChild(output.firstChild);
  }
}

function appendOutput(content, className) {
  const element = document.createElement("pre");
  element.textContent = content;
  element.className = className;
  output.appendChild(element);
}

function debounce(fn, delay) {
  let timer = null;
  return (...args) => {
    clearTimeout(timer);
    timer = setTimeout(() => fn(...args), delay);
  };
}

const editor = new CodeFlask("#editor-target", {
  language: "gleam",
});
editor.addLanguage("gleam", prismGrammar);
editor.updateCode(initialCode);

const compiler = await initGleamCompiler();
const project = compiler.newProject();

editor.onUpdate(debounce((code) => compileEval(project, code), 1));
compileEval(project, initialCode);
