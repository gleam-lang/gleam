import CodeFlask from "https://cdn.jsdelivr.net/npm/codeflask@1.4.1/+esm";
import initGleamCompiler from "./compiler.js";

const problems = document.querySelector("#problems");
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
  try {
    project.writeModule("main", code);
    project.compilePackage("javascript");
    const js = project.readCompiledJavaScript("main");
    const main = await loadProgram(js);
    if (main) output.textContent = main();
    problems.textContent = "";
  } catch (error) {
    output.textContent = "";
    problems.textContent = error.toString();
  }
}

async function loadProgram(js) {
  const href = import.meta.url.toString();
  const js1 = js.replaceAll(/from\s+"(.+)"/g, `from "${href}/../vendor/$1"`);
  const js2 = btoa(unescape(encodeURIComponent(js1)));
  const module = await import("data:text/javascript;base64," + js2);
  return module.main;
}

function debounce(fn, delay) {
  let timer = null;
  return (...args) => {
    clearTimeout(timer);
    timer = setTimeout(() => fn(...args), delay);
  };
}

const editor = new CodeFlask("#editor", {
  language: "gleam",
  lineNumbers: true,
});
editor.addLanguage("gleam", prismGrammar);
editor.updateCode(initialCode);

const compiler = await initGleamCompiler();
const project = compiler.newProject();

editor.onUpdate(debounce((code) => compileEval(project, code), 200));
compileEval(project, initialCode);
