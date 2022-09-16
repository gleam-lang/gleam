import hljs from "highlight.js/lib/core"
import elixirLanguage from "highlight.js/lib/languages/elixir"
import erlangLanguage from "highlight.js/lib/languages/erlang"
import "highlight.js/styles/atom-one-light.css"

import gleamLanguage from "./highlightjs-gleam"

import "./gleam"
import "./index.css"

hljs.registerLanguage('elixir', elixirLanguage)
hljs.registerLanguage('erlang', erlangLanguage)
hljs.registerLanguage('gleam', gleamLanguage)

document.querySelectorAll("pre code").forEach((block) => {
  if (block.className === "") {
    block.classList.add("gleam")
  }
  hljs.highlightBlock(block)
})
  