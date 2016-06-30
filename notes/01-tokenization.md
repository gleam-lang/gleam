Tokenization
============

The Elixir tokenizer is there for us to use. Let's check it out.

```elixir
iex(1)> :elixir_tokenizer.tokenize 'hi', 1, []
{:ok, 1, 3, [{:identifier, {1, 1, 3}, :hi}]}
```
