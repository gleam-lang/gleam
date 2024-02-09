defmodule TomlFFI do
  def get_reason({:invalid_toml, reason}) do
    reason
  end
end
