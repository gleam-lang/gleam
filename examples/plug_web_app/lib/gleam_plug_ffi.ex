defmodule GleamPlugFFI do
  @moduledoc """
  Natively implemented functions used by the Gleam Plug wrapper
  """

  def method(conn) do
    case conn.method do
      "POST" -> :post
      "PATCH" -> :patch
      "PUT" -> :put
      "DELETE" -> :delete
      "GET" -> :get
      "OPTIONS" -> :options
      other -> {:other, other}
    end
  end

  def path_info(conn) do
    conn.path_info
  end
end
