defmodule WebAppTest do
  use ExUnit.Case
  doctest WebApp

  test "greets the world" do
    assert WebApp.hello() == :world
  end
end
