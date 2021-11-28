import gleam/base
import gleam/list
import gleam/should

pub fn encode64_test() {
  <<255, 127, 254, 252>>
  |> base.encode64(True)
  |> should.equal("/3/+/A==")

  <<255, 127, 254, 252>>
  |> base.encode64(False)
  |> should.equal("/3/+/A")

  <<0, 0, 0>>
  |> base.encode64(True)
  |> should.equal("AAAA")

  <<>>
  |> base.encode64(True)
  |> should.equal("")
}

pub fn decode64_test() {
  "/3/+/A=="
  |> base.decode64()
  |> should.equal(Ok(<<255, 127, 254, 252>>))

  "/3/+/A"
  |> base.decode64()
  |> should.equal(Ok(<<255, 127, 254, 252>>))

  "AAAA"
  |> base.decode64()
  |> should.equal(Ok(<<0, 0, 0>>))

  ""
  |> base.decode64()
  |> should.equal(Ok(<<>>))

  ")!"
  |> base.decode64()
  |> should.equal(Error(Nil))
}

pub fn url_encode64_test() {
  <<255, 127, 254, 252>>
  |> base.url_encode64(True)
  |> should.equal("_3_-_A==")

  <<255, 127, 254, 252>>
  |> base.url_encode64(False)
  |> should.equal("_3_-_A")

  <<0, 0, 0>>
  |> base.url_encode64(True)
  |> should.equal("AAAA")

  <<>>
  |> base.url_encode64(True)
  |> should.equal("")
}

pub fn url_decode64_test() {
  "_3_-_A=="
  |> base.url_decode64()
  |> should.equal(Ok(<<255, 127, 254, 252>>))

  "_3_-_A"
  |> base.url_decode64()
  |> should.equal(Ok(<<255, 127, 254, 252>>))

  "AAAA"
  |> base.url_decode64()
  |> should.equal(Ok(<<0, 0, 0>>))

  ""
  |> base.url_decode64()
  |> should.equal(Ok(<<>>))

  ")!"
  |> base.url_decode64()
  |> should.equal(Error(Nil))
}
