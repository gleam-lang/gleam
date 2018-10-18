import io
import int
import random
import string
import result:[Ok, Error]
import order:[Gt, Eq, Lt]

fn run() {
  secret_number = random:int(0, 100)
  io:print("Hello! I'm thinking of a number. Can you guess what it is?")
  loop(secret_number)
}

fn loop(secret_number) {
  io:write("What's your guess? ")
  guess = io:read_line() |> string:trim |> string:to_int

  case guess {
  | Ok(i) ->
      compare(i, secret_number)

  | Error(_) ->
      io:print("That doesn't look like a number to me... Try again")
      loop(secret_number)
  }
}

doc """
Here is some documentation! Hooray!
"""
fn compare(guess, secret_number) {
  case int:compare(i, secret_number) {
  | Lt ->
    io:print("Too low!")
    loop(secret_number)

  | Gt ->
      io:print("Too low!")
      loop(secret_number)

  | Eq ->
      i = int:to_string(secret_number)
      io:print("You got it! The number was" <> i)
  }
}
