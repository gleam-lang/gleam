module InteractiveProgram

export run/0

import Maybe exposing Maybe(..)
import Order exposing Order(..)

fn run() =
  secret_number = Random.int(0, 100)
  IO.print("Hello! I'm thinking of a number. Can you guess what it is?")
  loop(secret_number)

spec |Int| -> ()
fn loop(secret_number) =
  IO.write("What's your guess? ")
  guess = IO.read_line() |> String.trim |> String.to_int
  case guess
  | Just(i) => compare(i, secret_number)
  | Nothing => (
      IO.print("That doesn't look like a number to me... Try again")
      loop(secret_number)
    )

doc """
Here is some documentation! Hooray!
"""
spec |Int, Int| -> ()
fn compare(guess, secret_number) =
  case Int.compare(i, secret_number)
  | LT => (
      IO.print("Too low!")
      loop(secret_number)
    )
  | GT => (
      IO.print("Too low!")
      loop(secret_number)
    )
  | EQ => (
      i = Int.to_string(secret_number)
      IO.print("You got it! The number was" <> i)
    )
