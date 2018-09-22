import IO
import Result:Result:*
import Order:Order

fn run() {
  secret_number = Random.int(0, 100)
  IO.print("Hello! I'm thinking of a number. Can you guess what it is?")
  loop(secret_number)
}

fn loop(secret_number) {
  IO.write("What's your guess? ")
  guess = IO.read_line() |> String.trim |> String.to_int
  case guess {
  | Ok(i) =>
      compare(i, secret_number)
  | Error(_) =>
      IO.print("That doesn't look like a number to me... Try again")
      loop(secret_number)
  }
}

doc """
Here is some documentation! Hooray!
"""
fn compare(guess, secret_number) {
  case Int.compare(i, secret_number) {
  | Order:LT =>
      IO.print("Too low!")
      loop(secret_number)
  | Order:GT =>
      IO.print("Too low!")
      loop(secret_number)
  | Order:EQ =>
      i = Int.to_string(secret_number)
      IO.print("You got it! The number was" <> i)
  }
}
