import gleam/int
import gleam/io

// This program demonstrates the use of flow control with a case statement.
// It generates a random exam score and prints a message based on the score.
// The possible messages are:
// - "You failed the exam." for scores below 50
// - "You passed the exam." for scores between 50 and 59
// - "You did well on the exam." for scores between 60 and 79
// - "You aced the exam!" for scores 80 and above

pub fn main() -> Nil {
  // #0 Case Expression sample
  let last_exam_score = int.random(101)
  io.println("Your last exam score was: " <> int.to_string(last_exam_score))

  case last_exam_score {
    score if score < 50 -> io.println("You failed the exam.")
    score if score < 60 -> io.println("You passed the exam.")
    score if score < 80 -> io.println("You did well on the exam.")
    _ -> io.println("You aced the exam!")
  }

  // #1 Case Expression with if sample
  let result = case last_exam_score {
    s if s < 50 -> "You failed the exam."
    s if s < 60 -> "You passed the exam."
    s if s < 80 -> "You did well on the exam."
    _ -> "You aced the exam!"
  }

  io.println(result)

  Nil
}
