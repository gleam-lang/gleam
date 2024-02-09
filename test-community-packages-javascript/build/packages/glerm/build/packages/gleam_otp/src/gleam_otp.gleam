import gleam/io
import gleam/int
import gleam/list
import gleam/otp/task
import gleam/erlang/process.{type Pid}

@external(erlang, "gleam_otp_test_external", "get_message_queue_length")
fn get_message_queue_length(pid pid: Pid) -> Int

fn spawn_task(i) {
  task.async(fn() {
    case i % 500 == 0 {
      True -> io.println("Hello from " <> int.to_string(i))
      False -> Nil
    }
  })
}

pub fn main() {
  io.debug(get_message_queue_length(process.self()))

  list.range(0, 1_000_000)
  |> list.map(spawn_task)
  |> list.each(task.await_forever)

  io.debug(get_message_queue_length(process.self()))
}
