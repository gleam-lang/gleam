import io
import process

fn run() {
  pid = process:spawn(fn() {
    receive {
    | ('ping', sender) -> process:send(sender, 'pong')
    | _ -> io:print("Huh?")
    }
  })
  process:send(pid, ('ping', process:self()))

  receive {
  | 'pong' -> 'ok'
  | _ -> io:print("Huh?")
  | after 10 -> 'too_slow'
  }
}
