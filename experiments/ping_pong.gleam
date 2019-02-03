import io
import process

enum PingPong =
  | Ping(Pid)
  | Pong

fn run() {
  pid = process:spawn(fn() {
    receive {
    | Ping(sender) -> process:send(sender, Pong)
    | _ -> io:print("Huh?")
    }
  })
  process:send(pid, (Ping, process:self()))

  receive {
  | Pong -> ()
  | _ -> io:print("Huh?")
  | after 10 -> ()
  }
}
