module PingPong

fn run() {
  pid = spawn(fn() {
    receive {
    | ('ping', sender) => send(sender, 'pong')
    | _ => IO.print("Huh?")
    }
  })
  send(pid, ('ping', self()))

  receive {
  | 'pong' => 'ok'
  | _ => IO.print("Huh?")
  after 10 => 'too_slow'
  }
}
