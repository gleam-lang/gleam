module PingPong

fn run(req, _args) =
  pid = spawn(||
    receive
    | (:ping, sender) => send(sender, :pong)
    | _ => IO.print("Huh?")
  )
  send(pid, (:ping, self()))

  receive
  | :pong => :ok
  after 10 => :too_slow
