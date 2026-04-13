module RecursiveChannelFunctionReceivesNonRecursive where

type PingPongRecursiveServer, PingPongRecursiveClient, PingPongServer, PingPongClient : 1C
type Ping, Pong : *T

type PingPongRecursiveServer = ?Ping ; !Pong ; PingPongRecursiveServer ; Close
type PingPongRecursiveClient = !Ping ; ?Pong ; PingPongRecursiveClient ; Wait
type PingPongServer = ?Ping ; !Pong ; Wait
type PingPongClient = Dual PingPongServer

type Ping = {ping: Int | 0 <= ping && ping < 10}
type Pong = {pong: Int | -10 < pong && pong <= 0}

runNonRecursiveServer : PingPongServer -> ()
runNonRecursiveServer c =
  let (ping, c) = receive c in
  c |> send -3 |> wait

runRecursiveServer : PingPongRecursiveServer -> ()
runRecursiveServer c =
  let (ping, c) = receive c in
  c |> send -5 |> wait

runNonRecursiveClient : PingPongClient -> ()
runNonRecursiveClient c = c |> send 7 |> receiveAndClose @Pong

runRecursiveClient : PingPongRecursiveClient -> ()
runRecursiveClient c = c |> send 5 |> receiveAndClose @Pong

main : ()
main =
  let (c1, s1) = channel @PingPongRecursiveClient in
  let (c2, s2) = channel @PingPongClient in
  fork (\(_ : ()) 1-> runNonRecursiveClient c1);
  fork (\(_ : ()) 1-> runRecursiveClient c2); -- X - cannot use non-recursive channel on a recursive channel
  fork (\(_ : ()) 1-> runNonRecursiveServer s1);
  runRecursiveServer s2
