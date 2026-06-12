module ChoiceNotSubtypePingPong where

{-
ThinPing <: Ping
ThinPingPongServer <: PingPongServer
(PingPongServer -> ()) <: (ThinPingPongServer -> ())
-}

type PingPongServer, PingPongClient, ThinPingPongServer, ThinPingPongClient : 1C
type Ping, Pong, ThinPing : *T

type PingPongServer = &{Send: ?Ping ; !Pong ; PingPongServer, Stop: Wait}
type PingPongClient = Dual PingPongServer
type ThinPingPongServer = &{Send: ?ThinPing ; !Pong ; ThinPingPongServer, Stop: Wait}
type ThinPingPongClient = Dual ThinPingPongServer

type Ping = {ping: Int | 0 <= ping && ping < 10}
type Pong = {pong: Int | -10 < pong && pong <= 0}
type ThinPing = {ping: Int | 3 <= ping && ping < 7}

pong : Pong
pong = -3

runServer : ThinPingPongServer -> ()
runServer c =
  case c of
    &Send c ->
      let (ping, c) = receive c in
      let c = send pong c in
      runServer c
    &Stop c -> wait c

runClient : Int -> ThinPingPongClient -> ()
runClient n_pings c =
  if n_pings == 0
  then c |> select Stop |> close
  else
    let c = select Send c in
    let c = send 3 c in
    let (pong, c) = receive c in
    runClient (n_pings - 1) c 

start : Int -> (PingPongServer -> ()) -> ()
start n_pings server =
  let (c, s) = channel @ThinPingPongClient in
  fork (\(_ : ()) 1-> server s);
  runClient n_pings c

main : ()
main = start 3 runServer -- X - (ThinPingPongServer -> ()) not subtype of (PingPongServer -> ())
