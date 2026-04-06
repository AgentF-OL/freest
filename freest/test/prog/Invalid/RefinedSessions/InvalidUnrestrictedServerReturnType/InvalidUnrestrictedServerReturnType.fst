module RefinedPingPong where

{-
TightPing  <: Ping
EitherPing <: Ping
TightPong  <: Pong
Pong       <: LoosePong
-}

type PingPongServer, PingPongClient : *C
type PingPong : 1C
type Ping, Pong, TightPing, TightPong, EitherPing, Nat : *T

type PingPong = ?Ping ; !Pong ; Wait

type PingPongServer = *?PingPong
type PingPongClient = Dual PingPongServer

type Ping       = {ping: Int | 0 <= ping && ping < 10}
type Pong       = {pong: Int | -10 < pong && pong <= 0}
type TightPing  = {ping: Int | 0 <= ping && ping <= 5}
type TightPong  = {pong: Int | -5 <= pong && pong <= 0}
type EitherPing = {ping: Int | 0 <= ping && ping < 4 || 6 <= ping && ping < 10}
type LoosePong =  {pong: Int | -15 <= pong && pong <= 0}

type Nat = {x: Int | x > 0}

tightPing : TightPing
tightPing = 3

tightPong : TightPong
tightPong = -3

eitherPing : EitherPing
eitherPing = 7

loosePong : LoosePong
loosePong = -15

runServer : PingPongServer -> Nat -> ()
runServer server threads =
  if threads == 0
  then ()
  else
    let c = accept @PingPong server in
    let (ping, c) = receive c in
    let c = send loosePong c in -- X - LoosePong not a subtype of Pong
    wait;
    runServer server (threads - 1)

runTightClient : PingPongClient -> ()
runTightClient client =
  let c = receive_ @PingPong client in
  c |> send tightPing |> receiveAndClose @Pong

runEitherClient : PingPongClient -> ()
runEitherClient client =
  let c = receive_ @PingPong client in
  c |> send eitherPing |> receiveAndClose @Pong

main : ()
main =
    let (c, s) = channel @PingPongClient in
    fork (\(_ : ()) 1-> runTightClient c);
    fork (\(_ : ()) 1-> runEitherClient c);
    runServer s 2