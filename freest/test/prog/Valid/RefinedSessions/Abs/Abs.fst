module Abs where

type AnyInt, Nat : *T
type AbsServer, AbsClient : 1C

type AnyInt = {x: Int}
type Nat = {n: Int | n >= 0}
type AbsServer = ?AnyInt ; !Nat ; Wait
type AbsClient = Dual AbsServer

absolute : AnyInt -> Nat -- let's pretend this is correctly implemented
absolute x
  | x < 0 = -x
  | otherwise = x

absServer : AbsServer -> ()
absServer c =
  let (x, c) = receive c in
  c |> send (absolute x) |> wait

absClient : AnyInt -> AbsClient -> Nat
absClient x c = c |> send x
                  |> receiveAndClose @Nat

startClient : AnyInt -> (AnyInt -> AbsClient -> Nat) -> Nat
startClient x client =
  let (w,r) = channel @AbsClient in
  fork @() (\(_ : ()) 1-> absServer r);
  client x w

main : Nat
main = startClient -5 absClient
