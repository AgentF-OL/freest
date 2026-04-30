module FactorialChannel where

{-
method       | expects            | receives       | from
#############|####################|################|################
startClient  | Nat func           | Nat funcMain   | main
factClient   | Int FactMainClient | Nat FactClient | startClient
factServer   | FactMainServer     | FactMainServer | startClient
factServer   | Int                | Int            | factClient
factorial    | Int                | Int            | factServer
factServer   | Nat                | Nat            | factorial
factClient   | Int                | Nat            | factServer
startClient  | Int                | Nat            | factClient
main         | Int                | Int            | startClient

where
  receives   <: expects  (for each element)
  Nat        <: Int
  FactServer <: FactMainServer
  FactClient <: FactMainClient
  funcMain   <: func
  funcMain   = Int -> FactMainClient -> Nat
  func       = Nat -> FactClient -> Int
-}

type Nat : *T
type FactServer, FactMainServer, FactClient, FactMainClient : 1C

type Nat = {n: Int | n >= 0}
type FactServer     = Skip ; ?Nat ; !Int ; Wait ; Skip
type FactMainServer =        ?Int ; !Nat ; Wait
type FactMainClient = Dual FactServer     -- !Nat ; ?Int ; Close
type FactClient     = Dual FactMainServer -- !Int ; ?Nat ; Close

factorial' : Int -> Nat
factorial' n
  | n <= 0 = 0
  | n == 0 = 1
  | otherwise = n * factorial' (n - 1)

factorial : Int -> Nat
factorial n =
  if n <= 0 then 0
  else if n == 0 then 1
  else n * factorial (n - 1)

factServer : FactMainServer -> ()
factServer c =
  let (n, c) = receive c in
  c |> send (factorial n) |> wait

factClient : Int -> FactMainClient -> Nat
factClient n c = c |> send n
                   |> receiveAndClose @Nat

startClient : Nat -> (Nat -> FactClient -> Int) -> Int
startClient n client =
  let (w,r) = channel @FactClient in
  fork @() (\(_ : ()) 1-> factServer r);
  client x w

main : Int
main = startClient 5 factClient
