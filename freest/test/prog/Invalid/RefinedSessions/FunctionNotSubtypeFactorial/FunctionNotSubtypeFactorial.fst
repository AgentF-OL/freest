module FunctionNotSubtypeFactorial where

{-
method       | expects            | receives       | from
-------------|--------------------|----------------|-------------
startClient  | Nat func           | Nat funcMain   | main           X
factClient   | Int FactMainClient | Nat FactClient | startClient
factServer   | FactMainServer     | FactServer     | startClient
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
  funcMain   <: func      X
  funcMain   = Int -> FactClient -> Nat        
  func       = Nat -> FactMainClient -> Int
-}

type Nat : *T
type FactServer, FactMainServer, FactClient, FactMainClient : 1C

type Nat = {n: Int | n >= 0}
type FactServer     = Skip ; ?Nat ; !Int ; Wait ; Skip
type FactMainServer =        ?Int ; !Nat ; Wait
type FactMainClient = Dual FactServer     -- !Nat ; ?Int ; Close
type FactClient     = Dual FactMainServer -- !Int ; ?Nat ; Close

factorial : Int -> Nat
factorial n
  | n <= 0 = -1 -- error
  | n == 0 = 1
  | otherwise = n * factorial (n - 1)

factServer : FactMainServer -> ()
factServer c =
  let (n, c) = receive c in
  c |> send (factorial n) |> wait

factClient : Int -> FactClient -> Nat
factClient n c = c |> send n
                   |> receiveAndClose @Nat

startClient : Nat -> (Nat -> FactMainClient -> Int) -> Int
startClient n client =
  let (w,_) = channel @FactClient in
  let (_,r) = channel @FactMainClient in
  fork @() (\(_ : ()) 1-> factServer r);
  client x w

main : Int
main = startClient 5 factClient
