module TryReceiveChainNotCancelled where

type Three : 1C
type Three = !Int ; !Int ; !Int ; Close

sendInts : (Int, Int, Int) -> Three -> ()
sendInts (x, y, z) c = c |> send x |> send y |> sendAndClose z

sumInts : Dual Three -> Int
sumInts c =
  try receive c as (n1, c) in
    try receive c as (n2, c) in
      try receiveAndWait c as n3 in
        n1 + n2 + n3
      otherwise
        n1 + n2
    otherwise
      n1
  otherwise
    0

_ = forkWith (sendInts (1, 2, 4)) |> sumInts
