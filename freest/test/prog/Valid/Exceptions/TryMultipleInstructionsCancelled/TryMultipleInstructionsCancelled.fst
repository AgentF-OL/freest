module TryMultipleInstructionsCancelled where

type Three : 1C
type Three = !Int ; !Int ; !Int ; Close

sendInts : (Int, Int, Int) -> Three -> ()
sendInts (x, y, z) c = c |> send x |> cancel

sumInts : Dual Three -> Maybe Int
sumInts c =
  try
    let (n1, c) = receive c
        (n2, c) = receive c
        (n3, c) = receive c
    in n1 + n2 + n3
  as res in Just res
  otherwise Nothing

_ = forkWith (sendInts (1, 2, 4)) |> sumInts
