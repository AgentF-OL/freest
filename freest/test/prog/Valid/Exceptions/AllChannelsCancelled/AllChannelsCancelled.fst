module AllChannelsCancelled where

type One, Two, Three : 1C
type One = !Int ; Close
type Two = !Int ; !(Dual One) ; Close
type Three = !Int ; !(Dual Two) ; Close

sendInts : (Int, Int, Int) -> Three -> ()
sendInts (x, y, z) c1 =
  let (c2, r2) = channel @Two
      (c3, r3) = channel @One
      c1 = c1 |> send x |> send r2
      c2 = c2 |> send y |> send r3
      c3 = c3 |> send z
  in close c3; close c2; close c1

sumInts : Dual Three -> Maybe Int
sumInts c1 = drop c1; Nothing -- c1 buffer has 2 channels inside, which should be cancelled without an error

{- sumInts : Dual Three -> Int
sumInts c1 =
  try receive c1 as (n1, c1) in
    try receive c1 as (c2, c1) in
      try receive c2 as (n2, c2) in
        try receive c2 as (c3, c2) in
          try receive c3 as (n3, c3) in
            try wait c3; wait c2; wait c3 as _ in n1 + n2 + n3
            otherwise print "Error waiting for all channels to close"; n1 + n2 + n3
          otherwise "Error receiving int from channel 3"; n1 + n2
        otherwise "Error receiving channel 3 from channel 2"; n1 + n2
      otherwise "Error receiving int from channel 2"; n1
    otherwise "Error receiving channel 2 from channel 1"; n1
  otherwise "Error receiving int from channel 1"; -1 -}

_ =
  let (w, r) = channel @Three in
  sendInts (1, 2, 4) w;
  print $ sumInts r
