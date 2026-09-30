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
sumInts c1 = cancel c1; Nothing -- c1 buffer has 2 channels inside, which should be cancelled without an error

_ =
  let (w, r) = channel @Three in
  sendInts (1, 2, 4) w;
  print $ sumInts r
