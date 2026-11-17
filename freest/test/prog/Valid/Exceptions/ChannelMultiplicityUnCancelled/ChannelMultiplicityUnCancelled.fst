type Ints = ?Int ; *?Int

sendInts' : [Int] -> *!Int -> ()
sendInts' [] c = cancel c
sendInts' (x::xs) c = cancel c; send_ x c; cancel c; sendInts' xs c

sendInts : [Int] -> Dual Ints -> ()
sendInts xs c = c |> send (length xs) |> sendInts' xs

sumInts' : Int -> *?Int -> Int
sumInts' n c
  | n <= 0 = cancel c; 0
  | otherwise = cancel c; receive_ c + sumInts' (n - 1) c

sumInts : Ints -> Maybe Int
sumInts c =
  try receive c as (n, c) in
    Just $ sumInts' n c
  unless Nothing

_ = forkWith (sendInts [1,2,4]) |> sumInts |> print