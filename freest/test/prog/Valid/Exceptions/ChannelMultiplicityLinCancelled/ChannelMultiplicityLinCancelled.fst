type Ints = ?Int ; *?Int

sendInts : [Int] -> Dual Ints -> ()
sendInts xs c = cancel c

sumInts' : Int -> *?Int -> Int
sumInts' n c
  | n <= 0 = 0
  | otherwise = receive_ c + sumInts' (n - 1) c

sumInts : Ints -> Maybe Int
sumInts c =
  try receive c as (n, c) in
    Just $ sumInts' n c
  unless Nothing

_ = forkWith (sendInts [1,2,4]) |> sumInts |> print