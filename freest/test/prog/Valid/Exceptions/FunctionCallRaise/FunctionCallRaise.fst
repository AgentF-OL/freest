sumInts' : [Int] -> Int
sumInts' [] = raise
sumInts' (x::xs) = x + sumInts' xs

sumInts : [Int] -> Maybe Int
sumInts xs = try sumInts' xs as res in Just res unless Nothing

_ = print $ sumInts [1,2,4]