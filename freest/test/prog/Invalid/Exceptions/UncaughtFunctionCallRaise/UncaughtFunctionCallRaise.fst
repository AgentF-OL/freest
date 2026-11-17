sumInts' : [Int] -> Int
sumInts' [] = raise
sumInts' (x::xs) = x + sumInts' xs

sumInts : [Int] -> Int
sumInts xs = sumInts' xs

_ = print $ sumInts [1,2,4]