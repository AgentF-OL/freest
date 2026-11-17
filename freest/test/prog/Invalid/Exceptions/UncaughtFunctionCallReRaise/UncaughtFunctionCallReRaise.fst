sumInts' : [Int] -> Int
sumInts' [] = raise
sumInts' (x::xs) = x + sumInts' xs

sumInts : [Int] -> Int
sumInts xs = try sumInts' xs as res in res unless raise

_ = print $ sumInts [1,2,4]