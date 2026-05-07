module InvalidMethodImplementation where

type SmallerThanZero, Result : *T

type SmallerThanZero = {x: Int | x < 0}
type Result = 
  {z: Int | 2 * z + 5 - (3 - 7 * 4 * 2 + 2 * (-1) * (z + z + 2 * z)) <= 48}

--z:
--  2 * x -> z <= -2
--  + 5 -> z <= 3
--  - 3 -> z <= 0
--  + 7 * 4 * 2 -> z <= 56
--  + 8x (x < 0) -> z <= 48
--  + 1 -> z <= 49           X
veryComplexArithmetic : SmallerThanZero -> Result
veryComplexArithmetic x = x + 1

main : Result
main = veryComplexArithmetic (-2)