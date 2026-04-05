module Arithmetic where

type SmallerThanZero, BetweenFives, Result : *T

type SmallerThanZero = {x: Int | x < 0}
type BetweenFives    = {y: Int | -5 <= y && y <= 5}
type Result          = {z: Int | z <= 96}

{-
z:
  2 * x -> z <= -2
  + 5 -> z <= 3
  - 3 -> z <= 0
  + 7 * 4 * 2 -> z <= 56
  + 8y (between -40 and 40) -> z <= 16 || ... || z <= 96 -> z <= 96
-}
arithmetic : SmallerThanZero -> BetweenFives -> Result
arithmetic x y = 2 * x + 5 - (3 - 7 * 4 * 2 + 2 * -1 * (y + y + 2 * y))

main : Result
main = arithmetic -2 2