module Error where

{-
Example adapted from
https://ucsd-progsys.github.io/liquidhaskell-tutorial/Tutorial_03_Basic.html
-}

type Die : *T
type Die = {v: Int | False} -- never typechecks

die : Die -> Int
die death = 1

betweenLifeAndDeath : Int -> Int
betweenLifeAndDeath x =
  if x + 1 == 3
  then die 1
  else 5

main : Int
main = betweenLifeAndDeath 1
