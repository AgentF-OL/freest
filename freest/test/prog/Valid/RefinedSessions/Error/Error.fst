module Error where

{-
Example adapted from 
https://ucsd-progsys.github.io/liquidhaskell-tutorial/Tutorial_03_Basic.html
-}

type Die : *T
type Die = {v: Int | False} -- never typechecks

die : Die
die = 1

betweenLifeAndDeath : Int
betweenLifeAndDeath =
  if 1 + 1 == 3
  then die
  else 5

main : Int
main = betweenLifeAndDeath
