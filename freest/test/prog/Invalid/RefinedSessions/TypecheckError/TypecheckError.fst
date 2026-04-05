module TypecheckError where

{-
Example adapted from 
https://ucsd-progsys.github.io/liquidhaskell-tutorial/Tutorial_03_Basic.html
-}

type Die : *T
type Die = {v: Int | False} -- never typechecks

die : Die
die = 1

betweenLifeAndDeath : ()
betweenLifeAndDeath =
  if 1 + 1 == 2
  then die -- typecheck error since this branch might happen (it will)
  else ()

main : ()
main = betweenLifeAndDeath
