module IfTyping where

type Nat, FancyNat : *T

type Nat = {n1: Int | n1 >= 0}
type FancyNat = {n2: Int | n2 >= 0 <=> True}

iF : Nat -> FancyNat -> FancyNat
iF x y =
  if x > 0 && y >= 0 then
    if x > 0 || y > 0 then
      if not (x == 1) && not (y == 1) then
        x
      else if x == 1 && not (y == 1) then
        x + y
      else
        y
    else
      y
  else
    x

main : FancyNat
main = iF 1 0