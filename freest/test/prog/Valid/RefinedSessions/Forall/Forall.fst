module Forall where

type Boomerang, BiggerThanOne, BiggerThanTwo, BiggerThanFive : *T

type Boomerang      = forall (a b c : *T) . (a, b, c)
type BiggerThanOne  = {x: Int | 2 * x - 2 > 0}
type BiggerThanTwo  = {y: Int | 3 * y - 6 > 0}
type BiggerThanFive = {z: Int | 6 * z - 30 > 0}

boomerang : forall (a b c : *T) . a -> b -> Boomerang @a @b @c
boomerang @a @b @c x y = (x, y, x * y)

main : (BiggerThanOne, BiggerThanTwo, BiggerThanFive)
main = boomerang @BiggerThanOne @BiggerThanTwo @BiggerThanFive 2 3
