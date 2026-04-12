module InvalidTypeInForall where

type Boomerang : *T -> *T -> *T -> *T
type BiggerThanOne, BiggerThanTwo, BiggerThanSix : *T

type Boomerang a b c = (a, b, c)
type BiggerThanOne   = {x: Int | 2 * x - 2 > 0}
type BiggerThanTwo   = {y: Int | 3 * y - 6 > 0}
type BiggerThanSix   = {z: Int | 6 * z - 36 > 0}

boomerang : forall (a b c : *T) . a -> b -> Boomerang a b c
boomerang @a @b @c x y = (x, y, x * y)

main : (BiggerThanOne, BiggerThanTwo, BiggerThanSix)
main = boomerang @BiggerThanOne @BiggerThanTwo @BiggerThanSix 2 3 -- 2*3 > 6  X
