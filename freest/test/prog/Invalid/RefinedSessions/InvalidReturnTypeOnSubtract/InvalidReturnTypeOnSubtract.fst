module InvalidReturnTypeOnSubtract where

type BiggerThanTwo, BiggerThanFive, Negative : *T

type BiggerThanTwo  = {x: Int | x >= 2}
type BiggerThanFive = {y: Int | y >= 5}
type Negative       = {z: Int | z < 0}

specialSubtract : BiggerThanTwo -> BiggerThanFive -> Negative
specialSubtract x y = x - y

main : Negative
main = specialSubtract 2 5
