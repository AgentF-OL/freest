module InvalidReturnTypeOnSubtract where

type BiggerThanTwo, BiggerThanFive, Negative : *T

BiggerThanTwo  = {x: Int | x >= 2}
BiggerThanFive = {y: Int | y >= 5}
Negative       = {z: Int | z < 0}

specialSubtract : BiggerThanTwo -> BiggerThanFive -> Negative
specialSubtract x y = x - y

main : Negative
main = specialSubtract 2 5
