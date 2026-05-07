module DivisionByZero where

type NotZero : *T

type NotZero = {n: Int | n /= 0}

divide : Int -> NotZero -> Int
divide x y = x / y

main : Int
main = divide 6 0