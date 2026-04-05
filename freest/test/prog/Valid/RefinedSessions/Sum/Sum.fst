module Sum where

type BiggerThanTwo, BiggerThanFive, BiggerThanSeven : *T

BiggerThanTwo   = {x: Int | x >= 2}
BiggerThanFive  = {y: Int | y >= 5}
BiggerThanSeven = {z: Int | z >= 7}

specialSum : BiggerThanTwo -> BiggerThanFive -> BiggerThanSeven
specialSum x y = x + y

main : BiggerThanSeven
main = specialSum 2 5
