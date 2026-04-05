module ExpectationStairs where

type BiggerThanZero, BiggerThanOne, BetweenTwoAndFive, FourOrTwo, Four : *T

type BiggerThanZero    = {n: Int | n > 0}
type BiggerThanOne     = {n: Int | n > 1}
type BetweenTwoAndFive = {n: Int | 2 <= n && n <= 5}
type FourOrTwo         = {n: Int | n == 4 || n == 2}
type Four              = {n: Int | n == 4}

four : Four
four = 4

fourOrTwo : FourOrTwo
fourOrTwo = four

betweenTwoAndFive : BetweenTwoAndFive
betweenTwoAndFive = fourOrTwo

biggerThanOne : BiggerThanOne
biggerThanOne = betweenTwoAndFive

biggerThanZero : BiggerThanZero
biggerThanZero = biggerThanOne

main : BiggerThanZero
main = biggerThanZero
