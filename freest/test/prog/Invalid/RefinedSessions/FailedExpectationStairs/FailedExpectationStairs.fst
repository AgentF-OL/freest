module FailedExpectationStairs where

type BiggerThanZero, BiggerThanOne, BetweenTwoAndFive, FourOrTwo, Four : *T

type BiggerThanZero  = {n: Int | n > 0}
type BiggerThanOne   = {n: Int | n > 1}
type BiggerThanZero2 = {n: Int | (if n == 1 then 2 else n) > 1} -- X
type FourOrTwo       = {n: Int | n == 4 || n == 2}
type Four            = {n: Int | n == 4}

four : Four
four = 4

fourOrTwo : FourOrTwo
fourOrTwo = four

biggerThanZero2 : BiggerThanZero2
biggerThanZero2 = fourOrTwo

biggerThanOne : BiggerThanOne
biggerThanOne = biggerThanZero2 -- X

biggerThanZero : BiggerThanZero
biggerThanZero = biggerThanOne

main : BiggerThanZero
main = biggerThanZero
