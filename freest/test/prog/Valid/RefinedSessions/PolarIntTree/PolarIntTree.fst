module InvalidIntInDataType where

type PositiveInt, NegativeInt : *T
type PolarTree : *T -> *T -> *T

type PositiveInt = {x: Int | x >= 0}
type NegativeInt = {y: Int | y <= 0}

-- Left of each node has type a and right has type b
data PolarTree a b =
    Leaf
  | Node (PolarTree a b) a (PolarTree b a)

buildPolarIntTree : PolarTree PositiveInt NegativeInt
buildPolarIntTree = Node @PositiveInt @NegativeInt left 0 right
  where
    leftLeft = Node @PositiveInt @NegativeInt Leaf 2 Leaf
    leftRight = Node @PositiveInt @NegativeInt (Node @PositiveInt @NegativeInt Leaf 3 Leaf) -2 Leaf
    left = Node @PositiveInt @NegativeInt leftLeft 1 leftRight
    right = Node @PositiveInt @NegativeInt Leaf -1 Leaf

main : ()
main = buildPolarIntTree; ()