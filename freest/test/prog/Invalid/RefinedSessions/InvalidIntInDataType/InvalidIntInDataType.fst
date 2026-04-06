module InvalidIntInDataType where

type PositiveInt, NegativeInt : *T
type PolarTree : *T -> *T

type PositiveInt = {x: Int | x >= 0}
type NegativeInt = {y: Int | y <= 0}

-- Left of each node has type a and right has type b
data PolarTree (a b : *T) =
    Leaf
  | Node (PolarTree @a @b) a (PolarTree @b @a)

buildPolarIntTree : PolarTree @PositiveInt @NegativeInt
buildPolarIntTree = Node left 0 right
  where                                     -- should be positive
    left = Node (Node Leaf 2 Leaf) 1 (Node (Node Leaf -3 Leaf) -2 Leaf)
    right = Node Leaf -1 Leaf

main : ()
main = buildPolarIntTree;