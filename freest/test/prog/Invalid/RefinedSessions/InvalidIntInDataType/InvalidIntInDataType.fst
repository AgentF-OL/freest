module InvalidIntInDataType where

type Pos, Neg : *T
type PolarTree : *T -> *T -> *T

type Pos = {x: Int | x >= 0}
type Neg = {y: Int | y <= 0}

-- Left of each node is of a type and right is of another
data PolarTree a b =
    Leaf
  | Node (PolarTree a b) a (PolarTree b a)

buildPolarIntTree : PolarTree Pos Neg
buildPolarIntTree = Node @Pos @Neg left 0 right
  where
    leftLeft = Node @Pos @Neg (Leaf @Pos @Neg) 2 (Leaf @Neg @Pos) -- X - 3 should be negative
    leftRight = Node @Neg @Pos (Node @Neg @Pos (Leaf @Neg @Pos) 3 (Leaf @Pos @Neg)) (-2) (Leaf @Pos @Neg)
    left = Node @Pos @Neg leftLeft 1 leftRight
    right = Node @Neg @Pos (Leaf @Neg @Pos) (-1) (Leaf @Pos @Neg)

main : ()
main = buildPolarIntTree; ()
