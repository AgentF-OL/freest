module PolarIntTree where

type Pos, Neg, GOne, LOne : *T
type PolarTree : *T -> *T -> *T

type Pos = {x: Int | x >= 0}
type Neg = {y: Int | y <= 0}
type GOne = {x: Int | x > 1}
type LOne = {y: Int | y < 1}

-- Left of each node has type a and right has type b
data PolarTree a b =
    Leaf
  | Node (PolarTree a b) a (PolarTree b a)

buildPolarIntTree : PolarTree Pos Neg
buildPolarIntTree = Node @Pos @Neg left 0 right
  where
    leftLeft = Node @GOne @LOne (Leaf @GOne @LOne) greaterThanOne (Leaf @GOne @LOne) -- depth-subtyping
    leftRight = Node @Pos @Neg (Node @Pos @Neg (Leaf @Pos @Neg) 3 (Leaf @Pos @Neg)) -2 (Leaf @Pos @Neg)
    left = Node @Pos @Neg leftLeft 1 leftRight
    right = Node @Pos @Neg (Leaf @Pos @Neg) -1 (Leaf @Pos @Neg)

greaterThanOne : GOne
greaterThanOne = 2

lowerThanOne : LOne
lowerThanOne = -2

main : ()
main = buildPolarIntTree; ()