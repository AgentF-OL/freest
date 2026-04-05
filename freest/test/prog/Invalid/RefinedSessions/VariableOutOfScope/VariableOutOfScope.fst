module VariableOutOfScope where

type OutOfScope : 1C
type OutOfScope = Skip ; Skip ; ?{x: Int | y > 0} ; Skip ; Skip ; Wait
