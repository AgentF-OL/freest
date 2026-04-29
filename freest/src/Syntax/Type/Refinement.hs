module Syntax.Type.Refinement
  ( Pred(..)
  , Exp(..)
  , Cmp(..)
  )
where

import Syntax.Base (Variable, Identifier, Level)

data Pred
  = Cmp Exp Cmp Exp
  | And Pred Pred
  | Or Pred Pred
  | Implies Pred Pred
  | Iff Pred Pred
  | Not Pred
  | PTrue
  | PFalse
  deriving (Eq, Ord)

data Exp
  = Var Variable
  | Const Int
  | Sum Exp Exp
  | Sub Exp Exp
  | Prod Int Exp
  | Cond Pred Exp Exp
  deriving (Eq, Ord)

data Cmp
  = L
  | LE
  | E
  | GE
  | G
  | Diff
  deriving (Eq, Ord)

instance Show Pred where
  show = \case
    Cmp e1 cmp e2 -> "(" ++ show e1 ++ " " ++ show cmp ++ " " ++ show e2 ++ ")"
    And p1 p2 -> "(" ++ show p1 ++ " && " ++ show p2 ++ ")"
    Or p1 p2 -> "(" ++ show p1 ++ " || " ++ show p2 ++ ")"
    Implies p1 p2 -> "(" ++ show p1 ++ " => " ++ show p2 ++ ")"
    Iff p1 p2 -> "(" ++ show p1 ++ " <=> " ++ show p2 ++ ")"
    Not p -> "(not " ++ show p ++ ")"
    PTrue -> "True"
    PFalse -> "False"

instance Show Exp where
  show = \case
    Var x -> show x
    Const c -> show c
    Sum e1 e2 -> "(" ++ show e1 ++ " + " ++ show e2 ++ ")"
    Sub e1 e2 -> "(" ++ show e1 ++ " - " ++ show e2 ++ ")"
    Prod c e -> "(" ++ show c ++ " * " ++ show e ++ ")"
    Cond p e1 e2 -> "(if " ++ show p ++ " then " ++ show e1 ++ " else " ++ show e2 ++ ")"

instance Show Cmp where
  show = \case
    L -> "<"
    LE -> "<="
    E -> "=="
    GE -> ">="
    G -> ">"
    Diff -> "/="
