module Syntax.Type.Refinement
  ( Pred(..)
  , Exp(..)
  , Cmp(..)
  , Payload
  , pAll
  , pAny
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
  | Let Variable Pred
  | PTrue
  | PFalse
  deriving (Eq, Ord)

data Exp
  = Var Variable
  | Const Int
  | Sum Exp Exp
  | Sub Exp Exp
  | Prod Exp Exp
  | Neg Exp
  | Cond Pred Exp Exp
  deriving (Eq, Ord)

data Cmp
  = Lt
  | Le
  | Eq
  | Ge
  | Gt
  | Diff
  deriving (Eq, Ord)

type Payload = [Pred]

instance Show Pred where
  show = \case
    Cmp e1 cmp e2 -> "(" ++ show e1 ++ " " ++ show cmp ++ " " ++ show e2 ++ ")"
    And p1 p2 -> "(" ++ show p1 ++ " && " ++ show p2 ++ ")"
    Or p1 p2 -> "(" ++ show p1 ++ " || " ++ show p2 ++ ")"
    Implies p1 p2 -> "(" ++ show p1 ++ " => " ++ show p2 ++ ")"
    Iff p1 p2 -> "(" ++ show p1 ++ " <=> " ++ show p2 ++ ")"
    Not p -> "(not " ++ show p ++ ")"
    Let v p -> "(let " ++ show v ++ " in " ++ show p ++ ")"
    PTrue -> "True"
    PFalse -> "False"

instance Show Exp where
  show = \case
    Var x -> show x
    Const c -> show c
    Sum e1 e2 -> "(" ++ show e1 ++ " + " ++ show e2 ++ ")"
    Sub e1 e2 -> "(" ++ show e1 ++ " - " ++ show e2 ++ ")"
    Prod e1 e2 -> "(" ++ show e1 ++ " * " ++ show e2 ++ ")"
    Neg e -> "-(" ++ show e ++ ")"
    Cond p e1 e2 -> "(if " ++ show p ++ " then " ++ show e1 ++ " else " ++ show e2 ++ ")"

instance Show Cmp where
  show = \case
    Lt -> "<"
    Le -> "<="
    Eq -> "=="
    Ge -> ">="
    Gt -> ">"
    Diff -> "/="

pAll :: [Pred] -> Pred
pAll [] = PTrue
pAll [p] = p
pAll (p:preds) = And p $ pAll preds

pAny :: [Pred] -> Pred
pAny [] = PTrue
pAny [p] = p
pAny (p:preds) = Or p $ pAny preds
