module Syntax.Refinement
  ( Refinement(..)
  , RefinementType(..)
  , Predicate(..)
  , PredicateExpression(..)
  , fromBoolLit
  )
where

import Syntax.Base (Variable)

data Refinement
  = Refined Variable RefinementType Predicate
  | Unrefined
  deriving (Eq, Ord)

data RefinementType
  = RefinedInt
  deriving (Eq, Ord)

data Predicate
  = PredicateComparison PredicateExpression Variable PredicateExpression
  | PredicateAnd Predicate Predicate
  | PredicateOr Predicate Predicate
  | PredicateImplies Predicate Predicate
  | PredicateIff Predicate Predicate
  | PredicateNot Predicate
  | PredicateTrue
  | PredicateFalse
  | PredicateParens Predicate
  | PredicateEmpty
  deriving (Eq, Ord)

data PredicateExpression
  = ExpressionVariable Variable
  | ExpressionConstant Int
  | ExpressionSum PredicateExpression PredicateExpression
  | ExpressionSubtraction PredicateExpression PredicateExpression
  | ExpressionProduct Int PredicateExpression
  | ExpressionConditional Predicate PredicateExpression PredicateExpression
  | ExpressionParens PredicateExpression
  deriving (Eq, Ord)

fromBoolLit :: String -> Predicate
fromBoolLit = \case
  "True" -> PredicateTrue
  "False" -> PredicateFalse
  _ -> error "Syntax.Refinement not a valid bool literal"

instance Show Refinement where
  show = \case
    Refined v t p -> "{" ++ show v ++ ": " ++ show t ++ case p of
      PredicateEmpty -> "}"
      _ -> " | " ++ show p ++ "}"
    Unrefined -> ""

instance Show RefinementType where
  show = \case
    RefinedInt -> "Int"

instance Show Predicate where
  show = \case
    PredicateComparison e1 cmp e2 -> show e1 ++ " " ++ show cmp ++ " " ++ show e2
    PredicateAnd p1 p2 -> show p1 ++ " && " ++ show p2
    PredicateOr p1 p2 -> show p1 ++ " || " ++ show p2
    PredicateImplies p1 p2 -> show p1 ++ " => " ++ show p2
    PredicateIff p1 p2 -> show p1 ++ " <=> " ++ show p2
    PredicateNot p -> "not " ++ show p
    PredicateTrue -> "True"
    PredicateFalse -> "False"
    PredicateParens p -> "(" ++ show p ++ ")"
    PredicateEmpty -> ""

instance Show PredicateExpression where
  show = \case
    ExpressionVariable x -> show x
    ExpressionConstant c -> show c
    ExpressionSum e1 e2 -> show e1 ++ " + " ++ show e2
    ExpressionSubtraction e1 e2 -> show e1 ++ " - " ++ show e2
    ExpressionProduct c e -> show c ++ " * " ++ show e
    ExpressionConditional p e1 e2 -> "if " ++ show p ++ " then " ++ show e1 ++ " else " ++ show e2
    ExpressionParens e -> "(" ++ show e ++ ")"
