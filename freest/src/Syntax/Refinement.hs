module Syntax.Refinement
  ( Refinement(..)
  , RefinementType(..)
  , Predicate(..)
  , PredicateExpression(..)
  , ExpressionConstant(..)
  , PredicateAppExp(..)
  )
where

import Syntax.Base (Variable, Identifier, Level)

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
  | PredicateNot PredicateAppExp
  | PredicateTrue
  | PredicateFalse
  deriving (Eq, Ord)

data PredicateExpression
  = ExpressionVariable Variable
  | ExpressionConstant ExpressionConstant
  | ExpressionSum PredicateExpression PredicateExpression
  | ExpressionSubtraction PredicateExpression PredicateExpression
  | ExpressionProduct ExpressionConstant PredicateExpression
  | ExpressionConditional Predicate PredicateExpression PredicateExpression
  deriving (Eq, Ord)

data ExpressionConstant
  = ConstantInt Int
  deriving (Eq, Ord)

data PredicateAppExp
  = Int Int
  | Var Variable
  | DCons Identifier
  | App PredicateAppExp [PredicateAppExp]
  | If PredicateAppExp PredicateAppExp PredicateAppExp
  deriving (Eq, Ord)

instance Show Refinement where
  show = \case
    Refined v t p -> "{" ++ show v ++ ": " ++ show t ++ " | " ++ show p ++ "}"
    Unrefined -> ""

instance Show RefinementType where
  show = \case
    RefinedInt -> "Int"

instance Show Predicate where
  show = \case
    PredicateComparison e1 cmp e2 -> "(" ++ show e1 ++ " " ++ show cmp ++ " " ++ show e2 ++ ")"
    PredicateAnd p1 p2 -> "(" ++ show p1 ++ " && " ++ show p2 ++ ")"
    PredicateOr p1 p2 -> "(" ++ show p1 ++ " || " ++ show p2 ++ ")"
    PredicateImplies p1 p2 -> "(" ++ show p1 ++ " => " ++ show p2 ++ ")"
    PredicateIff p1 p2 -> "(" ++ show p1 ++ " <=> " ++ show p2 ++ ")"
    PredicateNot p -> "(not " ++ show p ++ ")"
    PredicateTrue -> "True"
    PredicateFalse -> "False"

instance Show PredicateExpression where
  show = \case
    ExpressionVariable x -> show x
    ExpressionConstant c -> show c
    ExpressionSum e1 e2 -> "(" ++ show e1 ++ " + " ++ show e2 ++ ")"
    ExpressionSubtraction e1 e2 -> "(" ++ show e1 ++ " - " ++ show e2 ++ ")"
    ExpressionProduct c e -> "(" ++ show c ++ " * " ++ show e ++ ")"
    ExpressionConditional p e1 e2 -> "(if " ++ show p ++ " then " ++ show e1 ++ " else " ++ show e2 ++ ")"

instance Show ExpressionConstant where
  show = \case
    ConstantInt c -> show c

instance Show PredicateAppExp where
  show = \case
    Int i        -> show i
    Var x        -> show x
    DCons i      -> show i
    App f as     -> foldl (\s a -> "("++s++" "++show a++")") (show f) as
    If e1 e2 e3  -> "(if "++show e1++" then "++show e2++" else "++show e3++")"
