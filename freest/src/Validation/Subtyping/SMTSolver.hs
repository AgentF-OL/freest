{-
SMT Solver for refined types validation: https://hackage.haskell.org/package/sbv
-}
module Validation.Subtyping.SMTSolver
  ( predImplies
  )
where

import Data.SBV

import Syntax.Base (Variable)
import Syntax.Type.Refinement qualified as R

predImplies :: R.Pred -> R.Pred -> Bool
predImplies p1 p2 = True
