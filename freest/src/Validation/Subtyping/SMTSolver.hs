{-
SMT Solver for refined types validation: https://hackage.haskell.org/package/sbv
-}
module Validation.Subtyping.SMTSolver
  ( predImplies
  )
where

import Data.SBV
import System.IO.Unsafe (unsafePerformIO)
import Utils (internalError)
import qualified Data.Map as Map

import Syntax.Base (Variable(..))
import Syntax.Type.Refinement qualified as R
import Validation.Substitution (predSubs)

-- | Possible symbol values.
data SymbolValue
  = Integer SInteger

-- | Context that keeps track of variable representations and their respective symbol.
type SymbolCtx = Map.Map String SymbolValue

-- | Lookup the variable representation in the context. Use
--     * 'lookupIntegerSymbol' for integers;
lookupIntegerSymbol :: Variable -> SymbolCtx -> Maybe SInteger
lookupIntegerSymbol v sctx = case Map.lookup (show v) sctx of
  Just (Integer s) -> Just s
  _ -> Nothing

-- | Insert a variable representation in the symbol context. Use
--
--     * 'insertIntegerSymbol' for integers
insertIntegerSymbol :: Variable -> SymbolCtx -> Symbolic SymbolCtx
insertIntegerSymbol v sctx = case lookupIntegerSymbol v sctx of
  Just _ -> pure sctx
  Nothing -> do
    sv <- sInteger $ show v
    return $ Map.insert (show v) (Integer sv) sctx

proveTheorem :: Symbolic SBool -> Bool
proveTheorem = unsafePerformIO . isTheoremWith z3{verbose = False}

predImplies :: Variable -> R.Pred -> Variable -> R.Pred -> Bool
predImplies v1 p1 v2 p2 = proveTheorem $ do
  let p2Subs = predSubs v2 v1 p2
  sctx <- insertIntegerSymbol v1 Map.empty
  (sp, sctx') <- sPred sctx $ R.Implies p1 p2Subs
  return sp

sPred :: SymbolCtx -> R.Pred -> Symbolic (SBool, SymbolCtx)
sPred sctx = \case
  R.Cmp e1 cmp e2 -> do
    (se1, sctx') <- sPredExp sctx e1
    let fcmp = sCmp cmp
    (se2, sctx'') <- sPredExp sctx' e2
    return (se1 `fcmp` se2, sctx'')
  R.And p1 p2 -> do
    (sp1, sctx') <- sPred sctx p1
    (sp2, sctx'') <- sPred sctx' p2
    return (sp1 .&& sp2, sctx'')
  R.Or p1 p2 -> do
    (sp1, sctx') <- sPred sctx p1
    (sp2, sctx'') <- sPred sctx' p2
    return (sp1 .|| sp2, sctx'')
  R.Implies p1 p2 -> do
    (sp1, sctx') <- sPred sctx p1
    (sp2, sctx'') <- sPred sctx' p2
    return (sp1 .=> sp2, sctx'')
  R.Iff p1 p2 -> do
    (sp1, sctx') <- sPred sctx p1
    (sp2, sctx'') <- sPred sctx' p2
    return (sp1 .<=> sp2, sctx'')
  R.Not p -> do
    (sp, vars') <- sPred sctx p
    return (sNot sp, vars')
  R.Let v p -> do
    sctx' <- insertIntegerSymbol v sctx
    (sp, sctx'') <- sPred sctx' p
    return (sp, sctx'')
  R.PTrue -> pure (sTrue, sctx)
  R.PFalse -> pure (sFalse, sctx)

sPredExp :: SymbolCtx -> R.Exp -> Symbolic (SInteger, SymbolCtx)
sPredExp sctx = \case
  R.Var v -> case lookupIntegerSymbol v sctx of
    Just sv -> return (sv, sctx)
    Nothing -> internalError $ "Validation.Subtyping.SMTSolver integer variable not found for " ++ show v
  R.Const c -> pure (fromInteger $ toEnum c, sctx)
  R.Sum e1 e2 -> do
    (se1, sctx') <- sPredExp sctx e1
    (se2, sctx'') <- sPredExp sctx' e2
    return (se1 + se2, sctx'')
  R.Sub e1 e2 -> do
    (se1, sctx') <- sPredExp sctx e1
    (se2, sctx'') <- sPredExp sctx' e2
    return (se1 - se2, sctx'')
  R.Prod e1 e2 -> do
    (se1, sctx') <- sPredExp sctx e1
    (se2, sctx'') <- sPredExp sctx' e2
    return (se1 * se2, sctx'')
  R.Neg e -> do
   (se, sctx') <- sPredExp sctx e
   return (negate se, sctx')
  R.Cond p e1 e2 -> do
    (sp, sctx') <- sPred sctx p
    (se1, sctx'') <- sPredExp sctx' e1
    (se2, sctx''') <- sPredExp sctx'' e2
    return (ite sp se1 se2, sctx''')

sCmp :: R.Cmp -> SInteger -> SInteger -> SBool
sCmp = \case
  R.Lt -> (.<)
  R.Le -> (.<=)
  R.Eq -> (.==)
  R.Ge -> (.>=)
  R.Gt -> (.>)
  R.Diff -> (./=)
