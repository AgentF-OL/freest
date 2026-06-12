{- |
Module      :  Syntax.Substitution
Copyright   :  © The FreeST Team
Maintainer  :  freest-lang@listas.ciencias.ulisboa.pt

This module implements capture-avoiding substitution for types, adapted from 
the corrected version of Lennart Augustsson's naïve substitution found in 
[lambda-n-ways repository](https://github.com/sweirich/lambda-n-ways/blob/main/lib/Lennart/Simple.hs).
To be replaced by a more efficient alternative.
-}
module Validation.Substitution
  ( subs
  , subsAll
  , betaRule
  , freeVars
  , predSubs
  , predFreeVars
  )
where

import Syntax.Base
import Syntax.Type.Internal qualified as T
import Syntax.Type.Kinded qualified as TK
import Syntax.Type.Refinement qualified as R
import Syntax.Kind qualified as K
import Data.Set qualified as Set

-- | The set of free variables occurring in a type.
freeVars :: T.Type x -> Set.Set Variable
freeVars = \case
    T.Abs _ _ aks t -> freeVars t Set.\\ Set.fromList (map fst aks)
    T.Var _ _ a     -> Set.singleton a
    T.App _ _ t ts  -> Set.unions (freeVars t : map freeVars ts)
    _               -> Set.empty

-- | The set of all variables ocurring in a type.
allVars :: T.Type x -> Set.Set Variable
allVars = \case 
    T.Abs _ _ aks t -> allVars t
    T.Var _ _ a     -> Set.singleton a
    T.App _ _ t ts  -> Set.unions (allVars t : map allVars ts)
    _               -> Set.empty

-- | Type substitution. Substitutes ocurrences of a variable in a type for 
-- another type (usually written @[a -> u] t@).
subs :: Variable -> TK.KindedType -> TK.KindedType -> TK.KindedType
subs a u = \case 
  -- Variables
  t@(TK.Var _ _ b)
    | b == a    -> u
    | otherwise -> t
  -- Abstractions (can we do this more elegantly?)
  (TK.Abs s [] t') -> TK.Abs s [] (subs a u t')
  t@(TK.Abs s ((b,k):bks) t')
      | b == a -> t
      | b `Set.member` fvu ->
        let b' = mkFreshVar (getSpan b) (Set.insert a fvu `Set.union` allVars t')
            TK.Abs _ bks' t'' = subs a u (subs b (T.Var (getSpan b') k b') (TK.Abs s bks t'))
        in TK.Abs s ((b',k):bks') t''
      | otherwise ->
        let TK.Abs _ bks' t'' = subs a u (TK.Abs s bks t')
        in TK.Abs s ((b,k):bks') t''
    where  fvu = freeVars u
  -- Applications
  TK.App s f ts -> TK.smartApp s (subs a u f) (fmap (subs a u) ts)
  t -> t

-- Polyadic substituion (written @[as -> us] t@). Considers only the shortest
-- between @as@ and @us@.
subsAll :: [Variable] -> [TK.KindedType] -> TK.KindedType -> TK.KindedType
subsAll as us t = foldr (uncurry subs) t (zip as us)

-- | Type application, the beta rule.
-- (λα1...αn. T) U1 ... Um -->β
--     T[U1/α1]...[Un/αn]                  if n = m
--     (T[U1/α1]...[Un/αn]) Un+1 ... Um    if m > n
--     λαn+1...αm. T[U1/α1]...[Un/αn]      if n > m
betaRule :: TK.KindedType -> [TK.KindedType] -> TK.KindedType
betaRule (TK.Abs s aks t) us
  | n == m    = v
  | m > n     = TK.App s v (drop n us)
  | otherwise = TK.Abs s (drop m aks) v
  where n = length aks
        m = length us
        v = subsAll (map fst aks) us t

predFreeVars :: R.Pred -> Set.Set Variable
predFreeVars = predFreeVars' Set.empty

predFreeVars' :: Set.Set Variable -> R.Pred -> Set.Set Variable
predFreeVars' vars = \case
  R.Cmp e1 cmp e2 -> Set.unions [vars, predExpFreeVars e1, predExpFreeVars e2]
  R.And p1 p2 -> Set.unions [vars, predFreeVars p1, predFreeVars p2]
  R.Or p1 p2 -> Set.unions [vars, predFreeVars p1, predFreeVars p2]
  R.Implies p1 p2 -> Set.unions [vars, predFreeVars p1, predFreeVars p2]
  R.Iff p1 p2 -> Set.unions [vars, predFreeVars p1, predFreeVars p2]
  R.Not p -> Set.unions [vars, predFreeVars p]
  R.Let v p -> Set.unions [vars, Set.delete v $ predFreeVars p]
  R.PTrue -> Set.empty
  R.PFalse -> Set.empty

predExpFreeVars :: R.Exp -> Set.Set Variable
predExpFreeVars = predExpFreeVars' Set.empty

predExpFreeVars' :: Set.Set Variable -> R.Exp -> Set.Set Variable
predExpFreeVars' vars = \case
  R.Var v -> Set.insert v vars
  R.Const _ -> Set.empty
  R.Sum e1 e2 -> Set.unions [vars, predExpFreeVars e1, predExpFreeVars e2]
  R.Sub e1 e2 -> Set.unions [vars, predExpFreeVars e1, predExpFreeVars e2]
  R.Prod _ e -> Set.unions [vars, predExpFreeVars e]
  R.Neg e -> Set.unions [vars, predExpFreeVars e]
  R.Cond e1 e2 e3 -> Set.unions [vars, predFreeVars e1, predExpFreeVars e2, predExpFreeVars e3]

-- | Predicate substitution.
-- Substitutes free ocurrences of variable 'v1' in 'p' by 'v2'
predSubs :: Variable -> Variable -> R.Pred -> R.Pred
predSubs v1 v2 p
  | v1 `elem` predFreeVars p = predSubs' v1 v2 p
  | otherwise = p

predSubs' :: Variable -> Variable -> R.Pred -> R.Pred
predSubs' v1 v2 = \case
  R.Cmp e1 cmp e2 -> R.Cmp (predExpSubs v1 v2 e1) cmp (predExpSubs v1 v2 e2)
  R.And p1 p2 -> R.And (predSubs v1 v2 p1) (predSubs v1 v2 p2)
  R.Or p1 p2 -> R.Or (predSubs v1 v2 p1) (predSubs v1 v2 p2)
  R.Implies p1 p2 -> R.Implies (predSubs v1 v2 p1) (predSubs v1 v2 p2)
  R.Iff p1 p2 -> R.Iff (predSubs v1 v2 p1) (predSubs v1 v2 p2)
  R.Not p -> R.Not (predSubs v1 v2 p)
  R.Let x p -> R.Let x (predSubs v1 v2 p)
  R.PTrue -> R.PTrue
  R.PFalse -> R.PFalse

predExpSubs :: Variable -> Variable -> R.Exp -> R.Exp
predExpSubs v1 v2 = \case
  R.Var x
    | x == v1 -> R.Var v2
    | otherwise -> R.Var x
  R.Const c -> R.Const c
  R.Sum e1 e2 -> R.Sum (predExpSubs v1 v2 e1) (predExpSubs v1 v2 e2)
  R.Sub e1 e2 -> R.Sub (predExpSubs v1 v2 e1) (predExpSubs v1 v2 e2)
  R.Prod c e -> R.Prod c (predExpSubs v1 v2 e)
  R.Neg e -> R.Neg (predExpSubs v1 v2 e)
  R.Cond p e1 e2 -> R.Cond (predSubs v1 v2 p) (predExpSubs v1 v2 e1) (predExpSubs v1 v2 e2)
