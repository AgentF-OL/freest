{- |
Module      :  Bisimulation.SubtypingSimulation
Description :  Simulation-based subtyping, adapted from https://doi.org/10.4230/LIPIcs.CONCUR.2023.11
Copyright   :  (c) <Authors or Affiliations>
License     :  <license>

Maintainer  :  gsilva@lasige.di.fc.ul.pt
Stability   :  unstable | experimental | provisional | stable | frozen
Portability :  portable | non-portable (<reason>)

-}

module Validation.Subtyping.Compare ( subtype, bisimilar, equivalent ) where


import qualified Data.Map as Map
import qualified Data.Sequence as Queue
import qualified Data.Set as Set
import Data.Tuple (swap)
import Prelude hiding (Word)

import Syntax.Module qualified as M
import Syntax.Type.Kinded qualified as TK 
import Validation.Subtyping.Simulation
import Validation.Subtyping.Grammar
import Validation.Subtyping.Norm (allNormed)
import Validation.Subtyping.FromTypes ( fromTypes )

equivalent :: M.KindedModule -> TK.KindedType -> TK.KindedType -> Bool
equivalent modl t u = t == u || bisimilar modl t u

bisimilar :: M.KindedModule -> TK.KindedType -> TK.KindedType -> Bool
bisimilar modl t u = expand expandPairBisim queue rules ps
  where
    (ps, [xs, ys]) = fromTypes modl [t, u]
    rules | allNormed ps = [reflex, headCongruence, bpa2, filtering]
          | otherwise    = [reflex, headCongruence, bpa1, bpa2, filtering]
    queue = Queue.singleton (Set.singleton (xs, ys), Set.empty) 

expandPairBisim :: PairExpander
expandPairBisim ps (xs, ys) 
  | Map.keysSet m1 == Map.keysSet m2 = Just $ match m1 m2
  | otherwise                        = Nothing
 where 
  m1 = transitions xs ps
  m2 = transitions ys ps
  match :: Transitions -> Transitions -> Node
  match m1 m2 =
    Map.foldrWithKey (\l xs n -> Set.insert (xs, m2 Map.! l) n) Set.empty m1

subtype :: M.KindedModule -> TK.KindedType -> TK.KindedType -> Bool
subtype modl t u = expand expandPairSub queue rules ps
  where 
    (ps, [xs, ys]) = fromTypes modl [t, u]
    rules | allNormed ps = [reflex, headCongruence, bpa2]
          | otherwise    = [reflex, headCongruence, bpa1, bpa2]
    queue = Queue.singleton (Set.singleton (xs, ys), Set.empty)

-- XYZW-expansion at the level of pairs of words 
-- https://doi.org/10.4230/LIPIcs.CONCUR.2023.11, Definition 17.
expandPairSub :: PairExpander
expandPairSub ps (xs, ys) =
  -- extract transitions
  let ts1 = transitions xs ps
      ts2 = transitions ys ps in 
  -- expansion must hold on labels of all X, Y, Z and W sets
  Set.unions <$> sequence
    -- expand on X labels
    [ let ts1X = filterX ts1; ts2X = filterX ts2 in 
      if Map.keysSet ts1X `Set.isSubsetOf` Map.keysSet ts2X
        then Just (matchTrans ts1X ts2X)
        else Nothing
    -- expand on Y labels
    , let ts1Y = filterY ts1; ts2Y = filterY ts2 in 
      if Map.keysSet ts2Y `Set.isSubsetOf` Map.keysSet ts1Y
        then Just (matchTrans ts1Y ts2Y)
        else Nothing
    -- expand on Z labels
    , let ts1Z = filterZ ts1; ts2Z = filterZ ts2 in 
      if Map.keysSet ts1Z `Set.isSubsetOf` Map.keysSet ts2Z
        then Just (Set.map swap $ matchTrans ts1Z ts2Z)
        else Nothing
    -- expand on W labels
    , let ts1W = filterW ts1; ts2W = filterW ts2 in 
      if Map.keysSet ts2W `Set.isSubsetOf` Map.keysSet ts1W
        then Just (Set.map swap $ matchTrans ts1W ts2W)
        else Nothing
    ]
  where
    -- Membership in the X, Y, Z and W sets specifies the kind of
    -- simulation to be tested. This membership assignment specifies
    -- the subtyping simulation outlined in 
    -- https://doi.org/10.4230/LIPIcs.CONCUR.2023.11, Definition 8.
    memberX, memberY, memberZ, memberW :: Terminal -> Bool 
    -- X
    memberX = \case 
      Arrow1 -> False
      Bang1 -> False
      _ -> True
    -- Y
    memberY = \case
      Arrow1 -> False
      Bang1 -> False
      _ -> True
    -- Z
    memberZ = \case
      Arrow1 -> True
      Bang1 -> True
      _ -> False
    -- W
    memberW = memberZ

    -- Filter transitions according to the XYZW-membership of their labels
    filterX, filterY, filterZ, filterW :: Transitions -> Transitions
    [filterX, filterY, filterZ, filterW] = 
      map (\f -> Map.filterWithKey (\k _ -> f k)) 
          [memberX, memberY, memberZ, memberW]

    -- Match transitions with the "same" label:
    -- - for non-refined types, based on syntax equality
    -- - for refined types, based on predicate implication (p1 => p2)
    matchTrans :: Transitions -> Transitions -> Node
    matchTrans m1 m2 = Set.fromList $ Map.elems $ Map.intersectionWith (,) m1 m2