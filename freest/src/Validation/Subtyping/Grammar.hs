{- |
Module      :  TypeEquivalence.AlphaCongruence
Copyright   :  © The FreeST Team
Maintainer  :  freest-lang@listas.ciencias.ulisboa.pt

Simple grammars are context-free grammars where:

- Right-hand sides in productions are composed of (exactly) one terminal,
followed by a (possibly empty) word (a sequence of non-terminals)

- For each non-terminal symbol there is exactly one production.

This allows representing the productions of a grammar by a map from
non-terminals to a map from terminals to words.

-}
module Validation.Subtyping.Grammar 
  ( Terminal(..), sameLabel
  , Nonterminal
  , Word, emptyWord
  , LabelSet(..)
  , Transitions, emptyTransitions
  , Productions, emptyProductions
               , insertProduction
               , insertProductions
               , lookupTransitions
               , nonterminals
  , HasTransitions(..)
  , Norm(..), addNorm
  ) 
where

import Prelude hiding (Word)

import Data.Map qualified as Map
import Data.Maybe qualified as Maybe
import Data.Set qualified as Set
import Syntax.Base (Variable)
import qualified Syntax.Type.Kinded as T
import qualified Syntax.Type.Refinement as R
import Parser.Unparser (unparse)
import Validation.Subtyping.SMTSolver (predImplies)

-- Terminal symbols in the grammar
data Terminal = Default String | Arrow1 | Bang1 | Refinement Variable R.Pred
  deriving (Eq, Ord)

instance Show Terminal where
  show = \case
    Default s    -> s
    Arrow1       -> "(->)1"
    Bang1        -> "(!)1"
    Refinement v p -> show v ++ ": Int | " ++ unparse p

data LabelSet = X | Y | Z | W deriving (Eq, Ord, Show)

sameLabel :: Terminal -> Terminal -> LabelSet -> Bool
sameLabel t1 t2 l = case (t1, t2, l) of
  (Default t, Default u, _) -> t == u
  (Arrow1, Arrow1, _) -> True
  (Bang1, Bang1, _) -> True
  (Refinement v1 p1, Refinement v2 p2, X) -> predImplies v1 p1 v2 p2
  (Refinement v1 p1, Refinement v2 p2, _) -> True
  _ -> False

-- Non-terminal symbols in the grammar
type Nonterminal = Int

-- Words are strings of non-terminal symbols
type Word = [Nonterminal]

emptyWord :: Word
emptyWord = []

-- The transitions from a given non-terminal in the grammar
type Transitions = Map.Map Terminal Word

emptyTransitions :: Transitions
emptyTransitions = Map.empty

-- The productions of a grammar
type Productions = Map.Map Nonterminal Transitions

emptyProductions :: Productions
emptyProductions = Map.empty

-- Add a production to the grammar
insertProduction :: Nonterminal -> Terminal -> Word -> Productions -> Productions
insertProduction x a w = Map.insertWith Map.union x (Map.singleton a w)

-- Add productions to the grammar
insertProductions :: [(Nonterminal, Terminal, Word)]-> Productions -> Productions
insertProductions xs p =
  foldr (\(x, a, w) p -> insertProduction x a w p) p xs

lookupTransitions :: Nonterminal -> Productions -> Transitions
lookupTransitions x ps =
  Maybe.fromMaybe Map.empty (ps Map.!? x)

nonterminals :: Productions -> Set.Set Nonterminal
nonterminals ps = Set.union (Map.keysSet ps) (Set.fromList $ concat $ concatMap Map.elems $ Map.elems ps)

class HasTransitions t where
  transitions :: t -> Productions -> Transitions

-- The transitions from a non-terminal
instance HasTransitions Nonterminal where
  transitions = Map.findWithDefault Map.empty

-- The transitions from a word
instance HasTransitions Word where
  transitions []       _ = Map.empty
  transitions (x : xs) p = Map.map (++ xs) (transitions x p)

-- | The norm of a word is the length @n@ of the shortest sequence of 
-- transitions from that word to the empty word, represented as @Normed n@.
-- If no such sequence exists, the word is said to be unnormed and its norm
-- is represented as @Unnormed@.
data Norm = Normed Int | Unnormed
  deriving (Eq, Ord, Show)
  
Normed n `addNorm` Normed m = Normed (n + m)
_        `addNorm` _        = Unnormed