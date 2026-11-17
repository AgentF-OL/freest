{- |
Module      :  Interpreter.PatternMatching
Copyright   :  © The FreeST Team
Maintainer  :  freest-lang@listas.ciencias.ulisboa.pt

This module implements FreeST's runtime pattern matching: a value-directed
matcher that matches runtime 'Value's against surface 'Pat'terns, producing
variable bindings.

Session patterns perform communication, which is irreversible — so a channel
matched by a session pattern must be received from once, with the result then
selecting a clause (it must not be re-received as we try later clauses). We
therefore split matching into two passes:

  1. 'forceColumns' performs the session effects demanded by the clauses
     (receive a choice label, receive a value, wait), turning the channels into
     ordinary data values. Because the type system makes the session structure
     of a column the same across all clauses, this can be done once, up front,
     even when the session pattern is nested inside a data pattern (e.g. a
     tuple).

  2. 'matchClause' / 'matchPat' then match the clauses against those forced
     values purely, with ordinary backtracking and guard fall-through.
-}
module Interpreter.PatternMatching
  ( matchPat
  , matchClause
  , forceColumns
  ) where

import Control.Monad (zipWithM)
import Data.List (transpose)
import Data.Map (empty, singleton, union, insert)

import Data.Maybe (catMaybes)

import Interpreter.Value (Value(..), ValueCtx, Clause)
import Interpreter.Builtin (receive, receiveLabel, receiveLabel_)
import qualified Syntax.Base as B
import qualified Syntax.Declarations as D
import qualified Syntax.Expression as E
import qualified Syntax.Kind as K
import qualified Syntax.Type.Kinded as T

-- * Pure matching (run after 'forceColumns')

-- | Match a single pattern (one column) against a value, producing bindings on
-- success or 'Nothing' on failure. Session patterns are matched against the
-- *forced* representations produced by 'forceColumns'.
matchPat :: Value -> E.KindedPat -> IO (Maybe ValueCtx)
matchPat v = \case
  E.IntPat _ i   -> pure $ case v of VInt   i' | i == i' -> Just empty ; _ -> Nothing
  E.FloatPat _ f -> pure $ case v of VFloat f' | f == f' -> Just empty ; _ -> Nothing
  E.CharPat _ c  -> pure $ case v of VChar  c' | c == c' -> Just empty ; _ -> Nothing
  E.StringPat _ s -> pure $ if stringMatches s v then Just empty else Nothing
  E.WildPat _ _  -> pure (Just empty)
  E.VarPat _ x   -> pure (Just (singleton x v))
  E.AsPat _ x p  -> fmap (insert x v) <$> matchPat v p
  E.PackPat _ _ p ->
    case v of
      VPack _ inner -> matchPat inner p
      _             -> pure Nothing
  E.DConsPat _ (B.Identifier _ con) ps ->
    case v of
      VCons con' vs | con == con' && length ps == length vs -> matchClause (map B.ExpLevel ps) vs
      _                                                      -> pure Nothing
  -- Session patterns, matched against the forced representations:
  --   a chosen branch  &l   →  VCons l [continuation]
  --   a received value ?p;q →  VCons "(,)" [forced p, forced q]
  --   a closed channel Wait →  any (the wait was performed)
  E.ChoicePat _ _ (B.Identifier _ label) q ->
    case v of
      VCons label' [cont] | label == label' -> matchPat cont q
      _                                     -> pure Nothing
  E.InPat _ _ p1 p2 ->
    case v of
      VCons "(,)" [v1, v2] -> matchClause [B.ExpLevel p1, B.ExpLevel p2] [v1, v2]
      _                    -> pure Nothing
  E.TypeInPat _ _ p -> matchPat v p   -- the type input was already consumed
  E.WaitPat _       -> pure (Just empty)

-- | Does a string value equal a string-literal pattern? Matches structurally,
-- so the empty literal @""@ matches the empty list. (Contrast 'asString', which
-- leaves the empty list undecided because @""@ and @[]@ share a representation;
-- here typing guarantees @v@ is a String, so the empty list can only be @""@.)
stringMatches :: String -> Value -> Bool
stringMatches []       (VCons "[]"   [])              = True
stringMatches (c : cs) (VCons "(::)" [VChar d, rest]) = c == d && stringMatches cs rest
stringMatches _        _                              = False

-- | Match a column of patterns left-to-right against a list of values, failing
-- fast and unioning the bindings.
matchClause :: [B.Level E.KindedPat B.Variable B.Variable] -> [Value] -> IO (Maybe ValueCtx)
matchClause []       []       = pure (Just empty)
matchClause (B.ExpLevel p : ls) (v : vs) = do
  mb <- matchPat v p
  case mb of
    Nothing -> pure Nothing
    Just b  -> fmap (union b) <$> matchClause ls vs
matchClause (B.TypeLevel v : ls) (val@(VType _) : vs) = fmap (union (singleton v val)) <$> matchClause ls vs
matchClause (B.MultLevel v: ls) (val@(VMult _) : vs) = fmap (union (singleton v val)) <$> matchClause ls vs
matchClause _ _ = pure Nothing   -- arity mismatch

-- * Session forcing pass

-- | Perform, once, the session effects the clauses demand on the argument
-- values, turning channels into ordinary data values. The patterns of all the
-- clauses (one column list per clause) guide which effect to perform; by the
-- type system they agree on the session structure of every position.
forceColumns :: D.KindedTypeDecls -> [Clause] -> [Value] -> IO [Value]
forceColumns tdecls clauses = zipWithM (forceCol tdecls) (transpose (map fst clauses))

-- | Force one position, given the patterns the clauses use there.
forceCol :: D.KindedTypeDecls -> [B.Level E.KindedPat B.Variable B.Variable] -> Value -> IO Value
forceCol tdecls pats val = case val of
  -- a channel reached by a session pattern: perform its effect (once)
  VChan _ ->
    case filter isSessionPat (map stripAs pats) of
      (B.ExpLevel E.ChoicePat{} : _) -> forceChoice tdecls pats val   -- needs the whole column (see below)
      (B.ExpLevel sp : _)            -> performEffect tdecls sp val
      []                             -> pure val                      -- only bound (var pattern): no effect
  -- recurse into data, using the sub-patterns of the clauses that match `con`
  VCons con vs ->
    let subColumns = transpose [ ps | B.ExpLevel (E.DConsPat _ (B.Identifier _ con') ps) <- map stripAs pats
                                    , con' == con, length ps == length vs ]
    in if null subColumns then pure val
       else VCons con <$> zipWithM (forceCol tdecls) (map (map B.ExpLevel) subColumns) vs
  _ -> pure val

-- | Force an external choice: receive the label, then force the continuation
-- using the continuation patterns of the clauses that selected *this* label —
-- so a session pattern nested in the continuation (e.g. @&Bid (?x;c)@) is forced
-- too. (Mirrors the @VCons@ recursion in 'forceCol': commit to the branch the
-- peer actually chose, then recurse into it. The type system guarantees the
-- selected clauses agree on the continuation's session structure.)
forceChoice :: D.KindedTypeDecls -> [B.Level E.KindedPat B.Variable B.Variable] -> Value -> IO Value
forceChoice tdecls pats (VChan c@(_, _, t)) = do
  (label, c') <- case T.kindOf t of K.Proper _ K.Lin{} _ -> receiveLabel tdecls c; K.Proper _ K.Un{} _ -> receiveLabel_ tdecls c
  let conts = [ q | B.ExpLevel (E.ChoicePat _ _ (B.Identifier _ l) q) <- map stripAs pats, l == label ]
  forced <- forceCol tdecls (map B.ExpLevel conts) (VChan c')
  pure (VCons label [forced])
forceChoice _ _ v = pure v

-- | Perform a session pattern's effect on a channel, producing the forced value
-- that 'matchPat' then matches purely. (External choice is handled by
-- 'forceChoice', which needs the whole pattern column.)
performEffect :: D.KindedTypeDecls -> E.KindedPat -> Value -> IO Value
performEffect tdecls (E.InPat _ _ p1 p2) (VChan c) = do
  (v, c') <- receive tdecls c
  v'  <- forceCol tdecls [B.ExpLevel p1] v
  c'' <- forceCol tdecls [B.ExpLevel p2] (VChan c')
  pure (VCons "(,)" [v', c''])
performEffect tdecls (E.TypeInPat _ _ p) (VChan c) = do
  (_, c') <- receive tdecls c                  -- consume the (erased) type message
  forceCol tdecls [B.ExpLevel p] (VChan c')
performEffect tdecls (E.WaitPat _) (VChan c) = do
  _ <- receive tdecls c                        -- wait for the peer to close
  pure (VCons "()" [])
performEffect _ _ v = pure v

isSessionPat :: B.Level E.KindedPat B.Variable B.Variable -> Bool
isSessionPat = \case
  B.ExpLevel (E.ChoicePat{}) -> True
  B.ExpLevel (E.InPat{})     -> True
  B.ExpLevel (E.TypeInPat{}) -> True
  B.ExpLevel (E.WaitPat{})   -> True
  _             -> False

-- | 'AsPat' is transparent for the purpose of finding the session structure.
stripAs :: B.Level E.KindedPat B.Variable B.Variable -> B.Level E.KindedPat B.Variable B.Variable
stripAs = \case
  B.ExpLevel (E.AsPat _ _ p) -> stripAs $ B.ExpLevel p
  l             -> l
