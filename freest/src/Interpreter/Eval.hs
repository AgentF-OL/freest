{- |
Module      :  Interpreter.Interpreter
Copyright   :  © The FreeST Team
Maintainer  :  freest-lang@listas.ciencias.ulisboa.pt

This module implements FreeST's interpreter.
-}
module Interpreter.Eval
  ( evalModule
  ) where

{-
TODO:
- Implement channels for case: check https://github.com/freest-lang/freest3/blob/dev/FreeST/src/Interpreter/Eval.hs, evalCase
- When introducing closures, capture only free variables in the environment, and not the whole ctx
- Handling Prelude definitions, the search in the builtins should be more efficient: check if there's an undefined in the body. Also, what if the user redefines these?
- Handling of undefined is correct?
- Missing evaluation for E.Case (what about labels?)
- Eval can fail due to non-existent patterns during pattern marching. Hence return type should Either [IOE.Error] Value.
 -}

import Control.Concurrent (forkIO)
import Control.Exception (try, catch, throwIO)
import Control.Monad (zipWithM, foldM)
import Data.Bifunctor (first)
import Data.Functor (($>), void)
import Data.List (find)
import Data.Map (empty, singleton, union, unions, lookup, assocs, filterWithKey, insert, fromList)
import Data.Maybe (isJust, fromJust, catMaybes, fromMaybe)
import qualified Data.Set as Set

import Compiler.Bug (internalError)
import Interpreter.Exception (Exception(..), clearBuffers)
import Interpreter.PatternMatching (matchPat, matchClause, forceColumns)
import Interpreter.Value (ValueCtx, Clause, Value(..), mkClosure)
import Interpreter.Builtin (chan, send, builtins, fstToHsBool, hsToFstString, receive, receiveLabel)
import qualified Syntax.Type.Kinded as TK
import qualified Syntax.Base as B
import qualified Syntax.Declarations as D
import qualified Syntax.Expression as E
import qualified Syntax.Kind as K
import qualified Syntax.Module as M

-- | Bind a clause/alternative's `where` declarations; they are in scope for
-- both the guards and the bodies of the RHS.
bindWhere :: D.KindedTypeDecls -> ValueCtx -> Maybe [E.KindedLetDecl] -> IO ValueCtx
bindWhere tdecls ctx = \case
  Nothing    -> return ctx
  Just decls -> do
    binds <- collectLetDecls tdecls ctx decls
    return (binds `union` ctx)

-- | Evaluate guards in order, returning the first body whose guard holds, or
-- 'Nothing' if every guard fails (so matching falls through to the next clause).
tryGuards :: D.KindedTypeDecls -> ValueCtx -> [(E.KindedExp, E.KindedExp)] -> IO (Maybe E.KindedExp)
tryGuards tdecls ctx = \case
  [] -> return Nothing
  (guard, body) : guards -> do
    val <- eval tdecls ctx guard
    if fstToHsBool val then return (Just body) else tryGuards tdecls ctx guards

-- | Resolve a RHS in an environment that already holds the pattern bindings:
-- bind the `where` declarations, then (for a guarded RHS) select a guard.
-- Returns the chosen body and the environment to evaluate it in, or 'Nothing'
-- if all guards fail.
resolveRHS :: D.KindedTypeDecls -> ValueCtx -> E.KindedRHS -> IO (Maybe (E.KindedExp, ValueCtx))
resolveRHS tdecls ctx = \case
  E.UnguardedRHS body whereDecls -> do
    ctx' <- bindWhere tdecls ctx whereDecls
    return (Just (body, ctx'))
  E.GuardedRHS guards whereDecls -> do
    ctx' <- bindWhere tdecls ctx whereDecls
    mBody <- tryGuards tdecls ctx' guards
    return (fmap (, ctx') mBody)

-- | The single matcher shared by functions and @case@ expressions. Tries the
-- clauses in order against the (already-evaluated) argument values: a clause is
-- chosen only if its columns all match *and* its guards succeed; otherwise we
-- fall through to the next clause. The given environment is used to evaluate
-- guards and bodies.
matchClauses :: D.KindedTypeDecls -> ValueCtx -> [Value] -> [Clause] -> IO Value
matchClauses tdecls ctx vals clauses = do
  -- perform the session effects (receive/branch/wait) once, up front, turning
  -- channels into ordinary data values; then match the clauses purely.
  vals' <- forceColumns tdecls clauses vals
  goRows tdecls vals' clauses
  where
    goRows tdecls vals' = \case
      [] -> throwIO (NonExhaustivePatterns (clausesSpan clauses))
      (params, rhs) : rest -> do
        mb <- matchClause params vals'
        case mb of
          Nothing    -> goRows tdecls vals' rest                -- a column failed
          Just binds -> do
            res <- resolveRHS tdecls (binds `union` ctx) rhs
            case res of
              Nothing          -> goRows tdecls vals' rest      -- guards failed: fall through
              Just (exp', ctx') -> eval tdecls ctx' exp'

-- | A source span locating a group of clauses for a non-exhaustive-match error:
-- the whole group, from the head of the first clause to the body of the last.
clausesSpan :: [Clause] -> B.Span
clausesSpan = \case
  []  -> B.nullSpan
  cls -> B.spanFromTo (clauseHead (head cls)) (B.getSpan (snd (last cls)))
  where
    clauseHead (params, rhs) = case params of
      p : _ -> B.getSpan p
      []    -> B.getSpan rhs



-- | Collect the bindings introduced by a group of declarations, processed
-- top-down (as `let`/`where` are). A binding sees the earlier bindings; a
-- function additionally sees itself (recursion is the default), and the
-- functions of a @mutual@ block see one another. The knots are tied with an
-- ordinary recursive @let@ (@mkClosure@ captures its environment lazily).
--
-- A definition whose name is a builtin (e.g. @(+) = undefined@) is bound to the
-- builtin rather than to its user-written right-hand side.
collectLetDecls :: D.KindedTypeDecls -> ValueCtx -> [E.KindedLetDecl] -> IO ValueCtx
collectLetDecls tdecls outer = go tdecls outer empty
  where
    -- `ctx` = outer + the bindings so far; `acc` = just the new bindings
    go :: D.KindedTypeDecls -> ValueCtx -> ValueCtx -> [E.KindedLetDecl] -> IO ValueCtx
    go tdecls ctx acc = \case
      [] -> pure acc
      d : ds -> case d of
        E.TypeSig _ _ -> go tdecls ctx acc ds

        -- a single function is recursive in its own name (self-knot)
        E.FnDef var clauses ->
          let val = fromMaybe (mkClosure (insert var val ctx) clauses)
                              (Data.Map.lookup (B.external var) builtins)
          in go tdecls (insert var val ctx) (insert var val acc) ds

        -- the functions of a `mutual` block are mutually recursive
        E.Mutual mds ->
          let mbinds    = fromList [ (var, mkClosure mutualCtx clauses)
                                   | E.FnDef var clauses <- mds ]
              mutualCtx = ctx `union` mbinds
          in go tdecls mutualCtx (acc `union` mbinds) ds

        -- a value is not recursive: evaluate it in the current environment
        E.ValDef pat rhs
          | Just b <- builtinForPat pat ->
              go tdecls (insertVarPat pat b ctx) (insertVarPat pat b acc) ds
          | otherwise -> do
              res <- resolveRHS tdecls ctx rhs
              (e, ctx') <- maybe unmatched pure res
              v     <- eval tdecls ctx' e
              mb    <- matchPat v pat
              binds <- maybe unmatched pure mb
              go tdecls (binds `union` ctx) (binds `union` acc) ds
              where
              unmatched :: IO r
              unmatched = throwIO (NonExhaustivePatterns (B.getSpan pat)) :: IO r

    builtinForPat = \case
      E.VarPat _ var -> Data.Map.lookup (B.external var) builtins
      _              -> Nothing
    insertVarPat (E.VarPat _ var) val = insert var val
    insertVarPat _                _   = id

-- | Evaluate a module's declarations top-down in (and extending) an environment,
-- running their side effects and returning the new bindings. The REPL threads
-- this across inputs; 'interpret' runs it once from the empty environment.
evalModule :: ValueCtx -> M.KindedModule -> IO ValueCtx
evalModule ctx m = (`union` ctx) <$> collectLetDecls (M.typeDecls m) ctx (M.definitions m)

-- | Lookup a variable in the context
ctxLookup :: ValueCtx -> B.Variable -> Value
ctxLookup ctx var =
  case Data.Map.lookup var ctx of
    Just val -> val
    Nothing -> internalError $ "Variable `" ++ show var ++ "` not found in the context."

notTypeOrMult :: Value -> Bool
notTypeOrMult = \case
  VType _ -> False
  VMult _ -> False
  _ -> True

-- | Evaluate application expressions
handleApplication :: D.KindedTypeDecls -> ValueCtx -> Value -> [Value] -> IO Value
handleApplication _ _ func [] = return func
handleApplication _ _ (VCons cons vals) args = do
  return $ VCons cons $ vals ++ filter notTypeOrMult args
handleApplication tdecls ctx (VClosure collected clauses cctx) args = do
  let allArgs = collected ++ args
      arity   = length (fst (head clauses))
  -- not enough arguments yet (term *or* type/mult slots): stay a partial value
  if length allArgs < arity then
    return $ VClosure allArgs clauses cctx
  -- saturated: run the clause matcher on the term values of the first `arity`
  -- arguments, in the closure's self-contained captured environment
  else do
    let (these, rest) = splitAt arity allArgs
    result <- matchClauses tdecls cctx these clauses
    -- any leftover arguments are applied to the result
    if null rest then return result
    else handleApplication tdecls ctx result rest
handleApplication tdecls _ (VBuiltin builtin) args = foldM applyOne (VBuiltin builtin) (filter notTypeOrMult args)
  where
    -- Between builtin applications, force any 'VIO' result so effects
    -- (e.g. the exception thrown by 'error') fire in time for the
    -- enclosing App's 'catch' to attach the call-site span.
    applyOne (VBuiltin f) arg       = pure (f tdecls arg)
    applyOne (VIO io)     arg       = io >>= \v -> applyOne v arg
    applyOne v            _         = internalError $ "handleApplication: cannot apply value " ++ show v
handleApplication tdecls ctx VFork args = case filter notTypeOrMult args of
  [fun] -> forkIO (void $ handleApplication tdecls ctx fun [VCons "()" []]) $> VCons "()" []
  []    -> return VFork    -- only type/multiplicity applied so far
  _     -> internalError "fork applied to too many arguments"

-- | Attach a span to an exception, but only if the exception has none yet.
-- Enclosing 'App' layers keep the innermost span rather than clobbering it
-- with their own outer call site.
attachSpan :: B.Span -> Exception -> Exception
attachSpan s e
  | B.getSpan e == B.nullSpan = B.setSpan s e
  | otherwise                 = e
{- handleApplication (global, local) (VSelect label) args =
  case args of
    [VChan chan] -> do
      chan2 <- send (VLabel label) chan
      return $ VChan chan2
    _ -> internalError $ "Too many arguments applied to Select " ++ label ++ "! Type checking failed!" -}
{- handleApplication _ VSendType args =
  case args of
    [VChan chan] -> do
      chan2 <- send VUnit chan
      return $ VChan chan2
    _ -> internalError "Too many arguments applied to SendType! Type checking failed!" -}
{- handleApplication _ VRecvType args =
  case args of
    [VChan chan] -> do
      (_, chan2) <- receive chan
      return $ VChan chan2
    _ -> internalError "Too many arguments applied to ReceiveType! Type checking failed!" -}

-- MAIN FUNCTIONS

-- | Evaluate expressions, encoded as Syntax.Expression.Exp
eval :: D.KindedTypeDecls -> ValueCtx -> E.KindedExp -> IO Value
eval _ _ (E.Int _ i) =
  return $ VInt i
eval _ _ (E.Float _ f) =
  return $ VFloat f
eval _ _ (E.Char _ c) =
  return $ VChar c
eval _ _ (E.String _ s) =
  -- a string literal is the @Char@ cons-list @['a','b',...]@
  return $ hsToFstString s
eval _ _ (E.DCons _ (B.Identifier _ str)) =
  return $ VCons str []
eval tdecls ctx (E.List _ es) = do
  vs <- mapM (eval tdecls ctx) es
  return $ foldr (\v acc -> VCons "(::)" [v, acc]) (VCons "[]" []) vs
eval _ ctx e@(E.Var _ var) =
  case ctxLookup ctx var of
    VIO io -> io
    val -> return val
eval tdecls ctx (E.App span exp args) = do
  func <- eval tdecls ctx exp
  -- evaluate term arguments; type/multiplicity applications carry no value but
  -- still consume one of the closure's slots (so it stays a value until filled)
  evalArgs <- mapM evalArg args
  res <- handleApplication tdecls ctx func evalArgs `catch` \(e :: Exception) -> throwIO (attachSpan span e)
  case res of
    VIO io -> io `catch` \(e :: Exception) -> throwIO (attachSpan span e)
    _ -> return res
  where
    evalArg (B.ExpLevel e) = eval tdecls ctx e
    evalArg (B.TypeLevel t) = pure $ VType t
    evalArg (B.MultLevel m) = pure $ VMult m
eval _ ctx (E.Abs _ params _ body) =
  -- a lambda is a one-clause function (no guards, no `where`); the parameter
  -- list keeps its type/mult slots so a trailing one suspends the closure
  return $ mkClosure ctx [(map clauseParam params, E.UnguardedRHS body Nothing)]
  where
    clauseParam (B.ExpLevel (p, _))  = B.ExpLevel p
    clauseParam (B.TypeLevel (v, _)) = B.TypeLevel v
    clauseParam (B.MultLevel m)      = B.MultLevel m
eval tdecls ctx (E.Pack span types exp) = do
  val <- eval tdecls ctx exp
  return $ VPack types val
eval tdecls ctx (E.Asc span exp typ) = do
  eval tdecls ctx exp
eval tdecls ctx e@(E.Let _ decls exp) = do
  letBindings <- collectLetDecls tdecls ctx decls
  eval tdecls (ctx `union` letBindings) exp
eval tdecls ctx (E.Case _ exp alternatives) = do
  val <- eval tdecls ctx exp
  -- a `case` is the one-column instance of the clause matcher (session effects,
  -- including choice patterns, are handled inside it)
  matchClauses tdecls ctx [val] (map (\(p, rhs) -> ([B.ExpLevel p], rhs)) alternatives)
eval tdecls ctx (E.If _ ifExp thenExp elseExp) = do
  ifVal <- eval tdecls ctx ifExp
  if fstToHsBool ifVal
  then eval tdecls ctx thenExp
  else eval tdecls ctx elseExp
eval tdecls ctx (E.Try _ tryExp pat asinExp unlessExp) = do
  eitherTryVal <- (try :: IO Value -> IO (Either Exception Value)) $ eval tdecls ctx tryExp
  case eitherTryVal of
    Right tryVal -> do
      mb <- matchPat tryVal pat
      binds <- maybe unmatched pure mb
      eval tdecls (ctx `union` binds) asinExp
    Left exn -> eval tdecls ctx unlessExp
  where
    unmatched :: IO r
    unmatched = throwIO (NonExhaustivePatterns (B.getSpan pat)) :: IO r
eval _ ctx (E.Raise s) = throwIO (SomeException s "SomeException")
eval tdecls ctx (E.Channel s t) = do
  -- obtain channel ends for a fresh channel
  (chanL, chanR) <- chan $ case t of TK.Var _ _ B.ObjLv var -> (case ctxLookup ctx var of VType t' -> t'); _ -> t
  return $ VCons "(,)" [VChan chanL, VChan chanR]
eval tdecls ctx (E.Select _ m (B.Identifier _ iden)) = do
  let (Just (VBuiltin selectBuiltin)) =
        Data.Map.lookup (if K.isUn m then "select_" else "select") builtins
  return $ selectBuiltin tdecls (VLabel iden)
  {- return $ VSelect iden -}
eval _ _ (E.SendType _ _) =
  return $ fromJust $ Data.Map.lookup "sendType" builtins
  {- return VSendType -}
eval _ _ (E.ReceiveType _) =
  return $ fromJust $ Data.Map.lookup "receiveType" builtins
  {- return VRecvType -}
eval tdecls ctx (E.SectionL s e op) =
  eval tdecls ctx (E.App s (either (E.Var s) (E.DCons s) op) [B.ExpLevel e])
eval _ ctx (E.SectionR s x op e) =
  return $ mkClosure ctx
    [ ( [B.ExpLevel (E.VarPat s x)]
      , E.UnguardedRHS (E.App s (either (E.Var s) (E.DCons s) op)
                          [B.ExpLevel (E.Var s x), B.ExpLevel e]) Nothing ) ]

-- OLD DEFINITIONS

{- -- Here is where the function application is done.
-- Because of how the parser parses function applications (f a b c d => f [a, b, c, d] even if f only takes one arg)
-- is necessary to repeat the evaluation until [arg] is empty.
-- TODO: context is a mess must check if it is correct, don't like that there is a lot of repetition
consumeAllArgs :: (ValueCtx, ValueCtx) -> Value -> [Value] -> IO Value
consumeAllArgs (global, local) (VClosure pats exp local_ctx) args = case sequence (doPatternMatching pats args []) of
  Just patternMatching ->
    if length pats == length args then eval (global, patternMatching ++ local_ctx ++ local) exp
    else if length pats < length args then do val <- (eval (global, patternMatching ++ local_ctx ++ local) exp)
                                              consumeAllArgs (global, patternMatching ++ local_ctx ++ local) val (drop (length pats) args)
    else return $ VClosure (drop (length args) pats) exp (patternMatching ++ local_ctx)
  -- TODO: use freeST error handling to tell the user that that pattern mathcing was not exhautive
  Nothing -> undefined
consumeAllArgs (global, local) (VFun patExps) args = do
  labels <- mapM receiveLabel $ getInternalChoiceChannels (fst $ head patExps) args
  case chooseRhs patExps args labels of
    Just (rhs, matched, pats) ->
      do (exp, whereDecls) <- (case rhs of E.UnguardedRHS exp whereDecls -> return (exp, whereDecls)
                                           E.GuardedRHS predExps whereDecls -> do guardsCtx <- chooseGuard (global, matched) predExps
                                                                                  return (guardsCtx, whereDecls))
         let whereCtx = (case whereDecls of Just letDecls -> resolveLetDecls global letDecls
                                            Nothing -> return [])
         whereCtx2 <- whereCtx
         if length pats == length args then eval (global, matched++whereCtx2) exp
         else if length pats < length args then do val <- eval (global, matched++whereCtx2) exp
                                                   consumeAllArgs (global, matched++whereCtx2) val (drop (length pats) args)
         else return $ VClosure (drop (length args) pats) exp (matched++whereCtx2)
    -- TODO: use freeST error handling to tell the user that that pattern mathcing was not exhautive
    Nothing -> undefined
-- Is there builtins that take no arguments?
consumeAllArgs ctx (VBuiltin builtin) [] = return $ builtin VUnit
consumeAllArgs ctx (VBuiltin builtin) [arg] = return $ builtin arg
consumeAllArgs ctx (VBuiltin builtin) (arg:args) = consumeAllArgs ctx (builtin arg) args

consumeAllArgs ctx (VCons str vals) args = return $ VCons str (vals++args) -}

{- -- TODO: think of a better name for this function
resolveLetDecls :: ValueCtx -> [LetDecl] -> IO [(String, Value)]
resolveLetDecls _ [] = return []
resolveLetDecls global ((E.ValDef pat rhs):letDecls) = do (exp, whereDecls) <- case rhs of
    E.UnguardedRHS exp whereDecls -> return (exp, whereDecls)
    E.GuardedRHS predExps whereDecls -> do 
      guardsCtx <- chooseGuard (global, []) predExps
      return (guardsCtx, whereDecls)
  whereCtx2 <- (case whereDecls of Just letDecls -> resolveLetDecls global letDecls
                                   Nothing -> return [])
  val <- eval (global, whereCtx2) exp
  letDeclsCtx <- resolveLetDecls global letDecls
  return $ case sequence $ doPatternMatching [pat] [val] [] of
    Just matched -> matched ++ letDeclsCtx
  -- TODO: use freeST error handling to tell the user that that pattern mathcing was not exhautive
    Nothing -> undefined

resolveLetDecls global ((E.FnDef var levelRhss):letDecls) = do
  letDeclsCtx <- resolveLetDecls global letDecls
  return $ (B.external var, VFun (map (\(levels, rhs) -> (map (\(B.ExpLevel pat) -> pat) (filterTypesFromLevels levels), rhs)) levelRhss)) : letDeclsCtx -}

-- TODO: create a resolveWhereDecls for more readable code (ValueCtx -> Maybe [LetDecl] -> [(String, Value)])

{- -- TODO: DELETE, refactor to use map and filter?
-- do i need to do Nothing : doPatternMatching pats args or can i just return Nothing
doPatternMatching :: [E.KindedPat] -> [Value] -> [String] -> [Maybe (String, Value)]
doPatternMatching [] [] _ = []
doPatternMatching pats [] _ = []
doPatternMatching [] args _ = []
doPatternMatching (pat:pats) (arg:args) labels = case pat of
  E.WildPat _ _ -> doPatternMatching pats args labels
  E.VarPat _ var -> Just (B.external var, arg) : doPatternMatching pats args labels
  E.DConsPat _ (B.Identifier _ patIden) consPats -> case arg of
    VCons iden consArgs -> if iden == patIden then doPatternMatching consPats consArgs labels ++ doPatternMatching pats args labels else Nothing : doPatternMatching pats args labels
    VChan chan -> if patIden == head labels then Just (B.external $ (\[E.VarPat _ var] -> var) consPats, VChan chan) : doPatternMatching pats args (tail labels) else Nothing : doPatternMatching pats args (tail labels)
  -- E.TuplePat _ tupPats -> doPatternMatching tupPats ((\(VTuple tupVals) -> tupVals) arg) ++ doPatternMatching pats args
  E.IntPat _ n -> if (\(VInt n) -> n) arg == n then doPatternMatching pats args labels else Nothing : doPatternMatching pats args labels
  E.FloatPat _ n -> if (\(VFloat n) -> n) arg == n then doPatternMatching pats args labels else Nothing : doPatternMatching pats args labels
  E.CharPat _ c -> if (\(VChar c) -> c) arg == c then doPatternMatching pats args labels else Nothing : doPatternMatching pats args labels
  -- E.StringPat _ str -> if (\(VString str) -> str) arg == str then doPatternMatching pats args else Nothing : doPatternMatching pats args
  E.AsPat _ var pat2 -> Just (B.external var, arg) : doPatternMatching [pat2] [arg] labels ++ doPatternMatching pats args labels -}

{- -- necessary to find out if there is an internal choice in the pattern matching to pre receive the label
getInternalChoiceChannels :: [E.KindedPat] -> [Value] -> [ChannelEnd]
getInternalChoiceChannels [] [] = []
getInternalChoiceChannels pats [] = []
getInternalChoiceChannels [] args = []
getInternalChoiceChannels (pat:pats) (arg:args) = case pat of
  E.DConsPat _ (B.Identifier _ patIden) patCons -> case arg of
    VCons _ consArgs -> getInternalChoiceChannels patCons consArgs ++ getInternalChoiceChannels pats args
    VChan chan -> chan : getInternalChoiceChannels pats args
  _  -> getInternalChoiceChannels pats args
-}