{- |
Module      :  Interpreter.Exception
Copyright   :  © The FreeST Team
Maintainer  :  freest-lang@listas.ciencias.ulisboa.pt

Runtime exceptions raised by the interpreter.
-}
module Interpreter.Exception
  ( Exception(..)
  , raiseOnZap
  , clearBuffers
  , printException
  , reportThreadFailure
  ) where

import Control.Concurrent.STM ( atomically, readTQueue, writeTQueue, tryReadTQueue )
import Control.Exception qualified as E
import Data.Functor ( void )
import System.Exit ( ExitCode )
import System.IO ( BufferMode(BlockBuffering), hFlush, hPutStrLn, hSetBuffering, stderr )

import Compiler.Bug ( reportBug, internalError )
import Interpreter.Value ( Value(..) )
import Syntax.Base ( Span, Located(..), nullSpan )
import Syntax.Type.Kinded ( kindOf )
import Syntax.Kind ( Multiplicity(..), Kind(..) )
import UI.Error ( Source, header, snippet )

-- | A runtime error raised by the interpreter.
data Exception
  = NonExhaustivePatterns Span
  | BlockedIndefinitely Span String
  | UserError Span String
  | SomeException Span String

instance Located Exception where
  getSpan = \case
    NonExhaustivePatterns s -> s
    BlockedIndefinitely s _ -> s
    UserError s _ -> s
    SomeException s _ -> s
  setSpan s = \case
    NonExhaustivePatterns _ -> NonExhaustivePatterns s
    BlockedIndefinitely _ msg -> BlockedIndefinitely s msg
    UserError _ msg -> UserError s msg
    SomeException _ msg -> SomeException s msg

instance Show Exception where
  show e = show (getSpan e) ++ ": exception:\n" ++ message e

instance E.Exception Exception

-- | The message text of an exception (mirrors 'UI.Error.toMessage').
message :: Exception -> String
message = \case
  NonExhaustivePatterns _ -> "Non-exhaustive patterns"
  BlockedIndefinitely _ msg -> msg
  UserError _ msg -> msg
  SomeException _ msg -> msg

zap :: Value -> IO ()
zap = \case
  (VCons _ vals) -> mapM_ zap vals
  (VClosure vals _ _) -> mapM_ zap vals
  (VIO ioval) -> ioval >>= \val -> zap val
  c@(VChan (r, w, kindOf -> Proper _ Lin{} _)) -> do -- no effect on shared channels
    atomically (writeTQueue w VZap)
    buffer <- atomically (buffer r)
    mapM_ zap buffer
    where
      buffer r = do
        mval <- tryReadTQueue r
        case mval of
          Nothing -> pure []
          Just val -> (val :) <$> buffer r
  (VPack _ val) -> zap val
  _ -> pure ()

-- | Raise 'SomeException' with the given value context if the given value was VZap,
-- a special warning value sent by the 'cancel' built-in function. Otherwise, returns
-- the value.
raiseOnZap :: Value -> IO Value
raiseOnZap = \case
  VZap -> E.throwIO (SomeException nullSpan "Channel Cancelled")
  v -> pure v

-- | Clear the buffers of all linear sessions in the given values, including the buffers
-- of linear sessions inside another buffer. No effect on any other values.
clearBuffers :: Traversable t => t Value -> IO ()
clearBuffers = mapM_ zap

makeException :: Located a => Source -> a -> String -> String
makeException src (getSpan -> s) msg =
  header "exception" s ++ "\n" ++ msg ++ "\n" ++ snippet src s False

-- | Report a failure at the top of a forked thread. Blocking indefinitely is the
-- deadlock detector's view of a program winding down, not a diagnostic of its own.
reportThreadFailure :: Maybe FilePath -> Source -> E.SomeException -> IO ()
reportThreadFailure path src e = case E.fromException e of
  Just (BlockedIndefinitely _ _) -> pure ()
  Just exc                       -> printException src exc
  Nothing
    | Just (_ :: ExitCode)         <- E.fromException e -> pure ()
    | Just (_ :: E.AsyncException) <- E.fromException e -> pure ()
    | otherwise                                         -> reportBug path e

-- | Report a runtime exception against the source, in the same format as a
-- compile-time error (span header, message and source snippet).
printException :: Source -> Exception -> IO ()
printException src e = reportOn stderr (makeException src e (message e))
  where 
  reportOn h report = do
    hSetBuffering h (BlockBuffering Nothing)
    hPutStrLn h report
    hFlush h
