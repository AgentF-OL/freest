{- |
Module      :  Interpreter.Builtin
Copyright   :  © The FreeST Team
Maintainer  :  freest-lang@listas.ciencias.ulisboa.pt

The builtin functions exposed to FreeST programs, together with the marshalling
helpers (between Haskell and FreeST values) and the channel operations they and
the evaluator rely on. Value rendering for output goes through 'unparse'.
-}
module Interpreter.Builtin
  ( builtins
  , asString
  , fstToHsBool
  , hsToFstString
  , chan
  , receive
  , receiveLabel
  , receiveLabel_
  , send
  ) where

import Control.Concurrent ( forkIO )
import Control.Concurrent.STM ( atomically, newTQueueIO, readTQueue, writeTQueue, isEmptyTChan, tryReadTQueue )
import Control.Exception (BlockedIndefinitelyOnMVar, BlockedIndefinitelyOnSTM, IOException, SomeException, bracket_, catch, finally, fromException, throwIO)
import Data.Char ( chr, ord )
import Data.Functor ( ($>), void, (<&>) )
import qualified Data.Map as Map
import Data.Maybe ( fromJust )
import GHC.Float ( Floating(log1mexp, log1p, expm1, log1pexp) )

import Interpreter.Exception ( Exception(..), raiseOnZap, clearBuffers )
import Interpreter.Value ( Value(..), ChannelEnd )
import Compiler.Bug ( internalError )
import Parser.Unparser ( unparse )
import Syntax.Base ( Identifier(..), nullSpan, Polarity(..) )
import qualified Syntax.Type.Kinded as T
import Syntax.Declarations ( KindedTypeDecls )
import Syntax.Kind ( Multiplicity(..), Kind(..), lt )
import Validation.Normalisation ( normalise )
import System.Environment ( getArgs, getEnvironment, getProgName, lookupEnv )
import System.Exit ( ExitCode(..) )
import System.IO ( BufferMode(NoBuffering), Handle, IOMode(..), hClose, hFlush, hGetBuffering, hGetChar, hGetLine, hIsEOF, hIsTerminalDevice, hPutStr, hSetBuffering, isEOF, openFile, stderr, stdin, stdout )

-- | Read a single character with 'stdin' in character-at-a-time mode.
--
-- A terminal collects a whole line before releasing it, so under the default
-- settings a character read only returns once the user has typed a newline, and
-- then yields the first character of that line. Selecting 'NoBuffering' takes
-- the terminal out of canonical mode, so the read returns as soon as the
-- character is available, which is what @getChar@ is meant to do.
--
-- The mode is restored once the character has been read, because canonical mode
-- is what provides the rest of the line discipline: with it disabled the
-- terminal stops treating @Ctrl-D@ as end of input and stops honouring erase, so
-- leaving it off for the whole program would break 'getLine' and 'isEOF'.
--
-- Only terminals are adjusted. A pipe or a file has no line discipline to
-- disable, so switching modes would not change what a read returns, and
-- changing the buffering of a handle mid-read risks discarding what it has
-- already buffered.
charMode :: IO a -> IO a
charMode act = hIsTerminalDevice stdin >>= \tty ->
  if not tty then act else do
    mode <- hGetBuffering stdin
    bracket_ (hSetBuffering stdin NoBuffering) (hSetBuffering stdin mode) act

-- | Convert Haskell's True and False into FreeST's value representation
hsToFstBool :: Bool -> Value
hsToFstBool True = VCons "True" []
hsToFstBool False = VCons "False" []

-- | Extract True and False from FreeST's value representation
fstToHsBool :: Value -> Bool
fstToHsBool (VCons "True" []) = True
fstToHsBool (VCons "False" []) = False

-- | Build a FreeST string value from a Haskell 'String'.
hsToFstString :: String -> Value
hsToFstString = foldr (\c acc -> VCons "(::)" [VChar c, acc]) (VCons "[]" [])

-- | Build a FreeST list, from a conversion for its elements.
hsToFstList :: (a -> Value) -> [a] -> Value
hsToFstList f = foldr (\x acc -> VCons "(::)" [f x, acc]) (VCons "[]" [])

hsToFstMaybe :: Maybe String -> Value
hsToFstMaybe = maybe (VCons "Nothing" []) (\v -> VCons "Just" [hsToFstString v])

hsToFstPair :: (String, String) -> Value
hsToFstPair (a, b) = VCons "(,)" [hsToFstString a, hsToFstString b]

-- | An exit code as GHC wants it: 0 alone reports success. A negative code
-- would signal the process rather than set a status.
exitCode :: Int -> ExitCode
exitCode 0 = ExitSuccess
exitCode n = ExitFailure (if r == 0 then 255 else r)
  where r = n `mod` 256

-- | Extract a Haskell 'String' from a FreeST string value.
fstToHsString :: Value -> String
fstToHsString = \case
  VCons "[]"   []              -> ""
  VCons "(::)" [VChar c, rest] -> c : fstToHsString rest
  v                            -> internalError ("not a string: " ++ show v)

-- | Write a FreeST string on a handle, flushing it so that output written
-- through different handles keeps its order.
putStrOn :: Handle -> Value -> Value
putStrOn h s = VIO $ VCons "()" [] <$ (hPutStr h (fstToHsString s) >> hFlush h)

-- | A FreeST string value as a Haskell 'String', if it is one.
asString :: Value -> Maybe String
asString = \case
  VCons "[]"   []              -> Just ""
  VCons "(::)" [VChar c, rest] -> (c :) <$> asString rest
  _                            -> Nothing

chan :: T.KindedType -> IO (ChannelEnd, ChannelEnd)
chan t = do
  c1 <- newTQueueIO
  c2 <- newTQueueIO
  return ((c1, c2, t), (c2, c1, T.AppDual nullSpan t))

handleBlockedRead :: IO a -> IO a
handleBlockedRead action =
  action `catch` \(e :: SomeException) ->
    case fromException e of
      Just (_ :: BlockedIndefinitelyOnMVar) ->
        throwIO (BlockedIndefinitely nullSpan "Thread blocked indefinitely while waiting to read from a channel endpoint")
      _ -> case fromException e of
        Just (_ :: BlockedIndefinitelyOnSTM) ->
          throwIO (BlockedIndefinitely nullSpan "Thread blocked indefinitely while waiting to read from a channel endpoint")
        Nothing -> throwIO e

receive :: KindedTypeDecls -> ChannelEnd -> IO (Value, ChannelEnd)
receive tdecls (r, w, t) = handleBlockedRead $ do
  v <- atomically (readTQueue r)
  raiseOnZap v
  return (v, (r, w, case normalise tdecls t of
      T.AppMessage _ Lin{} Neg _ -> T.Skip nullSpan
      T.AppSemi _ (T.AppMessage _ Lin{} Neg _) t' -> t'
    ))

receive_ :: KindedTypeDecls -> ChannelEnd -> IO (Value, ChannelEnd)
receive_ tdecls (r, w, t) = handleBlockedRead $ do
  v <- atomically (readTQueue r)
  return (v, (r, w, case normalise tdecls t of
      t'@(T.AppMessage _ Un{} Neg _) -> t'
      T.AppSemi _ t'@(T.AppMessage _ Un{} Neg _) _ -> t'
    ))

receiveLabel :: KindedTypeDecls -> ChannelEnd -> IO (String, ChannelEnd)
receiveLabel tdecls (r, w, t) = handleBlockedRead $ do
  v <- atomically (readTQueue r)
  raiseOnZap v
  let s = case v of VLabel s -> s
  return (s, (r, w, case normalise tdecls t of
      T.AppLinChoice _ Neg lts -> fromJust $ lookup (Identifier nullSpan s) lts
    ))

receiveLabel_ :: KindedTypeDecls -> ChannelEnd -> IO (String, ChannelEnd)
receiveLabel_ tdecls (r, w, t) = handleBlockedRead $ do
  VLabel s <- atomically (readTQueue r)
  return (s, (r, w, case normalise tdecls t of
      t'@(T.UnChoice _ Neg _) -> t'
      T.AppSemi _ t'@(T.UnChoice _ Neg _) _ -> t'
    ))

receiveType :: KindedTypeDecls -> ChannelEnd -> IO (Value, ChannelEnd)
receiveType tdecls (r, w, t) = handleBlockedRead $ do
  v <- atomically (readTQueue r)
  raiseOnZap v
  return (v, (r, w, case normalise tdecls t of
      T.AppQuantS _ Neg _ _ t' -> t'
    ))

send :: KindedTypeDecls -> Value -> ChannelEnd -> IO ChannelEnd
send tdecls v (r, w, t) = do
  atomically (writeTQueue w v)
  return (r, w, case normalise tdecls t of
      T.AppMessage _ Lin{} Pos _ -> T.Skip nullSpan
      T.AppSemi _ (T.AppMessage _ Lin{} Pos _) t' -> t'
    )

send_ :: KindedTypeDecls -> Value -> ChannelEnd -> IO ChannelEnd
send_ tdecls v (r, w, t) = do
  atomically (writeTQueue w v)
  return (r, w, case normalise tdecls t of
      t'@(T.AppMessage _ Un{} Pos _) -> t'
      T.AppSemi _ t'@(T.AppMessage _ Un{} Pos _) _ -> t'
    )

sendLabel :: KindedTypeDecls -> String -> ChannelEnd -> IO ChannelEnd
sendLabel tdecls s (r, w, t) = do
  atomically (writeTQueue w (VLabel s))
  return (r, w, case normalise tdecls t of
      T.AppLinChoice _ Pos its -> fromJust $ lookup (Identifier nullSpan s) its
    )

sendLabel_ :: KindedTypeDecls -> String -> ChannelEnd -> IO ChannelEnd
sendLabel_ tdecls s (r, w, t) = do
  atomically (writeTQueue w (VLabel s))
  return (r, w, case normalise tdecls t of
      t'@(T.UnChoice _ Pos _) -> t'
      T.AppSemi _ t'@(T.UnChoice _ Pos _) _ -> t'
    )

sendType :: KindedTypeDecls -> Value -> ChannelEnd -> IO ChannelEnd
sendType tdecls v (r, w, t) = do
  atomically (writeTQueue w v)
  return (r, w, case normalise tdecls t of
      T.AppQuantS _ Pos _ _ t' -> t'
    )

wait :: Value -> IO Value
wait (VChan (r, _, _)) = handleBlockedRead $ do
  v <- atomically (readTQueue r)
  raiseOnZap v
  return v

close :: Value -> IO Value
close (VChan (_, w, _)) = do
  atomically (writeTQueue w (VCons "()" []))
  return (VCons "()" [])

cancel :: Value -> IO Value -- no effect on shared channels
cancel v = clearBuffers [v] >> pure (VCons "()" [])

-- | Report a failed file operation as the program's error, not the compiler's.
asUserError :: IO a -> IO a
asUserError act = act `catch` \(e :: IOException) -> throwIO (UserError nullSpan (show e))

-- | Open a file and serve it as a session endpoint, so that the handle is
-- reachable only through the protocol and is closed when the client stops.
--
-- TODO: a client that fails before stopping leaves the server parked, and the
-- backstop below only runs once the collector notices. Cancelling the endpoints
-- of a failing thread would close the file at the failure instead.
openStream :: IOMode -> (Handle -> ChannelEnd -> IO ()) -> Value -> Value
openStream mode serve path = VIO $ do
  h <- asUserError (openFile (fstToHsString path) mode)
  (client, server) <- chan $ T.TName nullSpan (lt nullSpan) $ Identifier nullSpan $ case mode of
    ReadMode -> "InStream"
    WriteMode -> "OutStream"
    AppendMode -> "OutStream"
  _ <- forkIO (asUserError (serve h server) `finally` hClose h)
  return (VChan client)

-- TODO: unchecked against Dual InStream/OutStream, unlike the Prelude's own
-- servers. Once more resources need one, add an opaque handle type and write
-- these in FreeST instead.
readFileServer :: KindedTypeDecls -> Handle -> ChannelEnd -> IO ()
readFileServer tdecls h c0 = receiveLabel tdecls c0 >>= \(label, c) -> case label of
  "GetChar" -> hGetChar h >>= \x -> send tdecls (VChar x) c >>= readFileServer tdecls h
  "GetLine" -> hGetLine h >>= \x -> send tdecls (hsToFstString x) c >>= readFileServer tdecls h
  "IsEOF"   -> hIsEOF h >>= \x -> send tdecls (hsToFstBool x) c >>= readFileServer tdecls h
  "Stop"    -> hClose h >> void (close (VChan c))
  _         -> internalError ("readFileServer: unexpected label " ++ label)

writeFileServer :: KindedTypeDecls -> Handle -> ChannelEnd -> IO ()
writeFileServer tdecls h c0 = receiveLabel tdecls c0 >>= \(label, c) -> case label of
  "PutStr"   -> put tdecls c id
  "PutStrLn" -> put tdecls c (++ "\n")
  "Stop"     -> hClose h >> void (close (VChan c))
  _          -> internalError ("writeFileServer: unexpected label " ++ label)
  where
    put tdecls c f = receive tdecls c >>= \(v, c') -> hPutStr h (f (fstToHsString v)) >> writeFileServer tdecls h c'

builtins :: Map.Map String Value
builtins = Map.fromList
  [
  -- * Undefined
    ("undefined",     VBuiltin undefined)
  -- * Error
  , ("error",         VBuiltin (\_ -> VIO . throwIO . UserError nullSpan . fstToHsString))

  -- * Standard types, classes and related functions
  -- ** Basic datatypes
  -- *** Logical operators
  , ("(&&)",          VBuiltin (\_ x -> VBuiltin (\_ y -> hsToFstBool (fstToHsBool x && fstToHsBool y))))
  , ("(||)",          VBuiltin (\_ x -> VBuiltin (\_ y -> hsToFstBool (fstToHsBool x || fstToHsBool y))))
  -- *** Strings
  , ("ord",           VBuiltin (\_ (VChar c) -> VInt (ord c)))
  , ("chr",           VBuiltin (\_ (VInt x) -> VChar (chr x)))
  , ("(^^)",          VBuiltin (\_ s1 -> VBuiltin (\_ s2 -> hsToFstString (fstToHsString s1 ++ fstToHsString s2))))
  , ("show",          VBuiltin (\_ -> hsToFstString . unparse))
  -- ** Comparison
  , ("(<)",           VBuiltin (\_ (VInt x) -> VBuiltin (\_ (VInt y) -> hsToFstBool (x < y))))
  , ("(<=)",          VBuiltin (\_ (VInt x) -> VBuiltin (\_ (VInt y) -> hsToFstBool (x <= y))))
  , ("(==)",          VBuiltin (\_ (VInt x) -> VBuiltin (\_ (VInt y) -> hsToFstBool (x == y))))
  , ("(>=)",          VBuiltin (\_ (VInt x) -> VBuiltin (\_ (VInt y) -> hsToFstBool (x >= y))))
  , ("(>)",           VBuiltin (\_ (VInt x) -> VBuiltin (\_ (VInt y) -> hsToFstBool (x > y))))
  , ("(/=)",          VBuiltin (\_ (VInt x) -> VBuiltin (\_ (VInt y) -> hsToFstBool (x /= y))))
  , ("(>.)",          VBuiltin (\_ (VFloat x) -> VBuiltin (\_ (VFloat y) -> hsToFstBool (x > y))))
  , ("(<.)",          VBuiltin (\_ (VFloat x) -> VBuiltin (\_ (VFloat y) -> hsToFstBool (x < y))))
  , ("(>=.)",         VBuiltin (\_ (VFloat x) -> VBuiltin (\_ (VFloat y) -> hsToFstBool (x >= y))))
  , ("(<=.)",         VBuiltin (\_ (VFloat x) -> VBuiltin (\_ (VFloat y) -> hsToFstBool (x <= y))))
  -- ** Numeric functions
  -- *** Int
  , ("(+)",           VBuiltin (\_ (VInt x) -> VBuiltin (\_ (VInt y) -> VInt (x + y))))
  , ("(-)",           VBuiltin (\_ (VInt x) -> VBuiltin (\_ (VInt y) -> VInt (x - y))))
  , ("(*)",           VBuiltin (\_ (VInt x) -> VBuiltin (\_ (VInt y) -> VInt (x * y))))
  , ("(/)",           VBuiltin (\_ (VInt x) -> VBuiltin (\_ (VInt y) -> VInt (div x y))))
  , ("(^)",           VBuiltin (\_ (VInt x) -> VBuiltin (\_ (VInt y) -> VInt (x ^ y))))
  , ("subtract",      VBuiltin (\_ (VInt x) -> VBuiltin (\_ (VInt y) -> VInt (x - y))))
  , ("quot",          VBuiltin (\_ (VInt x) -> VBuiltin (\_ (VInt y) -> VInt (quot x y))))
  , ("rem",           VBuiltin (\_ (VInt x) -> VBuiltin (\_ (VInt y) -> VInt (rem x y))))
  , ("div",           VBuiltin (\_ (VInt x) -> VBuiltin (\_ (VInt y) -> VInt (div x y))))
  , ("mod",           VBuiltin (\_ (VInt x) -> VBuiltin (\_ (VInt y) -> VInt (mod x y))))
  , ("min",           VBuiltin (\_ (VInt x) -> VBuiltin (\_ (VInt y) -> VInt (min x y))))
  , ("max",           VBuiltin (\_ (VInt x) -> VBuiltin (\_ (VInt y) -> VInt (max x y))))
  , ("gcd",           VBuiltin (\_ (VInt x) -> VBuiltin (\_ (VInt y) -> VInt (gcd x y))))
  , ("lcm",           VBuiltin (\_ (VInt x) -> VBuiltin (\_ (VInt y) -> VInt (lcm x y))))
  , ("succ",          VBuiltin (\_ (VInt x) -> VInt (succ x)))
  , ("pred",          VBuiltin (\_ (VInt x) -> VInt (pred x)))
  , ("abs",           VBuiltin (\_ (VInt x) -> VInt (abs x)))
  , ("negate",        VBuiltin (\_ (VInt x) -> VInt (-x)))
  , ("even",          VBuiltin (\_ (VInt x) -> hsToFstBool (even x)))
  , ("odd",           VBuiltin (\_ (VInt x) -> hsToFstBool (odd x)))
  -- *** Float
  , ("(+.)",          VBuiltin (\_ (VFloat x) -> VBuiltin (\_ (VFloat y) -> VFloat (x + y))))
  , ("(-.)",          VBuiltin (\_ (VFloat x) -> VBuiltin (\_ (VFloat y) -> VFloat (x - y))))
  , ("(*.)",          VBuiltin (\_ (VFloat x) -> VBuiltin (\_ (VFloat y) -> VFloat (x * y))))
  , ("(/.)",          VBuiltin (\_ (VFloat x) -> VBuiltin (\_ (VFloat y) -> VFloat (x / y))))
  , ("(**)",          VBuiltin (\_ (VFloat x) -> VBuiltin (\_ (VFloat y) -> VFloat (x ** y))))
  , ("maxF",          VBuiltin (\_ (VFloat x) -> VBuiltin (\_ (VFloat y) -> VFloat (max x y))))
  , ("minF",          VBuiltin (\_ (VFloat x) -> VBuiltin (\_ (VFloat y) -> VFloat (min x y))))
  , ("logBase",       VBuiltin (\_ (VFloat x) -> VBuiltin (\_ (VFloat y) -> VFloat (logBase x y))))
  , ("absF",          VBuiltin (\_ (VFloat x) -> VFloat (abs x)))
  , ("negateF",       VBuiltin (\_ (VFloat x) -> VFloat (negate x)))
  , ("recip",         VBuiltin (\_ (VFloat x) -> VFloat (recip x)))
  , ("exp",           VBuiltin (\_ (VFloat x) -> VFloat (exp x)))
  , ("log",           VBuiltin (\_ (VFloat x) -> VFloat (log x)))
  , ("sqrt",          VBuiltin (\_ (VFloat x) -> VFloat (sqrt x)))
  , ("log1p",         VBuiltin (\_ (VFloat x) -> VFloat (log1p x)))
  , ("expm1",         VBuiltin (\_ (VFloat x) -> VFloat (expm1 x)))
  , ("log1pexp",      VBuiltin (\_ (VFloat x) -> VFloat (log1pexp x)))
  , ("log1mexp",      VBuiltin (\_ (VFloat x) -> VFloat (log1mexp x)))
  , ("sin",           VBuiltin (\_ (VFloat x) -> VFloat (sin x)))
  , ("cos",           VBuiltin (\_ (VFloat x) -> VFloat (cos x)))
  , ("tan",           VBuiltin (\_ (VFloat x) -> VFloat (tan x)))
  , ("asin",          VBuiltin (\_ (VFloat x) -> VFloat (asin x)))
  , ("acos",          VBuiltin (\_ (VFloat x) -> VFloat (acos x)))
  , ("atan",          VBuiltin (\_ (VFloat x) -> VFloat (atan x)))
  , ("sinh",          VBuiltin (\_ (VFloat x) -> VFloat (sinh x)))
  , ("cosh",          VBuiltin (\_ (VFloat x) -> VFloat (cosh x)))
  , ("tanh",          VBuiltin (\_ (VFloat x) -> VFloat (tanh x)))
  , ("truncate",      VBuiltin (\_ (VFloat x) -> VInt (truncate x)))
  , ("round",         VBuiltin (\_ (VFloat x) -> VInt (round x)))
  , ("ceiling",       VBuiltin (\_ (VFloat x) -> VInt (ceiling x)))
  , ("floor",         VBuiltin (\_ (VFloat x) -> VInt (floor x)))
  , ("pi",            VFloat pi)
  , ("fromInteger",   VBuiltin (\_ (VInt x) -> VFloat (fromInteger (toInteger x))))
  -- * Concurrency
  , ("fork",          VFork)
  , ("send",          VBuiltin (\tdecls val -> VBuiltin (\_ (VChan c) -> VIO $ VChan <$> send tdecls val c)))
  , ("receive",       VBuiltin (\tdecls (VChan c) -> VIO $ receive tdecls c >>= \(val, c) -> return $ VCons "(,)" [val, VChan c]))
  , ("wait",          VBuiltin (\_ chan -> VIO $ wait chan))
  , ("close",         VBuiltin (\_ -> VIO . close))
  , ("cancel",        VBuiltin (\_ -> VIO . cancel))
  , ("send_",         VBuiltin (\tdecls val -> VBuiltin (\_ chan@(VChan c) -> VIO $ chan <$ send_ tdecls val c)))
  , ("receive_",      VBuiltin (\tdecls (VChan c) -> VIO $ receive_ tdecls c >>= \(val, c) -> return val))
  -- * I/O
  -- ** Standard I/O
  -- *** stdin
  -- **** Internal stdin functions
  , ("internalGetChar",       VBuiltin (\_ -> const $ VIO $ VChar <$> charMode getChar))
  , ("internalGetLine",       VBuiltin (\_ -> const $ VIO $ hsToFstString <$> getLine))
  , ("internalIsEOF",         VBuiltin (\_ -> const $ VIO $ hsToFstBool <$> isEOF))
  -- getContents makes sense in a lazy setting; FreeST is eager.
  -- , ("internalGetContents",   VBuiltin (const $ VIO $ hsToFstString <$> getContents))
  -- *** stdout and stderr
  -- **** Internal output functions
  , ("internalPutStrOut",     VBuiltin (\_ -> putStrOn stdout))
  , ("internalPutStrErr",     VBuiltin (\_ -> putStrOn stderr))
  -- ** Files
  , ("openReadFile",          VBuiltin (openStream ReadMode   . readFileServer))
  , ("openWriteFile",         VBuiltin (openStream WriteMode  . writeFileServer))
  , ("openAppendFile",        VBuiltin (openStream AppendMode . writeFileServer))
  -- ** Command line
  , ("getArgs",               VBuiltin (\_ -> const $ VIO $ hsToFstList hsToFstString <$> getArgs))
  , ("getProgName",           VBuiltin (\_ -> const $ VIO $ hsToFstString <$> getProgName))
  -- ** Environment
  , ("lookupEnv",             VBuiltin (\_ s -> VIO $ hsToFstMaybe <$> lookupEnv (fstToHsString s)))
  , ("getEnvironment",        VBuiltin (\_ -> const $ VIO $ hsToFstList hsToFstPair <$> getEnvironment))
  -- ** Exiting
  , ("exitWith",              VBuiltin (\_ (VInt n) -> VIO $ throwIO $ exitCode n))

  -- * Other Expressions
  , ("select",        VBuiltin (\tdecls (VLabel label) -> VBuiltin (\_ (VChan c) -> VIO $ VChan <$> sendLabel tdecls label c)))
  , ("select_",       VBuiltin (\tdecls (VLabel label) -> VBuiltin (\_ chan@(VChan c) -> VIO $ chan <$ sendLabel_ tdecls label c)))
  , ("sendType",      VBuiltin (\tdecls (VChan c) -> VIO $ VChan <$> sendType tdecls (VCons "()" []) c))
  -- The received type is erased at runtime; the returned 'VPack' exists so
  -- that an explicit @let (\@a, c) = receiveType c@ pattern can decompose it.
  , ("receiveType",   VBuiltin (\tdecls (VChan c) -> VIO $ receiveType tdecls c >>= \(_, c) -> return $ VPack [] (VChan c)))
  ]
