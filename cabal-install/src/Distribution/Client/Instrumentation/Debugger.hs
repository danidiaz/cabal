{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NamedFieldPuns #-}
module Distribution.Client.Instrumentation.Debugger (debuggerAllocator) where

-- import Prelude (read)
import Distribution.Client.Compat.Prelude

import Distribution.Client.Instrumentation
import Data.IORef
import qualified Data.Set as Set
import qualified Data.Map as Map
import Distribution.Client.Utils.Inspectable
import System.IO

data DebuggerState = DebuggerState {
        tracing :: Tracing
    ,   breakAt :: BreakAt
    }

data Tracing =
      TracingOn
    | TracingOff

parseTracing :: [String] -> Tracing
parseTracing entered = case entered of
    ["on"] -> TracingOn
    ["off"] -> TracingOff
    _ -> error "unknown trace mode"

data BreakAt = 
               BreakAtAll
             | BreakAtTheseFunctions (Set FunctionName)

parseBreakAt :: [String] -> BreakAt
parseBreakAt entered = case entered of
    "all" : [] -> BreakAtAll
    "none" : [] -> BreakAtTheseFunctions Set.empty 
    funcNames -> BreakAtTheseFunctions (Set.fromList funcNames)

data DebuggerCommand = 
      DCommandContinue
    | DCommandTrace Tracing
    | DCommandArgs (Maybe FilePath)
    | DCommandRes (Maybe FilePath)
    | DCommandBreakAt BreakAt

parseDebuggerCommand :: String -> Maybe DebuggerCommand
parseDebuggerCommand entered = case words entered of
    c : rest -> case Map.lookup c debuggerCommands of
        Just parser -> Just $ parser rest
        Nothing -> Nothing
    [] -> Nothing

debuggerAllocator :: Allocator Instrumentation
debuggerAllocator = Allocator $ \cont -> do 
    prompt "Welcome to the debugger!"
    prompt "Available commands are:"
    prompt "  t[race] on|off  Shows functions entered and exited."
    prompt "  c[continue] Continue the execution of the program."
    prompt "  a[rgs] [> <FILEPATH>] Show current args as JSON."
    prompt "  r[esult] [> <FILEPATH>] Show current result as JSON (only at function exit points)."
    prompt "  b[reak] none|all|<FUNCTION_NAME>* Stop when entering/exiting the given functions."
    dstate0 <- askUser [] Nothing (DebuggerState TracingOff (BreakAtTheseFunctions Set.empty))
    ref <- newIORef dstate0
    let debugger = Instrumentation $ \name args body -> do
            do dstate@(DebuggerState {tracing,breakAt}) <- readIORef ref
               case tracing of
                 TracingOn -> prompt $ "Entered " ++ name ++ "."
                 TracingOff -> pure ()
               let stopped = do
                    prompt $ "Stopped at " ++ name ++ " start. "
                    prompt $ "There are " ++ show (length args) ++ " args."
                    askUser args Nothing dstate
               dstate' <- case breakAt of
                 BreakAtAll -> stopped
                 BreakAtTheseFunctions fs | Set.member name fs -> stopped
                 _ -> pure dstate
               writeIORef ref dstate'
            r <- body
            do dstate@(DebuggerState {tracing,breakAt}) <- readIORef ref
               case tracing of
                 TracingOn -> prompt $ "Exited " ++ name ++ "."
                 TracingOff -> pure ()
               let stopped = do
                    prompt $ "Stopped at " ++ name ++ " end."
                    askUser args (Just (toInspectionJSON r)) dstate
               dstate' <- case breakAt of
                 BreakAtAll -> stopped
                 BreakAtTheseFunctions fs | Set.member name fs -> stopped
                 _ -> pure dstate
               writeIORef ref dstate'
            pure r
     in cont debugger

parseFileRedirect :: [String] -> Maybe FilePath
parseFileRedirect entered = case reverse entered of
    filepath : ">" : _ -> Just filepath
    _ -> Nothing

type DebuggerCommandText = String
debuggerCommands :: Map DebuggerCommandText ([String] -> DebuggerCommand)
debuggerCommands = 
    let ctrace = DCommandTrace . parseTracing
        continue _ = DCommandContinue
        args = DCommandArgs . parseFileRedirect
        ret = DCommandRes . parseFileRedirect
        cbreak = DCommandBreakAt . parseBreakAt
     in Map.fromList [
              ("trace",ctrace) 
            , ("tr",ctrace) 
            , ("t",ctrace) 
            , ("continue",continue) 
            , ("cont",continue) 
            , ("c",continue) 
            , ("args",args) 
            , ("a",args) 
            , ("result",ret) 
            , ("res",ret) 
            , ("r",ret) 
            , ("break",cbreak) 
            , ("br",cbreak) 
            , ("b",cbreak) 
            ]

prompt :: String -> IO ()
prompt msg = putStrLn $ "(dbg) " ++ msg

askUser :: [Value] -> Maybe Value -> DebuggerState -> IO DebuggerState
askUser args mresult = go
  where
    go dstate = do 
      prompt $ "Enter command:"
      mc <- parseDebuggerCommand <$> getLine
      case mc of
        Nothing -> go dstate
        Just c -> case c of
            DCommandContinue -> pure dstate
            DCommandTrace tracing -> go (dstate { tracing })
            DCommandBreakAt breakAt -> go (dstate { breakAt })
            DCommandArgs mfilepath -> do
                case mfilepath of
                    Nothing -> putStrLn (encodeToString args) 
                    Just filepath -> withFile filepath WriteMode $ \h -> hPutStrLn h (encodeToString args) 
                go dstate
            DCommandRes mfilepath -> case mresult of
                Nothing -> do
                    prompt "Function not executed yet."
                    go dstate
                Just result -> do
                    case mfilepath of
                        Nothing -> putStrLn (encodeToString result) 
                        Just filepath -> withFile filepath WriteMode $ \h -> hPutStrLn h (encodeToString result) 
                    go dstate
