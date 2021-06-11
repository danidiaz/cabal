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

data BreakAt = BreakAtNone
             | BreakAtAll
             | BreakAtTheseFunctions (Set FunctionName)

parseBreakAt :: [String] -> BreakAt
parseBreakAt entered = case entered of
    "none" : [] -> BreakAtNone
    "all" : [] -> BreakAtAll
    funcNames -> BreakAtTheseFunctions (Set.fromList funcNames)

data DebuggerCommand = 
      DCommandContinue
    | DCommandTrace Tracing
    | DCommandArgs
    | DCommandRet
    | DCommandBreakAt BreakAt

parseDebuggerCommand :: String -> Maybe DebuggerCommand
parseDebuggerCommand entered = case words entered of
    c : rest -> case Map.lookup c debuggerCommands of
        Just parser -> Just $ parser rest
        Nothing -> Nothing
    [] -> Nothing

debuggerAllocator :: Allocator Instrumentation
debuggerAllocator = Allocator $ \cont -> do 
    putStrLn "(dbg) Welcome to the debugger!"
    ref <- newIORef $ DebuggerState TracingOff BreakAtNone
    let debugger = Instrumentation $ \name args body -> do
            do dstate@(DebuggerState {tracing,breakAt}) <- readIORef ref
               case tracing of
                 TracingOn -> prompt $ "entered " ++ name
                 TracingOff -> pure ()
               let stopped = do
                    prompt $ "stopped at " ++ name ++ " start. "
                    prompt $ "there are " ++ show (length args) ++ "args."
                    askUser args Nothing dstate
               dstate' <- case breakAt of
                 BreakAtAll -> stopped
                 BreakAtTheseFunctions fs | Set.member name fs -> stopped
                 _ -> pure dstate
               writeIORef ref dstate'
            r <- body
            do dstate@(DebuggerState {tracing,breakAt}) <- readIORef ref
               case tracing of
                 TracingOn -> prompt $ "exited " ++ name
                 TracingOff -> pure ()
               let stopped = do
                    prompt $ "stopped at " ++ name ++ " end."
                    prompt $ "enter command:"
                    askUser args (Just (toInspectionJSON r)) dstate
               dstate' <- case breakAt of
                 BreakAtAll -> stopped
                 BreakAtTheseFunctions fs | Set.member name fs -> stopped
                 _ -> pure dstate
               writeIORef ref dstate'
            pure r
     in cont debugger

type DebuggerCommandText = String
debuggerCommands :: Map DebuggerCommandText ([String] -> DebuggerCommand)
debuggerCommands = 
    let ctrace = DCommandTrace . parseTracing
        continue _ = DCommandContinue
        args _ = DCommandArgs
        ret _ = DCommandRet
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
            , ("return",ret) 
            , ("ret",ret) 
            , ("r",ret) 
            , ("break",cbreak) 
            , ("br",cbreak) 
            , ("b",cbreak) 
            ]

prompt :: String -> IO ()
prompt msg = putStrLn $ "(debugger) " ++ msg

askUser :: [Value] -> Maybe Value -> DebuggerState -> IO DebuggerState
askUser args mresult = go
  where
    go dstate = do 
      prompt $ "enter command:"
      mc <- parseDebuggerCommand <$> getLine
      case mc of
        Nothing -> go dstate
        Just c -> case c of
            DCommandContinue -> pure dstate
            DCommandTrace tracing -> go (dstate { tracing })
            DCommandBreakAt breakAt -> go (dstate { breakAt })
            DCommandArgs -> do 
                putStrLn (encodeToString args) 
                go dstate
            DCommandRet -> case mresult of
                Nothing -> do
                    prompt "function not executed yet."
                    go dstate
                Just result -> do
                    putStrLn (encodeToString result) 
                    go dstate

