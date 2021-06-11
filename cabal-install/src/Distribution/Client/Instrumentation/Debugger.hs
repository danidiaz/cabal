{-# LANGUAGE ViewPatterns #-}
module Distribution.Client.Instrumentation.Debugger (debuggerAllocator) where

-- import Prelude (read)
import Distribution.Client.Compat.Prelude

import Distribution.Client.Instrumentation
import Data.IORef
import qualified Data.Set as Set

data DebuggerState = DebuggerState {
        tracing :: Tracing
    ,   breakAt :: BreakAt
    }

data Tracing =
      TracingOn
    | TracingOff

parseTracing :: String -> Tracing
parseTracing entered = case entered of
    "on" -> TracingOn
    "off" -> TracingOff

data BreakAt = None
             | TheseFunctions (Set FunctionName)

parseBreakAt :: [String] -> BreakAt
parseBreakAt entered = case entered of
    "none" : [] -> None
    funcNames -> TheseFunctions (Set.fromList funcNames)

data DebuggerCommand = 
      DCommandTrace Tracing
    | DCommandContinue
    | DCommandArgs
    | DCommandRet
    | DCommandBreakAt BreakAt

parseDeguggerCommand :: String -> Maybe DebuggerCommand
parseDeguggerCommand entered = case words entered of
    "trace" : [parseTracing -> shoudTrace] -> Just $ DCommandTrace shoudTrace
    "continue" : []-> Just $ DCommandContinue
    "args" : [] -> Just $ DCommandArgs
    "ret" : [] -> Just $ DCommandRet
    "break" : (parseBreakAt -> functions) -> Just $ DCommandBreakAt functions
    _ -> Nothing


debuggerAllocator :: Allocator Instrumentation
debuggerAllocator = Allocator $ \cont -> do 
    _ <- newIORef ()
    let debugger = Instrumentation $ \name args body -> do
            putStrLn $ "(debugger) entered " ++ name
            r <- body
            putStrLn $ "(debugger) exited " ++ name
            pure r
     in cont debugger
