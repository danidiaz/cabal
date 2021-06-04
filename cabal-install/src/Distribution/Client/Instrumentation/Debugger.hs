module Distribution.Client.Instrumentation.Debugger (debuggerAllocator) where

import Distribution.Client.Instrumentation
import Data.IORef

debuggerAllocator :: Allocator Instrumentation
debuggerAllocator = Allocator $ \cont -> do 
    _ <- newIORef ()
    let debugger = Instrumentation $ \name args body -> do
            putStrLn $ "(debugger) entered " ++ name
            r <- body
            putStrLn $ "(debugger) exited " ++ name
            pure r
     in cont debugger
