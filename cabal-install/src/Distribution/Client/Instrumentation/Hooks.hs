{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NamedFieldPuns #-}
module Distribution.Client.Instrumentation.Hooks (hooksAllocator, Hooks(..)) where

-- | This module defines instrumentations for phases of the build lifecycle.
--
-- The instrumentations are not generic and they target specific phases, 
-- so they don't have 'Instrumentable' instances.
import Distribution.Client.Compat.Prelude

import Distribution.Client.Instrumentation
import System.FilePath
import System.Directory
import Data.Monoid

import Distribution.Simple.Flag
import Distribution.Client.ProjectConfig
import Distribution.Client.ProjectOrchestration
import Distribution.Client.Compat.Process
import Distribution.Simple.Utils (dieNoVerbosity)

data Hooks = Hooks {
         runProjectPreBuildPhaseBefore :: Endo RunProjectPreBuildPhase
    }

makeRunProjectPreBuildPhaseHookBefore:: FilePath -> Endo RunProjectPreBuildPhase
makeRunProjectPreBuildPhaseHookBefore script = Endo $
    \(RunProjectPreBuildPhase {runProjectPreBuildPhase}) -> RunProjectPreBuildPhase $ 
    \verbosity projectBaseContext selectPlanSubset -> do
        let hcPath = projectConfigHcPath (projectConfigShared (projectConfig projectBaseContext))
        case hcPath of
            NoFlag -> pure () 
            Flag compilerPath -> do
                (ec,scriptStdout,scriptStderr) <- readProcessWithExitCode script [compilerPath] ""
                -- It would be better to show stdout and stderr as the script
                -- is being executed, not everything together at the end.
                putStrLn scriptStdout
                putStrLn scriptStderr
                case ec of
                   ExitSuccess -> pure ()
                   ExitFailure _ -> dieNoVerbosity $ "Script hook " ++ script ++ "returned non-success exit code." 
        runProjectPreBuildPhase verbosity projectBaseContext selectPlanSubset

hooksAllocator :: Allocator Hooks
hooksAllocator = Allocator $ \cont -> do
    let hooksFolder :: FilePath
        hooksFolder = "cabal.hooks"
    runProjectPreBuildPhaseBefore <- do
        let runProjectPreBuildPhaseBeforeScript = hooksFolder </> "before_pre_build.sh"
        exists <- doesFileExist runProjectPreBuildPhaseBeforeScript
        if exists 
            then pure $ makeRunProjectPreBuildPhaseHookBefore runProjectPreBuildPhaseBeforeScript
            else mempty
    cont (Hooks { runProjectPreBuildPhaseBefore })
                    

