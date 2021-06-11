{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}
module Distribution.Client.CompositionContext (
        withCompositionContext
    ) where

import           Prelude ()
import           Data.Foldable (fold)
import           Data.Functor.Identity
import           Distribution.Client.Compat.Prelude
import           Distribution.Client.Instrumentation
import           Distribution.Client.Instrumentation.Debugger
import           Distribution.Client.CmdBuild (BuildAction(..), makeBuildAction)
import           Distribution.Client.CmdRepl (ReplAction(..), makeReplAction)
import           Distribution.Client.CmdHaddock (HaddockAction(..), makeHaddockAction)
import           Distribution.Client.CmdTest (TestAction(..), makeTestAction)
import           Distribution.Client.CmdListBin (ListbinAction(..), makeListbinAction)
import           Distribution.Client.CmdInstall (InstallAction(..), makeInstallAction)
import           Distribution.Client.CmdExec (ExecAction(..), makeExecAction)
import           Distribution.Client.CmdBench (BenchAction(..), makeBenchAction)
import           Distribution.Client.CmdRun (
                        RunAction(..), 
                        makeRunAction,
                        HandleShebangAction(..), 
                        makeHandleShebangAction)
import           Distribution.Client.CmdFreeze (FreezeAction(..), makeFreezeAction)
import           Distribution.Client.ProjectOrchestration (
                    RunProjectPreBuildPhase(..),
                    makeRunProjectPreBuildPhase,
                    RunProjectBuildPhase(..),
                    makeRunProjectBuildPhase,
                    RunProjectPostBuildPhase(..),
                    makeRunProjectPostBuildPhase,
                 )
import           Distribution.Client.ProjectPlanning (
                    RebuildInstallPlan,
                    makeRebuildInstallPlan
                 )
import qualified Data.Map as M
import qualified Data.Set as S
import           System.Environment

type CompositionContext = CompositionContext_ Identity

data CompositionContext_ h = CompositionContext {
        _buildAction :: h BuildAction,
        _replAction :: h ReplAction,
        _haddockAction :: h HaddockAction,
        _testAction :: h TestAction,
        _listbinAction :: h ListbinAction,
        _installAction :: h InstallAction,
        _execAction :: h ExecAction,
        _benchAction :: h BenchAction,
        _runAction :: h RunAction,
        _handleShebangAction :: h HandleShebangAction,
        _freezeAction :: h FreezeAction,
        _runProjectPreBuildPhase :: h RunProjectPreBuildPhase,
        _runProjectBuildPhase :: h RunProjectBuildPhase,
        _runProjectPostBuildPhase :: h RunProjectPostBuildPhase,
        _rebuildInstallPlan :: h RebuildInstallPlan,
        _instrumentator :: h Instrumentator
    } 
    deriving Generic

instance Fixtrumentable CompositionContext_

instance Has BuildAction CompositionContext where
    has = runIdentity . _buildAction

instance Has ReplAction CompositionContext where
    has = runIdentity . _replAction

instance Has HaddockAction CompositionContext where
    has = runIdentity . _haddockAction

instance Has TestAction CompositionContext where
    has = runIdentity . _testAction

instance Has ListbinAction CompositionContext where
    has = runIdentity . _listbinAction

instance Has InstallAction CompositionContext where
    has = runIdentity . _installAction

instance Has ExecAction CompositionContext where
    has = runIdentity . _execAction

instance Has BenchAction CompositionContext where
    has = runIdentity . _benchAction

instance Has RunAction CompositionContext where
    has = runIdentity . _runAction

instance Has HandleShebangAction CompositionContext where
    has = runIdentity . _handleShebangAction

instance Has FreezeAction CompositionContext where
    has = runIdentity . _freezeAction

instance Has RunProjectPreBuildPhase CompositionContext where
    has = runIdentity . _runProjectPreBuildPhase
    
instance Has RunProjectBuildPhase CompositionContext where
    has = runIdentity . _runProjectBuildPhase

instance Has RunProjectPostBuildPhase CompositionContext where
    has = runIdentity . _runProjectPostBuildPhase

instance Has RebuildInstallPlan CompositionContext where
    has = runIdentity . _rebuildInstallPlan


instance Has Instrumentator CompositionContext where
    has = runIdentity . _instrumentator

open :: CompositionContext_ ((->) CompositionContext)
open = CompositionContext {
        _buildAction = makeBuildAction,
        _replAction = makeReplAction,
        _haddockAction = makeHaddockAction,
        _testAction = makeTestAction,
        _listbinAction = makeListbinAction,
        _installAction = makeInstallAction,
        _execAction = makeExecAction,
        _benchAction = makeBenchAction,
        _runAction = makeRunAction,
        _handleShebangAction = makeHandleShebangAction,
        _freezeAction = makeFreezeAction,
        _runProjectPreBuildPhase = makeRunProjectPreBuildPhase,
        _runProjectBuildPhase = makeRunProjectBuildPhase,
        _runProjectPostBuildPhase = makeRunProjectPostBuildPhase,
        _rebuildInstallPlan = makeRebuildInstallPlan,
        _instrumentator = \_ -> makeInstrumentator
}

-- 
-- Tweak here only to add or remove available instrumentations
--
type InstrumentationName = String

selectedInstrumentations :: IO (Map InstrumentationName (Allocator Instrumentation))
selectedInstrumentations = do
    val <- lookupEnv "CABAL_INSTALL_INSTRUMENTATIONS"
    return $ case S.fromList . words <$> val of
        Nothing -> M.empty
        Just selected -> M.restrictKeys availableInstrumentations selected


availableInstrumentations :: Map InstrumentationName (Allocator Instrumentation)
availableInstrumentations = M.fromList [
        ("debugger", debuggerAllocator)
    ]


--
-- Usually no need to tweak here.
--
withCompositionContext :: (CompositionContext -> IO a) -> IO a
withCompositionContext cont = do 
     allocators <- selectedInstrumentations
     withAllocated (fold allocators) $ 
        \instrumentation -> 
            let closed :: CompositionContext    
                closed = fixtrument instrumentation open
             in cont closed

