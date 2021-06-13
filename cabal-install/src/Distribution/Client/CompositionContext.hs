{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}

-- | This module collects from all parts of the codebase functions that
-- we wish to instrument, and puts them into a big record.
--
-- Then it applies instumentations (for example, generic logging) to all the
-- functions in the record.  
--
-- Finally, it wires all the functions together using knot-tying,
-- to that functions that depend on other functions use the instrumented
-- versions of their dependencies.
--
-- So, what if YOU want to instrument a new function?
--
-- * First you must intrument the top-level functions that lie above it on the 
-- call chain.
--
-- * Give instances of 'Inspectable' to the arguments and return type
-- of your function.
--
-- * Then you must wrap the function in a newtype like 'BuildAction' or 'TestAction'
-- and also provide the associated make- functions like 'makeBuildAction' or
-- 'makeTestAction'. The make- functions will take an additional argument,
-- an abstract composition context usually called @cc@. They can require
-- specific dependencies from the context using the 'Has' typeclass.
--
-- * Import the newtype and the make- function in this module. Add them
-- to the composition context record like the others. 
--
-- * Also remember to add the 'Has' instance.
--
-- * Finally, look for the invocations of your function throughout the 
-- codebase. You will need to add an extra @(has cc)@ parameter to them.
-- The @cc@ will be in scope because we instrumented the callers in
-- the first step. (Or, if we are in Main, we should use 
-- 'withCompositionContext'.)
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


-- | In the "closed" form of the composition context, each
-- field is directly available, we don't need to jump through 
-- any hoop to access them (other than the 'Identity' wrapper).
type CompositionContext = CompositionContext_ Identity

-- | This is a big record containing all the functions that we wish to
-- instrument.
--
-- The functions come wrapped in newtypes. The newtypes have instances of the
-- 'Instrumentable' typeclass.
--
-- In the record, the newtypes are wrapped in a functor @h@. Changing 
-- the functor lets us move from an "open" version of the newtype
-- to a "closed" version in which all the dependencies have been wired.
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
        -- This is a bit special. It's a bean that
        -- doesn't have logic of its own, but allows
        -- functions to instrument their own locally defined functions.
        _instrumentator :: h Instrumentator
    } 
    deriving Generic

-- | This means we can apply generic instrumentation to each field
-- of the composition context, and also "tie the knot" to
-- wire all the dependencies.
instance Fixtrumentable CompositionContext_

-- | A bunch of 'Has' instances that say how to find each particular
-- instrumentable function inside the composition context.
--
-- Clients use the 'Has' instance to find dependencies, and this way 
-- they avoid depending on the record as a whole.
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

-- | Here we build the "open" context.
--
-- In this version of the context, each field is a function 
-- that extracts its dependencies from the still-to-be-constructed
-- "closed" version of the context.
--
-- Instrumentation can only be applied before "closing" the context.
open :: CompositionContext_ ((->) (Self CompositionContext))
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
-- The instrumentations that we wish to apply are selected
-- using an environment variable.
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
-- This is the only function exported from this module.
--
-- Usually, there's no need to tweak it when adding new instrumentations.
withCompositionContext :: (Self CompositionContext -> IO a) -> IO a
withCompositionContext cont = do 
     allocators <- selectedInstrumentations
     withAllocated (fold allocators) $ 
        \instrumentation -> 
            let cc :: CompositionContext    
                cc = fixtrument instrumentation open
             in cont (selfie cc) -- there's also a selfie in Instrumentable... redundant?

