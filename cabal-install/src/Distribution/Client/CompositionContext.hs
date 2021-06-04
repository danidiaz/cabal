{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveAnyClass #-}
module Distribution.Client.CompositionContext (
        CompositionContext,
        withCompositionContext
    ) where

import           Prelude ()
import           Data.Coerce
import           Data.Foldable (fold)
import           Data.Functor.Identity
import           Distribution.Client.Compat.Prelude
import           Distribution.Client.Instrumentation
import           Distribution.Client.Instrumentation.Debugger
import           Distribution.Client.CmdBuild (BuildAction(..), makeBuildAction)
import qualified Data.Map as M
import qualified Data.Set as S
import           System.Environment

type CompositionContext = CompositionContext_ Identity

data CompositionContext_ h = CompositionContext {
        _buildAction :: h BuildAction 
    } 
    deriving stock Generic
    deriving anyclass Fixtrumentable

instance Has BuildAction CompositionContext where
    has = coerce . _buildAction

open :: CompositionContext_ ((->) CompositionContext)
open = CompositionContext {
        _buildAction = makeBuildAction
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
        \instrumentation -> cont (fixtrument instrumentation open)

