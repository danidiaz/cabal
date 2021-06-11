{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
module Distribution.Client.Instrumentation (
            Has(..),
            Instrumentation(..),
            Allocator(..),
            Instrumentable(..),
            InstrumentableFunction,
            InstrumentableContext(..),
            Fixtrumentable(..),
            Instrumentator(..),
            makeInstrumentator
        ) 
    where

import Prelude ()
import Distribution.Client.Compat.Prelude
import Distribution.Client.Utils.Inspectable
import GHC.Generics
import Data.Kind

-------------------------------------------------------------------------------
-- Has
-------------------------------------------------------------------------------

class Has r e where
    has :: e -> r

type FunctionName = String

newtype Instrumentation = Instrumentation { runInstrumentation :: forall r . Inspectable r => FunctionName -> [Value] -> IO r -> IO r }

instance Semigroup Instrumentation where
    Instrumentation outer <> Instrumentation inner = Instrumentation $ 
        \name args body -> outer name args (inner name args body)

instance Monoid Instrumentation where 
    mempty = Instrumentation $ \_ _ action -> action
    mappend = (<>)

newtype Allocator i  = Allocator { 
        withAllocated :: forall r . (i -> IO r) -> IO r
    }

instance Semigroup i => Semigroup (Allocator i) where
    Allocator outer <> Allocator inner = 
        Allocator $ \cont -> 
            outer $ \outerAllocated ->
                inner $ \innerAllocated ->
                    cont (outerAllocated <> innerAllocated)

instance Monoid i => Monoid (Allocator i) where
    mempty = Allocator $ \cont -> cont mempty


class InstrumentableFunction curried where
    instrumentFunction_ :: Instrumentation -> FunctionName -> [Value] -> curried -> curried

instance Inspectable r => InstrumentableFunction (IO r) where
    instrumentFunction_ instrumentation fname args body = 
        runInstrumentation instrumentation fname args body

instance (Inspectable a, InstrumentableFunction curried) 
    => InstrumentableFunction (a -> curried) where
    instrumentFunction_ instrumentation fname args f = \currentArg ->
        instrumentFunction_ instrumentation fname (toInspectionJSON currentArg : args) (f currentArg)

class Instrumentable bean where
    type Function bean :: Type
    type Function bean = GFunction (Rep bean)
    instrument :: Instrumentation -> bean -> bean
    default instrument :: (Generic bean, GInstrumentable (Rep bean)) => Instrumentation -> bean -> bean
    instrument instrumentation bean = to (gInstrument instrumentation (from bean))

--
-- Generic instance for newtype wrappers

class GInstrumentable g where
    type GFunction g :: Type
    gInstrument :: Instrumentation -> g x -> g x

instance (Selector metaSel, InstrumentableFunction f)
    => GInstrumentable (D1 metaData (C1 metaCons (S1 metaSel (Rec0 f)))) where
    type GFunction (D1 metaData (C1 metaCons (S1 metaSel (Rec0 f)))) = f
    gInstrument instrumentation (M1 (M1 sel@(M1 (K1 f)))) = 
        let f' = instrumentFunction_ instrumentation (selName sel) [] f
         in M1 (M1 (M1 (K1 f')))

--
-- Instrument whole composition contexts where each field is a newtype bean
--
-- These contexts must be still open.
--
-- TODO: make it work with open contexts.
class InstrumentableContext cc where
    instrumentContext :: Instrumentation -> cc -> cc
    default instrumentContext :: (Generic cc, GInstrumentableContext (Rep cc)) 
        => Instrumentation -> cc -> cc
    instrumentContext instrumentation cc = to (gInstrumentContext instrumentation (from cc))  

class GInstrumentableContext g where
    gInstrumentContext :: Instrumentation -> g x -> g x

instance GInstrumentableContext fields 
    => GInstrumentableContext (D1 metaData (C1 metaCons fields)) where
    gInstrumentContext instrumentation (M1 (M1 fields)) = 
        M1 (M1 (gInstrumentContext instrumentation fields))

instance Instrumentable bean => GInstrumentableContext (S1 metaSel (Rec0 bean)) where
     gInstrumentContext instrumentation (M1 (K1 bean)) = 
        let bean' = instrument instrumentation bean
         in M1 (K1 bean')

instance (GInstrumentableContext left, GInstrumentableContext right) 
    => GInstrumentableContext (left :*: right) where
     gInstrumentContext instrumentation (left :*: right) = 
        let left' = gInstrumentContext instrumentation left
            right' = gInstrumentContext instrumentation right
         in left' :*: right'

--
--
-- Instrument whole composition contexts where each field is a newtype bean
--
-- These contexts must be still open.
--
class Fixtrumentable (cc_ :: (Type -> Type) -> Type) where
    fixtrument 
        :: Instrumentation -> Open cc_ -> Closed cc_
    default fixtrument 
        :: ( Generic (Open cc_)
           , Generic (Closed cc_)
           , GFixtrumentable (Closed cc_)
                             (Rep (Open cc_))
                             (Rep (Closed cc_))
           )
       => Instrumentation -> Open cc_ -> Closed cc_
    fixtrument instrumentation cc_ =
        let result = to (gFixtrument instrumentation result (from cc_))  
         in result

type Closed cc_ = cc_ Identity

type Open cc_ = cc_ ((->) (Closed cc_))

class GFixtrumentable final g g' where
    gFixtrument :: Instrumentation -> final -> g x -> g' x

instance GFixtrumentable final fields fields'
    => GFixtrumentable final (D1 metaData (C1 metaCons fields)) 
                             (D1 metaData (C1 metaCons fields')) where
    gFixtrument instrumentation final (M1 (M1 fields)) = 
        M1 (M1 (gFixtrument instrumentation final fields))

instance Instrumentable bean 
    => GFixtrumentable final (S1 metaSel (Rec0 (final -> bean))) 
                             (S1 metaSel (Rec0 (Identity bean))) where
     gFixtrument instrumentation final (M1 (K1 beanf)) =
        let bean' = instrument instrumentation . beanf $ final
         in M1 (K1 (Identity bean'))

instance (GFixtrumentable final left left',
          GFixtrumentable final right right') 
        => GFixtrumentable final (left :*: right) (left' :*: right') where
     gFixtrument instrumentation final (left :*: right) = 
        let left' = gFixtrument instrumentation final left
            right' = gFixtrument instrumentation final right
         in left' :*: right'


--
--
newtype Instrumentator = Instrumentator { instrumentFunction :: forall curried . InstrumentableFunction curried => FunctionName -> curried -> curried }

makeInstrumentator :: Instrumentator
makeInstrumentator = Instrumentator $ \_ -> id

instance Instrumentable Instrumentator where 
    -- This is a bit of a special case. We can't return the inner function 
    -- because it's polymorphic. We simply return the type itself.
    type Function Instrumentator = Instrumentator
    instrument instrumentation (Instrumentator endo) =
        Instrumentator $ \fname f -> instrumentFunction_ instrumentation fname [] (endo fname f)

