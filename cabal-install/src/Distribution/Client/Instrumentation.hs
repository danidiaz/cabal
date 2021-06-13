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

-- | This module exposes functionality for generically instrumenting functions
-- from our program logic.
--
-- There are some restrictions though:
-- * Only monomophic functions can't be instrumented.
-- * Only functions that end in an IO action can be instrumented.
module Distribution.Client.Instrumentation (
            Has(..),
            Self(..),
            selfie,
            Instrumentation(..),
            FunctionName,
            Allocator(..),
            Instrumentable(..),
            InstrumentableFunction,
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

-- | A value of type @r@ can be extracted from the environment/context @e@.
--
-- The way this typeclass is typically used, @r@ is some newtype that wraps
-- a function, and it has some selector that extracts the inner function.
--
-- Then we can write something like
--
-- > theSelector (has e)
--
-- The selector determines its input newtype, telling the compiler what @r@
-- we are searching for.
class Has r e where
    has :: e -> r

newtype Self cc = Self {self :: forall r. Has r cc => r}

selfie :: cc -> Self cc
selfie cc = Self (has cc)

type FunctionName = String

-- | This newtype wraps a transformation on the monadic action that sits
-- at the tip of some instrumentable function.
--
-- The transformation also has access to the instrumented function's name,
-- and to a generic representation of its arguments.
--
-- Because the return type @r@ is inspectable as well, we can also
-- debug the results of the function.
newtype Instrumentation = Instrumentation { runInstrumentation :: forall r . Inspectable r => FunctionName -> [Value] -> IO r -> IO r }

-- | Instrumentations are composed in a leftmost-is-outermost way.
instance Semigroup Instrumentation where
    Instrumentation outer <> Instrumentation inner = Instrumentation $ 
        \name args body -> outer name args (inner name args body)

instance Monoid Instrumentation where 
    mempty = Instrumentation $ \_ _ action -> action
    mappend = (<>)

-- | Just a convenient newtype over continuations.
--
-- This is useful when an 'Instrumentation' has to allocate some resource at
-- the beginning of the program's execution.
newtype Allocator i  = Allocator { 
        withAllocated :: forall r . (i -> IO r) -> IO r
    }

-- | First we perform the required allocations, then we 
-- combine the results.
instance Semigroup i => Semigroup (Allocator i) where
    Allocator outer <> Allocator inner = 
        Allocator $ \cont -> 
            outer $ \outerAllocated ->
                inner $ \innerAllocated ->
                    cont (outerAllocated <> innerAllocated)

instance Monoid i => Monoid (Allocator i) where
    mempty = Allocator $ \cont -> cont mempty

-- | Auxiliary typeclass of functions that can be instrumented.
--
class InstrumentableFunction curried where
    -- The function name is required as a parameter because "bare" functions don't
    -- have access to their own names.
    --
    -- When the @curried@ type is itself the right-hand side of a function
    -- @a -> b -> ... -> curried@, the previous arguments are also
    -- received as parameters.
    instrumentFunction_ :: Instrumentation -> FunctionName -> [Value] -> curried -> curried

-- | We can only instrument functions that end in a plain IO action.
instance Inspectable r => InstrumentableFunction (IO r) where
    instrumentFunction_ instrumentation fname args body = 
        runInstrumentation instrumentation fname args body

-- | The inductive case. 
instance (Inspectable a, InstrumentableFunction curried) 
    => InstrumentableFunction (a -> curried) where
    -- We instrument the return value, passing the generic representation
    -- of the argument @a@.
    instrumentFunction_ instrumentation fname args f = \currentArg ->
        instrumentFunction_ instrumentation fname (toInspectionJSON currentArg : args) (f currentArg)

-- | A typeclass for newtypes that wrap "business logic" functions that
-- we wish to instrument.
--
-- Why wrap business logic functions in a newtype?
-- * The newtype helps find the function in the composition context, by means of the
--   'Has' typeclass.
-- * The name of the newtype accessor function can be inspected using 
--   generics and server as the natural name that identifies the function
--   to the instrumentations.
class Instrumentable bean where
    -- This associated type synonym is intended to return the type of the
    -- wrapped function. This helps to avoid repeating the whole function
    -- signature when writing helper functions.
    type Function bean :: Type
    type Function bean = GFunction (Rep bean)
    -- This is the principal function exported by this module. 
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

-- The typeclass of product-like datatypes where each field is an 
-- 'Instrumentable' bean, and where the datatype can be either "closed"
-- or "open" according to the functor parameter.
--
-- Typically, only one datatype in your whole application needs to
-- have an instance of this typeclass: the CompositionContext.
class Fixtrumentable (cc_ :: (Type -> Type) -> Type) where
    -- Apply some instrumentation to all the components of an "open" context,
    -- and then wire the components together and resolve their mutual
    -- dependencies, resulting in a "closed" context.
    fixtrument 
        :: Instrumentation -> Open cc_ -> Closed cc_
    default fixtrument 
        :: ( Generic (Open cc_)
           , Generic (Closed cc_)
           , GFixtrumentable (Self (Closed cc_))
                             (Rep (Open cc_))
                             (Rep (Closed cc_))
           )
       => Instrumentation -> Open cc_ -> Closed cc_
    fixtrument instrumentation cc_ =
        -- Dependency injection by knot-tying.
        let result = to (gFixtrument instrumentation (selfie result) (from cc_))  
         in result

-- A context where the component "beans" have been partially applied with their
-- own dependencies, and are ready to be used in the main application.
type Closed cc_ = cc_ Identity

-- In an open context, each field is actually a function from the
-- yet-to-be-constructed closed context. Instrumentable "beans" read their own
-- dependencies from the yet-to-be-constructed context.
type Open cc_ = cc_ ((->) (Self (Closed cc_)))

class GFixtrumentable final g g' where
    gFixtrument :: Instrumentation -> final -> g x -> g' x

-- I'm assuming that the @Generic@ instances of @SomeRecord Somef@ and
-- @SomeRecord SomeOtherF@ have the same overall "shape" in the trees of the
-- generated @Generic@ representation.
--
-- Otherwise this typeclass won't work!
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


-- Beans that need to instrument their own locally defined (in @let@ or @where@
-- clauses) auxiliary functions can to it through this special newtype (which they
-- must require as a dependency).
--
-- Despite having an 'Instrumentable' instance, this newtype doesn't wrap
-- program logic. Instead, it "remembers" any instrumentations which are applied
-- to it, and then exposes them to other beans.
newtype Instrumentator = Instrumentator { instrumentFunction :: forall curried . InstrumentableFunction curried => FunctionName -> curried -> curried }

makeInstrumentator :: Instrumentator
makeInstrumentator = Instrumentator $ \_ -> id

instance Instrumentable Instrumentator where 
    -- This is a special case of 'Function'. We can't return the inner function
    -- because it's polymorphic. We simply return the type itself.
    type Function Instrumentator = Instrumentator
    -- We rememeber the instrumentations that are applied. They later
    -- can be accessed through 'instrumentFunction'.
    instrument instrumentation (Instrumentator endo) =
        Instrumentator $ \fname f -> instrumentFunction_ instrumentation fname [] (endo fname f)

