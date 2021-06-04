{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DefaultSignatures #-}

module Distribution.Client.Utils.Inspectable
    (
        J.Value (..),
        Inspectable(..)
    ) where

import Distribution.Client.Compat.Prelude
import qualified Distribution.Client.Utils.Json as J
import GHC.Generics

-- Like ToJSON, but focused on allowing the inspection of values while
-- instrumenting functions. This warrants different instances.  
--
-- For example, functions could have an instance which basically said "this is
-- a function", serialized ADTs could always include the name of its type for
-- the sake of legibility, and so on.
class Inspectable a where
    toInspectionJSON :: a -> J.Value
    -- If we care about proper encapsulation in our datatypes, would deriving
    -- through Generic be discouraged?
    default toInspectionJSON :: (Generic a, GInspectable (Rep a)) => a -> J.Value
    toInspectionJSON a = gInspectable (from a)
    

instance Inspectable (a -> b) where 
    toInspectionJSON _ = J.object ["function" J..= J.Null]

instance Inspectable (IO a) where 
    toInspectionJSON _ = J.object ["IO" J..= J.Null]


instance Inspectable ()      where toInspectionJSON = J.toJSON
instance Inspectable Bool    where toInspectionJSON = J.toJSON

-- Workaround for the String and [a] overlapping instances, without using
-- overlapping instance pragmas.
instance InspectableString (IsTheElementChar a) a => Inspectable [a] where 
    toInspectionJSON = toInspectionJSONString (Proxy :: Proxy (IsTheElementChar a))

class InspectableString c a where
    toInspectionJSONString :: Proxy c -> [a] -> J.Value

type family IsTheElementChar a where
    IsTheElementChar Char = Char
    IsTheElementChar _ = ()

instance InspectableString Char Char where
    toInspectionJSONString _ = J.String

instance Inspectable a => InspectableString () a where
    toInspectionJSONString _ = J.Array . map toInspectionJSON
--

instance Inspectable a => Inspectable (Maybe a) where 
  toInspectionJSON Nothing  = J.Null
  toInspectionJSON (Just a) = toInspectionJSON a

instance (Inspectable a,
          Inspectable b) => Inspectable (a,b) where 
  toInspectionJSON (a,b) = J.Array [toInspectionJSON a, toInspectionJSON b]

instance (Inspectable a,
          Inspectable b,
          Inspectable c) => Inspectable (a,b,c) where 
  toInspectionJSON (a,b,c) = J.Array [toInspectionJSON a, toInspectionJSON b, toInspectionJSON c]

instance (Inspectable a,
          Inspectable b,
          Inspectable c, 
          Inspectable d) => Inspectable (a,b,c,d) where 
  toInspectionJSON (a,b,c,d) = J.Array [toInspectionJSON a, toInspectionJSON b, toInspectionJSON c, toInspectionJSON d]

instance Inspectable Float   where toInspectionJSON = J.toJSON
instance Inspectable Double  where toInspectionJSON = J.toJSON
instance Inspectable Int     where toInspectionJSON = J.toJSON
instance Inspectable Int8    where toInspectionJSON = J.toJSON
instance Inspectable Int16   where toInspectionJSON = J.toJSON
instance Inspectable Int32   where toInspectionJSON = J.toJSON
instance Inspectable Word    where toInspectionJSON = J.toJSON
instance Inspectable Word8   where toInspectionJSON = J.toJSON
instance Inspectable Word16  where toInspectionJSON = J.toJSON
instance Inspectable Word32  where toInspectionJSON = J.toJSON
instance Inspectable Int64   where toInspectionJSON = J.toJSON
instance Inspectable Word64  where toInspectionJSON = J.toJSON
instance Inspectable Integer where toInspectionJSON = J.toJSON

--
-- Generic instance for records.

class GInspectable g where
    gInspectable :: g x -> J.Value

instance (Datatype meta, GInspectableBranch branch)
    => GInspectable (D1 meta branch) where
    -- Should we add here the type of the object in a separate field, as well?
    -- Currently, only the name of the constructor, like with Show.
    gInspectable (M1 branch) = J.object [gInspectableBranch branch]
        
class GInspectableBranch g where
    gInspectableBranch :: g x -> J.Pair

instance ( GInspectableBranch t1,
           GInspectableBranch t2 
         ) =>
         GInspectableBranch (t1 :+: t2)
  where
    gInspectableBranch g = case g of
        L1 l -> gInspectableBranch l
        R1 r -> gInspectableBranch r

instance (Constructor meta, GInspectableFields fields) => 
    GInspectableBranch (C1 meta fields) where
    gInspectableBranch r@(M1 g) =
        let fields = gInspectableFields g []
            areFieldsAnonymous = all (null . fst) fields
            content = 
                if areFieldsAnonymous 
                    then J.Array (map snd fields)
                    else J.object fields
         in (conName r, content)

-- Difference list to avoid potential problems with records with
-- huge numbers of fields.
class GInspectableFields g where
    gInspectableFields :: g x -> J.Object -> J.Object

instance (Selector meta,
          Inspectable v) =>
          GInspectableFields (S1 meta (Rec0 v)) 
  where
    gInspectableFields field@(M1 (K1 v)) d = 
        -- selName will be blank if the field is anonymous
        d ++ [ selName field J..= toInspectionJSON v ]

instance ( GInspectableFields t1,
           GInspectableFields t2 
         ) =>
         GInspectableFields (t1 :*: t2)
  where
    gInspectableFields (t1 :*: t2) d = 
        gInspectableFields t2 $ gInspectableFields t1 $ d


