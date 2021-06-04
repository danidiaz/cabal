{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
-- {-# LANGUAGE StandaloneDeriving #-}
-- {-# LANGUAGE DeriveGeneric #-}
-- {-# LANGUAGE DeriveAnyClass #-}
-- {-# LANGUAGE DerivingStrategies #-}

module Distribution.Simple.Utils.Inspectable
    (
      Value (..)
    , Object
    , object 
    , Pair 
    , (.=)
    , Inspectable(..)
    , InspectableString
    , IsTheElementChar
    , encodeToString
    , encodeToBuilder
    ) where

-- Importing Compat.Prelude causes a cycle involving Distribution.Compat.Semigroup
-- import Prelude 
import Distribution.Compat.Prelude
import Distribution.Compat.Semigroup
import qualified Distribution.Compat.NonEmptySet (NonEmptySet)
import qualified Distribution.Compat.NonEmptySet as NonEmptySet
import GHC.Generics
import Data.Char (intToDigit)
import Data.ByteString.Builder (Builder)
import qualified Data.ByteString.Builder as BB
import qualified Data.Map as Map
import qualified Data.Set as Set

-- Like ToJSON, but focused on allowing the inspection of values while
-- instrumenting functions. This warrants different instances.  
--
-- For example, functions could have an instance which basically said "this is
-- a function". 
class Inspectable a where
    toInspectionJSON :: a -> Value
    -- If we care about proper encapsulation in our datatypes, would deriving
    -- through Generic be discouraged?
    default toInspectionJSON :: (Generic a, GInspectable (Rep a)) => a -> Value
    toInspectionJSON a = gInspectable (from a)
    

instance Inspectable (a -> b) where 
    toInspectionJSON _ = object [("function",Null)]

instance Inspectable (IO a) where 
    toInspectionJSON _ = object [("IO", Null)]


instance Inspectable ()      where toInspectionJSON () = Array []
instance Inspectable Bool    where toInspectionJSON = Bool

-- Workaround for the String and [a] overlapping instances, without using
-- overlapping instance pragmas.
instance InspectableString (IsTheElementChar a) a => Inspectable [a] where 
    toInspectionJSON = toInspectionJSONString (Proxy :: Proxy (IsTheElementChar a))

class InspectableString c a where
    toInspectionJSONString :: Proxy c -> [a] -> Value

type family IsTheElementChar a where
    IsTheElementChar Char = Char
    IsTheElementChar _ = ()

instance InspectableString Char Char where
    toInspectionJSONString _ = String

instance Inspectable a => InspectableString () a where
    toInspectionJSONString _ = Array . map toInspectionJSON
--

instance Inspectable a => Inspectable (Maybe a) where 
  toInspectionJSON Nothing  = Null
  toInspectionJSON (Just a) = toInspectionJSON a

instance (Inspectable a,
          Inspectable b) => Inspectable (a,b) where 
  toInspectionJSON (a,b) = Array [toInspectionJSON a, toInspectionJSON b]

instance (Inspectable a,
          Inspectable b,
          Inspectable c) => Inspectable (a,b,c) where 
  toInspectionJSON (a,b,c) = Array [toInspectionJSON a, toInspectionJSON b, toInspectionJSON c]

instance (Inspectable a,
          Inspectable b,
          Inspectable c, 
          Inspectable d) => Inspectable (a,b,c,d) where 
  toInspectionJSON (a,b,c,d) = Array [toInspectionJSON a, toInspectionJSON b, toInspectionJSON c, toInspectionJSON d]

instance (Inspectable k, Inspectable v) => Inspectable (Map.Map k v) where
  toInspectionJSON m = object ["map" .= Map.toList m]

instance (Inspectable v, InspectableString (IsTheElementChar v) v) 
    => Inspectable (Set.Set v) where
  toInspectionJSON m = object ["set" .= Set.toList m]

instance (Inspectable v, InspectableString (IsTheElementChar v) v) 
    => Inspectable (NonEmptySet v) where
  toInspectionJSON m = object ["set" .= NonEmptySet.toList m]

instance Inspectable x => Inspectable (Last' x) where
  toInspectionJSON (Last' x) = object ["getLast'" .= x]

instance Inspectable x => Inspectable (Option' x) where
  toInspectionJSON (Option' x) = toInspectionJSON x

instance Inspectable Float   where
  toInspectionJSON = Number . realToFrac
instance Inspectable Double  where 
  toInspectionJSON = Number
instance Inspectable Int     where 
  toInspectionJSON = Number . realToFrac 
instance Inspectable Int8    where
  toInspectionJSON = Number . realToFrac 
instance Inspectable Int16   where
  toInspectionJSON = Number . realToFrac 
instance Inspectable Int32   where
  toInspectionJSON = Number . realToFrac 
instance Inspectable Word    where
  toInspectionJSON = Number . realToFrac 
instance Inspectable Word8   where
  toInspectionJSON = Number . realToFrac 
instance Inspectable Word16  where
  toInspectionJSON = Number . realToFrac 
instance Inspectable Word32  where
  toInspectionJSON = Number . realToFrac 
-- | Possibly lossy due to conversion to 'Double'
instance Inspectable Int64   where
  toInspectionJSON = Number . realToFrac 
-- | Possibly lossy due to conversion to 'Double'
instance Inspectable Word64  where
  toInspectionJSON = Number . realToFrac 
-- | Possibly lossy due to conversion to 'Double'
instance Inspectable Integer where
  toInspectionJSON = Number . realToFrac 

--
-- Generic instance for records.

class GInspectable g where
    gInspectable :: g x -> Value

instance (Datatype meta, GInspectableBranch branch)
    => GInspectable (D1 meta branch) where
    -- Should we add here the type of the object in a separate field, as well?
    -- Currently, only the name of the constructor, like with Show.
    gInspectable (M1 branch) = object [gInspectableBranch branch]
        
class GInspectableBranch g where
    gInspectableBranch :: g x -> Pair

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
                    then Array (map snd fields)
                    else object fields
         in (conName r, content)

-- Difference list to avoid potential problems with records with
-- huge numbers of fields.
class GInspectableFields g where
    gInspectableFields :: g x -> Object -> Object

instance GInspectableFields U1
  where
    gInspectableFields U1 d = d

instance (Selector meta,
          Inspectable v) =>
          GInspectableFields (S1 meta (Rec0 v)) 
  where
    gInspectableFields field@(M1 (K1 v)) d = 
        -- selName will be blank if the field is anonymous
        d ++ [ selName field .= v ]

instance ( GInspectableFields t1,
           GInspectableFields t2 
         ) =>
         GInspectableFields (t1 :*: t2)
  where
    gInspectableFields (t1 :*: t2) d = 
        gInspectableFields t2 $ gInspectableFields t1 $ d

--
--
--


-- Imports from Cabal
-- import Distribution.Simple.Setup                         
--        (BenchmarkFlags, HaddockFlags, TestFlags)
-- import Distribution.Client.ProjectFlags
--        (ProjectFlags (..))
-- import Distribution.Client.Setup
--        (ConfigExFlags, ConfigFlags (..), InstallFlags (..))
-- import Distribution.Compat.Semigroup (Option',Last')     
-- import Distribution.Utils.NubList (NubList)
-- import Distribution.Simple.InstallDirs (InstallDirs)
-- import Distribution.Simple.Flag (Flag)
-- import Distribution.Simple.Program.Db (ProgramDb)
-- import Distribution.Compiler (CompilerFlavor)
-- import Distribution.Simple.Compiler (ProfDetailLevel, OptimisationLevel, DebugInfoLevel)
-- import Distribution.Simple.InstallDirs (PathTemplate)
-- import Distribution.Types.ComponentId (ComponentId)
-- import Distribution.Verbosity (Verbosity)
-- import Distribution.Simple.Compiler (PackageDB)
-- import Distribution.Types.PackageVersionConstraint (PackageVersionConstraint)
-- import Distribution.Types.GivenComponent (GivenComponent)
-- import Distribution.ModuleName (ModuleName)
-- import Distribution.Types.Module (Module)
-- import Distribution.Types.Flag (FlagAssignment)
--
--
-- Instances for classes from Cabal
--deriving anyclass instance Inspectable ConfigFlags
--deriving anyclass instance Inspectable a => Inspectable (Option' a)
--deriving anyclass instance Inspectable a => Inspectable (Last' a)
--deriving anyclass instance Inspectable a => Inspectable (NubList a)
--deriving anyclass instance Inspectable a => Inspectable (InstallDirs a)
--deriving anyclass instance Inspectable a => Inspectable (Flag a)
--deriving anyclass instance Inspectable ProgramDb
--deriving anyclass instance Inspectable CompilerFlavor
--deriving anyclass instance Inspectable ProfDetailLevel
--deriving anyclass instance Inspectable OptimisationLevel
--deriving anyclass instance Inspectable PathTemplate
--deriving anyclass instance Inspectable ComponentId
--deriving anyclass instance Inspectable Verbosity
--deriving anyclass instance Inspectable PackageDB
--deriving anyclass instance Inspectable PackageVersionConstraint
--deriving anyclass instance Inspectable GivenComponent
--deriving anyclass instance Inspectable ModuleName
--deriving anyclass instance Inspectable Module
--
--deriving anyclass instance Inspectable FlagAssignment -- map?
--deriving anyclass instance Inspectable DebugInfoLevel

--
--
--

-- Mostly a copy of Distribution.Client.Utils.Json
--
-- TODO: We may want to replace 'String' with 'Text' or 'ByteString'

-- | A JSON value represented as a Haskell value.
data Value = Object Object
           | Array  [Value]
           | String  String
           | Number !Double
           | Bool   !Bool
           | Null
           deriving (Eq, Read, Show)

-- | A key\/value pair for an 'Object'
type Pair = (String, Value)

-- | A JSON \"object\" (key/value map).
type Object = [Pair]

infixr 8 .=

-- | A key-value pair for encoding a JSON object.
(.=) :: Inspectable v => String -> v -> Pair
k .= v  = (k, toInspectionJSON v)

-- | Create a 'Value' from a list of name\/value 'Pair's.
object :: [Pair] -> Value
object = Object

instance IsString Value where
  fromString = String


-- -- | A type that can be converted to JSON.
-- class ToJSON a where
--   -- | Convert a Haskell value to a JSON-friendly intermediate type.
--   toJSON :: a -> Value

-- instance ToJSON () where
--   toJSON () = Array []
-- 
-- instance ToJSON Value where
--   toJSON = id
-- 
-- instance ToJSON Bool where
--   toJSON = Bool

-- instance ToJSON a => ToJSON [a] where
--   toJSON = Array . map toJSON

-- instance ToJSON a => ToJSON (Maybe a) where
--   toJSON Nothing  = Null
--   toJSON (Just a) = toJSON a
-- 
-- instance (ToJSON a,ToJSON b) => ToJSON (a,b) where
--   toJSON (a,b) = Array [toJSON a, toJSON b]
-- 
-- instance (ToJSON a,ToJSON b,ToJSON c) => ToJSON (a,b,c) where
--   toJSON (a,b,c) = Array [toJSON a, toJSON b, toJSON c]
-- 
-- instance (ToJSON a,ToJSON b,ToJSON c, ToJSON d) => ToJSON (a,b,c,d) where
--   toJSON (a,b,c,d) = Array [toJSON a, toJSON b, toJSON c, toJSON d]

-- instance ToJSON Float where
--   toJSON = Number . realToFrac
-- 
-- instance ToJSON Double where
--   toJSON = Number
-- 
-- instance ToJSON Int    where  toJSON = Number . realToFrac
-- instance ToJSON Int8   where  toJSON = Number . realToFrac
-- instance ToJSON Int16  where  toJSON = Number . realToFrac
-- instance ToJSON Int32  where  toJSON = Number . realToFrac
-- 
-- instance ToJSON Word   where  toJSON = Number . realToFrac
-- instance ToJSON Word8  where  toJSON = Number . realToFrac
-- instance ToJSON Word16 where  toJSON = Number . realToFrac
-- instance ToJSON Word32 where  toJSON = Number . realToFrac

-- -- | Possibly lossy due to conversion to 'Double'
-- instance ToJSON Int64  where  toJSON = Number . realToFrac
-- 
-- -- | Possibly lossy due to conversion to 'Double'
-- instance ToJSON Word64 where  toJSON = Number . realToFrac
-- 
-- -- | Possibly lossy due to conversion to 'Double'
-- instance ToJSON Integer where toJSON = Number . fromInteger

------------------------------------------------------------------------------
-- 'BB.Builder'-based encoding

-- | Serialise value as JSON/UTF8-encoded 'Builder'
encodeToBuilder :: Inspectable a => a -> Builder
encodeToBuilder = encodeValueBB . toInspectionJSON

encodeValueBB :: Value -> Builder
encodeValueBB jv = case jv of
  Bool True  -> "true"
  Bool False -> "false"
  Null       -> "null"
  Number n
    | isNaN n || isInfinite n   -> encodeValueBB Null
    | Just i <- doubleToInt64 n -> BB.int64Dec i
    | otherwise                 -> BB.doubleDec n
  Array a  -> encodeArrayBB a
  String s -> encodeStringBB s
  Object o -> encodeObjectBB o

encodeArrayBB :: [Value] -> Builder
encodeArrayBB [] = "[]"
encodeArrayBB jvs = BB.char8 '[' <> go jvs <> BB.char8 ']'
  where
    go = mconcat . intersperse (BB.char8 ',') . map encodeValueBB

encodeObjectBB :: Object -> Builder
encodeObjectBB [] = "{}"
encodeObjectBB jvs = BB.char8 '{' <> go jvs <> BB.char8 '}'
  where
    go = mconcat . intersperse (BB.char8 ',') . map encPair
    encPair (l,x) = encodeStringBB l <> BB.char8 ':' <> encodeValueBB x

encodeStringBB :: String -> Builder
encodeStringBB str = BB.char8 '"' <> go str <> BB.char8 '"'
  where
    go = BB.stringUtf8 . escapeString

------------------------------------------------------------------------------
-- 'String'-based encoding

-- | Serialise value as JSON-encoded Unicode 'String'
encodeToString :: Inspectable a => a -> String
encodeToString jv = encodeValue (toInspectionJSON jv) []

encodeValue :: Value -> ShowS
encodeValue jv = case jv of
  Bool b   -> showString (if b then "true" else "false")
  Null     -> showString "null"
  Number n
    | isNaN n || isInfinite n    -> encodeValue Null
    | Just i <- doubleToInt64 n -> shows i
    | otherwise                 -> shows n
  Array a -> encodeArray a
  String s -> encodeString s
  Object o -> encodeObject o

encodeArray :: [Value] -> ShowS
encodeArray [] = showString "[]"
encodeArray jvs = ('[':) . go jvs . (']':)
  where
    go []     = id
    go [x]    = encodeValue x
    go (x:xs) = encodeValue x . (',':) . go xs

encodeObject :: Object -> ShowS
encodeObject [] = showString "{}"
encodeObject jvs = ('{':) . go jvs . ('}':)
  where
    go []          = id
    go [(l,x)]     = encodeString l . (':':) . encodeValue x
    go ((l,x):lxs) = encodeString l . (':':) . encodeValue x . (',':) . go lxs

encodeString :: String -> ShowS
encodeString str = ('"':) . showString (escapeString str) . ('"':)

------------------------------------------------------------------------------
-- helpers

-- | Try to convert 'Double' into 'Int64', return 'Nothing' if not
-- representable loss-free as integral 'Int64' value.
doubleToInt64 :: Double -> Maybe Int64
doubleToInt64 x
  | fromInteger x' == x
  , x' <= toInteger (maxBound :: Int64)
  , x' >= toInteger (minBound :: Int64)
    = Just (fromIntegral x')
  | otherwise = Nothing
  where
    x' = round x

-- | Minimally escape a 'String' in accordance with RFC 7159, "7. Strings"
escapeString :: String -> String
escapeString s
  | not (any needsEscape s) = s
  | otherwise               = escape s
  where
    escape [] = []
    escape (x:xs) = case x of
      '\\' -> '\\':'\\':escape xs
      '"'  -> '\\':'"':escape xs
      '\b' -> '\\':'b':escape xs
      '\f' -> '\\':'f':escape xs
      '\n' -> '\\':'n':escape xs
      '\r' -> '\\':'r':escape xs
      '\t' -> '\\':'t':escape xs
      c | ord c < 0x10 -> '\\':'u':'0':'0':'0':intToDigit (ord c):escape xs
        | ord c < 0x20 -> '\\':'u':'0':'0':'1':intToDigit (ord c - 0x10):escape xs
        | otherwise    -> c : escape xs

    -- unescaped = %x20-21 / %x23-5B / %x5D-10FFFF
    needsEscape c = ord c < 0x20 || c `elem` ['\\','"']


