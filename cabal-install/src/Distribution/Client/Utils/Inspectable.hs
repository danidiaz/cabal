{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
module Distribution.Client.Utils.Inspectable
    (
        module Distribution.Simple.Utils.Inspectable
    ) where


import Distribution.Simple.Utils.Inspectable
import Network.URI (URI (..), uriToString)
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NonEmpty

instance Inspectable URI where
    toInspectionJSON uri = object ["uri" .= show uri]


instance InspectableString (IsTheElementChar a) a => Inspectable (NonEmpty a) where
    toInspectionJSON ne = toInspectionJSON (NonEmpty.toList ne)


