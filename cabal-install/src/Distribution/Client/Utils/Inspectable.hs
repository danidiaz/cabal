module Distribution.Client.Utils.Inspectable
    (
        module Distribution.Simple.Utils.Inspectable
    ) where

import Distribution.Simple.Utils.Inspectable

import Network.URI (URI (..), uriToString)

instance Inspectable URI where
    toInspectionJSON uri = object ["uri" .= show uri]


