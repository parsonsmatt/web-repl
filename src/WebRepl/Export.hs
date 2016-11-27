-- | This modules contains the top level definitions for exporting PureScript
-- types for the Haskell data types.
module WebRepl.Export where

import           Protolude

import           Language.PureScript.Bridge

import           WebRepl.App

writeFile :: MonadIO m => m ()
writeFile = liftIO $ writePSTypes "ui/src" (buildBridge defaultBridge)
    [ mkSumType (Proxy :: Proxy ServerCommand)
    , mkSumType (Proxy :: Proxy ServerReply)
    , mkSumType (Proxy :: Proxy ServiceError)
    ]
