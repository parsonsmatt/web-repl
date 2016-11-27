-- | This modules contains the top level definitions for exporting PureScript
-- types for the Haskell data types.
module WebRepl.Export.PureScript where

import           Protolude

import           Language.PureScript.Bridge

import           WebRepl.App

writePureScript :: MonadIO m => m ()
writePureScript = liftIO $ writePSTypes "ui/src" (buildBridge defaultBridge)
    [ mkSumType (Proxy :: Proxy ServerCommand)
    , mkSumType (Proxy :: Proxy ServerReply)
    , mkSumType (Proxy :: Proxy ServiceError)
    ]
