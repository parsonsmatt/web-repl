module WebRepl.Export where

import           Protolude

import           Language.PureScript.Bridge

import           WebRepl.App

writePureScript :: MonadIO m => m ()
writePureScript = liftIO $ writePSTypes "ui/src" (buildBridge defaultBridge)
    [ mkSumType (Proxy :: Proxy ServerCommand)
    , mkSumType (Proxy :: Proxy ServerReply)
    , mkSumType (Proxy :: Proxy ServiceError)
    ]
