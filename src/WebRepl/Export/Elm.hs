module WebRepl.Export.Elm where

import           Protolude

import           Elm

import           WebRepl.App

spec :: Spec
spec =
    Spec ["WebRepl", "Types"]
            [ "import Json.Decode exposing (..)"
            , "import Json.Decode.Extra exposing (apply,date)"
            , toElmTypeSource (Proxy :: Proxy ServerCommand)
            , toElmDecoderSource (Proxy :: Proxy ServerCommand)
            , toElmTypeSource (Proxy :: Proxy ServerReply)
            , toElmDecoderSource (Proxy :: Proxy ServerReply)
            , toElmTypeSource (Proxy :: Proxy ServiceError)
            , toElmDecoderSource (Proxy :: Proxy ServiceError)
            ]

writeElmFiles :: MonadIO m => m ()
writeElmFiles = liftIO $ specsToDir [spec] "elm-ui/src"
