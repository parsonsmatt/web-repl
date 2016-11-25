{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}

module WebRepl.App where

import           Protolude

import           Data.Aeson
import           Data.String.Conversions (cs)
import qualified Data.Text               as Text
import qualified Network.WebSockets      as WS

-- | So, the 'ServerApp' is a type alias for @'PendingConnection' -> 'IO' ()@.
-- We use 'acceptRequest', which sits around waiting for someone to connect to
-- the application. Once we're connected, we receive data on the connect,
-- reverse the text, and then respond back with it forever.
app :: WS.ServerApp
app pending = do
    conn <- WS.acceptRequest pending
    WS.forkPingThread conn 30
    forever $ do
        msg <- WS.receiveData conn
        case decode msg of
            Nothing ->
                sendJSON conn
                    . ReportError
                    . BadRequest
                    . cs
                    $ msg
            Just cmd ->
                case cmd of
                    ClearState ->
                        pure ()
                    CompileExpr expr ->
                        sendJSON conn
                            . ReportError
                            $ ParseError

sendJSON :: ToJSON a => WS.Connection -> a -> IO ()
sendJSON conn =
    WS.sendTextData conn
        . encode
        . toJSON

data ServerCommand
    = CompileExpr Text
    | ClearState
    deriving (Eq, Show, Generic, FromJSON)

data ServerReply
    = PrintStatement Text
    | ReportError ServiceError
    deriving (Eq, Show, Generic, ToJSON)

data ServiceError
    = ParseError
    | BadRequest Text
    deriving (Eq, Show, Generic, ToJSON)
