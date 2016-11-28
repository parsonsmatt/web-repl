{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}

module WebRepl.App where

import           Protolude

import Control.Monad.Catch
import           Data.Aeson
import           Data.IORef
import qualified Data.Map                as Map
import           Data.String.Conversions (cs)
import qualified Data.Text               as Text
import qualified Network.WebSockets      as WS
import qualified Text.Megaparsec         as P
import qualified Text.Megaparsec.Lexer   as P
import           Text.Megaparsec.Text    as P

import           WebRepl.Expr
import           WebRepl.Exn
import           WebRepl.Expr.Parser

-- | So, the 'ServerApp' is a type alias for @'PendingConnection' -> 'IO' ()@.
-- We use 'acceptRequest', which sits around waiting for someone to connect to
-- the application. Once we're connected, we receive data on the connect,
-- reverse the text, and then respond back with it forever.
app :: WS.ServerApp
app pending = do
    conn <- WS.acceptRequest pending
    WS.forkPingThread conn 30
    void . forever . flip runStateT mempty $ do
        msg <- liftIO $ WS.receiveData conn
        case decode msg of
            Nothing ->
                sendJSON conn
                    . ReportError
                    . BadRequest
                    . cs
                    $ msg
            Just cmd -> do
                ereply <- runExceptT $ handleCommand cmd
                case ereply of
                    Left err -> pure ()
                    Right reply -> sendJSON conn (PrintStatement reply)

type ServerState = Map Text Integer

handleCommand
    :: (MonadState ServerState m, MonadError ServiceError m)
    => ServerCommand
    -> m Text
handleCommand = \case
    ClearState -> do
        put mempty
        pure "Refs Cleared"
    CompileExpr input -> do
        case P.parse programOrExpr "web" input of
            Left err -> throwError . ParseError . show $ err
            Right app -> do
                case app of
                    Left prog ->
                        Text.unlines <$> interpret prog
                    Right expr ->
                        show <$> evalExpr expr
interpret
    :: (MonadError ServiceError m, MonadState EvalState m)
    => Program
    -> m [Text]
interpret [] =
    pure []
interpret (Assign x expr : xs) = do
    evaled <- evalExpr expr
    modify (Map.insert x evaled)
    interpret xs
interpret (Print x : xs) = do
    evaled <- evalExpr x
    rest <- interpret xs
    pure (show evaled : rest)

convertError :: MonadError e' m => (e -> e') -> m (Either e a) -> m a
convertError f a = either (throwError . f) pure =<< a

sendJSON :: (ToJSON a, MonadIO m) => WS.Connection -> a -> m ()
sendJSON conn =
    liftIO
        . WS.sendTextData conn
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
