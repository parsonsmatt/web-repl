{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}

module WebRepl.App where

import           Protolude

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
import           WebRepl.Expr.Parser

-- | So, the 'ServerApp' is a type alias for @'PendingConnection' -> 'IO' ()@.
-- We use 'acceptRequest', which sits around waiting for someone to connect to
-- the application. Once we're connected, we receive data on the connect,
-- reverse the text, and then respond back with it forever.
app :: WS.ServerApp
app pending = do
    conn <- WS.acceptRequest pending
    WS.forkPingThread conn 30
    varRef <- newIORef mempty
    forever $ do
        msg <- WS.receiveData conn
        case decode msg of
            Nothing ->
                sendJSON conn
                    . ReportError
                    . BadRequest
                    . cs
                    $ msg
            Just cmd -> do
                reply <- handleCommand cmd varRef
                sendJSON conn reply

type ServerState = Map Text Integer

handleCommand :: ServerCommand -> IORef ServerState -> IO ServerReply
handleCommand ClearState ref = do
    writeIORef ref mempty
    pure (PrintStatement "Refs Cleared")
handleCommand (CompileExpr input) ref = do
    case P.parse programOrExpr "web" input of
        Left err -> pure . ReportError . ParseError . show $ err
        Right app -> do
            vars <- readIORef ref
            case app of
                Left prog -> do
                    let Just (result, vars') = interpret vars prog
                    writeIORef ref vars'
                    pure
                        . PrintStatement
                        . show
                        $ result
                Right expr -> do
                    case evalExpr expr vars of
                        Nothing -> pure
                            . ReportError
                            . BadRequest
                            $ "Evaluation failed"
                        Just answer -> pure
                            . PrintStatement
                            . show
                            $ answer

interpret :: ServerState -> Program -> Maybe ([Text], ServerState)
interpret vars [] =
    pure ([], vars)
interpret vars (Assign x expr : xs) = do
    evaled <- evalExpr expr vars
    interpret (Map.insert x evaled vars) xs
interpret vars (Print x : xs) = do
    evaled <- evalExpr x vars
    (rest, vars') <- interpret vars xs
    pure (show evaled : rest, vars')

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
    = ParseError Text
    | BadRequest Text
    deriving (Eq, Show, Generic, ToJSON)
