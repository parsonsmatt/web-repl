module App.Layout where

import Prelude

import Data.Either
import Data.Maybe
import Data.Array as Array
import Control.Monad.Eff
import Control.Monad.Eff.Exception
import Control.Monad.Eff.Class
import Pux
import Debug.Trace as Trace
import Pux.Html (Html, div, h1, p, text)
import Pux.Html as H
import Pux.Html.Events as E
import Pux.Html.Attributes as A
import Control.Monad.Aff
import Unsafe.Coerce
import Signal.Channel
import DOM (DOM)
import WebSocket
import Data.Argonaut.Generic.Aeson as JSON
import Data.Argonaut.Parser as JSON
import Data.Generic

import App.NotFound as NotFound
import App.Routes (Route(Home, NotFound))
import App.Effects (AppEffects, AllEffects)
import WebRepl.App
import WebRepl.Exn
import App.Action
import App.State
import App.Messages as Messages

init :: State
init =
    { route: Home
    , currentMessage: ""
    , messages: []
    , sendSocket: \_ -> pure unit
    }

update
    :: Action
    -> State
    -> EffModel State Action AppEffects
update Noop state = noEffects state
update (PageView route) state =
    noEffects $ state { route = route }
update (ServerRecv str) state =
    noEffects case JSON.decodeJson =<< JSON.jsonParser str of
         Left _ ->
            state
         Right a ->
            state
                { messages =
                    Array.cons (renderReply a) state.messages
                }
update ServerSend state =
    { state: state { currentMessage = "" }
    , effects: [ do
        liftEff
            $ state.currentMessage
            # CompileExpr
            # JSON.encodeJson
            # show
            # state.sendSocket
        pure Noop
        ]
    }
update (ChangeMessage msg) state =
    noEffects state { currentMessage = msg }

renderReply :: ServerReply -> String
renderReply (PrintStatement str) = str
renderReply (ReportError svc) = "Error: " <> case svc of
                                                  ParseError err -> err
                                                  BadRequest err -> err
                                                  EvalError err -> gShow err



view :: State -> Html Action
view state =
    div
        []
        [ h1 [] [ text "My Starter App" ]
        , p [] [ text "Change src/App/Layout.purs and watch me hot-reload." ]
        , case state.route of
               Home -> Messages.view state
               NotFound -> NotFound.view state
        ]
