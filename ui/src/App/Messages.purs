module App.Messages where

import Prelude

import Pux.Html (Html, text)
import Pux.Html as H
import Pux.Html.Events as E
import Pux.Html.Attributes as A

import App.Action (Action(..))
import App.State (State)

view :: State -> Html Action
view state =
    H.div []
        [ H.form [E.onSubmit (const ServerSend)]
            [ H.input
                [ E.onChange (ChangeMessage <<< _.target.value)
                , A.value state.currentMessage
                ]
                []
            , H.button [] [text "Send"]
            ]
        , H.ul [] (map (\i -> H.li [] [text i]) state.messages)
        ]
