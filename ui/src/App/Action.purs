module App.Action where

import App.Routes (Route)

data Action
    = PageView Route
    | ServerSend
    | ServerRecv String
    | ChangeMessage String
    | Noop
