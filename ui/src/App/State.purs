module App.State where

import Prelude

import Control.Monad.Eff

import App.Routes
import App.Effects

type State =
    { route :: Route
    , currentMessage :: String
    , messages :: Array String
    , sendSocket :: String -> Eff AllEffects Unit
    }

