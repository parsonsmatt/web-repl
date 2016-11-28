{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}

module WebRepl.Exn where

import           Protolude

import           Data.Aeson

data ServiceError
    = ParseError Text
    | BadRequest Text
    | EvalError EvalError
    deriving (Eq, Show, Generic, ToJSON)

data EvalError = VarNotFound Text
    deriving (Show, Eq, Generic, ToJSON)
