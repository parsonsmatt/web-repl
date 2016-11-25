module App.Compile where

import Prelude

import Data.Generic

data ServerCommand
    = CompileExpr String
    | ClearState

derive instance genericServerCommand :: Generic ServerCommand

data ServerReply
    = PrintStatement String
    | ReportError ServiceError

derive instance genericServerReply :: Generic ServerReply

data ServiceError
    = ParseError
    | BadRequest String

derive instance genericServiceError :: Generic ServiceError
