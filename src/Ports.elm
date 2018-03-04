port module Ports
    exposing
        ( generatedElm
        , reportError
        , writeModule
        )

import Json.Decode exposing (Value)


port generatedElm : String -> Cmd msg


port writeModule : Value -> Cmd msg


port reportError : Value -> Cmd msg
