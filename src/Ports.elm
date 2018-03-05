port module Ports
    exposing
        ( fetchFile
        , fileReceived
        , reportError
        , writeModule
        )

import Json.Decode exposing (Value)


port fetchFile : String -> Cmd msg


port fileReceived : (Value -> msg) -> Sub msg


port reportError : Value -> Cmd msg


port writeModule : Value -> Cmd msg
