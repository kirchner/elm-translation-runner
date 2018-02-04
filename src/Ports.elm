port module Ports exposing (generatedElm, writeModule)

import Json.Decode exposing (Value)


port generatedElm : String -> Cmd msg


port writeModule : Value -> Cmd msg
