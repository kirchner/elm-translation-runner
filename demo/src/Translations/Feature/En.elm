module Translations.Feature.En exposing (..)

import Translation exposing (..)
import VirtualDom exposing (Node)
import Translation.En exposing (..)


emailInfo : Translation { args | count : Float } msg
emailInfo =
    final "emailInfo" <|
        concat
            [ s "You have "
            , cardinal decimalStandard .count "count" <|
                { one =
                    concat
                        [ count
                        , s " new email"
                        ]
                , other =
                    concat
                        [ count
                        , s " new emails"
                        ]
                }
            , s "."
            ]


greeting : Translation { args | name : String } msg
greeting =
    final "greeting" <|
        concat
            [ s "Good morning, "
            , string .name "name"
            , s "!"
            ]