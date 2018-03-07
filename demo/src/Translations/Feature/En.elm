module Translations.Feature.En exposing (..)

import Translation exposing (..)
import Translation.En exposing (..)


emailInfo : Translation { args | count : Float } node
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


greeting : Translation { args | name : String } node
greeting =
    final "greeting" <|
        concat
            [ s "Good morning, "
            , string .name "name"
            , s "!"
            ]


missingInDe : Translation args node
missingInDe =
    final "missingInDe" <|
        s "I am not translated to German."
