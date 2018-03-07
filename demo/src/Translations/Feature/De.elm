module Translations.Feature.De exposing (..)

import Translation exposing (..)
import Translation.De exposing (..)


emailInfo : Translation { args | count : Float } node
emailInfo =
    final "emailInfo" <|
        concat
            [ s "Du hast "
            , cardinal decimalStandard .count "count" <|
                { one =
                    concat
                        [ count
                        , s " neue Email"
                        ]
                , other =
                    concat
                        [ count
                        , s " neue Emails"
                        ]
                }
            , s "."
            ]


greeting : Translation { args | name : String } node
greeting =
    final "greeting" <|
        concat
            [ s "Guten morgen, "
            , string .name "name"
            , s "!"
            ]


missingInDe : Translation args node
missingInDe =
    fallback "missingInDe" <|
        s "I am not translated to German."
