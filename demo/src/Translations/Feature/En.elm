module Translations.Feature.En exposing (..)

import Translation exposing (..)
import Translation.En exposing (..)


documentationInfo : Translation { args | documentation : List node -> node } node
documentationInfo =
    fallback "feature.documentationInfo" <|
        concat
            [ s "Please take a look at our "
            , node .documentation "documentation" <|
                s "documentation"
            , s "."
            ]


emailInfo : Translation args node
emailInfo =
    final "feature.emailInfo" <|
        concat
            [ s "You have "
            , cardinal decimalLatnStandard .count "count" <|
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
    final "feature.greeting" <|
        concat
            [ s "Good morning, "
            , string .name "name"
            , s "!"
            ]


missingInDe : Translation args node
missingInDe =
    final "feature.missingInDe" <|
        s "I am not translated to German."
