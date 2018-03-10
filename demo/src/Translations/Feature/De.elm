module Translations.Feature.De exposing (..)

import Translation exposing (..)
import Translation.De exposing (..)


documentationInfo : Translation { args | documentation : List node -> node } node
documentationInfo =
    final "feature.documentationInfo" <|
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
    final "feature.greeting" <|
        concat
            [ s "Guten morgen, "
            , string .name "name"
            , s "!"
            ]


missingInDe : Translation args node
missingInDe =
    fallback "feature.missingInDe" <|
        s "I am not translated to German."


teamInfo : Translation args node
teamInfo =
    final "feature.teamInfo" <|
        concat
            [ s "Our Team: "
            , list and
                [ s "Alice"
                , s "Bob"
                , s "Cindy"
                ]
            ]
