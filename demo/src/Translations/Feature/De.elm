module Translations.Feature.De exposing (..)

import Cldr.De exposing (..)
import Text exposing (..)


documentationInfo : Text Static { args | documentation : List node -> node } node
documentationInfo =
    concat
        [ s "Please take a look at our "
        , node .documentation <|
            s "documentation"
        , s "."
        ]


emailInfo : Text Static { args | count : Float } node
emailInfo =
    concat
        [ s "Du hast "
        , cardinal .count decimalLatnStandard [] <|
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


greeting : Text Static { args | name : String } node
greeting =
    concat
        [ s "Guten morgen, "
        , string .name
        , s "!"
        ]


missingInDe : Text Static args node
missingInDe =
    s "I am not translated to German."


quotation : Text Static args node
quotation =
    concat
        [ s "They said: "
        , delimited quote <|
            s "Hello!"
        ]
