module Translations.Feature.En exposing (..)

import Cldr.En exposing (..)
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
        [ s "You have "
        , cardinal .count decimalLatnStandard [] <|
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


greeting : Text Static { args | name : String } node
greeting =
    concat
        [ s "Good morning, "
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
