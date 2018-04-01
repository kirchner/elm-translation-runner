module Translations.Feature.De exposing (..)

import Cldr.De
import Cldr.En.US.POSIX
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
        , Cldr.De.cardinal .count Cldr.De.decimalLatnStandard [] <|
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
        , delimited Cldr.En.US.POSIX.quote <|
            s "Hello!"
        ]
