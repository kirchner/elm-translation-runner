module Demo exposing (main)

import Html
import Locales exposing (..)
import Translation exposing (asStringWith)
import Translations.Feature exposing (..)


main =
    Html.div
        []
        [ Html.div []
            [ greeting De
                |> asStringWith { name = "Alice" }
                |> Html.text
            ]
        , Html.div []
            [ emailInfo De
                |> asStringWith { count = 1 }
                |> Html.text
            ]
        ]
