module Demo exposing (main)

import Html
import Translation exposing (asStringWith)
import Translations.Feature exposing (..)


main =
    Html.div
        []
        [ Html.div []
            [ greeting
                |> asStringWith { name = "Alice" }
                |> Html.text
            ]
        , Html.div []
            [ emailInfo
                |> asStringWith { count = 1 }
                |> Html.text
            ]
        ]
