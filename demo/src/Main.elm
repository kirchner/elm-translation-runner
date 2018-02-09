module Main exposing (main)

import Html
import Translation exposing (asStringWith)
import Translations.Feature exposing (..)
import Translations.Locales exposing (..)


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
