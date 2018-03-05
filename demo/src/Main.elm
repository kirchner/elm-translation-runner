module Main exposing (main)

import Html
import Locales exposing (..)
import Translation exposing (asString, asStringWith)
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
        , Html.div []
            [ missingInDe De
                |> asString
                |> Html.text
            ]
        ]
