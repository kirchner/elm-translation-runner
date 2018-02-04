module Demo exposing (main)

import Html
import Translation exposing (asStringWith)
import Translations.Branches.Conflicts exposing (..)


main =
    conflictCount
        |> asStringWith { count = 1 }
        |> Html.text
